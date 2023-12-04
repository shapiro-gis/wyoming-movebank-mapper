appRoot<<-getwd()
appRoot<<-dirname(appRoot)

startOther<<-function(appToRun){
    if(appToRun=='app1'){
      runApp(paste0(appRoot,'/app1_dataCleaning/',appToRun,'.R'),launch.browser=T)
    }
    if(appToRun=='app2'){
      runApp(paste0(appRoot,'/app2_mapviewer/',appToRun,'.R'),launch.browser=T)
    }

}

modalMessager <<- function(header, body) {
    showModal(
      modalDialog(
        title = header,
        body,
        footer=modalButton("OK"),
        tags$head(tags$style("#shiny-modal .modal-footer{ display:block}"))
      )
    )
  }

  firstOpen<<-TRUE

  changeToOtherApp<<-function(){
    showModal(modalDialog(
           title="Change to Other App",
           "Choose one of the options below to jump to another app.",
           br(),
           selectInput('changeAppsSelect','choose the app to jump to',choices=appJumpList,selected=whichAppIsRunning),
           fade = FALSE
       ))

       if(exists('openObserver')){
         openObserver$destroy()
         firstOpen<<-TRUE
       }
       openObserver<<-observeEvent(input$changeAppsSelect, {
         appToRun<<-input$changeAppsSelect
         if(firstOpen){
           firstOpen<<-FALSE
         }else{

           onStop(function() {
             startOther(appToRun)
           })
           runjs("window.close(); console.log('closing')")
           stopApp()
         }
       },ignoreInit=TRUE)
  }




  progressIndicator <-function(message,startStop,updateValue){
      if(startStop=="start")  {
        progress <- shiny::Progress$new()
        progress$set(message = message, value = 50)
      }
      else if(startStop=="update"){
        progress$set(message = message, value = updateValue)
      } else {
        progress$set(message = message, value = 100)
        progress$close()
      }
    }


closeApp<<-function(){
  print('closing')
  runjs("window.close(); console.log('closing')")
  stopApp()
}

saveSessionInfo<<-function(){
  sessionInfo<-list()
  sessionInfo$masterWorkingDirectory<-masterWorkingDirectory
  sessionInfo$time<-Sys.time()
  saveTo<-paste0(dirname(getwd()),'//session.rds')
  saveRDS(sessionInfo,saveTo)
}

 
MovebankFolder<<- function(){
  #app_dir <- normalizePath(file.path(masterWorkingDirectory, ".."), winslash = "/")
  #app_dir<-paste0(app_dir,'/movebank/')
  app_dir<-paste0(appRoot,'/movebank/')
  print(app_dir)
}


saveWorkingFile<<-function(){

 # progressIndicator('saving project files','start')
  #loadingScreenToggle('show','saving project files')
  workingFile$masterWorkingDirectory<<-masterWorkingDirectory
  workingFile$importedDatasetMaster<<-importedDatasetMaster
  saveRDS(workingFile,paste0(masterWorkingDirectory,'//workingFile.rds'),)
  dbConnection <<- dbConnect(RSQLite::SQLite(), paste0(masterWorkingDirectory,'//workingDb.db'))
  dbWriteTable(dbConnection, "importedDatasetMaster", importedDatasetMaster@data, overwrite=T)
  importedDatasetMaster@data <- dbGetQuery(dbConnection, "SELECT * FROM importedDatasetMaster")
  #print("Export 1")
 # progressIndicator('saving project files','stop')
  #loadingScreenToggle('hide','')
  saveSessionInfo()
}

loadingScreenToggle<-function(hideShow,msg){
  if(hideShow=='show'){
    html('loadingMessage',msg)
    showElement(id = 'loadingScreen', anim = FALSE)
  }else{
    hideElement(id = 'loadingScreen', anim = FALSE)
  }
}


checkForSession<<-function(fromApp){
  sessionCheckLocation<-paste0(dirname(getwd()),'//session.rds')
  if(file.exists(sessionCheckLocation)){
    sessionInfo<-readRDS(sessionCheckLocation)
    msgInfo<-paste0('A previous session was detected. This session was last active at ',sessionInfo$time,' using the project file stored at ',sessionInfo$masterWorkingDirectory,'. Would you like to reload the data from this session? If you click "no, clear session", you will need to choose a different project folder. This will not delete project files.')
    showModal(modalDialog(
           title="Reload previous session?",
           msgInfo,
           footer = tagList(actionButton("confirmReload", "Yes Reload Previous Session"),
                            actionButton("clearSession", "No, clear session")
           )
       ))

       observeEvent(input$confirmReload, {
         #print('reloading session')
         w <- Waiter$new(
           html = tagList(
             spin_3(),
             h4("Loading data...", style = "color: grey") # Add style attribute to h4 element
           ),
           color = transparent(.5)
         )

         w$show()
        # loadingScreenToggle('show','reloading session')
         if(fromApp=='app1'){
           appOneReload(paste0(sessionInfo$masterWorkingDirectory))
         }
         w$hide()
     })

     observeEvent(input$clearSession, {
       file.remove(sessionCheckLocation)
       removeModal()
       print('clearing session')
   })

  }
}

loadDependencies<-function(dependencies){
  for(i in 1:length(dependencies)){
    if(dependencies[i] %in% installed.packages()==FALSE){
      install.packages(dependencies[i])
      require(dependencies[i],character.only=TRUE)
    } else{
      require(dependencies[i],character.only=TRUE)
    }
  }
}




find_UTM_zone <- function(longitude, latitude) {

  if("numeric" %in% class(longitude) == FALSE)
    stop("longitude is not a numeric value or vector")
  if("numeric" %in% class(latitude) == FALSE)
    stop("latitude is not a numeric value or vector")
  if(length(longitude)!=length(latitude))
    stop("Your longitude and latitude are not the same length")

  if(length(longitude)==1){
    # Special zones for Svalbard and Norway
    if (latitude >= 72.0 && latitude < 84.0 )
      if (longitude >= 0.0  && longitude <  9.0)
        return(31);
    if (longitude >= 9.0  && longitude < 21.0)
      return(33)
    if (longitude >= 21.0 && longitude < 33.0)
      return(35)
    if (longitude >= 33.0 && longitude < 42.0)
      return(37)
    (floor((longitude + 180) / 6) %% 60) + 1
  }else{
    # loop through each value
    return(do.call(c, lapply(1:length(longitude), function(i){
      # Special zones for Svalbard and Norway
      if (latitude[i] >= 72.0 && latitude[i] < 84.0 )
        if (longitude[i] >= 0.0  && longitude[i] <  9.0)
          return(31);
      if (longitude[i] >= 9.0  && longitude[i] < 21.0)
        return(33)
      if (longitude[i] >= 21.0 && longitude[i] < 33.0)
        return(35)
      if (longitude[i] >= 33.0 && longitude[i] < 42.0)
        return(37)
      (floor((longitude[i] + 180) / 6) %% 60) + 1
    })))
  }
}


pointsToLines<-function(d){
  ## list of Lines per id, each with one Line in a list
  x <- lapply(split(d, d$newUid), function(x) Lines(list(Line(coordinates(x))), x$newUid[1L]))
  # the corrected part goes here:
  lines <- SpatialLines(x)
  data <- data.frame(newUid = unique(d$newUid))
  rownames(data) <- data$newUid
  l <- SpatialLinesDataFrame(lines, data)
  return(l)
}

saveConfig<-function(){
  if(exists('masterWorkingDirectory')){
    saveRDS(configOptions,paste0(masterWorkingDirectory,'//configOptions.rds'))
  }
}
