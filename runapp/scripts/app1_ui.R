app1_init<-function(input,output,session){

  input<<-input
  output<<-output
  session<<-session

  whichAppIsRunning<<-'app1'
  

  
  observeEvent(input$changeAppsButton, {
    changeToOtherApp()
  },ignoreInit=TRUE)
  hide("glideDiv")  # Use shinyjs to hide the div wrapping the glide
  
  # observeEvent(input$processDatesButton, {
  #   hide("glideDiv")  # Use shinyjs to hide the div wrapping the glide
  # })
 
  observeEvent(input$nextButton,{
    print("Next Button Clicked")
  })
  
  
  ### Get working directory path ###
  
  volumes <- getVolumes()()
  shinyDirChoose(input, 'chooseWorkingDirButton', roots=volumes, session=session)
  dirname <- reactive({parseDirPath(volumes, input$chooseWorkingDirButton)})
  
  
  observeEvent(dirname(), {
    if(!is.null(dirname()) && length(dirname()) > 0){
    masterWorkingDirectory<<-NULL

    shinyjs::disable("chooseWorkingDirButton")

    #masterWorkingDirectory<<-choose.dir(dataFolder)
    masterWorkingDirectory<<-dirname()

    if(is.na(masterWorkingDirectory) | is.null(masterWorkingDirectory)){
      shinyjs::enable("chooseWorkingDirButton")
      return()
    }

    if(!dir.exists(masterWorkingDirectory)){
      modalMessager('error','Please try selecting this folder again')
      shinyjs::enable("chooseWorkingDirButton")
      return()
    }


    files<-list.files(masterWorkingDirectory)

    if(length(files)>0){
      modalMessager('error','The folder you chose is not empty.
        This will cause errors in analysis. Please empty the folder or
        choose a different directory and try again.')
      shinyjs::enable("chooseWorkingDirButton")
      masterWorkingDirectory<<-NULL
      return()
    }

    configOptions$masterWorkingDirectory<<-masterWorkingDirectory
    saveConfig()


    output$selectedWorkingDirectoryLabel<-renderUI({
      strong(paste0('Your data will be exported to: ',masterWorkingDirectory))
    })


    output$selectedWorkingDirectoryLabel<-renderUI({
      HTML(paste0('<strong>',masterWorkingDirectory,'</strong>'))
    })

    showColumnChoiceInfo()
    shinyjs::enable("chooseWorkingDirButton")}
  },ignoreInit=TRUE)

  observeEvent(input$closeMappButton, {
    closeApp()
  },ignoreInit=TRUE)

  observeEvent(input$exportDataButton,{

    if('firstFileName'%in%names(configOptions)){
      fileExportName<<-configOptions$firstFileName
    }else{
      fileExportName<<-'exportedFile'
    }

    if(exists('exportObserver')){
      exportObserver$destroy()
    }

    isExportRunning<<-FALSE
    showModal(modalDialog(
           title="Choose a name for file to export",
           "Choose a name below for the file that will be exported. The default name is the name of the first imported file. The file will be exported into a subfolder called EXPORTS within your working folder. Once you choose a name click the EXPORT SHAPEFILE to continue.",
           br(),
           textInput('fileExportInput', '', value = fileExportName, width = NULL, placeholder = NULL),
           actionButton("exportShapefileButton", "EXPORT SHAPEFILE"),
           footer = modalButton("CANCEL EXPORT")
       ))

       exportObserver<<-observeEvent(input$exportShapefileButton,{
           exportShapefile()
       },ignoreInit=TRUE,once = TRUE)


       observeEvent(input$fileExportInput,{
         fileExportName<<-input$fileExportInput
       },ignoreInit=TRUE)

  },ignoreInit=TRUE)



  observeEvent(input$timezoneSelector,{
      selectedTimezone<<-input$timezoneSelector
  },ignoreInit=TRUE)




  observeEvent(input$movebankLoginButton,{
    toggleModal(session,'movebankModal',toggle='open')
  },ignoreInit=TRUE)
  
  observeEvent(input$downloadMovebankDataButton,{
    toggleModal(session,'movebankModal',toggle='close')
    delay(100,
          downloadMovebankData(input$movebankUserInput,input$movebankPasswordInput,input$movebankStudyInput)
    )
  },ignoreInit=TRUE)
  
  reload <- Waiter$new(
    html = tagList(
      spin_3(),
      h4("Reloading Project.", style = "color: grey") # Add style attribute to h4 element
    ),
    color = transparent(.5)
  )
  
  
  ### Get path to previous project ###
  
  volumes <- getVolumes()()
  shinyDirChoose(input, 'loadProjectButton', roots=volumes, session=session)
  dirloadProjectname <- reactive({parseDirPath(volumes, input$loadProjectButton)})
  
  
  observeEvent(dirloadProjectname(), {
    if(!is.null(dirloadProjectname()) && length(dirloadProjectname()) > 0){

 # observeEvent(input$loadProjectButton,{
      tryCatch({
        rdsLocation <- dirloadProjectname() #choose.dir(caption = "select your project folder and press OK")
        reload$show()
        appOneReload(rdsLocation)
        reload$hide()
      }, error = function(ex) {
        modalMessager('Error',paste0('Try choosing a file again'))
      })
    }
  },ignoreInit=TRUE)

  
  w <- Waiter$new(
    html = tagList(
      spin_3(),
      h4("Exporting shapefile..", style = "color: grey") # Add style attribute to h4 element
    ),
    color = transparent(.5)
  )
  
  
  observeEvent(input$exportShapefile,{
    showModal(modalDialog(
      title="Export Shapefile",
      textInput('fileName', 'Please provide a file name', ),
      footer = tagList(actionButton("confirmExport", "Export"),
                       modalButton("Cancel")
      )
    ))
  })
  
  observeEvent(input$confirmExport,{
    w$show()
    layername = input$fileName
    output_shapefile <- normalizePath(file.path(masterWorkingDirectory, paste0(layername, ".shp")))
    print(output_shapefile)
    print(names(importedDatasetMaster))
    writeOGR(importedDatasetMaster, dsn = output_shapefile, layer = layername, driver = "ESRI Shapefile",overwrite_layer = TRUE)
    w$hide()
    
    shinyalert("Success!", paste0("Your shapefile was written to the following location:", output_shapefile), type = "success")
    
    removeModal()
  })
  
  
  
  
  observeEvent(input$finishDataCleaning,{
    
    
    # Export the SpatialPointsDataFrame as a shapefile
    showModal(modalDialog(
      title = "Are you sure you're done cleaning all movement data for this project?",
      "Click below to confirm",
      footer = tagList(
        
        actionButton("confirmBtn", "Confirm", class = "btn-primary"),
        modalButton("Cancel")
      )
    ))
    
  })
  
  
  observeEvent(input$animalTable, {
    w <- Waiter$new(
      html = tagList(
        spin_3(),
        h4("Exporting data...", style = "color: grey") # Add style attribute to h4 element
      ),
      color = transparent(.5)
    )
    
    w$show()
    df <- data.frame(importedDatasetMaster)
    # Group by 'newuid' and summarize startdate and enddate
    grouped_df <- df %>%
      group_by(newUid) %>%
      summarize(deploy_on_date = first(start_date),
                deploy_off_date = last(end_date),
                animal_taxon = unique(species),
                studyname = unique(studyname),
                tagid = unique(tagID),
                sex = if("sex" %in% names(df)) unique(sex) else NA)
    
    grouped_df <- grouped_df %>%
      rename(
        animal_id = newUid,
        tag_id = tagid,
        deploy_on_date = deploy_on_date,
        deploy_off_date = deploy_off_date,
        animal_taxon = animal_taxon,
        study_site = studyname,
        animal_sex = sex
      )

    # Format date columns in the desired format
    grouped_df$deploy_on_date <- format(grouped_df$deploy_on_date, "%Y-%m-%d %H:%M:%S")
    grouped_df$deploy_off_date <- format(grouped_df$deploy_off_date, "%Y-%m-%d %H:%M:%S")
    
    # Add ".000" to the date columns to represent milliseconds
   # grouped_df$deploy_on_date <- paste0(grouped_df$deploy_on_date, ".000")
    grouped_df$deploy_off_date <- as.character(paste0(" ",grouped_df$deploy_off_date, ".000"))
    grouped_df$deploy_on_date <- as.character(paste0(" ",grouped_df$deploy_on_date, ".000"))
    
    colnames(grouped_df) <- gsub("_", "-", colnames(grouped_df), fixed = TRUE)
    
    # Specify the CSV file path
    output_csv <- normalizePath(file.path(masterWorkingDirectory, "movebank_reference_data.csv"))
    
    # Write the modified dataframe to a CSV file
    #write.csv(grouped_df, file = output_csv, row.names = FALSE)
    write.table(grouped_df, file = output_csv, sep = ",", col.names = TRUE, row.names = FALSE, quote = FALSE)
    
    shinyalert("Success!", paste0("Your reference table was written to the following location:", output_csv), type = "success")
  
  w$hide()
  
  })
  
  observeEvent(input$gpsTable, {
    w <- Waiter$new(
      html = tagList(
        spin_3(),
        h4("Exporting data...", style = "color: grey") # Add style attribute to h4 element
      ),
      color = transparent(.5)
    )
    
    w$show()
    df <- data.frame(importedDatasetMaster)
    
    df <- df[, -which(names(df) %in% c("dist", "displacementOverall", "nsdYear", "nsdOverall","dt","abs.angle","rel.angle","speed","x", "tag_id", "timestamp",
                                       "y","burst","month","day","jul", "year", "id_yr", "dateTest", "rowIds", "displacementYear","fixRateHours"
                                       ,"coords.x1","coords.x2"))]

    df$problem <- ifelse(df$problem == 1, "true", "false")
    
    df <- df %>%
      rename(
        animal_id = newUid,
        tag_id = tagID,
        timestamp = newMasterDate,
        deploy_on_date = start_date,
        deploy_off_date = end_date,
        animal_taxon = species,
        study_site = studyname,
        Location_Lon = lon,
        Location_Lat = lat
      )
    
    colnames(df) <- gsub("_", "-", colnames(df), fixed = TRUE)
    
    output_csv <- normalizePath(file.path(masterWorkingDirectory, paste0( "movebank_gps_data.csv")))
    write.csv(df, file = output_csv, row.names = FALSE)
    shinyalert("Success!", paste0("Your shapefile was written to the following location:", output_csv), type = "success")
    
    
    w$hide()
    
  })
  
  

  
  observeEvent(input$confirmBtn, {
    w <- Waiter$new(
      html = tagList(
        spin_3(),
        h4("Exporting data...", style = "color: grey") # Add style attribute to h4 element
      ),
      color = transparent(.5)
    )
    
    w$show()
    names(importedDatasetMaster) <- tolower(names(importedDatasetMaster))  # Convert colnames to lowercase
     print(names(importedDatasetMaster))
    
    requiredFields <- c("newuid","tagid", "species","studyname", "lat", "lon", "datetest", "problem", "mortality", "dt", "dist","burst","speed","id_yr","nsdoverall", "start_date", "end_date")
    
    importedDatasetMaster <- importedDatasetMaster[, requiredFields]
    
    #Remove all slashes from the layername
    layername <- as.character(gsub("[/\\\\]", "", unique(importedDatasetMaster$studyname)[1]))
   
    output_shapefile <- normalizePath(file.path(masterWorkingDirectory, paste0(layername, ".shp")))
    #writeOGR(importedDatasetMaster, dsn = output_shapefile, layer = layername, driver = "ESRI Shapefile",overwrite_layer = TRUE)
    
    output_rds <- normalizePath(file.path(MovebankFolder(), paste0(layername, ".rds")))
    saveRDS(importedDatasetMaster, file = output_rds)
    
   w$hide()

   showModal(modalDialog(
     title = "Select an option below",
     "When preparing data for Movebank upload, you need to download two tables: the reference table and the GPS table. These tables are in a CSV file format and when downloaded, are stored within the Movebank folder in the app's root directory.
     
      The reference table contains information about unique individuals and their deployment dates. It is used to associate animals with the movement data in the Movebank database.
     Make sure you have both tables downloaded for a successful data upload to Movebank",
     br(),
     br(),
     fluidRow(
       column(6, actionButton("animalTable", "Download Reference Table", class = "btn-primary")),
       column(6, actionButton("gpsTable", "Download GPS Table", class = "btn-primary"))
     ),
     
     footer = tagList(

       actionButton("mapViewer", "Go to Map Viewer", class = "btn-primary"),
       modalButton("Cancel")
     )
   ))


  })
  observeEvent(input$mapViewer,{
    removeModal()
    
    updateTabsetPanel(session, "navibar",selected = "mapviewer")
    
  })


  observeEvent(input$maxSpeedSelector,{
      configOptions$maxSpeedParameter<<-input$maxSpeedSelector
      saveConfig()
  },ignoreInit=TRUE)

  observeEvent(input$mortDistance,{
      configOptions$mortDistance<<-input$mortDistance
      saveConfig()
  },ignoreInit=TRUE)

  observeEvent(input$mortTime,{
      configOptions$mortTime<<-input$mortTime
      saveConfig()
  },ignoreInit=TRUE)
  
  wc <- Waiter$new(
    html = tagList(
      spin_3(),
      h4("Calculating movement parameters...", style = "color: grey") # Add style attribute to h4 element
    ),
    color = transparent(.5)
  )
  observeEvent(input$calcMoveParamsButton,{
    wc$show()
      findProblemPoints()
      wc$hide()
  },ignoreInit=TRUE)




  observeEvent(input$getStartedButton, {
    toggleModal(session,'welcomeModal',toggle='close')
  },ignoreInit=TRUE)

  toggleModal(session,'welcomeModal',toggle='open')


  observeEvent(input$parametersButton, {
    toggleModal(session,'configModal',toggle='open')
  },ignoreInit=TRUE)

  # choose.dir <- function() {
  #   system("osascript -e 'tell app \"R\" to POSIX path of (choose folder with prompt \"Choose Folder:\")' > /tmp/R_folder",
  #          intern = FALSE, ignore.stderr = TRUE)
  #   p <- system("cat /tmp/R_folder && rm -f /tmp/R_folder", intern = TRUE)
  #   return(ifelse(length(p), p, NA))
  # }
  # 

  ### Get path to shapefile folder ####
  
  volumes <- getVolumes()()
  shinyDirChoose(input, 'chooseDirButton', roots=volumes, session=session)
  dirshapefilename <- reactive({parseDirPath(volumes, input$chooseDirButton)})

  
  observeEvent(dirshapefilename(), {
    if(!is.null(dirshapefilename()) && length(dirshapefilename()) > 0){
      
  #observeEvent(input$chooseDirButton, {
  dataFolder<<- dirshapefilename()
  availableShapefiles <<- list.files(dataFolder, pattern = '.shp$')
  if (length(availableShapefiles) == 0) {
    modalMessager(
      "Folder Selection Error",
      "No valid shapefile are present in this directory. Please check the
      directory and try again"
    )
    return
  }
  availableShapefiles <- append("", availableShapefiles)

  ##--------------------------------make a label showing selected folder
  output$selectedDirectoryLabel <- renderUI({
    p(paste("You successfully imported ", dataFolder, sep = ""))
  })

  ##--------------------------------render the dropdown for available shapes
  output$selectedShapefileHeaderLabel <- renderUI({
    strong('(2) Choose shapefile(s) from the selected data directory')
  })
  output$fileUploadSelectorHolder <- renderUI({
    selectInput(
      "fileUploadSelector",
      "",
      availableShapefiles,
      selected = NULL,
      multiple = TRUE
    )
  })

  ##------------------ start file import
  output$fileUploadExecute<-renderUI({
      actionButton('fileUploadExecute','Begin File Import')
  })
    }
})


observeEvent(input$fileUploadExecute, {
    if(exists('importedDatasetMaster')){
      toggleModal(session,'moreDataModal',toggle='toggle')
      return()
    }
    if(is.null(input$fileUploadSelector)){
      modalMessager('Error','You need to select a shapefile to continue import')
      return()
    }
    prepareFileImport()
    show("glideDiv")
  })


  dtvRunning<<-FALSE;
  observeEvent(input$processDatesButton,{
          if(!dtvRunning){
            hide("glideDiv")
            dateTimeValidator()
          }
    else{
      show("glideDiv")
    }
      })
  set_token("pk.eyJ1IjoianNoYXBpcm8xIiwiYSI6ImNrdDA1OGR5MzAxeHIyb290am05MzF1c2IifQ.wuOxNF5KFK0pjUJ3O80OmA") #this is jessie's token
  

}


exportShapefile=function(){
    fileExportFolder<-paste0(masterWorkingDirectory,'\\EXPORTS')
    if(dir.exists(fileExportFolder)==FALSE){
      dir.create(fileExportFolder)
    }

    if(file.exists(paste0(fileExportFolder,'\\',fileExportName,'.shp'))){
      time<-Sys.time()
      time<-gsub(" ", "", time, fixed = TRUE)
      time<-gsub("-", "", time, fixed = TRUE)
      time<-gsub(":", "", time, fixed = TRUE)
      fileExportName<<-paste0(fileExportName,'_',time)
    }

    # selectedTimezone<<-input$timezoneSelector
    tryCatch({
      if('originalProjection' %in% names(configOptions)){
        dataToExport<-spTransform(importedDatasetMaster, CRS(configOptions$originalProjection))
        dataToExport<-dataToExport[,c(configOptions$originalColumns,'problem','mortality','comments')]
      }else{
        dataToExport<-importedDatasetMaster
        dataToExport<-dataToExport[,c(configOptions$originalColumns,'problem','mortality','comments')]
      }
      loadingScreenToggle('show',paste0('exporting file to ',fileExportFolder))
      writeOGR(dataToExport, fileExportFolder, fileExportName, driver = "ESRI Shapefile")
      modalMessager('File Exported',paste0('File exported succesfully.'))
      loadingScreenToggle('hide',paste0('exporting file to ',exportDirectory))
    }, error = function(ex) {
      modalMessager('Error',paste0('Try choosing a directory again'))
      loadingScreenToggle('hide',paste0('exporting file to ',fileExportFolder))
    })
  }





clearShapefileSelector<-function(){
    updateSelectInput(session=session, "fileUploadSelector",
      label = "",
      choices = availableShapefiles,
      selected = NULL
    )
  }
