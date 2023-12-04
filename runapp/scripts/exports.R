exportSamplePoints<<- function(data, inputInterval){
  data$date <- as.Date(data$datetest)
  
  if (inputInterval == "1 Point a Month") {
    data <- data %>%
      group_by(newuid, format(date, "%Y-%m")) %>%
      arrange(date) %>%
      slice(1) 
    #Randomly selcet a record in a month
    # data <- data %>%
    #   group_by(format(date, "%Y-%m")) %>%
    #   sample_n(1) %>%
    #   ungroup()
    
  } else if (inputInterval == "1 Point a Day") {
    data <- data %>%
      group_by(newuid, format(date, "%Y-%m-%d")) %>%
      arrange(date) %>%
      slice(1)
  } else if (inputInterval == "All Data"){
      return(data)
    }
}


exportQuery<<- function(){
  #wd<- getwd()
  # #masterWorkingDirectory<- sessionInfo$masterWorkingDirectory
  # 
 # app_dir <- normalizePath(file.path(wd, ".."), winslash = "/")
  #app_dir<-paste0(app_dir,'//Movebank//Exports//')
  app_dir<-paste0(appRoot,'/movebank/Exports/')
}