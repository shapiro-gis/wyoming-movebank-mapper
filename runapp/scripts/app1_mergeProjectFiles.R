mergeShapfilesHandler<-function(){

    if(!thisIsTestingRun){
      selectedUid<<-input$uniqueIdSelector
    }


    if(selectedUid==""){
      modalMessager('Error','You need to select a field for the unique
      identifier or choose NaN if your dataset does not have a unique ID')
      return()
    }
  
    species <-input$speciesSelector
    
    
    studyname <-  input$studynameSelector
    
    tagID <-  input$tagIdSelector
    if (input$tagIdSelector == "NaN") {
      for(i in 1:length(importedShapefilesHolder)){
      importedShapefilesHolder[[i]]@data['tagID']<<-NULL
      thisNewTag<-names(importedShapefilesHolder[i])
      # remove these line because of user needs for underscores to persist
      # also cannot have underscores.. could cause errors later
      thisNewTag<-gsub("_", "-", thisNewTag)
      importedShapefilesHolder[[i]]@data['tagID']<<-thisNewTag
      }
    }
      else {
        for(i in 1:length(importedShapefilesHolder)){
          importedShapefilesHolder[[i]]@data['tagID']<<-NULL
          thisNewTag<-gsub("_","-",importedShapefilesHolder[[i]]@data[,tagID])
          importedShapefilesHolder[[i]]@data['tagID']<<-thisNewTag
        }

      }
    

    # if nan is chosen then the uid will just be the filename
    if(selectedUid=='NaN'){
      for(i in 1:length(importedShapefilesHolder)){
        importedShapefilesHolder[[i]]@data['newUid']<<-NULL
        thisNewUid<-names(importedShapefilesHolder[i])
        # remove these line because of user needs for underscores to persist
        # also cannot have underscores.. could cause errors later
        thisNewUid<-gsub("_", "-", thisNewUid)
        importedShapefilesHolder[[i]]@data['newUid']<<-thisNewUid
      }
    } else{ #otherwise the UID is the field that was selected for the UID
      for(i in 1:length(importedShapefilesHolder)){
        importedShapefilesHolder[[i]]@data['newUid']<<-NULL
        # also cannot have underscores.. could cause errors later
        thisNewUid<-gsub("_","-",importedShapefilesHolder[[i]]@data[,selectedUid])
        # thisNewUid<-importedShapefilesHolder[[i]]@data[,selectedUid]
        importedShapefilesHolder[[i]]@data['newUid']<<-thisNewUid
      }
    }
    
    
    
    #Add study name field
    if (input$studynameSelector == "NaN") {
      for(i in 1:length(importedShapefilesHolder)){
        importedShapefilesHolder[[i]]@data['studyname']<<-NULL
        thisNewUid<- input$customStudyInput
        thisNewUid<-gsub("_", "-", thisNewUid)
        importedShapefilesHolder[[i]]@data['studyname']<<-thisNewUid
      }
    }
    else{for(i in 1:length(importedShapefilesHolder)){
      
      if ("STUDYNAME" %in% colnames(importedShapefilesHolder[[i]]@data)){
        colnames(importedShapefilesHolder[[i]]@data)[which(names(importedShapefilesHolder[[i]]@data) == "STUDYNAME")] <- "studyname"
        print( colnames(importedShapefilesHolder[[i]]@data))
        
      } else {
        importedShapefilesHolder[[i]]@data['studyname']<<-NULL
        thisNewstudyname<-gsub("_","-",importedShapefilesHolder[[i]]@data[,studyname])
        importedShapefilesHolder[[i]]@data['studyname']<<-thisNewstudyname
        
      }
    }}
    

   # progressIndicator('Processing.... Please wait...','start')
   # if (input$speciesSelector == "NaN") {
   #    for(i in 1:length(importedShapefilesHolder)){
   #      importedShapefilesHolder[[i]]@data['species'] <<- NULL
   #      thisNewUid <- input$customSpeciesInput
   #      thisNewUid <- gsub("_", "-", thisNewUid)
   #      importedShapefilesHolder[[i]]@data['species'] <<- thisNewUid
   #    }
   #  }
    if (input$speciesSelector == "NaN") {
      for (i in 1:length(importedShapefilesHolder)) {
        if ("SPECIES" %in% names(importedShapefilesHolder[[i]]@data)) {
          importedShapefilesHolder[[i]]@data$SPECIES_OLD <- importedShapefilesHolder[[i]]@data$SPECIES
        }
        thisNewUid <- input$customSpeciesInput
        thisNewUid <- gsub("_", "-", thisNewUid)
        importedShapefilesHolder[[i]]@data$species <- thisNewUid
      }
    }
    else {
      for(i in 1:length(importedShapefilesHolder)){
        if ("SPECIES" %in% colnames(importedShapefilesHolder[[i]]@data)){
          print(names(importedShapefilesHolder[[i]]@data[species]))
          colnames(importedShapefilesHolder[[i]]@data)[which(names(importedShapefilesHolder[[i]]@data) == "SPECIES")] <- "species"
          
        }
        
        else {
          importedShapefilesHolder[[i]]@data['species']<<-NULL
          thisNewSpecies<-gsub("_","-",importedShapefilesHolder[[i]]@data[,species])
          importedShapefilesHolder[[i]]@data['species']<<-thisNewSpecies
          print( colnames(importedShapefilesHolder[[i]]@data))
          
        }
     
      }
    }
    


    importedDatasetMaster <<- tryCatch({
        Reduce(rbind,importedShapefilesHolder)
      },
      error = function(cond) {
      modalMessager(
        "DATASET MERGE ERROR",
        paste(
          "There was a fatal error merging
          your datasets. Please check the data and try again. Detailed error from
          R is : ",
          cond,
          sep = ""
        )
      )
      return()
      },
      warning = function(cond) {
      modalMessager(
        "DATASET MERGE WARNING",
        paste(
          "There was a fatal error merging
          your datasets. Please check the data and try again. Detailed error from
          R is : ",
          cond,
          sep = ""
        )
      )
      return()
      }
    )

  projectShapefilesHandler()
 # progressIndicator('Processing.... Please wait...','stop')
}


projectShapefilesHandler<-function(){

  
     importedDatasetMaster<<-spTransform(importedDatasetMaster,CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'))

#   if(packageVersion("rgdal")>'1.5.7'){
#   importedDatasetMaster <<- tryCatch({
#       spTransform(importedDatasetMaster,CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0', SRS_string='EPSG:4326'))
#     },
#     error = function(cond) {
#     modalMessager(
#       "PROJECTION ERROR",
#       paste(
#         "There was a fatal error projecting
#         your datasets. Please check the data and try again. Detailed error from
#         R is : ",
#         cond,
#         sep = ""
#       )
#     )
#     return()
#     },
#     warning = function(cond) {
#     # modalMessager(
#     #   "PROJECTION WARNING",
#     #   paste(
#     #     "There was a fatal error projecting
#     #     your datasets. Please check the data and try again. Detailed error from
#     #     R is : ",
#     #     cond,
#     #     sep = ""
#     #   )
#     # )
#     # return()
#     }
#   )
# }else{  
#   print(')))))))))))))))))))))))))))))))))))))))))))))')
#   importedDatasetMaster <<- tryCatch({
#       importedDatasetMaster<<-spTransform(importedDatasetMaster,CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'))
#     },
#     error = function(cond) {
#     modalMessager(
#       "PROJECTION ERROR",
#       paste(
#         "There was a fatal error projecting
#         your datasets. Please check the data and try again. Detailed error from
#         R is : ",
#         cond,
#         sep = ""
#       )
#     )
#     return()
#     },
#     warning = function(cond) {
#     modalMessager(
#       "PROJECTION WARNING",
#       paste(
#         "There was a fatal error projecting
#         your datasets. Please check the data and try again. Detailed error from
#         R is : ",
#         cond,
#         sep = ""
#       )
#     )
#     return()
#     }
#   )
# }



  #if there's a z dimension in the coords, drop it
  if(ncol(importedDatasetMaster@coords)==3){
    print('---------------------------------------')
    print('-------- DROPPING Z DIM ---------------')
    print('---------------------------------------')
    importedDatasetMaster@coords <<- importedDatasetMaster@coords[, 1:2]
  }


  # add lat/lon for leaflet maps
  importedDatasetMaster@data[["lon"]]<<-importedDatasetMaster$coords.x1
  importedDatasetMaster@data[["lat"]]<<-importedDatasetMaster$coords.x2

  #progressIndicator('Extracting Elevation Data','start')
  importedDatasetMaster@data[['elev']]<<-raster::extract(elevation,importedDatasetMaster)
  #progressIndicator('Extracting Elevation Data','stop')

  importedDatasetMaster@data['comments']<<-''

  midLatLong <- c(importedDatasetMaster@data[1,'lat'],importedDatasetMaster@data[1,'lon'])
  zone <- find_UTM_zone(midLatLong[2], midLatLong[1])
  UTMcrs <- paste0("+proj=utm +zone=", zone, " +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
  importedDatasetMaster <<- spTransform(importedDatasetMaster, CRS(UTMcrs))


  importedDatasetMaster@data$rowIds<<-row.names(importedDatasetMaster@data)

  ###########################
  ###### TAB COMPLETED ######
  ###########################

  # this checks for null importedDatasetMaster.. this was occuring when users tried to import null UTM data
  if(is.null(importedDatasetMaster)){
    modalMessager(
      "DATASET IMPORT ERROR",'There was a fatal error while importing your dataset(s). Sometimes this occurs because of null values from imported points. Double check your dataset validity and try again.'
    )
    return()
  } else{
    # saveWorkingFile();
    showDateTimeSelectionPanel()
  }
}
