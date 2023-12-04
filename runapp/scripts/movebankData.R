#Get path to movebank folder data
app2MovebankFolder<<- function(){
  wd<- getwd()
  #masterWorkingDirectory<- sessionInfo$masterWorkingDirectory
  app_dir <- normalizePath(file.path(wd, ".."), winslash = "/")
  app_dir<-paste0(app_dir,'//Movebank//')
}

addShapefileDropdown <- function() {
  
  # Find all shapefiles in the folder
  shapefiles <- list.files(app2MovebankFolder(), pattern="\\.rds$", full.names=TRUE)
  
  # Read the shapefiles and extract their names
  shapefile_names <- lapply(shapefiles, function(file_path) {
    file_name <- basename(file_path) # Get just the filename with extension
    file_name_sans_ext <- tools::file_path_sans_ext(file_name) # Remove the file extension
    return(file_name_sans_ext)
  })
  
  dropdown_options <- unlist(shapefile_names)
  return(dropdown_options)
}

#-------------------- Combine projects selected

combineprojects <- function(MuleDeerHerdUnits, AntelopeHerdUnits, inputShapefile, DeerHuntAreas, AntelopeHuntAreas, AntelopeSeasonalRange, MuleDeerSeasonalRange, BisonHerdUnits, BisonHuntAreas) {
  shapefile_names <- as.character(inputShapefile)
  pattern <- paste0("^(", paste(shapefile_names, collapse = "|"), ")\\.rds$")
  file_list <- list.files(app2MovebankFolder(), pattern = pattern, full.names = TRUE)
  
  combined_sf <- lapply(file_list, function(file_path) {
    
    #Read in file
    sf_object <- readRDS(file_path)
    #sf_object <- st_read(file_path) ## Use for shapefiles
    sf_object$filename <- gsub(".rds", "", basename(file_path))
    
    #Convert to sf object for spatial joins
    sf_object <- sf::st_as_sf(sf_object)
    sf_object <- st_transform(sf_object, "+init=EPSG:4326")
    
    #Change to lowercase to standardize field names
    names(sf_object) <- tolower(names(sf_object))
    #sf_object <- sf_object[, c("newuid", "species", "studyname", "lat", "lon", "datetest", "problem", "mortality", "dt", "dist", "burst", "speed", "id_yr", "nsdoverall")]
    sf_object$newuid <- as.character(sf_object$newuid)
    
    ids <- unique(sf_object$species)
    
    for (id in ids) {
      if (!is.na(id) && startsWith(id, "Antilo")) {
        sf_object <- st_join(sf_object, AntelopeHerdUnits[, "HERDNAME"], join = st_within)
        colnames(sf_object)[colnames(sf_object) == "HERDNAME"] <- "herd_unit_col"
        
      } else if (!is.na(id) && startsWith(id, "Odo")) {
        sf_object <- st_join(sf_object, MuleDeerHerdUnits[, "MD_HERDNAME"], join = st_within)
        colnames(sf_object)[colnames(sf_object) == "MD_HERDNAME"] <- "herd_unit_col"
        
        
      } else if (!is.na(id) && startsWith(id, "Bis")) {
        sf_object <- st_join(sf_object, BisonHerdUnits[, "HERDNAME"], join = st_within)
        colnames(sf_object)[colnames(sf_object) == "HERDNAME"] <- "herd_unit_col"
        
        
      } else if (!is.na(id) && startsWith(id, "Cervus")) {
        sf_object <- st_join(sf_object, ElkHerdUnits[, "HERDNAME"], join = st_within)
        colnames(sf_object)[colnames(sf_object) == "HERDNAME"] <- "herd_unit_col"
        
        
      } else if (!is.na(id) && startsWith(id, "Ovis")) {
        sf_object <- st_join(sf_object, BighornSheepHerdUnits[, "HERDNAME"], join = st_within)
        colnames(sf_object)[colnames(sf_object) == "HERDNAME"] <- "herd_unit_col"
    
      } else if (!is.na(id) && startsWith(id, "Alces")) {
        sf_object <- st_join(sf_object, MooseHerdUnits[, "HERDNAME"], join = st_within)
        colnames(sf_object)[colnames(sf_object) == "HERDNAME"] <- "herd_unit_col"
        
      } else {
        sf_object$herd_unit_col <- NA  # Setting the column value to NA when none of the conditions are met
      }
    }
    
    sf_object <- sf_object[sf_object$problem == 0, ]
    sf_object<-sf_object %>%
      group_by(newuid) %>%
      filter(datetest >start_date) %>%
      filter(datetest <= end_date)
    
    
    sf_object_first_loc <- sf_object %>% group_by(newuid) %>% filter(row_number() == 1)
    first_loc_herdunit <- unique(sf_object_first_loc[, c("newuid", "herd_unit_col")], by = "newuid")
    colnames(first_loc_herdunit)[2] <- "first_loc_herdunit_name"
    sf_object <- left_join(sf_object, first_loc_herdunit, by = "newuid")

    return(sf_object)
  })
  
  combined_sf <- do.call(rbind, combined_sf)
  combined_sf <- as.data.frame(st_drop_geometry(combined_sf))
  combined_sf <- subset(combined_sf, select = -c(geometry.y))
  
  combined_sf$month <- format(as.Date(combined_sf$datetest), "%m")
  combined_sf$year <- format(as.Date(combined_sf$datetest), "%Y")
  
  return(combined_sf)
}

#Create a layer when data is initially loaded
locationViewer<<- function(data){
  
  if(nrow(data) > 50000){
  data$date <- as.Date(data$datetest)
  # Group the dataframe by animal and year-month
  monthlydata <- data %>%
    group_by(newuid, format(date, "%Y-%m")) %>%
    arrange(date) %>%
    slice(1)
  return(monthlydata)
  }else {
    return(data)
  }

}

#-------------------- Filter data from the Query Builder


queryFilter<<- function(data, dateRange, selectAnimal, selectProject, selectMonth, selectYear,selectUnit, selectSpecies, selectRange,merged_polygons, 
                        selectSpatialFilter,selectUnitID,selectHuntUnit,selectHerdUnit,selectLayer, selectColumn, selectColumnValue,
                        MuleDeerHerdUnits,MuleDeerSeasonalRange,DeerHuntAreas,AntelopeHerdUnits,AntelopeHuntAreas,BisonHerdUnits,BisonHuntAreas,ElkHerdUnits,ElkHuntAreas,MooseHerdUnits,MooseHuntAreas,BighornSheepHerdUnits,BighornSheepHuntAreas,BioDistricts,AdminRegions, selectWithin)

  {
    
  # if (is.null(selectAnimal) || selectAnimal == "All") {
  #   movebankFilter <- data
  # } else {
  #   movebankFilter <- data[as.character(data$newuid) %in% selectAnimal, ]
  # }
  if (is.null(selectAnimal) || all(selectAnimal == "All")) {
    movebankFilter <- data
  } else {
    movebankFilter <- data[data$newuid %in% selectAnimal, ]
  }
  

  # Filter by selectProject
  if (!is.null(selectProject) && !identical(selectProject, "All")) {
  #if (!is.null(selectProject) && selectProject != "All") {
    movebankFilter <- movebankFilter[movebankFilter$studyname %in% selectProject, ]
  }
  # Filter by Capture Unit
  if (!is.null(selectUnit) && !identical(selectUnit, "All")) {
    movebankFilter <- movebankFilter[movebankFilter$first_loc_herdunit_name %in% selectUnit, ]
  }
  
  # Filter by Species
  if (!is.null(selectSpecies) && !identical(selectSpecies, "All")) {
      movebankFilter <- movebankFilter[movebankFilter$species %in% selectSpecies, ]
  }
  
  start_date <- paste0(as.character(dateRange[1])," 00:00:00")
  end_date <- paste0(as.character(dateRange[2])," 00:00:00")
  #data <- data[data$datetst >= start_date & data$datetst <= end_date, ]
  
  # Filter data based on selected filters
  if (selectRange == 'Month & Year' && !is.null(selectMonth) && !identical(selectMonth, "All") && !is.null(selectYear) && !identical(selectYear, "All")) {
    movebankFilter <- movebankFilter[movebankFilter$month %in% selectMonth & movebankFilter$year %in% selectYear, ]
    print("Using select range")
  } else{
    # Otherwise, filter by date range (or just year/month if one is selected)
    if (!is.null(selectMonth) && !identical(selectMonth, "All")) {
      movebankFilter <- movebankFilter[movebankFilter$month %in% selectMonth, ]
    }
    if (!is.null(selectYear) && !identical(selectYear, "All")) {
      movebankFilter <- movebankFilter[movebankFilter$year %in% selectYear, ]
    }}
  
  if(selectRange == 'Custom Date Range'){
    movebankFilter <- movebankFilter[movebankFilter$datetest >= start_date & movebankFilter$datetest <= end_date, ]

  }
  
  
  # Perform spatial join if a GIS layer and column value are selected
  if (selectWithin == TRUE && !is.null(selectLayer) && !is.null(selectColumn) && !is.null(selectColumnValue)) {
    if (selectLayer == "MuleDeerHerdUnits") {
      selectedLayer <- MuleDeerHerdUnits 
    } else if(selectLayer == "DeerHuntAreas"){
      selectedLayer <- DeerHuntAreas 
    }else if(selectLayer == "AntelopeHerdUnits"){
      selectedLayer <- AntelopeHerdUnits 
    }else if(selectLayer == "AntelopeHuntAreas"){
      selectedLayer <- AntelopeHuntAreas 
    }else if(selectLayer == "BisonHerdUnits"){
      selectedLayer <- BisonHerdUnits 
    }else if(selectLayer == "BisonHuntAreas"){
      selectedLayer <- BisonHuntAreas 
    }else if(selectLayer == "ElkHerdUnits"){
      selectedLayer <- ElkHerdUnits 
    }else if(selectLayer == "ElkHuntAreas"){
      selectedLayer <- ElkHuntAreas 
    }else if(selectLayer == "MooseHerdUnits"){
      selectedLayer <- MooseHerdUnits 
    }else if(selectLayer == "MooseHuntAreas"){
      selectedLayer <- MooseHuntAreas 
    }else if(selectLayer == "BighornSheepHerdUnits"){
      selectedLayer <- BighornSheepHerdUnits 
    }else if(selectLayer == "BighornSheepHuntAreas"){
      selectedLayer <- BighornSheepHuntAreas 
    }else if(selectLayer == "BioDistricts"){
      selectedLayer <- BioDistricts 
    }else if(selectLayer == "AdminRegions"){
      selectedLayer <- AdminRegions 
    }
    print(movebankFilter)
      #movebankFilter <- subset(movebankFilter, select = -c(geometry.y))
      
      # Convert movebankFilter to an sf object
      movebankFilter_sf <- sf::st_as_sf(movebankFilter, coords = c("lon", "lat"), crs = 4326, remove = FALSE)
      #movebankFilter_sf <- st_transform(movebankFilter_sf, "+init=EPSG:4326")
      print("Checked")
      drops <- c("geometry.y") # list of col names
      movebankFilter_sf <- movebankFilter_sf[,!(names(movebankFilter_sf) %in% drops)] #remove columns "AREA" and "PERIMETER"
      movebankFilter_sf <- st_join(movebankFilter_sf, selectedLayer, join = st_within)


      #movebankFilter_sf <- sf::st_join(movebankFilter_sf, selected_layer[selected_layer[[selectLayerColumn]] == selected_value, ], join = sf::st_within)
   #  # coordinates <- sf::st_coordinates(movebankFilter_sf$geometry)
    # # movebankFilter_sf$lat <- coordinates[, "Y"]
  #  #  movebankFilter_sf$lon <- coordinates[, "X"]

      # Convert back to a data frame
      movebankFilter <- as.data.frame(movebankFilter_sf)

      movebankFilter <- movebankFilter[movebankFilter[[selectColumn]] %in% selectColumnValue, ]
      movebankFilter <- subset(movebankFilter, select = -c(geometry))
      
        }
  
  return(movebankFilter)
}




