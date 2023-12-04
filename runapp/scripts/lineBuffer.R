lineBuffer_result <- reactiveVal()  # Reactive value to store MCP result

lineBuffer <- function(data, bufferSize){
  setorder( data, newuid, datetest )
  output_data <- data.frame()
  if ("geometry.y" %in% colnames(data)) {
    data <- subset(data, select = -c(geometry.y))
  }    
  # Convert movebankFilter to an sf object
  data <- sf::st_as_sf(data, coords = c("lon", "lat"), crs = 4326, remove = FALSE)
  #movebankFilter_sf <- st_transform(movebankFilter_sf, "+init=EPSG:4326")
  
  drops <- c("geometry.y") # list of col names
  data <- data[,!(names(data) %in% drops)]
  
  # Get the unique ids from your dataframe
  unique_ids <- unique(data$newuid)
  # Loop through each unique id
  for (id in unique_ids) {
    # Subset the data for the current id
    subset_data <- subset(data, newuid == id )
    # Convert subset_data to sf object
    test <- st_as_sf(subset_data, coords = c("lon", "lat"))
    
    geom <- lapply(
      1:(length(st_coordinates(test)[, 1]) - 1),
      function(i) {
        rbind(
          as.numeric(st_coordinates(test)[i, 1:2]),
          as.numeric(st_coordinates(test)[i + 1, 1:2])
        )
      }
    ) %>%
      st_multilinestring() %>%
      st_sfc(crs = st_crs(test)) %>%
      st_cast("LINESTRING")
    
    # Buffer the segments
    buffered_segments <- sf::st_buffer(geom, dist = bufferSize)  # Adjust the distance as needed
    
    # Union the buffered segments
    unioned_line <- st_union(buffered_segments)
    
    result <- as.data.frame(unioned_line)
    result$id <- id
    # Append the result to output_data
    output_data <- rbind(output_data, result)
  }
  output_sf <- st_as_sf(output_data)
  #output_spdf <- as_Spatial(output_sf)
  output$plotLineBuffer <- renderLeaflet({
    leaflet(output_sf) %>%
      addTiles() %>%
      addPolygons(fillColor = "red", weight = 2)
  })
  plot(output_sf)
  lineBuffer_result(output_sf)
}


mcp_result <- reactiveVal()  # Reactive value to store MCP result


calculateMCP <- function(data){
  datapoints<-SpatialPoints(cbind(data$lon,data$lat))
  
  #MCP
  datapoints.mcp <- mcp(datapoints, percent = 95)
  output$plotMCP <- renderLeaflet({
    leaflet(datapoints.mcp) %>%
      addTiles() %>%
      addPolygons(fillColor = "red", weight = 2)
  })
  mcp_result(datapoints.mcp)  # Store MCP result in reactive value
}
