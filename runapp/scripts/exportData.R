
combineShapefiles <- function(folder_path) {
  # Get a list of all files in the folder
  file_list <- list.files(path = folder_path, pattern = "\\.shp$", full.names = TRUE)
  
  # Read each shapefile into an sf object and combine them
  combined_sf <- NULL
  for (file_path in file_list) {
    sf_object <- st_read(file_path)
    # Add a new column with the name of the shapefile
    sf_object$filename <- gsub(".shp", "", basename(file_path))
    combined_sf <- rbind(combined_sf, sf_object)
  }
  
  # Return the combined sf object
  return(combined_sf)
}