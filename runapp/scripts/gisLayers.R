load_geojson <- function(urls) {
  plan(multisession, workers = 5)  # Use multiple parallel sessions
  
  sf_list <- future_map(urls, function(url) {
    response <- GET(url)
    geojson <- content(response, "text")
    st_read(geojson, quiet = TRUE)
  })
  return(sf_list)
}

urls <- c(
  "https://services6.arcgis.com/cWzdqIyxbijuhPLw/arcgis/rest/services/MuleDeerCrucialRange/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson",
  "https://services6.arcgis.com/cWzdqIyxbijuhPLw/arcgis/rest/services/MuleDeerHerdUnits/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson",
  "https://services6.arcgis.com/cWzdqIyxbijuhPLw/arcgis/rest/services/MuleDeerSeasonalRange/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson",
  "https://services6.arcgis.com/cWzdqIyxbijuhPLw/arcgis/rest/services/DeerHuntAreas/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson",
  "https://services6.arcgis.com/cWzdqIyxbijuhPLw/arcgis/rest/services/AntelopeHerdUnits/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson",
  "https://services6.arcgis.com/cWzdqIyxbijuhPLw/arcgis/rest/services/AntelopeHuntAreas/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson",
  "https://services6.arcgis.com/cWzdqIyxbijuhPLw/arcgis/rest/services/BisonHerdUnits/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson",
  "https://services6.arcgis.com/cWzdqIyxbijuhPLw/arcgis/rest/services/BisonHuntAreas/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson",
  "https://services6.arcgis.com/cWzdqIyxbijuhPLw/arcgis/rest/services/ElkHerdUnits/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson",
  "https://services6.arcgis.com/cWzdqIyxbijuhPLw/arcgis/rest/services/ElkHuntAreas/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson",
  "https://services6.arcgis.com/cWzdqIyxbijuhPLw/arcgis/rest/services/MooseHerdUnits/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson",
  "https://services6.arcgis.com/cWzdqIyxbijuhPLw/arcgis/rest/services/MooseHuntAreas/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson",
  "https://services6.arcgis.com/cWzdqIyxbijuhPLw/arcgis/rest/services/BighornSheepHerdUnits/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson",
  "https://services6.arcgis.com/cWzdqIyxbijuhPLw/arcgis/rest/services/BighornSheepHuntAreas/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson",
"https://services.wygisc.org/HostGIS/rest/services/GeoHub/WGFDRegionWildlifeBiologists/MapServer/0/query?outFields=*&where=1%3D1&f=geojson",
"https://services6.arcgis.com/cWzdqIyxbijuhPLw/arcgis/rest/services/WildlifeAdministrativeRegions/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson")


