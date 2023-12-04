get_animal_info <- function(data) {
  BeginDate<- unique(as.character(min(data$datetest)))
  Enddate<- unique(as.character(max(data$datetest)))
  
  timelag<- round(median(data$dt))
  summary<- data %>%
    group_by(species) %>%
    summarize(n_unique_animals = n_distinct(newuid))
  
  print(summary)
  #List out text for app
  tags$div( class = "list-group",
            tags$span(class = "list-group-item",
                      tags$span(icon("map-marker-alt")),
                      paste(nrow(data), "GPS Points"),),
            tags$span(class = "list-group-item",
                      tags$span(icon("map-marker-alt")),
                      paste(summary$species, "Species"),),
            tags$span(class = "list-group-item",
                      tags$span(icon("calendar-alt")), 
                      paste("Begin Date: ", BeginDate)),
            tags$span(class = "list-group-item",
                      tags$span(icon("calendar-alt")), 
                      paste( "End Date: ", Enddate)),
            tags$span(class = "list-group-item",
                      tags$span(icon("clock")), 
                      paste("GPS Interval", timelag))
  )
}

animalSummary<<- function(data){
  data %>%
    group_by(species = species, unit = first_loc_herdunit_name) %>%
    summarize(
      points = n(),
      animals = n_distinct(newuid),
      begindate = min(datetest),
      enddate = max(datetest)
    ) %>%
    mutate(
      begindate = as.character(begindate),
      enddate = as.character(enddate)
    )
}

indivSummary<<- function(data){
  data %>%
    group_by(animal = newuid) %>%
    summarize(
      points = n(),
      dist = sum(ifelse(is.numeric(dist), as.numeric(dist), 0)),
      begindate = min(datetest),
      enddate = max(datetest)
    ) %>%
    mutate(
      begindate = as.character(begindate),
      enddate = as.character(enddate)
    )
}