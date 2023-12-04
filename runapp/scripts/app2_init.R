
app2_init<-function(input,output,session){
  
  
  
    dropdown_options <- addShapefileDropdown()
    print(dropdown_options)
    
    if (length(dropdown_options) == 0) {
      shinyalert("Oops!", "No Movebank projects were found. Please return to the Data Cleaning tab and clean Movebank data first.", type = "error")
    } else {
      updateSelectInput(session, "shapefileDropdown", choices = dropdown_options)
    }


  #Create a spinner
  w <- Waiter$new(
    html = tagList(
      spin_3(),
      h4("Loading Movebank data...", style = "color: grey") # Add style attribute to h4 element
    ),
    color = transparent(.5)
  )
  gis <- Waiter$new(
    html = tagList(
      spin_3(),
      h4("Loading GIS data...", style = "color: grey") # Add style attribute to h4 element
    ),
    color = transparent(.5)
  )
  
  # Create a reactive value to track if the data has been loaded
  data_loaded <- reactiveVal(FALSE)
#  gis$show()
  # Check if the data has already been loaded
  if (!data_loaded()) {
    
    layerNames <- c(
      "MuleDeerCrucialRange", "MuleDeerHerdUnits", "MuleDeerSeasonalRange", "DeerHuntAreas", 
      "AntelopeHerdUnits", "AntelopeHuntAreas", "BisonHerdUnits", "BisonHuntAreas", 
      "ElkHerdUnits", "ElkHuntAreas", "MooseHerdUnits", "MooseHuntAreas", 
      "BighornSheepHerdUnits", "BighornSheepHuntAreas", "BioDistricts", "AdminRegions"
    )
    
    # Create select inputs for layer, column, and column value
    updateSelectInput(session, "selectLayer", choices = layerNames, selected = NULL)
    updateSelectInput(session, "selectColumn", choices = NULL, selected = NULL)
    updateSelectInput(session, "selectColumnValue", choices = NULL)
    
    # Update the choices of selectColumn based on the selected layer
    observeEvent(input$selectLayer, {
      layerIndex <- match(input$selectLayer, layerNames)
      if (!is.na(layerIndex)) {
        layer <- sf_list[[layerIndex]]
        columnNames <- colnames(layer)
        updateSelectInput(session, "selectColumn", choices = columnNames)
      }
    })
    
    # Update the choices of selectColumnValue based on the selected layer and column
    observeEvent(c(input$selectLayer, input$selectColumn), {
      layerIndex <- match(input$selectLayer, layerNames)
      if (!is.na(layerIndex)) {
        layer <- sf_list[[layerIndex]]
        column <- input$selectColumn
        columnValues <- unique(layer[[column]])
        updateSelectInput(session, "selectColumnValue", choices = columnValues)
      }
    })
    
    # Set the reactive value to indicate that the data has been loaded
    data_loaded(TRUE)
  }

  
  
  
  filtered_animals <- reactive({
    req(!is.null(movebankData()))
    req(input$connect)
    if (input$selectProject == "All") {
      return(unique(movebankData()$newuid))
    } else {
      return(unique(movebankData()$newuid[movebankData()$studyname == input$selectProject]))
    }
  })

  
  # Update the selectAnimal dropdown whenever the selectProject value changes
  observeEvent(input$selectProject, {
    req(!is.null(movebankData()))
    if (input$selectProject == "All") {
      filtered_data <- movebankData()
    }
    else{
    filtered_data<- movebankData()[movebankData()$studyname == input$selectProject,]
    }
    updateSelectInput(session, "selectAnimal", choices =  c("All",sort(filtered_animals())), selected = NULL) 
    updateSelectInput(session, "selectYear", choices = sort(unique(filtered_data$year)), selected = NULL)
    updateDateRangeInput(session, "dateRange", start = min(filtered_data$datetest),
                         end = max(filtered_data$datetest))    
  })
  observeEvent(input$selectAnimal, {
    req(!is.null(movebankData()))
    filtered_data <- movebankData()[movebankData()$newuid == input$selectAnimal, ]
    unique_months <- unique(filtered_data$month)
    unique_years <- unique(filtered_data$year)
    
    updateSelectInput(session, "selectMonth", choices = sort(unique_months), selected = NULL)
    updateSelectInput(session, "selectYear", choices = sort(unique_years), selected = NULL)
    updateDateRangeInput(session, "dateRange", start = min(filtered_data$datetest),
                         end = max(filtered_data$datetest))
  })
  
  
  
  ## Connect to selected projects
  movebankData<- eventReactive(input$connect,{
    req(input$connect)
    w$show()
    movebankData<- combineprojects(MuleDeerHerdUnits,AntelopeHerdUnits,input$shapefileDropdown,DeerHuntAreas,AntelopeHuntAreas,AntelopeSeasonalRange,MuleDeerSeasonalRange,BisonHerdUnits,BisonHuntAreas)
    updateSelectInput(session, "selectAnimal", choices = c("All", sort(unique(movebankData$newuid)), decreasing = FALSE), selected = NULL) #c("All", sort(unique(data$tag.local.identifier))), selected = NULL)
    updateSelectInput(session, "selectProject", choices = c("All",sort(unique(movebankData$studyname ))), selected = NULL) #c("All", sort(unique(data$tag.local.identifier))), selected = NULL)
    updateSelectInput(session, "selectMonth", choices = sort(unique(movebankData$month )), selected = NULL) #c("All", sort(unique(data$tag.local.identifier))), selected = NULL)
    updateSelectInput(session, "selectYear", choices = sort(unique(movebankData$year )), selected = NULL) #c("All", sort(unique(data$tag.local.identifier))), selected = NULL)
    updateSelectInput(session, "selectUnit", choices = sort(unique(movebankData$first_loc_herdunit_name )), selected = NULL) #c("All", sort(unique(data$tag.local.identifier))), selected = NULL)
    updateSelectInput(session, "selectSpecies", choices = sort(unique(movebankData$species )), selected = NULL) #c("All", sort(unique(data$tag.local.identifier))), selected = NULL)
    updateDateRangeInput(session, "dateRange", start = min(movebankData$datetest),
                         end = max(movebankData$datetest))
    
     w$hide()
    # Return the combined sf object
    updateTabsetPanel(session, "QueryBuilder",selected = "Query Builder")
    return(movebankData)
  })
  
  set_token("pk.eyJ1IjoianNoYXBpcm8xIiwiYSI6ImNrdDA1OGR5MzAxeHIyb290am05MzF1c2IifQ.wuOxNF5KFK0pjUJ3O80OmA") #this is jessie's token
  
  output$myMap <- renderMapdeck({
    mapboxer(center = c(-107.290283,43.075970), 
            # style = input$basemapStyle, zoom = 5)
             style = 'mapbox://styles/mapbox/outdoors-v11', zoom = 5)
  })
  
  
  
  movebankFilter <- eventReactive(input$query,{
    req(input$query)
    data <- movebankData()
    movebankFilter <- execute_safely(queryFilter(data, input$dateRange, input$selectAnimal, input$selectProject, input$selectMonth, input$selectYear, input$selectUnit, 
                                                 input$selectSpecies, input$selectRange, merged_polygons, input$selectSpatialFilter,
                                                 input$selectUnitID, input$selectHuntUnit, input$selectHerdUnit,input$selectLayer, input$selectColumn, 
                                                 input$selectColumnValue, MuleDeerHerdUnits,MuleDeerSeasonalRange,DeerHuntAreas,AntelopeHerdUnits,AntelopeHuntAreas,BisonHerdUnits,BisonHuntAreas,ElkHerdUnits,ElkHuntAreas,MooseHerdUnits,MooseHuntAreas,BighornSheepHerdUnits,BighornSheepHuntAreas,BioDistricts,AdminRegions,input$selectWithin))
     if (is.null(movebankFilter) || nrow(movebankFilter) == 0) {
      showNotification("No data found. Please adjust your search criteria.", type = "error")
      return(NULL)
    }
    
    updateTabsetPanel(session, "QueryBuilder",selected = "Results")
    
    return(movebankFilter)
  })
  
  
  
  ############-------------------- Results Tab --------------------#############
  
  
  output$info_animals <- renderUI({
    req(nrow(movebankFilter()) > 1, "The data must have more than one record.")
    
    data<- movebankFilter()
    get_animal_info(data) 
    
  })
  animalSum <- reactive({
    req(input$query)
    data<- movebankFilter()
    animalSummary(data)
  })
  indvSum <- reactive({
    req(input$query)
    data<- movebankFilter()
    indivSummary(data)
  })
  
  output$herdsummary <- renderTable(animalSum())
  output$indvsummary <- renderTable(indvSum(),
                                    rownames = TRUE, 
                                    colnames = TRUE, 
                                    class = "compact-table")
  
  lines<- reactive({
    req(nrow(movebankFilter()) > 1, "The data must have more than one record.")
    
    data<- movebankFilter()
    setorder( data, newuid, datetest )
    lines<- sfheaders::sf_linestring(
      obj = data
      , x = "lon"
      , y = "lat"
      , linestring_id = "newuid"
    )
    return(lines)
  })
  
  
  
  
  ############-------------------- Map --------------------#############

  
  observe({
    req(movebankData())
    data<- movebankData()
    data<- locationViewer(data)
    unique_values <- unique(data$newuid)
    num_colors <- length(unique_values)
    colors <- colorRampPalette(brewer.pal(9, "Set1"))(num_colors)
    color_mapping <- setNames(colors, unique_values)

    data$color <- color_mapping[data$newuid]
    
    mapboxer_proxy("myMap") %>%
      add_circle_layer(
        source = as_mapbox_source(data,lat="lat",lng="lon"),
      #  circle_color = '#000cff',
      circle_color =  c("get", "color"), 
        circle_radius = 5,
        popup = "<b>AID:</b> {{newuid}}</br>
        <b>Time:</b> {{datetest}}</br>
        <b>Capture Herd:</b> {{first_loc_herdunit_name}}</br>",
        id = "circle-layer"
      ) %>%
      update_mapboxer()
    
  })
  
  # Reactive values to store the last layer ids
  last_layer_id <- reactiveValues(circle = NULL, line = NULL)
  
  observeEvent(input$query, {
    
    # Filter the data
    filtered <- movebankFilter()
    lines <- lines()
    unique_values <- unique(lines$newuid)
    #   # Define the number of colors to generate
    num_colors <- length(unique_values)
    colors <- colorRampPalette(brewer.pal(9, "Set1"))(num_colors)
    color_mapping <- setNames(colors, unique_values)
    #   
    #   # Create a new column of hex colors based on the values in the "newuid" column
    lines$color <- color_mapping[lines$newuid]
    filtered$color <- color_mapping[filtered$newuid]
    
    mapboxer_proxy("myMap") %>%
      set_layout_property(
        layer_id = "circle-layer",
        property = "visibility",
        value = "none"
      )  %>%
      update_mapboxer()
    
    # If not the first run, set the visibility of old layers to none
    if (!is.null(last_layer_id$circle) && !is.null(last_layer_id$line)) {
      mapboxer_proxy("myMap") %>%
        set_layout_property(
          layer_id = last_layer_id$circle,
          property = "visibility",
          value = "none"
        ) %>%
        set_layout_property(
          layer_id = last_layer_id$line,
          property = "visibility",
          value = "none"
        ) %>%
        update_mapboxer()
    }
    
    # Generate unique layer IDs
    new_circle_layer_id <- paste0("circle-layer-filtered-", Sys.time())
    new_line_layer_id <- paste0("line-layer-filtered-", Sys.time())
    
    # Add new layers
    mapboxer_proxy("myMap") %>%
      add_circle_layer(
        source = as_mapbox_source(filtered,lat="lat",lng="lon"),
        circle_color =  c("get", "color"), #'#000cff',
        popup = "<b>AID:</b> {{newuid}}</br>
        <b>Time:</b> {{datetest}}</br>
        <b>Capture Herd:</b> {{first_loc_herdunit_name}}</br>",
        circle_radius = 5,
        id = new_circle_layer_id
      ) %>%
      add_line_layer(
        source = as_mapbox_source(lines),
        line_color = c("get", "color"),
        
        #line_color=  lines$color,  #'#ffffff',
        line_width = 1,
        id = new_line_layer_id
      ) %>%
      update_mapboxer()
    
    # Store the layer IDs
    last_layer_id$circle <- new_circle_layer_id
    last_layer_id$line <- new_line_layer_id
  })
  
  
  observeEvent(input$resetQuery,{
    
    movebankData<- movebankData()
    
    updateSelectInput(session, "selectAnimal", choices = c("All",sort(unique(movebankData$newuid)), decreasing = FALSE), selected = NULL) #c("All", sort(unique(data$tag.local.identifier))), selected = NULL)
    updateSelectInput(session, "selectProject", choices = c("All",sort(unique(movebankData$studyname ))), selected = NULL) #c("All", sort(unique(data$tag.local.identifier))), selected = NULL)
    updateSelectInput(session, "selectMonth", choices = sort(unique(movebankData$month )), selected = NULL) #c("All", sort(unique(data$tag.local.identifier))), selected = NULL)
    updateSelectInput(session, "selectYear", choices = sort(unique(movebankData$year )), selected = NULL) #c("All", sort(unique(data$tag.local.identifier))), selected = NULL)
    updateSelectInput(session, "selectUnit", choices = sort(unique(movebankData$first_loc_herdunit_name )), selected = NULL) #c("All", sort(unique(data$tag.local.identifier))), selected = NULL)
    updateSelectInput(session, "selectSpecies", choices = sort(unique(movebankData$species )), selected = NULL) #c("All", sort(unique(data$tag.local.identifier))), selected = NULL)
    updateDateRangeInput(session, "dateRange", start = min(movebankData$datetest),
                         end = max(movebankData$datetest))
    data<- movebankData()
    data<- locationViewer(data)
    mapboxer_proxy("myMap") %>%
      set_layout_property(
        layer_id = "circle-layer",
        property = "visibility",
        value = "visible"
      )  %>%
      update_mapboxer()

    mapboxer_proxy("myMap") %>%
      set_layout_property(
        layer_id = last_layer_id$circle,
        property = "visibility",
        value = "none"
      )  %>%
      update_mapboxer()
    
    mapboxer_proxy("myMap") %>%
      set_layout_property(
        layer_id = last_layer_id$line,
        property = "visibility",
        value = "none"
      )  %>%
      update_mapboxer()
    
    mapboxer_proxy("myMap") %>%
      set_layout_property(
        layer_id = "mcpResult",
        property = "visibility",
        value = "none"
      )  %>%
      update_mapboxer()
    
    mapboxer_proxy("myMap") %>%
      set_layout_property(
        layer_id = "lineBuffer_result",
        property = "visibility",
        value = "none"
      )  %>%
      update_mapboxer()
    
    
    mapboxer_proxy("myMap") %>%
      set_layout_property(
        layer_id = "kdeResult",
        property = "visibility",
        value = "none"
      )  %>%
      update_mapboxer()
    
    
  })
  
  

  
  ## Heat Map Toggle
  # observeEvent(input$heatMap,{
  #   if ( input$heatMap == TRUE) {
  #   req(nrow(movebankFilter()) > 1, "The data must have more than one record.")
  #   mapdeck_update(map_id = "myMap") %>%
  #     
  #     add_heatmap(
  #       data = movebankFilter()
  #       , lat = "lat"
  #       , lon = "lon"
  #       , layer_id = "heatmap_layer"
  #     )  
  #   } else {
  #       mapdeck_update(map_id = "myMap") %>%
  #         clear_heatmap(layer_id = "heatmap_layer")%>%
  #       clear_scatterplot(layer_id = "locationViewer")
  #     }
  # })
  

  
  ## GIS Layers
  # Initial state of the layer is not added
  MuleDeerCrucialRange_layer_added <- reactiveVal(FALSE)

  
  observeEvent(input$MuleDeerCrucialRange, {
    map_proxy <- mapboxer_proxy("myMap")
    
    if (is.null(input$MuleDeerCrucialRange)) {
      return(NULL)
    }
    else if (input$MuleDeerCrucialRange == TRUE) {
      if (MuleDeerCrucialRange_layer_added()) {
        # If the layer was previously added, make it visible
        map_proxy %>%
          set_layout_property(
            layer_id = "MuleDeerCrucialRange",
            property = "visibility",
            value = "visible"
          ) %>%
          update_mapboxer()
      } else {
        # If the layer wasn't previously added, add it and make it visible
        map_proxy %>%
          add_source(as_mapbox_source(MuleDeerCrucialRange), 'MuleDeerCrucialRange') %>%
          add_fill_layer(source = 'MuleDeerCrucialRange', id = "MuleDeerCrucialRange") %>%
          update_mapboxer()
        
        # Update the state of the layer to added
        MuleDeerCrucialRange_layer_added(TRUE)
      }
    } else if (input$MuleDeerCrucialRange == FALSE && MuleDeerCrucialRange_layer_added()) {
      # If the layer was previously added, hide it
      map_proxy %>%
        set_layout_property(
          layer_id = "MuleDeerCrucialRange",
          property = "visibility",
          value = "none"
        ) %>%
        update_mapboxer()
    }
  })
  
  
  MuleDeerHerdUnits_layer_added <- reactiveVal(FALSE)
  
  observeEvent(input$MuleDeerHerdUnits, {
    map_proxy <- mapboxer_proxy("myMap")
    
    if (is.null(input$MuleDeerHerdUnits)) {
      return(NULL)
    }
    else if (input$MuleDeerHerdUnits == TRUE) {
      if (MuleDeerHerdUnits_layer_added()) {
        # If the layer was previously added, make it visible
        map_proxy %>%
          set_layout_property(
            layer_id = "MuleDeerHerdUnits",
            property = "visibility",
            value = "visible"
          ) %>%
          update_mapboxer()
      } else {
        # If the layer wasn't previously added, add it and make it visible
        map_proxy %>%
          add_source(as_mapbox_source(MuleDeerHerdUnits), 'MuleDeerHerdUnits') %>%
          add_fill_layer(source = 'MuleDeerHerdUnits', id = "MuleDeerHerdUnits", fill_color = "blue",
                         fill_opacity = 0.2, popup = "{{MD_HERDNAME}}") %>%
          update_mapboxer()
        
        # Update the state of the layer to added
        MuleDeerHerdUnits_layer_added(TRUE)
      }
    } else if (input$MuleDeerHerdUnits == FALSE && MuleDeerHerdUnits_layer_added()) {
      # If the layer was previously added, hide it
      map_proxy %>%
        set_layout_property(
          layer_id = "MuleDeerHerdUnits",
          property = "visibility",
          value = "none"
        ) %>%
        update_mapboxer()
    }
  })
  
  MuleDeerSeasonalRange_layer_added <- reactiveVal(FALSE)
  
  observeEvent(input$MuleDeerSeasonalRange, {
    map_proxy <- mapboxer_proxy("myMap")
    
    if (is.null(input$MuleDeerSeasonalRange)) {
      return(NULL)
    }
    else if (input$MuleDeerSeasonalRange == TRUE) {
      if (MuleDeerSeasonalRange_layer_added()) {
        # If the layer was previously added, make it visible
        map_proxy %>%
          set_layout_property(
            layer_id = "MuleDeerSeasonalRange",
            property = "visibility",
            value = "visible"
          ) %>%
          update_mapboxer()
      } else {
        # If the layer wasn't previously added, add it and make it visible
        map_proxy %>%
          add_source(as_mapbox_source(MuleDeerSeasonalRange), 'MuleDeerSeasonalRange') %>%
          add_fill_layer(source = 'MuleDeerSeasonalRange', id = "MuleDeerSeasonalRange", fill_color = "blue", fill_opacity = 0.2, popup = "{{RANGE}}") %>%
          update_mapboxer()
        
        # Update the state of the layer to added
        MuleDeerSeasonalRange_layer_added(TRUE)
      }
    } else if (input$MuleDeerSeasonalRange == FALSE && MuleDeerSeasonalRange_layer_added()) {
      # If the layer was previously added, hide it
      map_proxy %>%
        set_layout_property(
          layer_id = "MuleDeerSeasonalRange",
          property = "visibility",
          value = "none"
        ) %>%
        update_mapboxer()
    }
  })
  DeerHuntAreas_layer_added <- reactiveVal(FALSE)
  
  observeEvent(input$DeerHuntAreas, {
    map_proxy <- mapboxer_proxy("myMap")
    
    if (is.null(input$DeerHuntAreas)) {
      return(NULL)
    }
    else if (input$DeerHuntAreas == TRUE) {
      if (DeerHuntAreas_layer_added()) {
        # If the layer was previously added, make it visible
        map_proxy %>%
          set_layout_property(
            layer_id = "DeerHuntAreas",
            property = "visibility",
            value = "visible"
          ) %>%
          update_mapboxer()
      } else {
        # If the layer wasn't previously added, add it and make it visible
        map_proxy %>%
          add_source(as_mapbox_source(DeerHuntAreas), 'DeerHuntAreas') %>%
          add_fill_layer(source = 'DeerHuntAreas', id = "DeerHuntAreas", fill_color = "blue", fill_opacity = 0.2, popup = "{{HUNTNAME}}") %>%
          update_mapboxer()
        
        # Update the state of the layer to added
        DeerHuntAreas_layer_added(TRUE)
      }
    } else if (input$DeerHuntAreas == FALSE && DeerHuntAreas_layer_added()) {
      # If the layer was previously added, hide it
      map_proxy %>%
        set_layout_property(
          layer_id = "DeerHuntAreas",
          property = "visibility",
          value = "none"
        ) %>%
        update_mapboxer()
    }
  })
  
  
  AntelopeHerdUnits_layer_added <- reactiveVal(FALSE)
  
  observeEvent(input$AntelopeHerdUnits, {
    map_proxy <- mapboxer_proxy("myMap")
    
    if (is.null(input$AntelopeHerdUnits)) {
      return(NULL)
    }
    else if (input$AntelopeHerdUnits == TRUE) {
      if (AntelopeHerdUnits_layer_added()) {
        # If the layer was previously added, make it visible
        map_proxy %>%
          set_layout_property(
            layer_id = "AntelopeHerdUnits",
            property = "visibility",
            value = "visible"
          ) %>%
          update_mapboxer()
      } else {
        # If the layer wasn't previously added, add it and make it visible
        map_proxy %>%
          add_source(as_mapbox_source(AntelopeHerdUnits), 'AntelopeHerdUnits') %>%
          add_fill_layer(source = 'AntelopeHerdUnits', id = "AntelopeHerdUnits", fill_color = "blue", fill_opacity = 0.2, popup = "{{HERDNAME}}") %>%
          update_mapboxer()
        
        # Update the state of the layer to added
        AntelopeHerdUnits_layer_added(TRUE)
      }
    } else if (input$AntelopeHerdUnits == FALSE && AntelopeHerdUnits_layer_added()) {
      # If the layer was previously added, hide it
      map_proxy %>%
        set_layout_property(
          layer_id = "AntelopeHerdUnits",
          property = "visibility",
          value = "none"
        ) %>%
        update_mapboxer()
    }
  })
  
  AntelopeHuntAreas_layer_added <- reactiveVal(FALSE)
  
  observeEvent(input$AntelopeHuntAreas, {
    map_proxy <- mapboxer_proxy("myMap")
    
    if (is.null(input$AntelopeHuntAreas)) {
      return(NULL)
    }
    else if (input$AntelopeHuntAreas == TRUE) {
      if (AntelopeHerdUnits_layer_added()) {
        # If the layer was previously added, make it visible
        map_proxy %>%
          set_layout_property(
            layer_id = "AntelopeHuntAreas",
            property = "visibility",
            value = "visible"
          ) %>%
          update_mapboxer()
      } else {
        # If the layer wasn't previously added, add it and make it visible
        map_proxy %>%
          add_source(as_mapbox_source(AntelopeHuntAreas), 'AntelopeHuntAreas') %>%
          add_fill_layer(source = 'AntelopeHuntAreas', id = "AntelopeHuntAreas", fill_color = "blue", fill_opacity = 0.2, popup = "{{HUNTNAME}}") %>%
          update_mapboxer()
        
        # Update the state of the layer to added
        AntelopeHuntAreas_layer_added(TRUE)
      }
    } else if (input$AntelopeHuntAreas == FALSE && AntelopeHuntAreas_layer_added()) {
      # If the layer was previously added, hide it
      map_proxy %>%
        set_layout_property(
          layer_id = "AntelopeHuntAreas",
          property = "visibility",
          value = "none"
        ) %>%
        update_mapboxer()
    }
  })

  BisonHuntAreas_layer_added <- reactiveVal(FALSE)
  
  observeEvent(input$BisonHuntAreas, {
    map_proxy <- mapboxer_proxy("myMap")
    
    if (is.null(input$BisonHuntAreas)) {
      return(NULL)
    }
    else if (input$BisonHuntAreas == TRUE) {
      if (AntelopeHerdUnits_layer_added()) {
        # If the layer was previously added, make it visible
        map_proxy %>%
          set_layout_property(
            layer_id = "BisonHuntAreas",
            property = "visibility",
            value = "visible"
          ) %>%
          update_mapboxer()
      } else {
        # If the layer wasn't previously added, add it and make it visible
        map_proxy %>%
          add_source(as_mapbox_source(BisonHuntAreas), 'BisonHuntAreas') %>%
          add_fill_layer(source = 'BisonHuntAreas', id = "BisonHuntAreas", fill_color = "blue", fill_opacity = 0.2, popup = "{{HUNTNAME}}") %>%
          update_mapboxer()
        
        # Update the state of the layer to added
        BisonHuntAreas_layer_added(TRUE)
      }
    } else if (input$BisonHuntAreas == FALSE && BisonHuntAreas_layer_added()) {
      # If the layer was previously added, hide it
      map_proxy %>%
        set_layout_property(
          layer_id = "BisonHuntAreas",
          property = "visibility",
          value = "none"
        ) %>%
        update_mapboxer()
    }
  })
  
  
  BisonHerdUnits_layer_added <- reactiveVal(FALSE)
  
  observeEvent(input$BisonHerdUnits, {
    map_proxy <- mapboxer_proxy("myMap")
    
    if (is.null(input$BisonHerdUnits)) {
      return(NULL)
    }
    else if (input$BisonHerdUnits == TRUE) {
      if (BisonHerdUnits_layer_added()) {
        # If the layer was previously added, make it visible
        map_proxy %>%
          set_layout_property(
            layer_id = "BisonHerdUnits",
            property = "visibility",
            value = "visible"
          ) %>%
          update_mapboxer()
      } else {
        # If the layer wasn't previously added, add it and make it visible
        map_proxy %>%
          add_source(as_mapbox_source(BisonHerdUnits), 'BisonHerdUnits') %>%
          add_fill_layer(source = 'BisonHerdUnits', id = "BisonHerdUnits", fill_color = "blue", fill_opacity = 0.2, popup = "{{HERDNAME}}") %>%
          update_mapboxer()
        
        # Update the state of the layer to added
        BisonHerdUnits_layer_added(TRUE)
      }
    } else if (input$BisonHerdUnits == FALSE && BisonHerdUnits_layer_added()) {
      # If the layer was previously added, hide it
      map_proxy %>%
        set_layout_property(
          layer_id = "BisonHerdUnits",
          property = "visibility",
          value = "none"
        ) %>%
        update_mapboxer()
    }
  })

  BighornSheepHerdUnits_layer_added <- reactiveVal(FALSE)
  
  observeEvent(input$BighornSheepHerdUnits, {
    map_proxy <- mapboxer_proxy("myMap")
    
    if (is.null(input$BighornSheepHerdUnits)) {
      return(NULL)
    }
    else if (input$BighornSheepHerdUnits == TRUE) {
      if (BighornSheepHerdUnits_layer_added()) {
        # If the layer was previously added, make it visible
        map_proxy %>%
          set_layout_property(
            layer_id = "BighornSheepHerdUnits",
            property = "visibility",
            value = "visible"
          ) %>%
          update_mapboxer()
      } else {
        # If the layer wasn't previously added, add it and make it visible
        map_proxy %>%
          add_source(as_mapbox_source(BighornSheepHerdUnits), 'BighornSheepHerdUnits') %>%
          add_fill_layer(source = 'BighornSheepHerdUnits', id = "BighornSheepHerdUnits", fill_color = "blue", fill_opacity = 0.2, popup = "{{HERDNAME}}") %>%
          update_mapboxer()
        
        # Update the state of the layer to added
        BighornSheepHerdUnits_layer_added(TRUE)
      }
    } else if (input$BighornSheepHerdUnits == FALSE && BighornSheepHerdUnits_layer_added()) {
      # If the layer was previously added, hide it
      map_proxy %>%
        set_layout_property(
          layer_id = "BighornSheepHerdUnits",
          property = "visibility",
          value = "none"
        ) %>%
        update_mapboxer()
    }
  })
  
  BighornSheepHuntAreas_layer_added <- reactiveVal(FALSE)
  
  observeEvent(input$BighornSheepHuntAreas, {
    map_proxy <- mapboxer_proxy("myMap")
    
    if (is.null(input$BighornSheepHuntAreas)) {
      return(NULL)
    }
    else if (input$BighornSheepHuntAreas == TRUE) {
      if (BighornSheepHuntAreas_layer_added()) {
        # If the layer was previously added, make it visible
        map_proxy %>%
          set_layout_property(
            layer_id = "BighornSheepHuntAreas",
            property = "visibility",
            value = "visible"
          ) %>%
          update_mapboxer()
      } else {
        # If the layer wasn't previously added, add it and make it visible
        map_proxy %>%
          add_source(as_mapbox_source(BighornSheepHuntAreas), 'BighornSheepHuntAreas') %>%
          add_fill_layer(source = 'BighornSheepHuntAreas', id = "BighornSheepHuntAreas", fill_color = "blue", fill_opacity = 0.2, popup = "{{HUNTNAME}}") %>%
          update_mapboxer()
        
        # Update the state of the layer to added
        BighornSheepHuntAreas_layer_added(TRUE)
      }
    } else if (input$BighornSheepHuntAreas == FALSE && BighornSheepHuntAreas_layer_added()) {
      # If the layer was previously added, hide it
      map_proxy %>%
        set_layout_property(
          layer_id = "BighornSheepHuntAreas",
          property = "visibility",
          value = "none"
        ) %>%
        update_mapboxer()
    }
  })

  

  ElkHerdUnits_layer_added <- reactiveVal(FALSE)
  
  observeEvent(input$ElkHerdUnits, {
    map_proxy <- mapboxer_proxy("myMap")
    
    if (is.null(input$ElkHerdUnits)) {
      return(NULL)
    }
    else if (input$ElkHerdUnits == TRUE) {
      if (ElkHerdUnits_layer_added()) {
        # If the layer was previously added, make it visible
        map_proxy %>%
          set_layout_property(
            layer_id = "ElkHerdUnits",
            property = "visibility",
            value = "visible"
          ) %>%
          update_mapboxer()
      } else {
        # If the layer wasn't previously added, add it and make it visible
        map_proxy %>%
          add_source(as_mapbox_source(ElkHerdUnits), 'ElkHerdUnits') %>%
          add_fill_layer(source = 'ElkHerdUnits', id = "ElkHerdUnits", fill_color = "blue", fill_opacity = 0.2, popup = "{{HERDNAME}}") %>%
          update_mapboxer()
        
        # Update the state of the layer to added
        ElkHerdUnits_layer_added(TRUE)
      }
    } else if (input$ElkHerdUnits == FALSE && ElkHerdUnits_layer_added()) {
      # If the layer was previously added, hide it
      map_proxy %>%
        set_layout_property(
          layer_id = "ElkHerdUnits",
          property = "visibility",
          value = "none"
        ) %>%
        update_mapboxer()
    }
  })
  
  ElkHuntAreas_layer_added <- reactiveVal(FALSE)
  
  observeEvent(input$ElkHuntAreas, {
    map_proxy <- mapboxer_proxy("myMap")
    
    if (is.null(input$ElkHuntAreas)) {
      return(NULL)
    }
    else if (input$ElkHuntAreas == TRUE) {
      if (ElkHuntAreas_layer_added()) {
        # If the layer was previously added, make it visible
        map_proxy %>%
          set_layout_property(
            layer_id = "ElkHuntAreas",
            property = "visibility",
            value = "visible"
          ) %>%
          update_mapboxer()
      } else {
        # If the layer wasn't previously added, add it and make it visible
        map_proxy %>%
          add_source(as_mapbox_source(ElkHuntAreas), 'ElkHuntAreas') %>%
          add_fill_layer(source = 'ElkHuntAreas', id = "ElkHuntAreas", fill_color = "blue", fill_opacity = 0.5, popup = "{{HUNTNAME}}") %>%
          update_mapboxer()
        
        # Update the state of the layer to added
        ElkHuntAreas_layer_added(TRUE)
      }
    } else if (input$ElkHuntAreas == FALSE && ElkHuntAreas_layer_added()) {
      # If the layer was previously added, hide it
      map_proxy %>%
        set_layout_property(
          layer_id = "ElkHuntAreas",
          property = "visibility",
          value = "none"
        ) %>%
        update_mapboxer()
    }
  })
  
  
  MooseHuntAreas_layer_added <- reactiveVal(FALSE)
  
  observeEvent(input$MooseHuntAreas, {
    map_proxy <- mapboxer_proxy("myMap")
    
    if (is.null(input$MooseHuntAreas)) {
      return(NULL)
    }
    else if (input$MooseHuntAreas == TRUE) {
      if (MooseHuntAreas_layer_added()) {
        # If the layer was previously added, make it visible
        map_proxy %>%
          set_layout_property(
            layer_id = "MooseHuntAreas",
            property = "visibility",
            value = "visible"
          ) %>%
          update_mapboxer()
      } else {
        # If the layer wasn't previously added, add it and make it visible
        map_proxy %>%
          add_source(as_mapbox_source(MooseHuntAreas), 'MooseHuntAreas') %>%
          add_fill_layer(source = 'MooseHuntAreas', id = "MooseHuntAreas", fill_color = "blue", fill_opacity = 0.5, popup = "{{HUNTNAME}}") %>%
          update_mapboxer()
        
        # Update the state of the layer to added
        MooseHuntAreas_layer_added(TRUE)
      }
    } else if (input$MooseHerdUnits == FALSE && MooseHuntAreas_layer_added()) {
      # If the layer was previously added, hide it
      map_proxy %>%
        set_layout_property(
          layer_id = "MooseHuntAreas",
          property = "visibility",
          value = "none"
        ) %>%
        update_mapboxer()
    }
  })
  MooseHerdUnits_layer_added <- reactiveVal(FALSE)
  
  observeEvent(input$MooseHerdUnits, {
    map_proxy <- mapboxer_proxy("myMap")
    
    if (is.null(input$MooseHerdUnits)) {
      return(NULL)
    }
    else if (input$MooseHerdUnits == TRUE) {
      if (MooseHerdUnits_layer_added()) {
        # If the layer was previously added, make it visible
        map_proxy %>%
          set_layout_property(
            layer_id = "MooseHerdUnits",
            property = "visibility",
            value = "visible"
          ) %>%
          update_mapboxer()
      } else {
        # If the layer wasn't previously added, add it and make it visible
        map_proxy %>%
          add_source(as_mapbox_source(MooseHerdUnits), 'MooseHerdUnits') %>%
          add_fill_layer(source = 'MooseHerdUnits', id = "MooseHerdUnits", fill_color = "blue", fill_opacity = 0.5, popup = "{{HERDNAME}}") %>%
          update_mapboxer()
        
        # Update the state of the layer to added
        MooseHerdUnits_layer_added(TRUE)
      }
    } else if (input$MooseHerdUnits == FALSE && MooseHerdUnits_layer_added()) {
      # If the layer was previously added, hide it
      map_proxy %>%
        set_layout_property(
          layer_id = "MooseHerdUnits",
          property = "visibility",
          value = "none"
        ) %>%
        update_mapboxer()
    }
  })

  AdminRegions_layer_added <- reactiveVal(FALSE)
  
  observeEvent(input$AdminRegions, {
    map_proxy <- mapboxer_proxy("myMap")
    
    if (is.null(input$AdminRegions)) {
      return(NULL)
    }
    else if (input$AdminRegions == TRUE) {
      if (AdminRegions_layer_added()) {
        # If the layer was previously added, make it visible
        map_proxy %>%
          set_layout_property(
            layer_id = "AdminRegions",
            property = "visibility",
            value = "visible"
          ) %>%
          update_mapboxer()
      } else {
        # If the layer wasn't previously added, add it and make it visible
        map_proxy %>%
          add_source(as_mapbox_source(AdminRegions), 'AdminRegions') %>%
          add_fill_layer(source = 'AdminRegions', id = "AdminRegions", fill_color = "blue", fill_opacity = 0.5, popup = "{{Region}}") %>%
          update_mapboxer()
        
        # Update the state of the layer to added
        AdminRegions_layer_added(TRUE)
      }
    } else if (input$AdminRegions == FALSE && AdminRegions_layer_added()) {
      # If the layer was previously added, hide it
      map_proxy %>%
        set_layout_property(
          layer_id = "AdminRegions",
          property = "visibility",
          value = "none"
        ) %>%
        update_mapboxer()
    }
  })
  

  BioDistricts_layer_added <- reactiveVal(FALSE)
  
  observeEvent(input$BioDistricts, {
    map_proxy <- mapboxer_proxy("myMap")
    
    if (is.null(input$BioDistricts)) {
      return(NULL)
    }
    else if (input$BioDistricts == TRUE) {
      if (BioDistricts_layer_added()) {
        # If the layer was previously added, make it visible
        map_proxy %>%
          set_layout_property(
            layer_id = "BioDistricts",
            property = "visibility",
            value = "visible"
          ) %>%
          update_mapboxer()
      } else {
        # If the layer wasn't previously added, add it and make it visible
        map_proxy %>%
          add_source(as_mapbox_source(BioDistricts), 'BioDistricts') %>%
          add_fill_layer(source = 'BioDistricts', id = "BioDistricts", fill_color = "blue", fill_opacity = 0.5, popup = "{{BIOLOGIST}}") %>%
          update_mapboxer()
        
        # Update the state of the layer to added
        BioDistricts_layer_added(TRUE)
      }
    } else if (input$BioDistricts == FALSE && BioDistricts_layer_added()) {
      # If the layer was previously added, hide it
      map_proxy %>%
        set_layout_property(
          layer_id = "BioDistricts",
          property = "visibility",
          value = "none"
        ) %>%
        update_mapboxer()
    }
  })

  
  # observeEvent(input$addMCP,{
  #   removeModal() 
  #   
  #   mcpResult<- mcp_result()
  #   mcpResult <- sf::st_as_sf(mcpResult)
  #   print(mcpResult)
  #   
  #   map_proxy <- mapboxer_proxy("myMap")
  #   map_proxy %>%
  #     add_source(as_mapbox_source(mcpResult), 'mcpResult') %>%
  #     add_fill_layer(source = 'mcpResult', id = "mcpResult", fill_color = "blue", fill_opacity = 0.5) %>%
  #     update_mapboxer()
  # 
  # })
  
  # observeEvent(input$addLineBuffer,{
  #   lineBuffer_result <- lineBuffer_result()
  #   lineBuffer_result <- sf::st_as_sf(lineBuffer_result)
  #   
  #   map_proxy <- mapboxer_proxy("myMap")
  #   map_proxy %>%
  #     add_source(as_mapbox_source(lineBuffer_result), 'lineBuffer_result') %>%
  #     add_fill_layer(source = 'lineBuffer_result', id = "lineBuffer_result", fill_color = "blue", fill_opacity = 0.5) %>%
  #     update_mapboxer()
  #   
  # })
  # 
  # observeEvent(input$addKDE,{
  #   kdeResult<- kde_result()
  #   kdeResult <- sf::st_as_sf(kdeResult)
  #   
  #   map_proxy <- mapboxer_proxy("myMap")
  #   map_proxy %>%
  #     add_source(as_mapbox_source(kdeResult), 'kdeResult') %>%
  #     add_fill_layer(source = 'kdeResult', id = "kdeResult", fill_color = "blue", fill_opacity = 0.5) %>%
  #     update_mapboxer()
  #   
  # })
  



############-------------------- Export Results --------------------#############

  
  
  
  

  observeEvent(input$exportQuery,{
    showModal(modalDialog(
      title="Export queried data to a shapefile",
      textInput('fileNameQuery', 'Please provide a name for your shapefile', 
                value = ""),
      use_bs_popover(),
      selectInput("selectInterval", "Select Interval", choices = c("All Data", "1 Point a Month", "1 Point a Day")) %>%
      shinyInput_label_embed(
        shiny_iconlink() %>%
          bs_embed_popover(
            content = "Split out a subsample of the data queried. 1 point a month returns one point for every indidvual a month, 1 point a day returns one point a day for every indivdiaul in your queried data.", placement = "left"
          )
      ),
      footer = tagList(
        actionButton("exportSample","Export Sampled Points"),
        actionButton("exportLines","Export Lines"),
        modalButton("Cancel")
      )
    ))
  })
  
  
  observeEvent(input$exportSample,{
    data <- movebankFilter()
    data<- exportSamplePoints(data,input$selectInterval)
    #data <- subset(data, select = -geometry.y)
    coords <- cbind(data$lon, data$lat)
    spdf <- SpatialPointsDataFrame(coords, data, proj4string = CRS("+proj=longlat +datum=WGS84"))

    layername = input$fileNameQuery
    output_shapefile <- normalizePath(file.path(exportQuery(), paste0(layername, ".shp")))
    writeOGR(spdf, dsn = output_shapefile, layer = layername, driver = "ESRI Shapefile",overwrite_layer = TRUE)
    shinyalert("Success!", paste0("Your shapefile was written to the following location:", output_shapefile), type = "success")
  }) 
  
  
  
  observeEvent(input$exportLines,{
    lines <- lines()
    projection_string <- "+proj=longlat +datum=WGS84"
    
    lines_sf <- sf::st_as_sf(lines)
    lines_sf <- st_set_crs(lines_sf, projection_string)
    
    lines_spatial <- sf::as_Spatial(lines_sf)
    
    layername = input$fileNameQuery
    output_shapefile <- normalizePath(file.path(exportQuery(), paste0(layername, ".shp")))
    writeOGR(lines_spatial, dsn = output_shapefile, layer = layername, driver = "ESRI Shapefile",overwrite_layer = TRUE)
    
    shinyalert("Success!", paste0("Your shapefile was written to the following location:", output_shapefile ), type = "success")

  }) 
  
  
  
  
  
  
  ############-------------------- Home Range Calculations --------------------#############
  
    
    
  observeEvent(input$exportCalcRange,{
    showModal(modalDialog(
      title="Calculate Home Ranges",
      fluidRow(
        column(
          width = 10,
          HTML("<strong>MCP:</strong> Create Minimum Convex Polygon(MCP) of the movement data. These are the simplest areas defined by the outside extent of the data points.")
        ),
        #prettySwitch("allIndvMCP", strong("Select Features Within:"), FALSE),
        
        column(
          width = 2,
          align = "center",
          style = "padding: 10px;",
          div(
            style = "position: relative; left: -10px;",
          actionButton("runMCP", "Run MCP")
        ))
      ),
      br(),
      fluidRow(
        column(
          width = 10,
      HTML("<strong>KDE:</strong> Create kernel density estimates to measure home ranges of the movement data. This uses contour lines to measure home ranges with kernels. "),
      ),
      br(),
      column(
        width = 2,
        align = "center",
        style = "padding: 10px;",
        div(
          style = "position: relative; left: -10px;",
      actionButton("runKDE", "Run KDE")))),
      br(),
      fluidRow(
        column(
          width = 10,
      HTML("<strong>Line Buffer:</strong> Create buffer home ranges by buffering the GPS-based movement paths of animals based on their step lengths. Input a value below to change the buffer size."),
      ),
      column(
        width = 2,
        align = "center",
        style = "padding: 10px;",
        div(
          style = "position: relative; left: -10px;",
      actionButton("runLineBuffer", "Run Line Buffer",style = "white-space: normal; word-wrap: break-word;"),
  
      ))),
      numericInput(inputId = "bufferSize", label = "Input Buffer Size", 
                   value = 300),
      footer = tagList(
        modalButton("Cancel")
      )
    ))
    
  })
  
  
    
    
  kernlUD <- function(data) {
    tryCatch({
      data$datetest <- as.POSIXct(data$datetest, tz = "MST", format = "%Y-%m-%d %H:%M:%S")
     # datapoints <- SpatialPoints(cbind(data$lon, data$lat))
      
      datapoints <- SpatialPoints(cbind(data$lon, data$lat), proj4string=CRS("+proj=longlat +datum=WGS84"))
      datapoints_transformed <- spTransform(datapoints, CRS("+init=epsg:5072"))  
      
      # KDE
      kud <- kernelUD(datapoints_transformed, h = "href") # kernel density utilization distribution (h = smoothing factor)
      image(kud)
      homerange99 <- getverticeshr(kud, percent = 99)
      homerange95 <- getverticeshr(kud, percent = 95) 
      homerange90 <- getverticeshr(kud, percent = 90) 
      homerange75 <- getverticeshr(kud, percent = 75)
      homerange50 <- getverticeshr(kud, percent = 50) 
      homerange25 <- getverticeshr(kud, percent = 25) 
      
      homerange99$percent <- 99
      homerange95$percent <- 95
      homerange90$percent <- 90
      homerange75$percent <- 75
      homerange50$percent <- 50
      homerange25$percent <- 25
      
      combined_vertices <- rbind(homerange99, homerange95, homerange90, homerange75, homerange50, homerange25)
      
      output$plotKUD <- renderLeaflet({
        kdemap<- spTransform(combined_vertices, CRS("+proj=longlat +datum=WGS84"))
        
        leaflet(kdemap) %>%
          addTiles() %>%
          addPolygons(fillColor = "red", weight = 2)
      })
      
     # proj4string(combined_vertices) <- CRS("+proj=longlat +datum=WGS84")
      
      kde_result(combined_vertices) # Store MCP result in a reactive value
      
    }, 
    error = function(e) {
      cat("Error occurred during the execution of kernlUD:", conditionMessage(e), "\n")
      return(NULL)
    })
  }
  
  kde_result <- reactiveVal() 
  
  observeEvent(input$runKDE,{
    data <- movebankFilter()
    kernlUD(data)
    showModal(modalDialog(
      title="Kernel Density Estimates",
      leafletOutput("plotKUD"),
      textInput('kdeFileName', 'Please provide a name for your shapefile', 
                value = ""),
      footer = tagList(
        actionButton("exportKDE","Export KDE"),
        
        modalButton("Cancel")
      )
    ))

  }) 
  
  observeEvent(input$exportKDE,{
    kdeResult<- kde_result()
    proj4string(kdeResult) <- CRS("+init=epsg:5072")
    
    layername = input$kdeFileName
    output_shapefile <- normalizePath(file.path(exportQuery(), paste0(layername, ".shp")))
    writeOGR( kdeResult , dsn = output_shapefile, layer = layername, driver = "ESRI Shapefile",overwrite_layer = TRUE)
    shinyalert("Success!", paste0("Your shapefile was written to the following location:", output_shapefile ), type = "success")
    # showModal(modalDialog(
    #   title="Do you want to add results to the map?",
    #   footer = tagList(
    #     actionButton("addKDE","Yes"),
    #     
    #     modalButton("Cancel")
    #   )
    # ))
    removeModal()
  }) 
  
   mcp_result <- reactiveVal()  # Reactive value to store MCP result
  # 
  # 
  # calculateMCP <- function(data){
  #   datapoints<-SpatialPoints(cbind(data$lon,data$lat))
  # 
  #   #MCP
  #   datapoints.mcp <- mcp(datapoints, percent = 95)
  #   output$plotMCP <- renderLeaflet({
  #     leaflet(datapoints.mcp) %>%
  #       addTiles() %>%
  #       addPolygons(fillColor = "red", weight = 2)
  #   })
  #   proj4string(datapoints.mcp) <- CRS("+proj=longlat +datum=WGS84")
  # 
  #   mcp_result(datapoints.mcp)  # Store MCP result in reactive value
  # 
  #   print(proj4string(datapoints.mcp))
  #   print(class(datapoints.mcp))
  # }

  calculateMCP <- function(data) {
  #  mcp_result(NULL)
    
    split_data <- split(data, data$newuid)
    
    mcp_results <- lapply(split_data, function(animal_data) {
    #  datapoints <- SpatialPoints(cbind(animal_data$lon, animal_data$lat))
      datapoints <- SpatialPoints(cbind(animal_data$lon, animal_data$lat), proj4string=CRS("+proj=longlat +datum=WGS84"))
      
      datapoints_transformed <- spTransform(datapoints, CRS("+init=epsg:5072"))
      
      datapoints.mcp <- mcp(datapoints_transformed, percent = 95)
      datapoints.mcp@data$area <- round(datapoints.mcp@data$area, 1)
      
    #  proj4string(datapoints.mcp) <- CRS("+proj=longlat +datum=WGS84")

      newuid_value <- unique(animal_data$newuid)
      attr(datapoints.mcp, "newuid") <- as.character(newuid_value)
      datapoints.mcp@data$id <- as.character(newuid_value)
      
      
      return(datapoints.mcp)
    })
    
    # Combine all MCP results into a single SpatialPolygonsDataFrame
    mcp_result_combined <- do.call("rbind", mcp_results)
    
      output$plotMCP <- renderLeaflet({
      m <- leaflet() %>% addTiles()
      mcp_results_wgs84 <- lapply(mcp_results, function(mcp) {
        spTransform(mcp, CRS("+proj=longlat +datum=WGS84"))
      })
      
      for (i in seq_along(mcp_results_wgs84)) {
        m <- m %>% addPolygons(data = mcp_results_wgs84[[i]], fillColor = "red", weight = 2)
      }
      
      return(m)
      })
      
        mcp_result(mcp_result_combined)
        return(mcp_result)
  }



  
  
  mcp <- Waiter$new(
    html = tagList(
      spin_3(),
      h4("Calculating MCP", style = "color: grey") # Add style attribute to h4 element
    ),
    color = transparent(.5)
  )
  
  observeEvent(input$runMCP,{
    mcp$show()
    data <- movebankFilter()
    calculateMCP(data)
    mcp$hide()
    showModal(modalDialog(
      title="MCP 95%",
      use_bs_popover(),
     leafletOutput("plotMCP"),
      textInput('mcpFileName', 'Please provide a name for your shapefile', 
                value = ""),
      footer = tagList(
        actionButton("exportMCP","Export MCP"),
        modalButton("Cancel")
      )
    ))

  })

  
  observeEvent(input$exportMCP,{
     mcpResult<- mcp_result()
     proj4string(mcpResult) <- CRS("+init=epsg:5072")
     
    layername = input$mcpFileName
    output_shapefile <- normalizePath(file.path(exportQuery(), paste0(layername, ".shp")))
    writeOGR( mcpResult , dsn = output_shapefile, layer = layername, driver = "ESRI Shapefile",overwrite_layer = TRUE)
    shinyalert("Success!", paste0("Your shapefile was written to the following location:", output_shapefile ), type = "success")
    removeModal()
  })

  
  
  lineBuffer_result <- reactiveVal()  # Reactive value to store MCP result
  
  lineBuffer <- function(data, bufferSize){
    setorder( data, newuid, datetest )
    output_data <- data.frame()
    if ("geometry.y" %in% colnames(data)) {
      data <- subset(data, select = -c(geometry.y))
    }    
    # Convert movebankFilter to an sf object
    data <- sf::st_as_sf(data, coords = c("lon", "lat"), crs = 4326, remove = FALSE)
    data <- st_transform(data, "+init=EPSG:5072")

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
    #output_sf <- st_geometry(output_sf)
    output_sf <- output_sf[st_geometry_type(output_sf) %in% c("POLYGON", "MULTIPOLYGON"), ]
    

    output$plotLineBuffer <- renderLeaflet({
      lineBuffer_map <- st_transform(output_sf, "+init=EPSG:4326")
      
      leaflet(lineBuffer_map) %>%
        addTiles() %>%
        addPolygons(fillColor = "red", weight = 2)
    })

    lineBuffer_result(output_sf)
  }
  
  buffer <- Waiter$new(
    html = tagList(
      spin_3(),
      h4("Calculating line buffers", style = "color: grey") # Add style attribute to h4 element
    ),
    color = transparent(.5)
  )
  observeEvent(input$runLineBuffer,{
    data <- movebankFilter()
    buffer$show()
    lineBuffer(data, input$bufferSize)
    buffer$hide()
    showModal(modalDialog(
      title="Line Buffer",
      use_bs_popover(),
      leafletOutput("plotLineBuffer"),
      textInput('lineBufferFileName', 'Please provide a name for your shapefile', 
                value = ""),
      footer = tagList(
        actionButton("exportLineBuffer","Export Line Buffer"),
        
        modalButton("Cancel")
      )
    ))
    
  })
  

  observeEvent(input$exportLineBuffer, {
    lineBuffer_result <- lineBuffer_result()
    lineBuffer_result <- st_transform(lineBuffer_result, "+init=EPSG:5072")
    
    layername <- input$lineBufferFileName
    output_shapefile <- normalizePath(file.path(exportQuery(), paste0(layername, ".shp")))
    st_write(lineBuffer_result, output_shapefile, append = FALSE)
    shinyalert("Success!", paste0("Your shapefile was written to the following location:", output_shapefile), type = "success")
    
    removeModal()
    })

  
  
}
  