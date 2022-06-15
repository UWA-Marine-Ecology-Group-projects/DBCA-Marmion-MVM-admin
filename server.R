# Server ----
server = function(input, output, session) {
  
  # call the server part
  # check_credentials returns a function to authenticate users
  res_auth <- secure_server(
    check_credentials = check_credentials(credentials))
  
  output$auth_output <- renderPrint({
    reactiveValuesToList(res_auth)
  })
  
  # Pretty disconnect message ----
  observeEvent(input$disconnect, {
    session$close()
  })
  
  
  output$sourceui <- renderUI({
    
   selectizeInput(
        inputId = "sourceinput", width = "100%",
        label = "Choose source (can select multiple):",
        choices = c(sort(unique(submitted.answers$source))),
        multiple = TRUE)
    
  })
  
  
  output$sublevel <- renderUI({
    
    req(input$go)
    
    drop.activities <- selected.polygons.activities %>%
      filter(source %in% c(input$sourceinput))
    
    if(input$level %in% c("cat")){
    
    selectizeInput(
      inputId = "sublevelinput", width = "100%",
      label = "Choose a category:",
      choices = c(sort(unique(drop.activities$nice.cat))),
      multiple = TRUE)
      
    } else if (input$level %in% c("sub")){
      selectInput(
        inputId = "sublevelinput", width = "100%",
        label = "Choose a subcategory:",
        choices = c(sort(unique(drop.activities$nice.sub.dropdown))),
        # selected = NULL,
        multiple = TRUE)
      
    } else if (input$level %in% c("act")){
      selectInput(
        inputId = "sublevelinput", width = "100%",
        label = "Choose an activity:",
        choices = c(sort(unique(drop.activities$nice.act.dropdown))),
        # selected = NULL,
        multiple = TRUE)
    } 
    
  })
  
  output$sublevelun <- renderUI({
    
    req(input$go)
    
    drop.activities <- selected.polygons.activities %>%
      filter(source %in% c(input$sourceinput))
    
    if(input$levelun %in% c("cat")){
      
      selectInput(
        inputId = "sublevelinputun", width = "100%",
        label = "Choose a category:",
        choices = c(sort(unique(drop.activities$nice.cat))),
        # selected = NULL,
        multiple = TRUE)
      
    } else if (input$levelun %in% c("sub")){
      selectInput(
        inputId = "sublevelinputun", width = "100%",
        label = "Choose a subcategory:",
        choices = c(sort(unique(drop.activities$nice.sub.dropdown))),
        # selected = NULL,
        multiple = TRUE)
      
    } else if (input$levelun %in% c("act")){
      selectInput(
        inputId = "sublevelinputun", width = "100%",
        label = "Choose an activity:",
        choices = c(sort(unique(drop.activities$nice.act.dropdown))),
        # selected = NULL,
        multiple = TRUE)
    } 
    
  })
  
  
  output$selectlk <- renderUI({
    
    req(input$go)
    
    drop.localknowledge <- selected.polygons.values %>%
      filter(source %in% c(input$sourceinput))
    
    selectInput("inputvalues", width = "100%",
              "Select local knowledge to explore (can select multiple):",
              sort(c(unique(drop.localknowledge$nice.act))),
              multiple = TRUE)
  })
  
  output$selectpt <- renderUI({
    
    req(input$go)
    
    drop.pressures <- selected.polygons.pressures %>%
      filter(source %in% c(input$sourceinput)) %>%
      mutate(tidy.name = paste(nice.sub, nice.act, sep = " - "))
    
    selectInput("inputpressures", width = "100%",
                "Select pressures and threats to explore (can select multiple):",
                sort(c(unique(drop.pressures$tidy.name))),
                multiple = TRUE)
  })
  
  ## Value boxes ----
  output$totalbox <- renderValueBox({
    valueBox(
      paste0(nrow(metadata)), "Total responses", icon = icon("users"), color = "blue"
    )
  })
  
  output$wabox <- renderValueBox({
    valueBox(
      paste0(nrow(wa)), "Responses from Western Australia", icon = icon("users"), color = "blue"
    )
  })
  
  output$localbox <- renderValueBox({
    valueBox(
      paste0(nrow(local)), "Responses from local postcodes", icon = icon("users"), color = "blue"
    )
  })
  
  output$socialbox <- renderValueBox({
    valueBox(
      paste0(nrow(socialmedia)), "Responses from social media", icon = icon("facebook"), color = "teal"
    )
  })
  
  output$crcbox <- renderValueBox({
    valueBox(
      paste0(nrow(crc)), "Responses from CRC and SAG", icon = icon("envelope"), color = "teal"
    )
  })
  
  output$websitebox <- renderValueBox({
    valueBox(
      paste0(nrow(website)), "Responses from Marmion Marine Park website", icon = icon("chrome"), color = "teal"
    )
  })
  
  output$facebox <- renderValueBox({
    valueBox(
      paste0(nrow(face)), "Responses from face to face surveys", icon = icon("users"), color = "teal"
    )
  })
  
  ## Submitted responses by date ----
  output$submitteddate <- renderPlot({
    
    summarise.submitted <- metadata %>%
      distinct(name, email, phone, source, date) %>%
      group_by(date) %>%
      summarise(value = n())
    
    ggplot(summarise.submitted,
           aes(x = date, y = value)) +
      geom_bar(width = 1, stat = "identity", fill = "#6baed6", col = "black") +
      xlab("Date") +
      ylab("Number of responses") +
      Theme1 +
      scale_y_continuous(expand = c(0, 0))
    
  })
  
  ## Submitted responses by source ----
  output$submittedsource <- renderPlot({
    
    summarise.submitted <- metadata %>%
      distinct(name, email, phone, source) %>%
      group_by(source) %>%
      summarise(value = n()) %>%
      replace_na(list(source = "Unknown"))
    
    ggplot(summarise.submitted,
           aes(x = source, y = value)) +
      geom_bar(width = 1, stat = "identity", fill = "#6baed6", col = "black") +
      xlab("Source") +
      ylab("Number of responses") +
      Theme1 +
      scale_y_continuous(expand = c(0, 0))+
      scale_x_discrete(labels = function(x) str_wrap(x, width = 30))
    
  })
  
  ## Submitted responses by category (Bar plot) ----
  output$submittedcategory <- renderPlot({
    
    req(input$go)
    
    summarise.submitted <- selected.activities %>%
      filter(source %in% c(input$sourceinput)) %>%
      group_by(nice.cat) %>%
      summarise(value = n()) %>%
      filter(!nice.cat %in% c(NA, "NA"))
    
    ggplot(summarise.submitted, aes(x = nice.cat, y = value, fill = nice.cat)) +
      geom_bar(width = 1, stat = "identity", col = "black") +
      scale_fill_brewer("Category") +
      xlab("Category") +
      ylab("Number of responses") +
      Theme1 +
      scale_y_continuous(expand = c(0, 0))
    
  })
  
  ## Submitted responses by category (Pie chart) ----
  output$submittedpie <- renderPlot({
    
    req(input$go)
    
    summarise.submitted <- selected.activities %>%
      filter(source %in% c(input$sourceinput)) %>%
      dplyr::group_by(nice.cat) %>%
      dplyr::summarise(value = n()) %>%
      dplyr::filter(!nice.cat %in%c ("NA", NA)) %>%
      mutate(prop = value / sum(value) * 100) %>%
      mutate(ypos = cumsum(prop) - 0.5 * prop) %>%
      mutate(labels = paste (round(prop, digits = 1), "%", sep = ""))
    
    ggplot(summarise.submitted, aes(x = "", y = prop, fill = nice.cat)) +
      geom_bar(width = 1, stat = "identity", col = "black") +
      coord_polar("y", start = 0) +
      geom_text(aes(label = labels),
                position = position_stack(vjust = 0.5)) +
      scale_fill_brewer("Category", labels = function(x) str_wrap(x, width = 25)) +
      Theme1 +
      blank_theme
    
  })
  
  ## Submitted responses by local knowledge (Bar chart) ----
  output$submittedvalues <- renderPlot({
    
    req(input$go)
    
    summarise.submitted <- selected.values %>%
      filter(source %in% c(input$sourceinput)) %>%
      group_by(nice.act) %>%
      summarise(value = n()) %>%
      filter(!nice.act %in% c(NA, "NA"))
    
    colourCount = length(unique(summarise.submitted$nice.act))
    
    ggplot(summarise.submitted, aes(x = nice.act, y = value, fill = nice.act)) +
      geom_bar(width = 1, stat = "identity", col = "black") +
      scale_fill_manual(values = getPalette(colourCount), labels = function(x) str_wrap(x, width = 25))+
      xlab("Local Knowledge") +
      ylab("Number of responses") +
      Theme1 +
      scale_y_continuous(expand = c(0, 0)) +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 10))
    
  })
  
  ## Submitted responses by local knowledge (Pie chart) ----
  output$submittedvaluespie <- renderPlot({
    
    req(input$go)
    
    summarise.submitted <- selected.values %>%
      filter(source %in% c(input$sourceinput)) %>%
      group_by(nice.act) %>%
      summarise(value = n()) %>%
      filter(!nice.act %in% c(NA, "NA")) %>%
      mutate(prop = value / sum(value) * 100) %>%
      mutate(ypos = cumsum(prop) - 0.5 * prop) %>%
      mutate(labels = paste (round(prop, digits = 1), "%", sep = ""))
    
    colourCount = length(unique(summarise.submitted$nice.act))
    
    ggplot(summarise.submitted, aes(x = "", y = prop, fill = nice.act)) +
      geom_bar(width = 1, stat = "identity", col = "black") +
      coord_polar("y", start = 0) +
      geom_text(aes(label = labels),
                position = position_stack(vjust = 0.5)) +
      scale_fill_manual(values = getPalette(colourCount), labels = function(x) str_wrap(x, width = 25)) +
      Theme1 +
      blank_theme
  })
  
  
  ## Submitted responses by pressures and threats (Bar chart) ----
  output$submittedpressures <- renderPlot({
    
    req(input$go)
    
    summarise.submitted <- selected.pressures %>%
      filter(source %in% c(input$sourceinput)) %>%
      group_by(nice.sub) %>%
      summarise(value = n()) %>%
      filter(!nice.sub %in% c(NA, "NA"))
    
    colourCount = length(unique(summarise.submitted$nice.sub))
    
    ggplot(summarise.submitted, aes(x = nice.sub, y = value, fill = nice.sub)) +
      geom_bar(width = 1, stat = "identity", col = "black") +
      scale_fill_manual(values = getPalette(colourCount), labels = function(x) str_wrap(x, width = 25))+
      xlab("Pressures and Threats") +
      ylab("Number of responses") +
      Theme1 +
      scale_y_continuous(expand = c(0, 0)) +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 10))
    
  })
  
  ## Submitted responses by pressures and threats (Pie chart) ----
  output$submittedpressurespie <- renderPlot({
    
    req(input$go)
    
    summarise.submitted <- selected.pressures %>%
      filter(source %in% c(input$sourceinput)) %>%
      group_by(nice.sub) %>%
      summarise(value = n()) %>%
      filter(!nice.sub %in% c(NA, "NA")) %>%
      mutate(prop = value / sum(value) * 100) %>%
      mutate(ypos = cumsum(prop) - 0.5 * prop) %>%
      mutate(labels = paste (round(prop, digits = 1), "%", sep = ""))
    
    colourCount = length(unique(summarise.submitted$nice.sub))
    
    ggplot(summarise.submitted, aes(x = "", y = prop, fill = nice.sub)) +
      geom_bar(width = 1, stat = "identity", col = "black") +
      coord_polar("y", start = 0) +
      
      geom_text(aes(label = labels),
                position = position_stack(vjust = 0.5)) +
      Theme1 +
      scale_fill_manual(values = getPalette(colourCount), labels = function(x) str_wrap(x, width = 25)) +
      blank_theme
  })
  
  ## Submitted responses by gender ----
  output$submittedgender <- renderPlot({
    
    req(input$go)
    
    summarise.submitted <- metadata %>%
      distinct(name, email, phone, source, gender) %>%
      filter(source %in% c(input$sourceinput)) %>%
      dplyr::group_by(gender) %>%
      dplyr::summarise(value = n())
    
    ggplot(summarise.submitted, aes(x = gender, y = value, fill = gender)) +
      geom_bar(width = 1, stat = "identity", col = "black") +
      scale_fill_brewer("Gender") +
      theme_minimal() +
      xlab("Gender") +
      ylab("Number of responses") +
      Theme1 +
      scale_y_continuous(expand = c(0, 0))
    
  })
  
  ## Submitted responses by age ----
  output$submittedage <- renderPlot({
    
    req(input$go)
    
    summarise.submitted <- metadata %>%
      distinct(name, email, phone, source, age) %>%
      filter(source %in% c(input$sourceinput)) %>%
      dplyr::group_by(age) %>%
      dplyr::summarise(value = n())
    
    ggplot(summarise.submitted, aes(x = age, y = value, fill = age)) +
      geom_bar(width = 1, stat = "identity", col = "black") +
      scale_fill_brewer("Age group") +
      xlab("Age group") +
      ylab("Number of responses") +
      Theme1 +
      scale_y_continuous(expand = c(0, 0))
    
  })
  
  ## Submitted responses by residence ----
  output$submittedlocation <- renderPlot({
    
    req(input$go)
    
    summarise.submitted <- metadata %>%
      distinct(name, email, phone, source, residence) %>%
      filter(source %in% c(input$sourceinput)) %>%
      dplyr::group_by(residence) %>%
      dplyr::summarise(value = n())
    
    ggplot(summarise.submitted,
           aes(x = residence, y = value, fill = residence)) +
      geom_bar(width = 1, stat = "identity", col = "black") +
      scale_fill_brewer("Residence") +
      xlab("Residence") +
      ylab("Number of responses") +
      Theme1 +
      scale_y_continuous(expand = c(0, 0))
    
  })
  
  ## Leaflet for all activities unweighted ----
  output$leafallunweighted <- renderLeaflet({
    
    req(input$go)
    
    polygons <- selected.polygons.activities
    
    polygon.id.list <-
      data.frame(ID = as.character(c(1:(
        length(SpP@polygons)
      ))))
    
    total.clicked <- polygons %>%
      filter(source %in% c(input$sourceinput)) %>%
      dplyr::group_by(id) %>%
      dplyr::summarise(total = n()) %>%
      dplyr::rename(ID = id) %>%
      dplyr::mutate(ID = as.character(ID)) %>%
      dplyr::full_join(polygon.id.list) %>%
      tidyr::replace_na(list(total = 0)) %>%
      mutate(ID = as.numeric(ID)) %>%
      arrange(ID) %>%
      dplyr::mutate(ID = as.character(ID))
    
    data <-
      SpatialPolygonsDataFrame(SpP, data = total.clicked, match.ID = FALSE)
    
    data <- data[data@data$total > 0,]
    
    bounds <- data %>% 
      st_bbox() %>% 
      as.character()
    
    bins <- c(0, 1, 5, 10, 15, 20, 30, 50, 80, Inf)
    # pal <- colorBin("YlOrRd", domain = data$total, bins = bins)
    pal <- colorNumeric(
      palette = "YlOrRd",
      domain = data$total)
    
    map <- leaflet(
      padding = 1000,
      width = "80%",
      height = "700px",
      options = leafletOptions(
        zoomControl = TRUE,
        dragging = TRUE,
        scrollWheelZoom = FALSE
      )
    ) %>%
      addmouselatlon() %>%
      addProviderTiles(
        'Esri.WorldImagery',
        group = "World Imagery"
      ) %>%
      addTiles(group = "Open Street Map") %>%
      fitBounds(bounds[1], bounds[2], bounds[3], bounds[4]) %>%
      addMapPane("polygons", zIndex = 410) %>%
      addPolygons(
        data = data,
        label = data$total,
        layerId = data$ID,
        fillColor = ~ pal(data$total),
        fillOpacity = 1,
        color = "#444444",
        weight = 1) %>%
      addLegend("bottomright", 
                data = data,
                pal = pal,
                values = ~data$total,
                title = "Number of clicks",
                opacity = 0.9)
    map
    
  })
  
  ## Leaflet all activities weighted ----
  output$leafallweighted <- renderLeaflet({
    
    req(input$go)
    
    polygons <- selected.polygons.activities
    
    polygon.id.list <-
      data.frame(ID = as.character(c(1:(
        length(SpP@polygons)
      ))))
    
    total.clicked <- polygons %>%
      filter(source %in% c(input$sourceinput)) %>%
      dplyr::group_by(id) %>%
      dplyr::summarise(total = sum(weighted.score)) %>%
      dplyr::rename(ID = id) %>%
      dplyr::mutate(ID = as.character(ID)) %>%
      dplyr::full_join(polygon.id.list) %>%
      tidyr::replace_na(list(total = 0)) %>%
      mutate(ID = as.numeric(ID)) %>%
      arrange(ID) %>%
      dplyr::mutate(ID = as.character(ID))
    
    data <-
      SpatialPolygonsDataFrame(SpP, data = total.clicked, match.ID = FALSE)
    
    data <- data[data@data$total > 0,]
    
    bounds <- data %>% 
      st_bbox() %>% 
      as.character()
    
    bins <- c(0, 1, 5, 10, 15, 20, 30, 50, 80, Inf)
    # pal <- colorBin("YlOrRd", domain = data$total, bins = bins)
    
    pal <- colorNumeric(
      palette = "YlOrRd",
      domain = data$total)
    
    map <- leaflet(
      padding = 1000,
      width = "80%",
      height = "700px",
      options = leafletOptions(
        zoomControl = TRUE,
        dragging = TRUE,
        scrollWheelZoom = FALSE
      )
    ) %>%
      addmouselatlon() %>%
      addProviderTiles(
        'Esri.WorldImagery',
        group = "World Imagery"
      ) %>%
      addTiles(group = "Open Street Map") %>%
      fitBounds(bounds[1], bounds[2], bounds[3], bounds[4]) %>%
      addMapPane("polygons", zIndex = 410) %>%
      addPolygons(
        data = data,
        label = data$total,
        layerId = data$ID,
        fillColor = ~ pal(data$total),
        fillOpacity = 1,
        color = "#444444",
        weight = 1
      )  %>%
      addLegend("bottomright", 
                data = data,
                pal = pal,
                values = ~data$total,
                title = "Number of clicks weighted by days",
                opacity = 0.9
      )
    map
    
  })
  
  ## Leaflet filter activities weighted ----
  output$leafactivityweighted <- renderLeaflet({
    
    req(input$sublevelinput, input$sourceinput)
    
    polygon.id.list <-
      data.frame(ID = as.character(c(1:(
        length(SpP@polygons)
      ))))
    
    if(input$level %in% c("cat")){
      
      polygons <- selected.polygons.activities %>%
        filter(nice.cat %in% c(input$sublevelinput))%>%
        filter(weighted.score > 0) %>%
        glimpse()
      
    } else if (input$level %in% c("sub")){
      
      polygons <- selected.polygons.activities %>%
        filter(nice.sub.dropdown %in% c(input$sublevelinput))%>%
        filter(weighted.score > 0) %>%
        glimpse()
      
    } else if (input$level %in% c("act")){
      
      polygons <- selected.polygons.activities %>%
        filter(nice.act.dropdown %in% c(input$sublevelinput))%>%
        filter(weighted.score > 0) %>%
        glimpse()
    } 
    
    total.clicked <- polygons %>%
      filter(source %in% c(input$sourceinput)) %>%
      dplyr::group_by(id) %>%
      dplyr::summarise(total = sum(weighted.score)) %>%
      dplyr::rename(ID = id) %>%
      dplyr::mutate(ID = as.character(ID)) %>%
      dplyr::full_join(polygon.id.list) %>%
      tidyr::replace_na(list(total = 0)) %>%
      mutate(ID = as.numeric(ID)) %>%
      arrange(ID) %>%
      dplyr::mutate(ID = as.character(ID))
    
    data <-
      SpatialPolygonsDataFrame(SpP, data = total.clicked, match.ID = FALSE)
    
    data <- data[data@data$total > 0,]
    
    bounds <- data %>% 
      st_bbox() %>% 
      as.character()
    
    bins <- c(0, 5, 10, 15, 25, 35, 50, 80, Inf)
    # pal <- colorBin("YlOrRd", domain = data$total, bins = bins)
    
    pal <- colorNumeric(
      palette = "YlOrRd",
      domain = data$total)
    
    map <- leaflet(
      padding = 1000,
      width = "80%",
      height = "700px",
      options = leafletOptions(
        zoomControl = TRUE,
        dragging = TRUE,
        scrollWheelZoom = FALSE
      )
    ) %>%
      addmouselatlon() %>%
      addProviderTiles(
        'Esri.WorldImagery',
        group = "World Imagery") %>%
      addTiles(group = "Open Street Map") %>%
      fitBounds(bounds[1], bounds[2], bounds[3], bounds[4]) %>%
      addMapPane("polygons", zIndex = 410) %>%
      addPolygons(
        data = data,
        label = data$total,
        layerId = data$ID,
        fillColor = ~ pal(data$total),
        fillOpacity = 1,
        color = "#444444",
        weight = 1
      )   %>%
      addLegend("bottomright", 
                data = data,
                pal = pal,
                values = ~data$total,
                title = "Number of clickes weighted by days",
                opacity = 0.9
      )
    map
    
  })
  
  ## Leaflet filter activity unweighted ----
  output$leafactivityunweighted <- renderLeaflet({
    
    req(input$sublevelinputun, input$sourceinput)
    
    polygon.id.list <-
      data.frame(ID = as.character(c(1:(
        length(SpP@polygons)
      ))))
    
    if(input$levelun %in% c("cat")){
      
      polygons <- selected.polygons.activities %>%
        filter(nice.cat %in% c(input$sublevelinputun))%>%
        filter(weighted.score > 0) %>%
        glimpse()
      
    } else if (input$levelun %in% c("sub")){
      
      polygons <- selected.polygons.activities %>%
        filter(nice.sub.dropdown %in% c(input$sublevelinputun))%>%
        filter(weighted.score > 0) %>%
        glimpse()
      
    } else if (input$levelun %in% c("act")){
      
      polygons <- selected.polygons.activities %>%
        filter(nice.act.dropdown %in% c(input$sublevelinputun))%>%
        filter(weighted.score > 0) %>%
        glimpse()
    } 
    
    total.clicked <- polygons %>%
      filter(source %in% c(input$sourceinput)) %>%
      dplyr::group_by(id) %>%
      dplyr::summarise(total = n()) %>%
      dplyr::rename(ID = id) %>%
      dplyr::mutate(ID = as.character(ID)) %>%
      dplyr::full_join(polygon.id.list) %>%
      tidyr::replace_na(list(total = 0)) %>%
      mutate(ID = as.numeric(ID)) %>%
      arrange(ID) %>%
      dplyr::mutate(ID = as.character(ID))
    
    data <-
      SpatialPolygonsDataFrame(SpP, data = total.clicked, match.ID = FALSE)
    
    data <- data[data@data$total > 0,]
    
    bounds <- data %>% 
      st_bbox() %>% 
      as.character()
    
    bins <- c(0, 5, 10, 15, 25, 35, 50, 80, Inf)
    # pal <- colorBin("YlOrRd", domain = data$total, bins = bins)
    
    pal <- colorNumeric(
      palette = "YlOrRd",
      domain = data$total)
    
    map <- leaflet(
      padding = 1000,
      width = "80%",
      height = "700px",
      options = leafletOptions(
        zoomControl = TRUE,
        dragging = TRUE,
        scrollWheelZoom = FALSE
      )
    ) %>%
      addmouselatlon() %>%
      addProviderTiles(
        'Esri.WorldImagery',
        group = "World Imagery") %>%
      addTiles(group = "Open Street Map") %>%
      fitBounds(bounds[1], bounds[2], bounds[3], bounds[4]) %>%
      addMapPane("polygons", zIndex = 410) %>%
      addPolygons(
        data = data,
        label = data$total,
        layerId = data$ID,
        fillColor = ~ pal(data$total),
        fillOpacity = 1,
        color = "#444444",
        weight = 1
      )   %>%
      addLegend("bottomright", 
                data = data,
                pal = pal,
                values = ~data$total,
                title = "Number of clicks",
                opacity = 0.9
      )
    map
    
  })
  
  ## Leaflet values ----
  output$leafvalues <- renderLeaflet({
    
    req(input$sourceinput, input$inputvalues)
    
    polygon.id.list <-
      data.frame(ID = as.character(c(1:(
        length(SpP@polygons)
      ))))
    
      polygons <- selected.polygons.values %>%
        filter(source %in% c(input$sourceinput)) %>%
        filter(nice.act %in% c(input$inputvalues)) %>%
        glimpse()
      
      descriptions <- polygons %>%
        select(id, nice.act, description) %>% 
        filter(!description %in% c(NA, "NA", "")) %>%
        group_by(id, nice.act) %>%
        mutate(description = paste(description, "new line", sep = " ")) %>%
        mutate(description = list(description)) %>%
        mutate(description = as.character(description)) %>%
        distinct() %>%
        ungroup() %>%
        mutate(description = str_replace_all(.$description, c("c[[:punct:]]" = "",
                                                              "[[:punct:]]" = "",
                                                              "new line" = ".<br>"))) %>%
        mutate(label = paste("<b>", nice.act, "colon", "endb <br>", 
                             description, "<br>", sep = "")) %>%
        group_by(id) %>%
        mutate(label = list(label)) %>%
        distinct(id, label) %>%
        ungroup() %>%
        mutate(label = as.character(label)) %>%
        mutate(label = str_replace_all(.$label, c("c[[:punct:]]" = "",
                                                  "[[:punct:]]" = "",
                                                  "colon" = ":",
                                                  "endb" = "</b>")))%>%
        dplyr::rename(ID = id) %>%
        dplyr::mutate(ID = as.character(ID))
      
    total.clicked <- polygons %>%
      dplyr::group_by(id) %>%
      dplyr::summarise(total = n()) %>%
      dplyr::rename(ID = id) %>%
      dplyr::mutate(ID = as.character(ID)) %>%
      dplyr::full_join(polygon.id.list) %>%
      tidyr::replace_na(list(total = 0)) %>%
      mutate(ID = as.numeric(ID)) %>%
      arrange(ID) %>%
      dplyr::mutate(ID = as.character(ID)) %>%
      left_join(descriptions)
    
    data <-
      SpatialPolygonsDataFrame(SpP, data = total.clicked, match.ID = FALSE)
    
    data <- data[data@data$total > 0,]
    
    bounds <- data %>% 
      st_bbox() %>% 
      as.character()
    
    bins <- c(0, 5, 10, 15, 25, 35, 50, 80, Inf)
    # pal <- colorBin("YlOrRd", domain = data$total, bins = bins)
    pal <- colorNumeric(
      palette = "YlOrRd",
      domain = data$total)
    
    map <- leaflet(
      padding = 1000,
      width = "80%",
      height = "700px",
      options = leafletOptions(
        zoomControl = TRUE,
        dragging = TRUE,
        scrollWheelZoom = FALSE
      )
    ) %>%
      addmouselatlon() %>%
      addProviderTiles(
        'Esri.WorldImagery',
        group = "World Imagery") %>%
      addTiles(group = "Open Street Map") %>%
      fitBounds(bounds[1], bounds[2], bounds[3], bounds[4]) %>%
      addMapPane("polygons", zIndex = 410) %>%
      addPolygons(
        data = data,
        label = data$total,
        popup = paste0(
          data$label
          ),
        layerId = data$ID,
        fillColor = ~ pal(data$total),
        fillOpacity = 1,
        color = "#444444",
        weight = 1
      )   %>%
      addLegend("bottomright", 
                data = data,
                pal = pal,
                values = ~data$total,
                title = "Number of clicks",
                opacity = 0.9
      )
    map
    
  })
  
  ## Leaflet pressures ----
  output$leafpressures <- renderLeaflet({
    
    req(input$sourceinput, input$inputpressures)
    
    polygon.id.list <-
      data.frame(ID = as.character(c(1:(
        length(SpP@polygons)
      ))))
    
    polygons <- selected.polygons.pressures %>%
      filter(source %in% c(input$sourceinput)) %>%
      mutate(tidy.name = paste(nice.sub, nice.act, sep = " - ")) %>%
      filter(tidy.name %in% c(input$inputpressures)) %>%
      glimpse()
    
    
    descriptions <- polygons %>%
      select(id, tidy.name, description) %>% 
      filter(!description %in% c(NA, "NA", "")) %>%
      group_by(id, tidy.name) %>%
      mutate(description = paste(description, "new line", sep = " ")) %>%
      mutate(description = list(description)) %>%
      mutate(description = as.character(description)) %>%
      distinct() %>%
      ungroup() %>%
      mutate(description = str_replace_all(.$description, c("c[[:punct:]]" = "",
                                                            "[[:punct:]]" = "",
                                                            "new line" = ".<br>"))) %>%
      mutate(label = paste("<b>", tidy.name, "colon", "endb <br>", 
                           description, "<br>", sep = "")) %>%
      group_by(id) %>%
      mutate(label = list(label)) %>%
      distinct(id, label) %>%
      ungroup() %>%
      mutate(label = as.character(label)) %>%
      mutate(label = str_replace_all(.$label, c("c[[:punct:]]" = "",
                                                "[[:punct:]]" = "",
                                                "colon" = ":",
                                                "endb" = "</b>")))%>%
      dplyr::rename(ID = id) %>%
      dplyr::mutate(ID = as.character(ID))
    
    total.clicked <- polygons %>%
      dplyr::group_by(id) %>%
      dplyr::summarise(total = n()) %>%
      dplyr::rename(ID = id) %>%
      dplyr::mutate(ID = as.character(ID)) %>%
      dplyr::full_join(polygon.id.list) %>%
      tidyr::replace_na(list(total = 0)) %>%
      mutate(ID = as.numeric(ID)) %>%
      arrange(ID) %>%
      dplyr::mutate(ID = as.character(ID)) %>%
      left_join(descriptions)
    
    data <- SpatialPolygonsDataFrame(SpP, data = total.clicked, match.ID = FALSE)
    
    data <- data[data@data$total > 0,]
    
    bounds <- data %>% 
      st_bbox() %>% 
      as.character()
    
    bins <- c(0, 5, 10, 15, 25, 35, 50, 80, Inf)
    # pal <- colorBin("YlOrRd", domain = data$total, bins = bins)
    
    pal <- colorNumeric(
      palette = "YlOrRd",
      domain = data$total)
    
    map <- leaflet(
      padding = 1000,
      width = "80%",
      height = "700px",
      options = leafletOptions(
        zoomControl = TRUE,
        dragging = TRUE,
        scrollWheelZoom = FALSE
      )
    ) %>%
      addmouselatlon() %>%
      addProviderTiles(
        'Esri.WorldImagery',
        group = "World Imagery") %>%
      addTiles(group = "Open Street Map") %>%
      fitBounds(bounds[1], bounds[2], bounds[3], bounds[4]) %>%
      addMapPane("polygons", zIndex = 410) %>%
      addPolygons(
        data = data,
        popup = paste0(
          data$label
        ),
        label = data$total,
        layerId = data$ID,
        fillColor = ~ pal(data$total),
        fillOpacity = 1,
        color = "#444444",
        weight = 1
      )   %>%
      addLegend("bottomright", 
                data = data,
                pal = pal,
                values = ~data$total,
                title = "Number of clicks",
                opacity = 0.9
      )
    map
    
  })
  
  ## Leaflet postcodes ----
  output$leafpostcodes <- renderLeaflet({
    
    req(input$go)
    
    sum.postcodes <- metadata %>%
      distinct(name, email, phone, source, postcode) %>%
      filter(source %in% c(input$sourceinput)) %>%
      group_by(postcode) %>%
      summarise(total.responses = n())
    
    gadm <- st_as_sf(postcodes.single) %>%
      rename(postcode = POA_CODE21) %>%
      left_join(sum.postcodes) %>%
      filter(total.responses > 0) %>%
      dplyr::select(postcode, total.responses, geometry)
    
    lines <- st_cast(gadm, "LINESTRING")
    gadm <- st_cast(gadm, "POLYGON")
    
    post.pal <- colorNumeric(
      palette = "YlOrRd",
      domain = gadm$total.responses)
    
    cols = grey.colors(nrow(gadm))
    cols_fill = topo.colors(nrow(gadm))
    
    m <- leaflet() %>%
      addTiles(group = "Open Street Map") %>%
      addGlPolygons(data = gadm, 
                    color = ~post.pal(total.responses),
                    fillColor = ~post.pal(total.responses),
                    popup = TRUE,
                    opacity = 0.9) %>%
      addGlPolylines(data = lines, 
                    color = "black",
                    opacity = 1) %>%
      addLegend("bottomright", 
                data = gadm,
                pal = post.pal,
                values = ~gadm$total.responses,
                title = "Number of users",
                opacity = 0.9
      )
    
    m
    
  })
  
  
  output$q9plot <- renderPlot({
    
    req(input$go)
    
    summarise.submitted <- submitted.values %>%
      filter(source %in% c(input$sourceinput)) %>%
      semi_join(., q.9) %>%
      dplyr::group_by(value) %>%
      dplyr::mutate(response= as.numeric(response)) %>%
      dplyr::summarise(mean.response = mean(response))
    
    colourCount = length(unique(summarise.submitted$value))
    
    ggplot(summarise.submitted,
           aes(x = value, y = mean.response, fill = value)) +
      geom_bar(width = 1, stat = "identity", col = "black") +
      # xlab("Value") +
      ylab("Average response") +
      ggtitle(paste(strwrap(unique(q.9$value), width = 90), collapse ="\n")) +
      Theme1 +
      scale_fill_manual(values = getPalette(colourCount)) +
      theme(axis.title.y = element_blank(), axis.text.y = element_blank()) + # blank as only one row
      coord_flip()+
      theme(legend.position = "none") +
      scale_y_continuous(limits = c(0, 7), expand = c(0, 0), breaks = 0:7,  labels = c(" " ,"Strongly\ndisagree","Disagree", "Somewhat\ndisagree",
                                                                                       "Neither agree\nor disagree", "Somewhat\nagree", "Agree", "Strongly\nagree"))+
      theme(plot.margin = margin(5,1,5,1, "cm"))
    
  })
  

  output$q10plot <- renderPlot({
    
    req(input$go)
    
    summarise.submitted <- submitted.values %>%
      filter(source %in% c(input$sourceinput)) %>%
      semi_join(., q.10) %>%
      dplyr::group_by(value) %>%
      dplyr::mutate(response= as.numeric(response)) %>%
      dplyr::summarise(mean.response = mean(response))
    
    colourCount = length(unique(summarise.submitted$value))
    
    ggplot(summarise.submitted,
           aes(x = value, y = mean.response, fill = value)) +
      geom_bar(width = 1, stat = "identity", col = "black") +
      # xlab("Value") +
      ylab("Average response") +
      ggtitle(paste(strwrap(unique(q.10$question), width = 90), collapse ="\n")) +
      Theme1 +
      scale_fill_manual(values = getPalette(colourCount)) +
      coord_flip()+
      theme(legend.position = "none", axis.title.y = element_blank()) +
      scale_y_continuous(limits = c(0, 7), expand = c(0, 0), breaks = 0:7,  labels = c(" " ,"Strongly\ndisagree","Disagree", "Somewhat\ndisagree",
                                                                                       "Neither agree\nor disagree", "Somewhat\nagree", "Agree", "Strongly\nagree"))+
      theme(plot.margin = margin(1,1,1,1, "cm"))

  })
  
  output$q11plot <- renderPlot({
    
    req(input$go)
    
    summarise.submitted <- submitted.values %>%
      filter(source %in% c(input$sourceinput)) %>%
      semi_join(., q.11) %>%
      dplyr::group_by(value) %>%
      dplyr::mutate(response= as.numeric(response)) %>%
      dplyr::summarise(mean.response = mean(response))
    
    colourCount = length(unique(summarise.submitted$value))
    
    ggplot(summarise.submitted,
           aes(x = value, y = mean.response, fill = value)) +
      geom_bar(width = 1, stat = "identity", col = "black") +
      scale_fill_manual(values = getPalette(colourCount)) +
      # xlab("Value") +
      ylab("Average response") +
      Theme1 +
      coord_flip() + 
      ggtitle(paste(strwrap(unique(q.11$question), width = 90), collapse ="\n")) +
      theme(legend.position = "none", axis.title.y = element_blank()) +
      scale_y_continuous(limits = c(0, 7), expand = c(0, 0), breaks = 0:7,  labels = c(" " ,"Strongly\ndisagree","Disagree", "Somewhat\ndisagree",
                                                                                       "Neither agree\nor disagree", "Somewhat\nagree", "Agree", "Strongly\nagree"))+
      theme(plot.margin = margin(1,1,1,1, "cm"))
    
  })
  
  output$visitedplot <- renderPlot({
    
    req(input$go)
    
    summarise.submitted <- metadata %>%
      distinct(name, email, phone, source, visited) %>%
      filter(source %in% c(input$sourceinput)) %>%
      dplyr::group_by(visited) %>%
      dplyr::summarise(value = n()) %>%
      filter(!is.na(visited)) %>%
      mutate(visited = fct_relevel(.$visited, "Yes", "No", "Unsure"))
    
    colourCount = length(unique(summarise.submitted$visited))
    
    ggplot(summarise.submitted,
           aes(x = visited, y = value, fill = visited)) +
      geom_bar(width = 1, stat = "identity", col = "black") +
      ylab("Number of responses") +
      xlab("Response") +
      ggtitle(paste(strwrap("Have you ever visited a marine park in Western Australia?", width = 90), collapse ="\n")) +
      Theme1 +
      scale_fill_manual(values = getPalette(colourCount))+
      scale_y_continuous(expand = c(0, 0))
    
  })
  
  
  output$awarenessplot <- renderPlot({
    
    req(input$go)
    
    summarise.submitted <- submitted.values %>%
      filter(source %in% c(input$sourceinput)) %>%
      distinct(value, response, name, email) %>%
      filter(!is.na(response)) %>%
      dplyr::filter(value == "Please rate your level of understanding of what a marine parks is ") %>%
      dplyr::group_by(response) %>%
      dplyr::summarise(value = n()) %>%
      mutate(response = fct_relevel(.$response, "Not at all aware", "Slightly aware", "Somewhat aware", "Moderately aware", "Extremely aware"))
    
    colourCount = length(unique(summarise.submitted$response))
    
    ggplot(summarise.submitted,
           aes(x = response, y = value, fill = response)) +
      geom_bar(width = 1, stat = "identity", col = "black") +
      ylab("Number of responses") +
      xlab("Response") +
      ggtitle(paste(strwrap("Please rate your level of understanding of what a marine parks is:", width = 90), collapse ="\n")) +
      Theme1 +
      scale_fill_manual(values = getPalette(colourCount))+
      scale_y_continuous(expand = c(0, 0))
    
  })
  
  # Aware marmion is a marine park
  output$awaremarmionmarineparkplot <- renderPlot({
    
    req(input$go)
    
    summarise.submitted <- metadata %>%
      distinct(name, email, phone, source, awaremarmionmarinepark) %>%
      filter(source %in% c(input$sourceinput)) %>%
      dplyr::group_by(awaremarmionmarinepark) %>%
      dplyr::summarise(value = n()) %>%
      filter(!is.na(awaremarmionmarinepark)) %>%
      mutate(awaremarmionmarinepark = fct_relevel(.$awaremarmionmarinepark, "Yes", "No", "Unsure"))
    
    colourCount = length(unique(summarise.submitted$awaremarmionmarinepark))
    
    ggplot(summarise.submitted,
           aes(x = awaremarmionmarinepark, y = value, fill = awaremarmionmarinepark)) +
      geom_bar(width = 1, stat = "identity", col = "black") +
      ylab("Number of responses") +
      xlab("Response") +
      ggtitle(paste(strwrap("Were you aware that Marmion is a Marine Park?", width = 90), collapse ="\n")) +
      Theme1 +
      scale_fill_manual(values = getPalette(colourCount))+
      scale_y_continuous(expand = c(0, 0))
    
  })
  
  # Fishing in sanctuary zones
  output$fishinginsanctuaryplot <- renderPlot({
    
    req(input$go)
    
    summarise.submitted <- metadata %>%
      distinct(name, email, phone, source, fishinginsanctuary) %>%
      filter(source %in% c(input$sourceinput)) %>%
      dplyr::group_by(fishinginsanctuary) %>%
      dplyr::summarise(value = n()) %>%
      filter(!is.na(fishinginsanctuary)) %>%
      mutate(fishinginsanctuary = fct_relevel(.$fishinginsanctuary, "Yes", "No", "Unsure"))
    
    colourCount = length(unique(summarise.submitted$fishinginsanctuary))
    
    ggplot(summarise.submitted,
           aes(x = fishinginsanctuary, y = value, fill = fishinginsanctuary)) +
      geom_bar(width = 1, stat = "identity", col = "black") +
      ylab("Number of responses") +
      xlab("Response") +
      ggtitle(paste(strwrap("Is recreation fishing permitted in the sanctuary zones?", width = 90), collapse ="\n")) +
      Theme1 +
      scale_fill_manual(values = getPalette(colourCount))+
      scale_y_continuous(expand = c(0, 0))
    
  })
  
  # Recreation in sanctuary
  output$recreationinsanctuaryplot <- renderPlot({
    
    req(input$go)
    
    summarise.submitted <- metadata %>%
      distinct(name, email, phone, source, recreationinsanctuary) %>%
      filter(source %in% c(input$sourceinput)) %>%
      dplyr::group_by(recreationinsanctuary) %>%
      dplyr::summarise(value = n()) %>%
      filter(!is.na(recreationinsanctuary)) %>%
      mutate(recreationinsanctuary = fct_relevel(.$recreationinsanctuary, "Yes", "No", "Unsure"))
    
    colourCount = length(unique(summarise.submitted$recreationinsanctuary))
    
    ggplot(summarise.submitted,
           aes(x = recreationinsanctuary, y = value, fill = recreationinsanctuary)) +
      geom_bar(width = 1, stat = "identity", col = "black") +
      ylab("Number of responses") +
      xlab("Response") +
      ggtitle(paste(strwrap("Are boating, diving and snorkelling permitted in the sanctuary zones?", width = 90), collapse ="\n")) +
      Theme1 +
      scale_fill_manual(values = getPalette(colourCount))+
      scale_y_continuous(expand = c(0, 0))
    
  })
  
  
  # Download data ----
  output$downloadmetadata <- downloadHandler(
    filename = function() {
      paste('marinevaluesmapper_user-metadata_', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.csv(dl.user.metadata, con, row.names = FALSE)
    }
  )
  
  output$downloadpolygons <- downloadHandler(
    filename = function() {
      paste('marinevaluesmapper_polygons-selected_', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.csv(dl.all.polygons, con, row.names = FALSE)
    }
  )
  
  output$downloadmatrix <- downloadHandler(
    filename = function() {
      paste('marinevaluesmapper_matrix-responses_', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.csv(dl.matrix, con, row.names = FALSE)
    }
  )
  
  output$downloadactivityshp <- downloadHandler(
    
    filename = function() {
      paste0("activities",
             Sys.Date(),
             '.zip')
    }, content = function(file) {  
      
      shinyalert(
        title = "Downloading...",
        size = "s",
        closeOnEsc = FALSE,
        closeOnClickOutside = FALSE,
        imageUrl = "https://cutewallpaper.org/21/loading-gif-transparent-background/Tag-For-Loading-Bar-Gif-Transparent-Loading-Gif-.gif",
        html = FALSE,
        showConfirmButton = FALSE,
        timer = 0,
        animation = FALSE
      )
      on.exit(shinyjs::runjs("swal.close();"))
      
      temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
      dir.create(temp_directory)
      
      polygon.id.list <-
        data.frame(ID = as.character(c(1:(
          length(SpP@polygons)
        ))))
      
      for(i in unique(selected.activities$nice.title)){
        
        polygons.pressures <- selected.polygons.activities %>%
          filter(nice.title == i) %>%
          dplyr::group_by(id) %>%
          dplyr::summarise(total.clicks = n()) %>%
          dplyr::rename(ID = id) %>%
          dplyr::mutate(ID = as.character(ID)) %>%
          dplyr::ungroup() %>%
          dplyr::full_join(polygon.id.list) %>%
          tidyr::replace_na(list(total.clicks = 0)) %>%
          mutate(ID = as.numeric(ID)) %>%
          arrange(ID) %>%
          dplyr::mutate(ID = as.character(ID))
        
        pressures.shp <- SpatialPolygonsDataFrame(SpP, data = polygons.pressures, match.ID = FALSE)
        
        fileName <- str_replace_all(i, c("-" = ".", "[^[:alnum:]]" = "_", "_e_g__4WD__" = ""))
        
        fileName <- paste(fileName, ".shp", sep = "")
        
        writeSpatialShape(pressures.shp, file.path(temp_directory, fileName))
        
        
      }
      
      #create the zip file
      zip::zip(zipfile = file, files = dir(temp_directory), root = temp_directory)
      
    }, contentType = "application/zip"
  )
  
  output$downloadvalueshp <- downloadHandler(
    
    filename = function() {
      paste0("values",
             Sys.Date(),
             '.zip')
    }, content = function(file) {  
      
      shinyalert(
        title = "Downloading...",
        size = "s",
        closeOnEsc = FALSE,
        closeOnClickOutside = FALSE,
        imageUrl = "https://cutewallpaper.org/21/loading-gif-transparent-background/Tag-For-Loading-Bar-Gif-Transparent-Loading-Gif-.gif",
        html = FALSE,
        showConfirmButton = FALSE,
        timer = 0,
        animation = FALSE
      )
      on.exit(shinyjs::runjs("swal.close();"))
      
      temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
      dir.create(temp_directory)
      
      polygon.id.list <-
        data.frame(ID = as.character(c(1:(
          length(SpP@polygons)
        ))))
      
      for(i in unique(selected.values$nice.title)){
        
        polygons.pressures <- selected.polygons.values %>%
          filter(nice.title == i) %>%
          dplyr::group_by(id) %>%
          dplyr::summarise(total.clicks = n()) %>%
          dplyr::rename(ID = id) %>%
          dplyr::mutate(ID = as.character(ID)) %>%
          dplyr::ungroup() %>%
          dplyr::full_join(polygon.id.list) %>%
          tidyr::replace_na(list(total.clicks = 0)) %>%
          mutate(ID = as.numeric(ID)) %>%
          arrange(ID) %>%
          dplyr::mutate(ID = as.character(ID))
        
        pressures.shp <- SpatialPolygonsDataFrame(SpP, data = polygons.pressures, match.ID = FALSE)
        
        fileName <- str_replace_all(i, c("-" = ".", "[^[:alnum:]]" = "_", "_e_g__4WD__" = ""))
        
        fileName <- paste(fileName, ".shp", sep = "")
        
        writeSpatialShape(pressures.shp, file.path(temp_directory, fileName))
        
        
      }
      
      #create the zip file
      zip::zip(zipfile = file, files = dir(temp_directory), root = temp_directory)
      
    }, contentType = "application/zip"
  )
  
  
  output$downloadpressureshp <- downloadHandler(
    
    filename = function() {
      paste0("pressures_",
             Sys.Date(),
             '.zip')
    }, content = function(file) {  
      
      shinyalert(
        title = "Downloading...",
        size = "s",
        closeOnEsc = FALSE,
        closeOnClickOutside = FALSE,
        imageUrl = "https://cutewallpaper.org/21/loading-gif-transparent-background/Tag-For-Loading-Bar-Gif-Transparent-Loading-Gif-.gif",
        html = FALSE,
        showConfirmButton = FALSE,
        timer = 0,
        animation = FALSE
      )
      on.exit(shinyjs::runjs("swal.close();"))
      
      temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
      dir.create(temp_directory)
      
      polygon.id.list <-
        data.frame(ID = as.character(c(1:(
          length(SpP@polygons)
        ))))
      
      for(i in unique(selected.pressures$nice.title)){
        
        polygons.pressures <- selected.polygons.pressures %>%
          filter(nice.title == i) %>%
          dplyr::group_by(id) %>%
          dplyr::summarise(total.clicks = n()) %>%
          dplyr::rename(ID = id) %>%
          dplyr::mutate(ID = as.character(ID)) %>%
          dplyr::ungroup() %>%
          dplyr::full_join(polygon.id.list) %>%
          tidyr::replace_na(list(total.clicks = 0)) %>%
          mutate(ID = as.numeric(ID)) %>%
          arrange(ID) %>%
          dplyr::mutate(ID = as.character(ID))
        
        pressures.shp <- SpatialPolygonsDataFrame(SpP, data = polygons.pressures, match.ID = FALSE)
        
        fileName <- str_replace_all(i, c("-" = ".", "[^[:alnum:]]" = "_", "_e_g__4WD__" = ""))
        
        fileName <- paste(fileName, ".shp", sep = "")
        
        writeSpatialShape(pressures.shp, file.path(temp_directory, fileName))
        
        
      }
      
      #create the zip file
      zip::zip(zipfile = file, files = dir(temp_directory), root = temp_directory)
      
    }, contentType = "application/zip"
  )
  
}