server <- function(input, output, session) {
  #########################################
  ##### 0 - Panel Light pollution     #####
  #########################################

  #display error message for erroneous inputs and save correct input to csv
  observeEvent(input$submit, {
    # reset first feedback after repeated input - important!
    hideFeedback("festival_name")
    hideFeedback("festival_year")
    hideFeedback("festival_type")
    
    # Initialize a flag
    valid <- TRUE
    
    # Validate festival_name
    if (nchar(input$festival_name) < 1) {
      showFeedbackDanger("festival_name", "Der Festivalname muss mindestens 1, maximal 100 Zeichen lang sein.")
      valid <- FALSE
    }
    
    # Validate festival_year
    if (is.na(input$festival_year)) {
      showFeedbackDanger("festival_year", "Bitte gib eine Jahreszahl ein.")
      valid <- FALSE
    } else if (input$festival_year < 2000 || input$festival_year > 2030) {
      showFeedbackDanger("festival_year", "Die Jahreszahl muss zwischen 2000 und 2030 liegen.")
      valid <- FALSE
    }
    
    # validate festival_type
    if (input$festival_type == "") {
      showFeedbackDanger("festival_type", "Bitte wähle einen Festivaltyp aus.")
      valid <- FALSE
    }
    
    # Only proceed if all inputs are valid
    if (valid) {
      showModal(
        modalDialog(
          title = "Das hat geklappt",
          "Deine Eingaben wurden gespeichert!",
          easyClose = TRUE,
          footer = modalButton("Schließen")  # Change 'dismiss' to 'Schließen'
        )
      )
      # save to CSV
      new_entry <- data.frame(
        festival_name = input$festival_name,
        festival_year = input$festival_year,
        festival_type = input$festival_type,
        Timestamp = Sys.time()
      )
      
      file_path <- "user_inputs.csv"
      
      if (!file.exists(file_path)) {
        write.csv(new_entry, file = file_path, row.names = FALSE)
      } else {
        write.table(new_entry, file = file_path, append = TRUE, sep = ",",
                    col.names = FALSE, row.names = FALSE)
      }
    }
  }) 
    
  
  
  df_map0 <- reactive({
    
    dataset <- light_pollution_indicators %>% filter(type_variable == input$indicator_pollum)
    
    return(dataset)
    
  })
  
  map0 <- reactive({
    
    
    pal <-  c("#1749e4", "#F9DC4C", "#fd0504")
    
    domain <- factor(df_map0()$level, levels = c("Low","Moderate", "High"))
    
    pal2 <- colorNumeric(pal, 
                         domain =  df_map0()$indice)
    
    pal3 <- colorFactor(pal, 
                        domain =  domain)
    
    labels <- sprintf(
                       "%s",
                       paste0(df_map0()$level, " level of upward emission")
                     ) %>% lapply(htmltools::HTML)
    
    
    title <- "Level of upward emission"
    
    map0 <- leaflet() %>%
      
      addProviderTiles(providers$CartoDB.DarkMatter) %>%
      
      
      addPolygons(data = base_map_mmm,
                  color = "black", weight = 1, smoothFactor = 0.5,
                  opacity = 1.0,
                  fillColor = "white",
                  fillOpacity = 0.2#,
                  # highlightOptions = highlightOptions(color = "black", weight = 2,
                  #                                     bringToFront = TRUE)
      ) %>%
      
      addPolygons(data = df_map0(), color = "#ffffff00", weight = 1, smoothFactor = 0.5,
                  opacity = 1.0, fillOpacity = 0.9,
                  fillColor = ~pal2(df_map0()$indice),
                  # highlightOptions = highlightOptions(color = "white", weight = 2,
                  #                                     bringToFront = TRUE),
                  label = labels,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto")) %>%
      
      addLegend("bottomleft", pal = pal3, values = domain,
                title = title,
                opacity = 1
      )
    
    
  })
  
  output$map0 <- renderLeaflet(map0())
  
  
  
  
  #########################################
  ##### 1 - Panel Ecological stakes   #####
  #########################################
  
  
  df <- tibble("Nightjar" = c("Impact of light pollution on dispersion according to ecological stakes",
                              "Biodiversity reservoirs"),
               "Amphibian" = c("Biodiversity reservoirs", 
                               NA), 
               "Lampyridae" = c("Impact of light pollution on dispersion according to ecological stakes",
                                "Biodiversity reservoirs"), 
               "Insects" = c("Impact of light pollution on dispersion according to ecological stakes",
                             "Biodiversity reservoirs"), 
               "Murine" = c("Impact of light pollution on dispersion according to ecological stakes",
                            NA),
               "Rhinolophus" = c("Impact of light pollution on dispersion according to ecological stakes",
                                 NA),
               "global" = c("Global score", 
                            "Priority areas for light pollution mitigation policies")) %>%
    gather(species, variable, Nightjar:global) %>%
    filter(!is.na(variable))
  
  observe({
    x <- df[df$species == input$famille_espece,]$variable
    
    if (is.null(x))
      x <- character(0)
    
    h6(updateSelectInput(getDefaultReactiveDomain(), "indicateur_ecolo",
                         label = NULL,
                         choices = x
    ))})
  
  
  
  
  map1 <- reactive({
    
    df_connectivity_map3_nightjar <- ecological_indicators %>%
      filter(species == "Nightjar", 
             type_variable == "impact_light_pollution",
             baseline_ecological_stakes == "3") %>%
      mutate(legend = factor(legend, levels = c("Strong loss of connectivity", "Moderate loss of connectivity")))
    
    
    df_connectivity_map2_nightjar <- ecological_indicators %>%
      filter(species == "Nightjar", 
             type_variable == "impact_light_pollution",
             baseline_ecological_stakes == "2") %>%
      mutate(legend = factor(legend, levels = c("Strong loss of connectivity", "Moderate loss of connectivity")))
    
    
    pal3 <- colorFactor(c("#092e6a", "#3b97c8"), 
                        domain = df_connectivity_map3_nightjar[["indicator"]])
    
    pal33 <- colorFactor(c("#092e6a", "#3b97c8"), 
                         domain = df_connectivity_map3_nightjar[["legend"]])
    
    pal2 <- colorFactor(c("#b31700", "#ef6547"), 
                        domain =  df_connectivity_map2_nightjar[["indicator"]])
    
    pal22 <- colorFactor(c("#b31700", "#ef6547"), 
                         domain = df_connectivity_map2_nightjar[["legend"]])
    
    labels2 <- sprintf(
      paste0("<strong> Nightjar </strong><br/>%s<br/>%s"),
      "Areas with high ecological stakes due to light pollution",
      paste0(df_connectivity_map2_nightjar[["legend"]], " due to light pollution")
    ) %>% lapply(htmltools::HTML)
    
    labels3 <- sprintf(
      paste0("<strong> Nightjar </strong><br/>%s<br/>%s"),
      "Areas with moderate ecological stakes due to light pollution",
      paste0(df_connectivity_map3_nightjar[["legend"]], " due to light pollution")
    ) %>% lapply(htmltools::HTML)
    
    map2 <- leaflet() %>%
      
      addProviderTiles(providers$CartoDB.DarkMatter) %>%
      
      addPolygons(data = df_connectivity_map2_nightjar,
                  color = "#ffffff00", weight = 1, smoothFactor = 0.5,
                  opacity = 1, fillOpacity = 0.7,
                  fillColor = ~pal2(df_connectivity_map2_nightjar[["indicator"]]),
                  highlightOptions = highlightOptions(color = "grey", weight = 1,
                                                      bringToFront = TRUE),
                  label = labels2,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto")) %>%
      
      addPolygons(data = df_connectivity_map3_nightjar,
                  color = "#ffffff00", weight = 1, smoothFactor = 0.5,
                  opacity = 1, fillOpacity = 0.7,
                  fillColor = ~pal3(df_connectivity_map3_nightjar[["indicator"]]),
                  highlightOptions = highlightOptions(color = "white", weight = 1,
                                                      bringToFront = TRUE),
                  label = labels3,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto")) %>%
      
      addLegend("bottomleft", pal = pal33, values = df_connectivity_map3_nightjar[["legend"]],
                #title = "<strong style='font-weight:normal'>Areas with moderate ecological stakes without light pollution</strong><br>",
                title = HTML("
                  <div style='width: 380px;'>
                    <span style='font-weight: normal;'>Areas with moderate ecological stakes without light pollution</span><br>
                  </div>
                "),
                opacity = 1
      ) %>%
      
      addLegend("bottomleft", pal = pal22, values = df_connectivity_map2_nightjar[["legend"]],
                #title = paste0("Impact of light pollution on dispersion (Nightjar) <br> <strong style='font-weight:normal'>Areas with high ecological stakes without light pollution</strong><br>"),
                title = HTML("
                  <div style='width: 380px;'>
                    <b>Impact of light pollution on dispersion (Nightjar)</b><br>
                    <span style='font-weight: normal;'>Areas with high ecological stakes without light pollution</span><br>
                  </div>
                "),
                opacity = 1
      ) 
    
    
  })
  
  output$map1 <- renderLeaflet(map1())
  
  df_map1 <- reactive({
    
    indicateur_ecolo_abreviation <- switch(input$indicateur_ecolo,
                                           "Impact of light pollution on dispersion according to ecological stakes" = "impact_light_pollution",
                                           "Biodiversity reservoirs" = "rb",
                                           "Global score" = "global_score",
                                           "Priority areas for light pollution mitigation policies" = "priority_area")
    
    dataset <- ecological_indicators %>%
      filter(species == input$famille_espece, 
             type_variable == indicateur_ecolo_abreviation)
    
    return(dataset)
    
  })
  
  observeEvent(input$go, {
    
    indicateur_ecolo_abreviation <- switch(input$indicateur_ecolo,
                                           "Impact of light pollution on dispersion according to ecological stakes" = "impact_light_pollution",
                                           "Biodiversity reservoirs" = "rb",
                                           "Global score" = "global_score",
                                           "Priority areas for light pollution mitigation policies" = "priority_area")
    
    if(indicateur_ecolo_abreviation == "impact_light_pollution"){
      
      df_connectivity_map3 <- df_map1() %>%
        filter(baseline_ecological_stakes == "3") %>%
        mutate(legend = factor(legend, levels = c("Strong loss of connectivity", "Moderate loss of connectivity")))
      
      
      df_connectivity_map2 <- df_map1() %>%
        filter(baseline_ecological_stakes == "2") %>%
        mutate(legend = factor(legend, levels = c("Strong loss of connectivity", "Moderate loss of connectivity")))
      
      
      pal3 <- colorFactor(c("#092e6a", "#3b97c8"), 
                          domain = df_connectivity_map3[["indicator"]])
      
      pal33 <- colorFactor(c("#092e6a", "#3b97c8"), 
                           domain = df_connectivity_map3[["legend"]])
      
      pal2 <- colorFactor(c("#b31700", "#ef6547"), 
                          domain =  df_connectivity_map2[["indicator"]])
      
      pal22 <- colorFactor(c("#b31700", "#ef6547"), 
                           domain = df_connectivity_map2[["legend"]])
      
      labels2 <- sprintf(
        paste0("<strong>", as.character(input$famille_espece), "</strong><br/>%s<br/>%s"),
        "Areas with high ecological stakes due to light pollution",
        paste0(df_connectivity_map2[["legend"]], " due to light pollution")
      ) %>% lapply(htmltools::HTML)
      
      labels3 <- sprintf(
        paste0("<strong>", as.character(input$famille_espece), "</strong><br/>%s<br/>%s"),
        "Areas with moderate ecological stakes due to light pollution",
        paste0(df_connectivity_map3[["legend"]], " due to light pollution")
      ) %>% lapply(htmltools::HTML)
      
      map2 <- leaflet() %>%
        
        addProviderTiles(providers$CartoDB.DarkMatter) %>%
        
        addPolygons(data = df_connectivity_map2,
                    color = "#ffffff00", weight = 1, smoothFactor = 0.5,
                    opacity = 1, fillOpacity = 0.7,
                    fillColor = ~pal2(df_connectivity_map2[["indicator"]]),
                    highlightOptions = highlightOptions(color = "grey", weight = 1,
                                                        bringToFront = TRUE),
                    label = labels2,
                    labelOptions = labelOptions(
                      style = list("font-weight" = "normal", padding = "3px 8px"),
                      textsize = "15px",
                      direction = "auto")) %>%
        
        addPolygons(data = df_connectivity_map3,
                    color = "#ffffff00", weight = 1, smoothFactor = 0.5,
                    opacity = 1, fillOpacity = 0.7,
                    fillColor = ~pal3(df_connectivity_map3[["indicator"]]),
                    highlightOptions = highlightOptions(color = "white", weight = 1,
                                                        bringToFront = TRUE),
                    label = labels3,
                    labelOptions = labelOptions(
                      style = list("font-weight" = "normal", padding = "3px 8px"),
                      textsize = "15px",
                      direction = "auto")) %>%
        
        addLegend("bottomleft", pal = pal33, values = df_connectivity_map3[["legend"]],
                  #title = "<strong style='font-weight:normal'>Areas with moderate ecological stakes without light pollution</strong><br>",
                  title = HTML("
                  <div style='width: 380px;'>
                    <span style='font-weight: normal;'>Areas with moderate ecological stakes without light pollution</span><br>
                  </div>
                "),
                  opacity = 1
        )%>%
        
        addLegend("bottomleft", pal = pal22, values = df_connectivity_map2[["legend"]],
                  #title = paste0("Impact of light pollution on dispersion (", as.character(input$famille_espece), ") <br> <strong style='font-weight:normal'>Areas with high ecological stakes without light pollution</strong><br>"),
                  title = HTML(
                    paste0("<div style='width: 380px;'>",
                           "<b>Impact of light pollution on dispersion (", as.character(input$famille_espece),")</b><br>",
                           "<span style='font-weight: normal;'>Areas with high ecological stakes without light pollution</span><br>
                  </div>")
                  ),
                  opacity = 1
        ) 
      
      
      
      
      
      
      
    }
    
    else if (indicateur_ecolo_abreviation == "rb") {
      df_rb <- df_map1() %>%
        mutate(legend = factor(legend, levels = c("Unaffected by light pollution", 
                                                  "Affected by light pollution")))
      
      pal2 <- colorFactor(c("#092e6a", "#a0c9e4"), 
                          domain =  df_rb$indicator)
      
      pal22 <- colorFactor(c("#092e6a", "#a0c9e4"), 
                           domain =  df_rb$legend)
      
      labels2 <- sprintf(
        paste0("<strong>",  as.character(input$famille_espece), "</strong><br/>%s"),
        df_rb$legend
      ) %>% lapply(htmltools::HTML)
      
      map2 <- leaflet() %>%
        
        addProviderTiles(providers$CartoDB.DarkMatter) %>%
        
        addPolygons(data = df_rb, 
                    color = "#ffffff00", weight = 1, smoothFactor = 0.5,
                    opacity = 1, fillOpacity = 0.7,
                    fillColor = ~pal2(df_rb[["indicator"]]),
                    highlightOptions = highlightOptions(color = "white", weight = 1,
                                                        bringToFront = TRUE),
                    label = labels2,
                    labelOptions = labelOptions(
                      style = list("font-weight" = "normal", padding = "3px 8px"),
                      textsize = "15px",
                      direction = "auto")) %>%
        
        addLegend("bottomleft", pal = pal22, values = df_rb$legend,
                  title = paste0("Biodiversity reservoirs (", as.character(input$famille_espece), ")"), 
                  
                  opacity = 1
        ) 
      
      
    }
    
    else{
      palette_indicator <- switch(indicateur_ecolo_abreviation, 
                                  "global_score" = c("#fde725", "#9ed93a", "#4ac26d", "#1fa288", "#277f8e", "#365c8d", "#46327f", "#440154"), 
                                  "priority_area" = c("#fde725", "#9ed93a", "#4ac26d", "#1fa288", "#277f8e", "#365c8d", "#46327f", "#440154"))
      
      pal2 <- colorFactor(palette_indicator, 
                          domain =  df_map1()$indicator)
      
      
      labels2 <- switch(indicateur_ecolo_abreviation, 
                        "global_score" = sprintf(
                          "<strong>%s</strong><br/>%s<br/>%s<br/>%s",
                          "Global score", 
                          paste0(ifelse(df_map1()$baseline_ecological_stakes %in% c("2"), "High ecological stakes without light pollution", "Moderate ecological stakes without light pollution")), 
                          paste0(df_map1()$loss_connectivity, " loss of connectivity"),
                          paste0(df_map1()$impacted_species, " impacted groups of species")
                        ) %>% lapply(htmltools::HTML), 
                        
                        "priority_area" = sprintf(
                          "<strong>%s</strong><br/>%s<br/>%s<br/>%s",
                          "Global score", 
                          paste0(ifelse(df_map1()$baseline_ecological_stakes %in% c("2"), "High ecological stakes without light pollution", "Moderate ecological stakes without light pollution")), 
                          paste0(df_map1()$loss_connectivity, " loss of connectivity"),
                          paste0(df_map1()$impacted_species, " impacted groups of species")
                        ) %>% lapply(htmltools::HTML))
      
      image <- switch(indicateur_ecolo_abreviation, 
                      "global_score" = "legend_map_global", 
                      "priority_area" = "legend_map_light")
      
      title <- switch(indicateur_ecolo_abreviation,
                      "global_score" = "Global score",
                      "priority_area" = "Priority areas for light pollution mitigation policies")
      
      
      
      map2 <- leaflet() %>%
        
        addProviderTiles(providers$CartoDB.DarkMatter) %>%
        
        addPolygons(data = df_map1(),
                    color = "#ffffff00", weight = 1, smoothFactor = 0.5,
                    opacity = 1, fillOpacity = 0.7,
                    fillColor = ~pal2(df_map1()[["indicator"]]),
                    highlightOptions = highlightOptions(color = "white", weight = 1,
                                                        bringToFront = TRUE),
                    label = labels2,
                    labelOptions = labelOptions(
                      style = list("font-weight" = "normal", padding = "3px 8px"),
                      textsize = "15px",
                      direction = "auto")
        )  %>%
        
        addLegendImage(images = paste0("./WWW/", image, ".png"), 
                       labels = "", 
                       position = "bottomleft",
                       title = title, 
                       width = 350,
                       height = 330)
    }
    
    
    
    
    
    
    output$map1 <- renderLeaflet(map2)
    
  })
  
  
  
  
  #####################################################
  ##### 2 - Onglet enjeux d'acceptabilité sociale #####
  #####################################################
  
  
  
  map3 <- reactive({
    
    bins <- switch(input$choix_indic_accept, 
                   "1" = c(-77, -20, -5, 5, 15, 30, 50), 
                   "2" = c(-121, -20, -5, 5, 15, 30, 47))
    
    domain <- switch(input$choix_indic_accept, 
                     "1" = socio_eco_indicators$cat_acceptability_score_extinction1, 
                     "2" = socio_eco_indicators$cat_acceptability_score_extinction2)
    
    pal2 <- colorBin(c("#d73027", "#fdae61", "#fffdbf", "#abd9e9", "#4575b4", "#323695"), 
                     bins = bins, 
                     na.color = "white")
    pal3 <- colorFactor(c("#d73027", "#fdae61", "#fffdbf", "#abd9e9", "#4575b4", "#323695"), 
                        domain =  domain)
    
    labels <- switch(input$choix_indic_accept, 
                     "1" = sprintf(
                       "<strong>%s</strong><br/>Acceptation score: %g (%s)",
                       paste0(socio_eco_indicators[["LIBCOM"]], ", ", socio_eco_indicators[["LIBIRIS"]]), round(socio_eco_indicators$acceptability_score_extinction1, 1), 
                       socio_eco_indicators$cat_acceptability_score_extinction1
                     ) %>% lapply(htmltools::HTML), 
                     
                     "2" = sprintf(
                       "<strong>%s</strong><br/>Acceptation score: %g (%s)",
                       paste0(socio_eco_indicators[["LIBCOM"]], ", ", socio_eco_indicators[["LIBIRIS"]]), round(socio_eco_indicators$acceptability_score_extinction2, 1), 
                       socio_eco_indicators$cat_acceptability_score_extinction2
                     ) %>% lapply(htmltools::HTML))
    
    chloropeth <- switch(input$choix_indic_accept, 
                         "1" = socio_eco_indicators$acceptability_score_extinction1, 
                         "2" = socio_eco_indicators$acceptability_score_extinction2)
    
    title <- switch(input$choix_indic_accept, 
                    "1" = "Social acceptation for public</br>lighting extinction from 1 a.m. to 5 a.m.", 
                    "2" = "Social acceptation for public</br>lighting extinction from 11 p.m. to 6 a.m.")
    
    map2 <- leaflet() %>%
      
      addProviderTiles(providers$CartoDB.DarkMatter) %>%
      
      
      addPolygons(data = base_map_mmm,
                  color = "black", weight = 1, smoothFactor = 0.5,
                  opacity = 1.0,
                  fillColor = "white",
                  fillOpacity = 0.2#,
                  # highlightOptions = highlightOptions(color = "black", weight = 2,
                  #                                     bringToFront = TRUE)
      ) %>%
      
      addPolygons(data = socio_eco_indicators, 
                  color = "darkgrey", weight = 1, smoothFactor = 0.5,
                  opacity = 1.0, fillOpacity = 0.9,
                  fillColor = ~pal2(chloropeth),
                  highlightOptions = highlightOptions(color = "white", weight = 2,
                                                      bringToFront = TRUE),
                  label = labels,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto")) %>%
      
      addLegend("bottomleft", pal = pal3, values = domain,
                title = title, 
                
                opacity = 1
      )
    
    
  })
  
  output$map3 <- renderLeaflet(map3())
  
  
  
  ########################################
  ##### 3 - Onglet carte de synthese #####
  ########################################
  
  map4 <- reactive({ 
    
    pal44 <- colorFactor(palette = c( "#94bdd4", "#51a5d6", "#0088d9", 
                                      "#0285a1", 
                                      "#d7c659", "#99b35a", "#529c5a", "#03815c", 
                                      "#d9be01", "#9aab00", "#539600", "#007b00"),
                         domain = summary_indicators_high$indicator_extinction1,
                         na.color = "white")
    
    labels <- sprintf(
      "<strong>%s</strong><br/>Acceptation score : %g (%s)<br/>%s loss of connectivity <br/> <strong>%s</strong> groups of impacted species",
      paste0(summary_indicators_high[["LIBCOM"]], ", ", summary_indicators_high[["LIBIRIS"]]), 
      round(summary_indicators_high$acceptability_score_extinction1, 1),
      summary_indicators_high$cat_acceptability_score_extinction1,
      summary_indicators_high$loss_connectivity, 
      summary_indicators_high$impacted_species
    ) %>% lapply(htmltools::HTML)
    
    map4 <- leaflet() %>%
      
      addProviderTiles(providers$CartoDB.DarkMatter) %>%
      
      addPolygons(data = base_map_mmm,
                  color = "black", weight = 1, smoothFactor = 0.5,
                  opacity = 1.0, 
                  fillColor = "white", 
                  fillOpacity = 0.2#,
                  # highlightOptions = highlightOptions(color = "white", weight = 2,
                  #                                     bringToFront = TRUE) 
      ) %>%
      
      addPolygons(data = summary_indicators_high,
                  color = "#ffffff00", 
                  fillColor = ~pal44(summary_indicators_high$indicator_extinction1),
                  highlightOptions = highlightOptions(color = "white", weight = 1,
                                                      bringToFront = TRUE),
                  opacity = 1.0,  
                  fillOpacity = 0.9,
                  label = labels,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto")) %>%
      
      
      addLegendImage(images =  "./www/legend_map_4x4.png", 
                     labels = "", 
                     position = "bottomleft",
                     width = 400,
                     height = 400)
    
  })
  
  output$map4 <- renderLeaflet(map4())
  
  
  
  
  observeEvent(input$go3, {
    
    
    if(input$baseline_ecological_stakes_synthese == "2"){
      
      pal44 <- switch(input$choix_indic_accept_synthese, 
                      "1" = colorFactor(palette = c( "#94bdd4", "#51a5d6", "#0088d9", 
                                                     "#0285a1", 
                                                     "#d7c659", "#99b35a", "#529c5a", "#03815c", 
                                                     "#d9be01", "#9aab00", "#539600", "#007b00"),
                                        domain = summary_indicators_high$indicator_extinction1,
                                        na.color = "white"), 
                      
                      "2" = colorFactor(palette = c( "#d3d3d3", "#94bdd4", "#51a5d6", "#0088d9", 
                                                     "#53a19f", "#0285a1", 
                                                     "#d7c659", "#99b35a", "#529c5a", "#03815c", 
                                                     "#d9be01", "#9aab00", "#539600", "#007b00"),
                                        domain = summary_indicators_high$indicator_extinction2,
                                        na.color = "white")) 
      
      wtp <- switch(input$choix_indic_accept_synthese, 
                    "1" = summary_indicators_high$acceptability_score_extinction1, 
                    "2" = summary_indicators_high$acceptability_score_extinction2)
      
      indicator <- switch(input$choix_indic_accept_synthese, 
                          "1" = summary_indicators_high$indicator_extinction1, 
                          "2" = summary_indicators_high$indicator_extinction2)
      
      
      
      labels <- switch(input$choix_indic_accept_synthese, 
                       "1" = sprintf(
                         "<strong>%s</strong><br/>Acceptation score : %g (%s)<br/>%s loss of connectivity <br/> <strong>%s</strong> groups of impacted species",
                         paste0(summary_indicators_high[["LIBCOM"]], ", ", summary_indicators_high[["LIBIRIS"]]), 
                         round(wtp, 1),
                         summary_indicators_high$cat_acceptability_score_extinction1,
                         summary_indicators_high$loss_connectivity, 
                         summary_indicators_high$impacted_species
                       ) %>% lapply(htmltools::HTML), 
                       "2" = sprintf(
                         "<strong>%s</strong><br/>Acceptation score : %g (%s)<br/>%s loss of connectivity <br/> <strong>%s</strong> groups of impacted species",
                         paste0(summary_indicators_high[["LIBCOM"]], ", ", summary_indicators_high[["LIBIRIS"]]), 
                         round(wtp, 1),
                         summary_indicators_high$cat_acceptability_score_extinction2,
                         summary_indicators_high$loss_connectivity, 
                         summary_indicators_high$impacted_species
                       ) %>% lapply(htmltools::HTML)
                       
                       )
      
      
      map4 <- leaflet() %>%
        
        addProviderTiles(providers$CartoDB.DarkMatter) %>%
        
        addPolygons(data = base_map_mmm,
                    color = "black", weight = 1, smoothFactor = 0.5,
                    opacity = 1.0, 
                    fillColor = "white", 
                    fillOpacity = 0.2#,
                    # highlightOptions = highlightOptions(color = "white", weight = 2,
                    #                                     bringToFront = TRUE) 
        ) %>%
        
        addPolygons(data = summary_indicators_high,
                    color = "#ffffff00", 
                    fillColor = ~pal44(indicator),
                    highlightOptions = highlightOptions(color = "white", weight = 1,
                                                        bringToFront = TRUE),
                    opacity = 1.0,  
                    fillOpacity = 0.9,
                    label = labels,
                    labelOptions = labelOptions(
                      style = list("font-weight" = "normal", padding = "3px 8px"),
                      textsize = "15px",
                      direction = "auto")) %>%
        
        
        addLegendImage(images =  "./www/legend_map_4x4.png", 
                       labels = "", 
                       position = "bottomleft",
                       width = 400,
                       height = 400)
      
      
    }
    
    else{
      pal44 <- switch(input$choix_indic_accept_synthese, 
                      "1" = colorFactor(palette = c( "#51a5d6", 
                                                     "#95b89e", "#53a19f", "#0285a1", 
                                                     "#d7c659", "#99b35a", "#529c5a", "#03815c",
                                                     "#d9be01", "#9aab00", "#539600", "#007b00"),
                                        domain = summary_indicators_moderate$indicator_extinction1,
                                        na.color = "white"), 
                      
                      "2" = colorFactor(palette = c(  "#d3d3d3",  
                                                      "#d5ce9c", "#95b89e", "#53a19f", "#0285a1", 
                                                      "#d7c659", "#99b35a", "#529c5a", "#03815c",
                                                      "#d9be01", "#9aab00", "#539600", "#007b00"),
                                        domain = summary_indicators_moderate$indicator_extinction2,
                                        na.color = "white")) 
      
      wtp <- switch(input$choix_indic_accept_synthese, 
                    "1" = summary_indicators_moderate$acceptability_score_extinction1, 
                    "2" = summary_indicators_moderate$acceptability_score_extinction2)
      
      
      indicator <- switch(input$choix_indic_accept_synthese, 
                          "1" = summary_indicators_moderate$indicator_extinction1, 
                          "2" = summary_indicators_moderate$indicator_extinction2)
      
      
      labels <- switch(input$choix_indic_accept_synthese,
                      "1" = sprintf(
                        "<strong>%s</strong><br/>Acceptation score : %g (%s)<br/>%s loss of connectivity <br/> <strong>%s</strong> groups of impacted species",
                        paste0(summary_indicators_moderate[["LIBCOM"]], ", ", summary_indicators_moderate[["LIBIRIS"]]), 
                        round(wtp, 1),
                        summary_indicators_moderate$cat_acceptability_score_extinction1,
                        summary_indicators_moderate$loss_connectivity, 
                        summary_indicators_moderate$impacted_species
                      ) %>% lapply(htmltools::HTML), 
                      "2" = sprintf(
                        "<strong>%s</strong><br/>Acceptation score : %g (%s)<br/>%s loss of connectivity <br/> <strong>%s</strong> groups of impacted species",
                        paste0(summary_indicators_moderate[["LIBCOM"]], ", ", summary_indicators_moderate[["LIBIRIS"]]), 
                        round(wtp, 1),
                        summary_indicators_moderate$cat_acceptability_score_extinction2,
                        summary_indicators_moderate$loss_connectivity, 
                        summary_indicators_moderate$impacted_species
                      ) %>% lapply(htmltools::HTML))
                      
      
      map4 <- leaflet() %>%
        
        addProviderTiles(providers$CartoDB.DarkMatter) %>%
        
        addPolygons(data = base_map_mmm,
                    color = "black", weight = 1, smoothFactor = 0.5,
                    opacity = 1.0, 
                    fillColor = "white", 
                    fillOpacity = 0.2
        ) %>%
        
        addPolygons(data = summary_indicators_moderate,
                    color = "#ffffff00", 
                    fillColor = ~pal44(indicator),
                    highlightOptions = highlightOptions(color = "white", weight = 1,
                                                        bringToFront = TRUE),
                    opacity = 1.0,  
                    fillOpacity = 0.9,
                    label = labels,
                    labelOptions = labelOptions(
                      style = list("font-weight" = "normal", padding = "3px 8px"),
                      textsize = "15px",
                      direction = "auto")) %>%
        
        
        addLegendImage(images =  "./www/legend_map_4x4.png", 
                       labels = "", 
                       position = "bottomleft",
                       width = 400,
                       height = 400)
      
      
    }
    
    
    
    
    
    
    
    output$map4 <- renderLeaflet(map4)
    
  })
  
  
  
}

