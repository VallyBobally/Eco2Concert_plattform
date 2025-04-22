ui <- bootstrapPage(
  useShinyFeedback(),
  
  # Custom styles to hide the feedback icon
  tags$head(
    tags$style(HTML("
      .shiny-feedback-icon {
        display: none !important;
        visibility: hidden !important;
        width: 0 !important;
        height: 0 !important;
      }

      .form-group.has-feedback .form-control-feedback {
        display: none !important;
      }

      .shiny-input-container .shiny-feedback-text {
        margin-left: 0 !important;
        padding-left: 0 !important;
      }
    "))
  ),
  navbarPage(
    title = HTML('<a style="text-decoration:none;cursor:default;color:#FFFFFF;font-weight:normal;" class="active" href="#">"ECO2CONCERT" Planungsplattform</a>'), id="nav",
    windowTitle = "ECO2CONCERT", 
    
    tabPanelAbout(), 
    
    # Summary Map
    tabPanel(HTML('<span style="font-size:100%;color:white;font-weight:normal;">Deine Festivaldaten</span></a>'),
             
             #for error messages on user input
             useShinyFeedback(),
             
             tags$head(
               includeCSS("styles.css"), tags$link(rel = "icon", type = "image/png", href = "starry_sky.jpg"),
               
               #should listen for dynamic navBar adjustments (does not work quite yet)
               tags$script(HTML("
               function updateLayoutOffsets() {
                  const nav = document.querySelector('.navbar'); // get height of navbar
                  const navCollapse = document.querySelector('.navbar-collapse'); // get height of collapsed menu area
                  let navHeight = nav ? nav.offsetHeight : 60;
                  
                  //add height to total navHeight when hamburger is expanded
                  if (navCollapse && navCollapse.classList.contains('in')) {
                    // If expanded, include height of the dropdown
                    navHeight += navCollapse.offsetHeight;
                    }
                    
                  // adjust background image wrapper to any current height
                  const bgWrapper = document.querySelector('.background-image-wrapper');
                  if (bgWrapper) {
                    bgWrapper.style.top = navHeight + 'px';
                    bgWrapper.style.height = 'calc(100vh - ' + navHeight + 'px)';
                  }
                  
                  // Adjust position of explainText panel
                  const explainPanel = document.getElementById('explainText');
                  if (explainPanel) {
                    explainPanel.style.top = navHeight + 'px';
                  }
                  
                  // Adjust position of yourFestival panel
                  const festivalPanel = document.getElementById('yourFestival');
                  if (explainPanel && festivalPanel) {
                    const explainRect = explainPanel.getBoundingClientRect();
                    festivalPanel.style.top = (explainRect.bottom + 30) + 'px'; // 30px for the space between panels
                  }
                }
                
                // Update offsets when the page is loaded and when the window is resized
                window.addEventListener('load', updateLayoutOffsets);
                window.addEventListener('resize', updateLayoutOffsets);
                
                "))
               ),
             
             div(class = "background-image-wrapper"),
             
             absolutePanel(                   
               id = "explainText",
               class = "panel panel-default",
               fixed = TRUE, left = "50%", right = "auto", bottom = "auto",
               width = "auto", height = "auto",
               style = "
               transform: translateX(-50%);
               background-color: white;
               padding: 0px 5px;
               border-radius: 0px;
               z-index: 1000;
               ",
               h4(HTML('<div style="text-align:justify;">
               Gib hier Daten über dein Festival ein, die in der Simulation verwendet werden. <br>
               Optionale Daten machen die Simulation genauer. Wenn du sie weglässt, verwenden wir Durchschnittswerte verschiedener Festivals, 
               welche in den angegebenen Punkten deinem Festival ähneln <br> 
               (<i>z.B. wenn du das Einzugsgebiet deines Festivals nicht kennst</i>).
                       </div>'))
             ),
             
             absolutePanel(
               id = "yourFestival", class = "panel panel-default", fixed = TRUE, left = "50%", 
               right = "auto", bottom = "auto",
               width = 600, height = "auto", style = "transform: translate(-50%, 0); padding: 20px;",
               
               h4("BASISDATEN"),
               
               # Two-column layout using HTML and divs
               tags$div(style = "display: flex; align-items: center; margin-bottom: 10px;",
                        tags$div(style = "width: 40%; font-weight: normal;", HTML('<div style="text-align:left;">
                        Festivalname<br><i>Der Name deines Festivals</i></div>')),
                        tags$div(style = "width: 60%;", textInput("festival_name", NULL, placeholder = "z.B. splash!"))
               ),
               
               tags$div(style = "display: flex; align-items: center; margin-bottom: 10px;",
                        tags$div(style = "width: 40%; font-weight: normal;", HTML('<div style="text-align:left;">
                        Jahr:<br><i>Das Jahr, das du als Datengrundlage nehmen möchtest</i></div>')),
                        tags$div(style = "width: 60%;", numericInput("festival_year", NULL, value = NA, min = 2024, max = 3000), 
                                 #adds placeholder instead of default value
                                 tags$script(HTML("
                                                  $(document).ready(function() {
                                                  $('#festival_year').attr('placeholder', 'z.B. 2024');})")))
               ),
               
               tags$div(style = "display: flex; align-items: center; margin-bottom: 10px;",
                        tags$div(style = "width: 40%; font-weight: normal;", HTML('<div style="text-align:left;">
                        Festivaltyp:<br><i>Gib den Festivaltyp an</i></div>')),
                        tags$div(style = "width: 60%;", 
                                 selectInput("festival_type", NULL, choices = c("Klicke um auszuwählen" = "", "Urban", "Semi-Urban", "Greenfield"), selected = "")), 
                                 tags$script(HTML("
                                 $(document).on('shiny:inputinitialized', function() {
                                    const select = document.getElementById('festival_type');
                                    select.addEventListener('change', function() {
                                      if (select.value !== '') {
                                        select.querySelector('option[value=\"\"]').disabled = true;
                                      }
                                    });
                                  });
                                                  "))
                        ),
               
               checkboxInput("givedata", "Daten für Forschung spenden", value = FALSE),
               
               hr(),
               actionButton("submit", "Weiter", 
                            style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")
             )),
    
    # Socio-economic indicators
    tabPanel(HTML('<span style="font-size:100%;color:white;font-weight:normal;">Simulationstool</span></a>'),
             
             tags$head(
               includeCSS("styles.css"), tags$link(rel = "icon", type = "image/png", href = "starry_sky.jpg")
             ),
             div(class="outer",
                 withLoader(leafletOutput("map3", height="95vh"), loader = "loader3"),
                 
                 absolutePanel(
                   id = "control1", class = "panel panel-default", fixed = TRUE,
                   draggable = TRUE, top = 80, left = "auto", right = 20, bottom = "auto",
                   width = 395, height = "auto",
                   
                   h2("Socio-economic indicators"),
                   #h3("Montpellier metropolitan area"),
                   
                   selectInput("choix_indic_accept",
                               label = strong("Choose the indicator of social acceptation"), 
                               choice = c("Extinction 1 a.m. - 5 a.m." = "1", 
                                          "Extinction 11 p.m. - 6 a.m." = "2")
                   )
                 )
             )), 
    
    
    
    # Ecological indicators
    tabPanel(HTML('<span style="font-size:100%;color:white;font-weight:normal;">Ecological indicators</span></a>'),
             
             tags$head(
               includeCSS("styles.css"), tags$link(rel = "icon", type = "image/png", href = "starry_sky.jpg")
             ),
             div(class="outer",
                 
                 withLoader(leafletOutput("map1", height="95vh"), loader = "loader3"),
                 
                 absolutePanel(                   
                   id = "control2", class = "panel panel-default", fixed = TRUE,
                   draggable = TRUE, top = 80, left = "auto", right = 20, bottom = "auto",
                   width = 345, height = "auto",
                   
                   h2("Ecological indicators"),
                   #h3("Montpellier metropolitan area"),
                   
                   selectInput("famille_espece",
                               label = strong("Choose a species family"), 
                               choice = c("European nightjar" = "Nightjar",
                                          "Amphibians"= "Amphibian", 
                                          "Insects (wetlands)" = "Insects",
                                          "Lampyridae"  = "Lampyridae",
                                          "Myotis spp" = "Murine",
                                          "Rhinolophus spp" = "Rhinolophus", 
                                          "Global Indicators for ecological stakes" = "global"
                               )
                   ),
                   selectInput("indicateur_ecolo",
                               label = strong("Choose an indicator"),
                               choice = c("Impact of light pollution on dispersion according to ecological stakes" = "impact_light_pollution",
                                          "Biodiversity reservoirs" = "rb")
                   ), 
                   
                   actionButton("go", label = "Go!", style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                   #tags$style(type='text/css', "button#go {margin-left: 60%;}")
                   
                 )
             )),
    # Light pollution indicator
    tabPanel(HTML('<span style="font-size:100%;color:white;font-weight:normal;">Light pollution indicator</span></a>'),
             
             tags$head(
               includeCSS("styles.css"), tags$link(rel = "icon", type = "image/png", href = "starry_sky.jpg")
             ),
             div(class="outer",
                 
                 withLoader(leafletOutput("map0", height="95vh"), loader = "loader3"),
                 
                 absolutePanel(
                   id = "control3", class = "panel panel-default", fixed = TRUE,
                   draggable = TRUE, top = 80, left = "auto", right = 20, bottom = "auto",
                   width = 365, height = "auto",
                   
                   h2("Light pollution indicator"),
                   #h3("Montpellier metropolitan area"),
                   
                   selectInput("indicator_pollum",
                               label = strong(""), 
                               choice = c("Level of upward emission" = "upward_emission"
                               ),  
                   )
                 )
                 
             )
    )
    
    
  ))
