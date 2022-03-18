secure_app(function(request) {
  dashboardPage(
    dashboardHeader(title = "South Coast Marine Values Mapper",
                    tags$li(class = "dropdown",
                            a(href="https://www.dbca.wa.gov.au/", target="_blank", 
                              img(#width = "10%", 
                                height = "70px", 
                                src="dbca_logo_white.png")
                            )),
                    tags$li(a(href = 'https://marineecology.io/', target="_blank", 
                              img(src = 'https://github.com/UWAMEGFisheries/UWAMEGFisheries.github.io/blob/master/images/MEG-white.png?raw=true',
                                  title = "Marine Ecology Group", 
                                  # width = "10%", 
                                  height = "70px")
                    ), class = "dropdown")),
    
    dashboardSidebar(width = "0px"),
    dashboardBody(
      tags$head(tags$link(rel = "shortcut icon", href = "favicon.ico")),
      tags$head(tags$style(HTML(
        '.myClass { 
        font-size: 25px;
        line-height: 100px;
        text-align: left;
        font-family: "Source Sans Pro",sans-serif;
        padding: -0 10px;
        overflow: hidden;
        color: white;
      }
    '))),
      tags$script(HTML('
      $(document).ready(function() {
        $("header").find("nav").append(\'<span class="myClass">  <b> South Coast Marine Values Mapper (Admin)</b> </span>\');
      })
     ')),
      
      
      shinyjs::useShinyjs(), # Have to put here in dashboard SEE https://cran.r-project.org/web/packages/shinyjs/vignettes/shinyjs-usage.html
      useShinyalert(), # To show shiny alerts
      
      disconnectMessage(
        text = "An error occurred. Please refresh the page and try again.",
        refresh = "Refresh",
        background = "#FFFFFF",
        colour = "#444444",
        refreshColour = "#337AB7",
        overlayColour = "#000000",
        overlayOpacity = 0.6,
        width = "full",
        top = "center",
        size = 22,
        css = ""
      ),
      
      # add logout button UI
      div(class = "pull-right", shinyauthr::logoutUI(id = "logout")),
      # add login panel UI function
      shinyauthr::loginUI(id = "login"),
      
                fluidRow(
                  
                  tags$head(tags$style(HTML('
                  
                   .skin-blue .main-header .navbar .sidebar-toggle {
 display: none;
                   }
                   
.main-header .logo {
 display: none;
}

.main-header .navbar {
    margin-left: 30px;
}
                   
.skin-blue .left-side, .skin-blue .main-sidebar, .skin-blue .wrapper {
    background-color: #3c8dbc;
}

 
h1 {
  margin-left: 30px;
  margin-right: 30px;
  margin-bottom: 10px;
  font-size: 22px;
}

h2 {
  margin-left: 30px;
  margin-right: 30px;
  margin-bottom: 10px;
  font-size: 18px;
}

h3 {
  margin-left: 60px;
  margin-right: 30px;
  margin-bottom: 10px;
  font-size: 16px;
}

.h4, h4 {
  font-size: 14px;
  margin-left: 30px;
  margin-right: 30px;
  margin-bottom: 15px;
}

.h5, h5 {
  font-size: 18px;
}

.h6, h6 {
  font-size: 16px;
}

.bttn-unite.bttn-md {
    font-size: 25px;
    font-family: inherit;
    padding: 5px 52px;
    margin-right: 30px;
}
'))),
                  
                  valueBoxOutput("totalbox"),
                  valueBoxOutput("wabox"),
                  valueBoxOutput("localbox"),
                  
                  valueBoxOutput("socialbox", width = 3),
                  valueBoxOutput("emailbox", width = 3),
                  valueBoxOutput("websitebox", width = 3),
                  valueBoxOutput("launchbox", width = 3),
                  
                  box(width = 12,
                      title = "Number of responses over time and by source:",
                      status = "primary",
                      splitLayout(cellWidths = c("50%", "50%"),
                      withSpinner(plotOutput("submitteddate")),
                      withSpinner(plotOutput("submittedsource")))),
                  
                  box(width = 12,
                      title = "Filter data by source:",
                      status = "primary",
                      uiOutput("sourceui")),
                  
                  box(width = 12,
                      title = "Users by postcode:",
                      status = "primary",
                  leafglOutput("leafpostcodes", width = "100%", height = 670)),
                  
                  box(width = 12,
                      title = "Demographics:",
                      status = "primary",
                      splitLayout(cellWidths = c("33%", "33%", "33%"),
                                  withSpinner(plotOutput("submittedlocation")),
                                  withSpinner(plotOutput("submittedgender")),
                                  withSpinner(plotOutput("submittedage")))),
                  
                  box(width = 12,
                      title = "Number of responses per activity categories:",
                      status = "primary",
                      splitLayout(cellWidths = c("75%", "25%"),
                      withSpinner(plotOutput("submittedcategory")),
                      withSpinner(plotOutput("submittedpie")))),
                  
                  box(width = 12,
                      title = "Number of responses per local knowledge topic:",
                      status = "primary",
                      splitLayout(cellWidths = c("75%", "25%"),
                                  withSpinner(plotOutput("submittedvalues")),
                                  withSpinner(plotOutput("submittedvaluespie")))),
                  
                  box(width = 12,
                      title = "Number of responses per pressure or threat sub-category:",
                      status = "primary",
                      splitLayout(cellWidths = c("75%", "25%"),
                                  withSpinner(plotOutput("submittedpressures")),
                                  withSpinner(plotOutput("submittedpressurespie")))),

                  tabBox(width = 12, title = "Spatial responses",
                         height = "820px", id = "tabbox",
                         
                         tabPanel("All activities (unweighted)",
                                  status = "primary",
                                  br(),
                                  br(),br(),
                                  withSpinner(leafletOutput("leafallunweighted", height = "670px"))),
                         
                         tabPanel("All activities (weighted)",
                                  status = "primary",
                                  br(),
                                  br(),br(),
                                  withSpinner(leafletOutput("leafallweighted", height = "670px"))),
                         
                         tabPanel("Filter by activity (unweighted)",
                                  status = "primary",column(width = 3,
                                                            selectInput("levelun", width = "100%",
                                                                        "Select a level to filter activities by:",
                                                                        c("Category (e.g. Recreational fishing)" = "cat",
                                                                          "Subcategory (e.g. Boat fishing or Jetty fishing)" = "sub",
                                                                          "Activity (e.g. Pelagic fish or Pots)" = "act"))),
                                  column(width = 9,
                                         uiOutput("sublevelun")),
                                  withSpinner(leafletOutput("leafactivityunweighted", height = "670px"))),
                         
                         tabPanel("Filter by activity (weighted)",
                                  status = "primary",
                                  column(width = 3,
                                         selectInput("level", width = "100%",
                                                     "Select a level to filter activities by:",
                                                     c("Category (e.g. Recreational fishing)" = "cat",
                                                       "Subcategory (e.g. Boat fishing or Jetty fishing)" = "sub",
                                                       "Activity (e.g. Pelagic fish or Pots)" = "act"))),
                                  column(width = 9,
                                         uiOutput("sublevel")),
                                  withSpinner(leafletOutput("leafactivityweighted", height = "670px"))),
                         
                         tabPanel("Filter by local knowledge (unweighted)",
                                  status = "primary",
                                  column(width = 12,
                                         uiOutput("selectlk")),
                                  withSpinner(leafletOutput("leafvalues", height = "670px"))),
                         
                         tabPanel("Filter by pressures and threats (unweighted)",
                                  status = "primary",
                                  column(width = 12,
                                         uiOutput("selectpt")),
                                  withSpinner(leafletOutput("leafpressures", height = "670px")))),
                  
                  box(width = 12,
                      title = "Average response to matrix questions:",
                      status = "primary",
                      splitLayout(cellWidths = c("33%", "33%", "33%"),
                                  withSpinner(plotOutput("awarenessplot")),
                                  withSpinner(plotOutput("visitedplot")),
                                  withSpinner(plotOutput("q9plot"))),
                      splitLayout(cellWidths = c("50%", "50%"),
                                  withSpinner(plotOutput("q10plot")),
                                  withSpinner(plotOutput("q11plot")))),
                  
                  div(style="display:inline-block;width:100%;text-align: center;", 
                  
                  downloadBttn(
                    outputId = "downloadmetadata",
                    "Download user metadata",
                    style = "unite",
                    color = "primary"
                  ),

                  downloadBttn(
                    outputId = "downloadpolygons",
                    "Download polygons clicked",
                    style = "unite",
                    color = "primary"
                  ),
                  
                  downloadBttn(
                    outputId = "downloadmatrix",
                    "Download matrix responses",
                    style = "unite",
                    color = "primary"
                  )),
                  
                )
        )
      )
})