
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

library(shiny)

library(rCharts)

shinyUI(
  navbarPage("Weather Explorer 1950-2015",
             tabPanel("Weather Plot Explorer",
                      sidebarPanel(
                        sliderInput("range", 
                                    "Range of years:", 
                                    min = 1950, 
                                    max = 2015, 
                                    value = c(1950, 2015),
                                    format="####"),
                        uiOutput("evtypeControls"),
                        actionButton(inputId = "clear_all", label = "Clear selection", icon = icon("check-square")),
                        actionButton(inputId = "select_all", label = "Select all", icon = icon("check-square-o"))
                      ),
                      
                      mainPanel(
                        tabsetPanel(
                          
                          # Data by state
                          tabPanel(p(icon("map-marker"), "Weather Explorer By States"),
                                   column(3,
                                          wellPanel(
                                            radioButtons(
                                              "populationCategory",
                                              "Population Impact Category:",
                                              c("Both" = "both", "Injuries" = "injuries", "Fatalities" = "fatalities"))
                                          )
                                   ),
                                   column(3,
                                          wellPanel(
                                            radioButtons(
                                              "economicCategory",
                                              "Economic Impact Category:",
                                              c("Both" = "both", "Property damage" = "property", "Crops damage" = "crops"))
                                          )
                                   ),
                                   column(7,
                                          plotOutput("populationImpactByState", width="800px"),
                                          plotOutput("economicImpactByState", width="800px")
                                   )
                                   
                          ),
                          
                          # Time series data
                          tabPanel(p(icon("line-chart"), "Weather Explorer By Years"),
                                   h4('Number of events by years', align = "center"),
                                   showOutput("eventsByYear", "nvd3"),
                                   h4('Population impact by years', align = "center"),
                                   showOutput("populationImpact", "nvd3"),
                                   h4('Economic impact by years', align = "center"),
                                   showOutput("economicImpact", "nvd3")
                          ),
                          
                          
                          
                          # Data 
                          tabPanel(p(icon("table"), "Weather Data Viewing"),
                                   dataTableOutput(outputId="table"),
                                   downloadButton('downloadData', 'Download')
                          )
                        )
                      )
                      
             ),
             
             tabPanel("Database Information",
                      mainPanel(
                        includeMarkdown("database.md")
                      )
             )
  )
)
