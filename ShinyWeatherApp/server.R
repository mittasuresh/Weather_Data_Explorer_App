library(shiny)

# Plotting 
library(ggplot2)
library(rCharts)
library(ggvis)

# Data processing libraries
library(data.table)
library(reshape2)
library(dplyr)

# Required by includeMarkdown
library(markdown)

# It has to loaded to plot ggplot maps on shinyapps.io
library(mapproj)
library(maps)
library(ggthemes)

# Load helper functions
source("utils.R", local = TRUE)


# Load data
states_map <- map_data("state")
rawdt <- fread('data/StormData_to2015_after_aggregating.csv') %>% mutate(EVENT_TYPE = tolower(EVENT_TYPE))

rawdt$STATE_NAME <- tolower(rawdt$STATE_NAME)
rawdt$EVENT_TYPE <- sapply(rawdt$EVENT_TYPE, simpleCap)
# Remove unsupported States
dt <- rawdt[!rawdt$STATE_NAME %in% c("american samoa", "atlantic north", "atlantic south", "district of columbia",
                                     "e pacific", "guam", "gulf of alaska", "gulf of mexico", "hawaii waters", "lake erie", "lake huron", 
                                     "lake michigan", "lake ontario", "lake st clair", "lake superior", "puerto rico", "st lawrence r", 
                                     "virgin islands"),]
event_types <- sort(unique(dt$EVENT_TYPE))

# Shiny server 
shinyServer(function(input, output, session) {
  
  # Define and initialize reactive values
  values <- reactiveValues()
  values$event_types <- event_types
  
  # Create event type checkbox
  output$evtypeControls <- renderUI({
    checkboxGroupInput('event_types', 'Event Types:', event_types, selected=values$event_types)
  })
  
  # Add observers on clear and select all buttons
  observe({
    if(input$clear_all == 0) return()
    values$event_types <- c()
  })
  
  observe({
    if(input$select_all == 0) return()
    values$event_types <- event_types
  })
  
  # Preapre datasets
  
  # Prepare dataset for maps
  dt.agg <- reactive({
    aggregate_by_state(dt, input$range[1], input$range[2], input$event_types)
  })
  
  # Prepare dataset for time series
  dt.agg.year <- reactive({
    aggregate_by_year(dt, input$range[1], input$range[2], input$event_types)
  })
  
  # Prepare dataset for downloads
  dataTable <- reactive({
    prepare_downloads(dt.agg())
  })
  
  # Render Plots
  
  # Population impact by state
  output$populationImpactByState <- renderPlot({
    print(plot_impact_by_state (
      dt = compute_affected(dt.agg(), input$populationCategory),
      states_map = states_map, 
      year_min = input$range[1],
      year_max = input$range[2],
      title = "Population Impact %d - %d (Number of Affected)",
      fill = "Affected"
    ))
  })
  
  # Economic impact by state
  output$economicImpactByState <- renderPlot({
    print(plot_impact_by_state(
      dt = compute_damages(dt.agg(), input$economicCategory),
      states_map = states_map, 
      year_min = input$range[1],
      year_max = input$range[2],
      title = "Economic Impact %d - %d (Million USD)",
      fill = "Damages"
    ))
  })
  
  # Events by year
  output$eventsByYear <- renderChart({
    plot_events_by_year(dt.agg.year())
  })
  
  # Population impact by year
  output$populationImpact <- renderChart({
    plot_impact_by_year(
      dt = dt.agg.year() %>% select(Year, Injuries, Fatalities),
      dom = "populationImpact",
      yAxisLabel = "Affected",
      desc = TRUE
    )
  })
  
  # Economic impact by state
  output$economicImpact <- renderChart({
    plot_impact_by_year(
      dt = dt.agg.year() %>% select(Year, Crops, Property),
      dom = "economicImpact",
      yAxisLabel = "Total Damage (Million USD)"
    )
  })
  
  # Render data table and create download handler
  output$table <- renderDataTable(
{dataTable()}, options = list(bFilter = FALSE, iDisplayLength = 50))

output$downloadData <- downloadHandler(
  filename = 'data.csv',
  content = function(file) {
    write.csv(dataTable(), file, row.names=FALSE)
  }
)
})

