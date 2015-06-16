#' Aggregate dataset by state
#' 
#' @param dt data.table
#' @param year_min integer
#' @param year_max integer
#' @param evtypes character vector
#' @return data.table
#'
aggregate_by_state <- function(dt, year_min, year_max, evtypes) {
  replace_na <- function(x) ifelse(is.na(x), 0, x)
  round_2 <- function(x) round(x, 2)
  
  states <- data.table(STATE_NAME=sort(unique(dt$STATE_NAME)))
  
  aggregated <- dt %>% filter(YEAR >= year_min, YEAR <= year_max, EVENT_TYPE %in% evtypes) %>%
    group_by(STATE_NAME) %>%
    summarise_each(funs(sum), COUNT:CROPS_DAMAGE)
  
  # We want all states to be present even if nothing happened
  left_join(states,  aggregated, by = "STATE_NAME") %>%
    mutate_each(funs(replace_na), FATALITIES:CROPS_DAMAGE) %>%
    mutate_each(funs(round_2), PROPERTY_DAMAGE, CROPS_DAMAGE)    
}

#' Aggregate dataset by year
#' 
#' @param dt data.table
#' @param year_min integer
#' @param year_max integer
#' @param evtypes character vector
#' @return data.table
#'
aggregate_by_year <- function(dt, year_min, year_max, evtypes) {
  round_2 <- function(x) round(x, 2)
  
  # Filter
  dt %>% filter(YEAR >= year_min, YEAR <= year_max, EVENT_TYPE %in% evtypes) %>%
    # Group and aggregate
    group_by(YEAR) %>% summarise_each(funs(sum), COUNT:CROPS_DAMAGE) %>%
    # Round
    mutate_each(funs(round_2), PROPERTY_DAMAGE, CROPS_DAMAGE) %>%
    rename(
      Year = YEAR, Count = COUNT,
      Fatalities = FATALITIES, Injuries = INJURIES,
      Property = PROPERTY_DAMAGE, Crops = CROPS_DAMAGE
    )
}

#' Add Affected column based on category
#'
#' @param dt data.table
#' @param category character
#' @return data.table
#'
compute_affected <- function(dt, category) {
  dt %>% mutate(Affected = {
    if(category == 'both') {
      INJURIES + FATALITIES
    } else if(category == 'fatalities') {
      FATALITIES
    } else {
      INJURIES
    }
  })
}

#' Add Damages column based on category
#' 
#' @param dt data.table
#' @param category character
#' @return data.table
#'
compute_damages <- function(dt, category) {
  dt %>% mutate(Damages = {
    if(category == 'both') {
      PROPERTY_DAMAGE + CROPS_DAMAGE
    } else if(category == 'crops') {
      CROPS_DAMAGE
    } else {
      PROPERTY_DAMAGE
    }
  })
}

#' Prepare map of economic or population impact
#' 
#' @param dt data.table
#' @param states_map data.frame returned from map_data("state")
#' @param year_min integer
#' @param year_max integer
#' @param fill character name of the variable
#' @param title character
#' @param low character hex
#' @param high character hex
#' @return ggplot
#' 
plot_impact_by_state <- function (dt, states_map, year_min, year_max, fill, title, low = "#d3e992", high = "#669900") {
  title <- sprintf(title, year_min, year_max)
  p <- ggplot(dt, aes(map_id = STATE_NAME))
  p <- p + geom_map(aes_string(fill = fill), map = states_map, colour='black')
  p <- p + expand_limits(x = states_map$long, y = states_map$lat)
  p <- p + coord_map() + theme_bw(base_size = 14)
  p <- p + labs(x = "Long", y = "Lat", title = title)
  p <- p + geom_point(x=5, y=5) + coord_fixed()
  p + scale_fill_gradient(low = low, high = high)
}

#' Prepare plots of impact by year
#'
#' @param dt data.table
#' @param dom
#' @param yAxisLabel
#' @param desc
#' @return plot
#' 
plot_impact_by_year <- function(dt, dom, yAxisLabel, desc = FALSE) {
  impactPlot <- nPlot(
    value ~ Year, group = "variable",
    data = melt(dt, id="Year") %>% arrange(Year, if (desc) { desc(variable) } else { variable }),
    type = "stackedAreaChart", dom = dom, width = 650
  )
  impactPlot$chart(margin = list(left = 100))
  impactPlot$yAxis(axisLabel = yAxisLabel, width = 80)
  impactPlot$xAxis(axisLabel = "Year", width = 70)
  
  impactPlot
}

#' Prepare plot of number of events by year
#'
#' @param dt data.table
#' @param dom
#' @param yAxisLabel
#' @return plot

plot_events_by_year <- function(dt, dom = "eventsByYear", yAxisLabel = "Count") {
  eventsByYear <- nPlot(
    Count ~ Year,
    data = dt,
    type = "lineChart", dom = dom, width = 650
  )
  
  eventsByYear$chart(margin = list(left = 100))
  eventsByYear$yAxis( axisLabel = yAxisLabel, width = 80)
  eventsByYear$xAxis( axisLabel = "Year", width = 70)
  eventsByYear
}

#' Prepare dataset for downloads
#'
#' @param dt data.table
#' @return data.table
prepare_downloads <- function(dt) {
  dt %>% rename(
    State = STATE_NAME, Count = COUNT,
    Injuries = INJURIES, Fatalities = FATALITIES,
    Property.damage = PROPERTY_DAMAGE, Crops.damage = CROPS_DAMAGE
  ) %>% mutate(State=state.abb[match(State, tolower(state.name))])
}

#' Upper case the first letter of a String X
#'
#' @param x String
#' @return String
simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}
