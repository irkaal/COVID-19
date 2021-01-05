suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(leaflet)
  library(tidyverse)
  library(lubridate)
  library(scales)
  library(tsibble)
  library(DT)
  library(feather)
  library(glue)
  library(plotly)
  library(rlang)
})

Sys.setenv(TZ = "America/Vancouver")
options(dplyr.summarise.inform = FALSE, warn = -1)

source("data.R")
source("modules/map.R")
source("modules/plot.R")
source("modules/about.R")

ui <- dashboardPage(
  dashboardHeader(
    title = "COVID-19 Dashboard"
  ),
  dashboardSidebar(
    dateInput("date", "Date",
      value = Sys.Date() - 1,
      min   = "2020-01-23",
      max   = Sys.Date() - 1
    ),
    selectInput("continent", "Continent",
      choices = c("All" = ""), selected = "All", multiple = TRUE
    ),
    conditionalPanel(
      condition = "input.continent",
      selectInput("country_region", "Country/Region",
        choices = c("All" = ""), selected = "All", multiple = TRUE
      )
    ),
    conditionalPanel(
      condition = "input.country_region",
      selectInput("prov_state", "Province/State",
        choices = c("All" = ""), selected = "All", multiple = TRUE
      )
    )
  ),
  dashboardBody(
    tags$style(
      type = "text/css",
      ".nav-tabs-custom {margin-bottom: 0;}"
    ),
    fluidRow(
      tabBox(id = "tab", width = 12,
        tabPanel("Map", mapViewUI("map")),
        tabPanel("Plot", plotViewUI("plot")),
        tabPanel("About", aboutViewUI("about"))
      )
    )
  )
)

server <- function(input, output, session) {
  ### Data =====================================================================
  
  data <- reactive({
    invalidateLater(12 * 3600 * 1000, session)
    withProgress(message = "Loading...", value = 0, {
      processed <- covid_data()
      incProgress(1)
    })
    processed
  })
  
  interim_data <- reactive(filter(data(), date == input$date))
  
  cum_interim_data <- reactive(filter(data(), date <= input$date))
  
  filter_by_input <- function(data, cum = FALSE) {
    if (cum) {
      filtered <- cum_interim_data()
    } else {
      filtered <- interim_data()
    }
    if (!is.null(input$continent)) {
      filtered <- filter(filtered, continent %in% input$continent)
    }
    if (!is.null(input$country_region)) {
      filtered <- filter(filtered, country_region %in% input$country_region)
    }
    if (!is.null(input$prov_state)) {
      filtered <- filter(filtered, prov_state %in% input$prov_state)
    }
    filtered
  }
  
  selected_data <- reactive(filter_by_input(data()))
  
  cum_selected_data <- reactive(filter_by_input(data(), cum = TRUE))
  
  map_data <- reactive({
    scaled <- selected_data() %>%
      select(cum_confirmed:active_change) %>%
      as.matrix() %>%
      rescale(to = c(5e4, 1e6)) %>%
      as_tibble()
    names(scaled) <- paste(names(scaled), "scaled", sep = "_") 
    bind_cols(selected_data(), scaled)
  })
  
  plot_map_data <- reactive(
    cum_selected_data() %>%
      select(-c(loc_id, lat, lng, active_change)) %>%
      group_by(date) %>%
      summarize(across(where(is.numeric), sum)) %>%
      pivot_longer(-date, names_to = "type", values_to = "cases")
  )
  
  ### Outputs ==================================================================

  #### Dashboard Sidebar -------------------------------------------------------

  # Continent Select Input
  observe({
    continents <- interim_data() %>%
      distinct(continent) %>%
      arrange(continent) %>%
      pull()
    updateSelectizeInput(session, "continent",
      choices = continents,
      selected = isolate(input$continent[input$continent %in% continents]),
      server = T
    )
  })
    
  # Country/Region Select Input
  observe({
    cr <- interim_data()
    if (!is.null(input$continent)) {
      cr <- filter(cr, continent %in% input$continent)    
    }
    cr <- cr %>%
      select(country_region) %>%
      arrange(country_region) %>%
      pull()
    updateSelectizeInput(session, "country_region",
      choices = cr,
      selected = isolate(input$country_region[input$country_region %in% cr]),
      server = T
    )
  })
  
  # Province/State Select Input
  observe({
    if (is.null(input$country_region) || length(input$country_region) > 1) {
      ps <- character(0)
    } else {
      ps <- interim_data() %>%
        filter(country_region %in% input$country_region) %>%
        distinct(prov_state) %>%
        arrange(prov_state) %>%
        pull()
    }
    updateSelectizeInput(session, "prov_state",
      choices = ps,
      selected = isolate(input$prov_state[input$prov_state %in% ps]),
      server = T
    )
  })
  
  #### Map tab -----------------------------------------------------------------
  
  callModule(mapView, "map", map_data, plot_map_data)
  
  #### Plot tab ----------------------------------------------------------------

  callModule(plotView, "plot", cum_selected_data)
  
  #### About tab ---------------------------------------------------------------
  
  callModule(aboutView, "about", cum_selected_data)
}

shinyApp(ui = ui, server = server)
