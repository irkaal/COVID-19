mapViewUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(8,
        valueBoxOutput(ns("active"), width = 3),
        valueBoxOutput(ns("cum_confirmed"), width = 3),
        valueBoxOutput(ns("cum_recovered"), width = 3),
        valueBoxOutput(ns("cum_deaths"), width = 3),
        column(12,
          checkboxGroupInput(
            ns("cases"), NULL,
            choices = c(
              "Active"        = "active",
              "Active Change" = "active_change",
              "New Confirmed" = "confirmed",
              "Confirmed"     = "cum_confirmed",
              "New Recovered" = "recovered",
              "Recovered"     = "cum_recovered",
              "New Deaths"    = "deaths",
              "Deaths"        = "cum_deaths"
            ),
            selected = c("active"),
            inline = TRUE
          )
        ),
        column(12,
          tags$style(
            type = "text/css",
            "#map-map {height: calc(100vh - 302px) !important;}"
          ),
          leafletOutput(ns("map")),
        )
      ),
      column(4,
        div(
          radioButtons(
            ns("plot_mode"), NULL,
            choices = c("Daily", "Total"), selected = "Daily",
            inline = TRUE
          ),
          style = "text-align: center;"
        ),
        plotlyOutput(ns("plot")),
        fluidRow(
          tabBox(id = ns("tab"), width = 12,
            tabPanel("Active", dataTableOutput(ns("table_active"))),
            tabPanel("Confirmed", dataTableOutput(ns("table_confirmed"))),
            tabPanel("Recovered", dataTableOutput(ns("table_recovered"))),
            tabPanel("Deaths", dataTableOutput(ns("table_deaths")))
          )
        ),
        tags$style(
          type = "text/css",
          "#map-plot {height: calc(100vh - 509px) !important;}"
        )
      )
    )
  )
}

mapView <- function(input, output, session, data, plot_map_data) {
  
  map_data <- reactive({
    clearPopups(leafletProxy("map"))
    data()
  })
  
  ### Left Column ==============================================================
  
  #### Value Boxes -------------------------------------------------------------
  
  get_count <- function(col, round = TRUE) {
    count <- map_data() %>%
      summarize(sum({{ col }})) %>%
      pull()
    ifelse(round, label_number_si(accuracy = 0.1)(count), label_comma()(count))
  }
  
  get_valueBox <- function(col, diff_col, subtitle) {
    diff_count <- get_count({{ diff_col }}, round = FALSE)
    diff_label <- ifelse(subtitle == "Active", "Change", "New")
    valueBox(
      value = get_count({{ col }}),
      subtitle = glue("{subtitle} ({diff_label}: {diff_count})")
    )
  }
  
  output$active <- renderValueBox(
    get_valueBox(active, active_change, subtitle = "Active")
  )
  
  output$cum_confirmed <- renderValueBox(
    get_valueBox(cum_confirmed, confirmed, "Confirmed")
  )
  
  output$cum_recovered <- renderValueBox(
    get_valueBox(cum_recovered, recovered, "Recovered")
  )
  
  output$cum_deaths <- renderValueBox({
    get_valueBox(cum_deaths, deaths, "Deaths")
  })
  
  #### Leaflet Map -------------------------------------------------------------
  
  output$map <- renderLeaflet(
    leaflet() %>%
      setView(lng = 10, lat = 40, zoom = 3) %>%
      addProviderTiles("Esri.WorldStreetMap")
  )

  marker_color <- function(case, values) {
    if (case == "active_change") {
      ifelse(values > 0, "red", "green")
    } else {
      switch (case,
        "active"        = "red",
        "confirmed"     = "orange",
        "cum_confirmed" = "orange",
        "recovered"     = "green",
        "cum_recovered" = "green",
        "deaths"        = "blue",
        "cum_deaths"    = "blue"
      )
    }
  }

  observe({
    clearShapes(leafletProxy("map"))
    for (case in input$cases) {
      type <- sym(case)
      addCircles(leafletProxy("map", data = filter(map_data(), !!type != 0)),
        lng         = ~lng,
        lat         = ~lat,
        radius      = ~eval(sym(paste(case, "scaled", sep = "_"))),
        layerId     = ~paste(loc_id, case, sep = "_"),
        color       = ~marker_color(case, eval(type)),
        stroke      = FALSE,
        fillOpacity = 0.5
      )
    }
  })

  popup_content <- function(selected) {
    attr_val_pair <- function(attr, col) {
      val <- label_comma()(pull(selected, {{ col }}))
      tags$div(paste(attr, val, sep = ": "))
    }
    
    country_region <- selected %>% pull(country_region)
    prov_state <- selected %>% pull(prov_state)
    
    as.character(
      tagList(
        tags$h4(
          ifelse(
            is.na(prov_state),
            country_region,
            paste(prov_state, country_region, sep = ", ")
          )
        ),
        attr_val_pair("Active", active),
        attr_val_pair("Active Change", active_change),
        attr_val_pair("New Confirmed", confirmed),
        attr_val_pair("Confirmed", cum_confirmed),
        attr_val_pair("New Recovered", recovered),
        attr_val_pair("Recovered", cum_recovered),
        attr_val_pair("New Deaths", deaths),
        attr_val_pair("Deaths", cum_deaths)
      )
    )
  }

  show_popup <- function(id, lat, lng) {
    raw_id <- str_split(id, "_")[[1]][1]
    selected <- filter(map_data(), loc_id == raw_id)
    content <- popup_content(selected)
    addPopups(leafletProxy("map"), lng, lat, content, layerId = id)
  }

  observe({
    clearPopups(leafletProxy("map"))
    event <- input$map_shape_click
    if (is.null(event)) {
      return(NULL)
    }
    isolate(show_popup(event$id, event$lat, event$lng))
  })
  
  ### Right Column =============================================================
  
  #### Tables ------------------------------------------------------------------
  
  should_ungroup_country <- reactive({
    has_one_country <- map_data() %>%
      distinct(country_region) %>%
      nrow() == 1
    has_multiple_provs <- map_data() %>%
      filter(!is.na(prov_state)) %>%
      nrow() > 0
    has_one_country && has_multiple_provs
  })
  
  grouped_data <- reactive(
    if (should_ungroup_country()) {
      map_data() %>%
        group_by(prov_state) %>%
        rename(`Province/State` = prov_state)
    } else {
      map_data() %>%
        group_by(country_region) %>%
        rename(`Country/Region` = country_region)
    }
  )
  
  get_datatable <- function(cumulative, ...) {
    datatable(
      grouped_data() %>%
        summarize(Cumulative = {{ cumulative }}, ...) %>%
        arrange(desc(Cumulative)),
      options = list(
        pageLength = 5, bFilter = FALSE, bLengthChange = FALSE, bInfo = FALSE
      )
    )
  }

  output$table_active <- renderDataTable(
    get_datatable(active, Change = active_change)
  )
  
  output$table_confirmed <- renderDataTable(
    get_datatable(cum_confirmed, New = confirmed)
  )

  output$table_recovered <- renderDataTable(
    get_datatable(cum_recovered, New = recovered)
  )

  output$table_deaths <- renderDataTable(
    get_datatable(cum_deaths, New = deaths)
  )
  
  #### Plot --------------------------------------------------------------------
  
  plot_data <- reactive(
    plot_map_data() %>%
      mutate(
        type = type %>%
          str_replace_all("cum_", "Cumulative ") %>%
          str_to_title() %>%
          fct_reorder2(date, cases)
      ) %>%
      rename(Date = date, Cases = cases)
  )
  
  filtered_plot_data <- reactive(
    if (input$plot_mode == "Daily") {
      filter(plot_data(), !str_detect(type, "Cumulative|Active"))
    } else {
      filter(plot_data(), str_detect(type, "Cumulative|Active"))
    }
  )
  
  output$plot <- renderPlotly(
    tryCatch({
      p <- filtered_plot_data() %>%
        ggplot(mapping = aes(x = Date, y = Cases)) +
        geom_line(aes(color = type)) +
        labs(title = "COVID-19 Cases") +
        scale_y_continuous(labels = label_number_si()) +
        theme_minimal() +
        theme(
          axis.title.y = element_blank(),
          axis.title.x = element_blank(),
          legend.title = element_blank(),
        )
      p %>%
        ggplotly(tooltip = c("Date", "Cases")) %>%
        layout(
          legend = list(orientation = "h", xanchor = "center", x = 0.5, y = 0)
        ) %>%
        toWebGL()
    }, error = function(e) NULL)
  )
}
