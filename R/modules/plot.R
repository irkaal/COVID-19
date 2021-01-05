plotViewUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(2,
        selectInput(ns("x_var"), "X Variable",
          choices  = c(
            "Active"        = "active",
            "New Confirmed" = "confirmed",
            "Confirmed"     = "cum_confirmed",
            "New Recovered" = "recovered",
            "Recovered"     = "cum_recovered",
            "New Deaths"    = "deaths",
            "Deaths"        = "cum_deaths"
          ),
          selected = "cum_deaths"
        )
      ),
      column(2,
        selectInput(ns("y_var"), "Y Variable",
          choices  = c(
            "Active"        = "active",
            "New Confirmed" = "confirmed",
            "Confirmed"     = "cum_confirmed",
            "New Recovered" = "recovered",
            "Recovered"     = "cum_recovered",
            "New Deaths"    = "deaths",
            "Deaths"        = "cum_deaths"
          ),
          selected = "cum_recovered"
        )
      ),
      column(2,
        selectInput(ns("size_var"), "Size Variable",
          choices  = c(
            "Active"        = "active",
            "New Confirmed" = "confirmed",
            "Confirmed"     = "cum_confirmed",
            "New Recovered" = "recovered",
            "Recovered"     = "cum_recovered",
            "New Deaths"    = "deaths",
            "Deaths"        = "cum_deaths"
          ),
          selected = "cum_confirmed"
        )
      ),
      column(2,
        selectInput(ns("group_by"), "Group By",
          choices  = c(
            "None"           = "None",
            "Continent"      = "continent",
            "Country/Region" = "country_region"
          ),
          selected = "None"
        )
      ),
      column(2,
        conditionalPanel(
          condition = "input.group_by",
          ns        = ns,
          selectInput(ns("colour_var"), "Colour Variable",
            choices  = c(
              "Continent"      = "continent",
              "Country/Region" = "country_region",
              "Province/State" = "prov_state"
            ),
            selected = "continent"
          )
        )
      )
    ),
    fluidRow(
      column(2,
        selectInput(ns("x_scale"), "X-axis Scale",
          choices  = c("None", "Logarithmic" = "log", "Square-root" = "sqrt"),
          selected = "None"
        )
      ),
      column(2,
        selectInput(ns("y_scale"), "Y-axis Scale",
          choices  = c("None", "Logarithmic" =  "log", "Square-root" = "sqrt"),
          selected = "None"
        )
      ),
      column(2,
        selectInput(ns("animation"), "Animation",
          choices  = c("Enabled" = TRUE, "Disabled" = FALSE),
          selected = FALSE
        )
      ),
      column(2,
        selectInput(ns("engine"), "Engine",
          choices  = c("ggplot2", "plotly"),
          selected = "ggplot2"
        )
      )
    ),
    plotlyOutput(ns("plotly")),
    tags$style(
      type = "text/css",
      "#plot-plotly { height: calc(100vh - 302px) !important; }"
    )
  )
}

plotView <- function(input, output, session, cum_selected_data) {
  ## Inputs ====================================================================
  
  # Color Variable Select Input
  observe({
    choices <- c(
      "Continent" = "continent",
      "Country/Region" = "country_region",
      "Province/State" = "prov_state"
    )
    available_colour_vars <- switch(
      input$group_by,
      "None" = choices,
      "continent" = choices["Continent"],
      "country_region" = choices[c("Continent", "Country/Region")]
    )
    updateSelectizeInput(
      session, "colour_var",
      choices = available_colour_vars,
      selected = isolate(
        ifelse(
          input$group_by == "None",
          input$colour_var,
          available_colour_vars[available_colour_vars == input$group_by][[1]]
        )
      ),
      server = T
    )
  })
  
  # Engine Select Input
  observe({
    if (input$animation) {
      available_engines <- c("plotly")
    } else {
      available_engines <- c("ggplot2", "plotly")
    }
    updateSelectizeInput(
      session, "engine",
      choices = available_engines,
      selected = isolate(
        ifelse(
          length(available_engines) > 1,
          input$engine,
          available_engines[[1]]
        )
      ),
      server = T
    )
  })
  
  ### Data =====================================================================
  
  data <- reactive(filter(cum_data(), date == max(date)))

  cum_data <- reactive(
    cum_selected_data() %>%
      select(-c(loc_id, lat, lng, active_change)) %>%
      drop_na(continent)
  )

  grouped_data <- reactive(
    if (input$group_by != "None") {
      data() %>%
        group_by(!!!unique(exprs(!!sym(input$group_by), continent))) %>%
        summarise(across(where(is.numeric), sum)) %>%
        mutate(hover_text = !!sym(input$group_by))
    } else {
      mutate(
        data(),
        hover_text = ifelse(
          is.na(prov_state),
          country_region,
          paste(prov_state, country_region, sep = ", ")
        )
      )
    }
  )
  
  cum_grouped_data <- reactive(
    if (input$group_by != "None") {
      cum_data() %>%
        group_by(date, !!!unique(exprs(!!sym(input$group_by), continent))) %>%
        summarise(across(where(is.numeric), sum)) %>%
        mutate(hover_text = !!sym(input$group_by))
    } else {
      mutate(
        cum_data(),
        hover_text = ifelse(
          is.na(prov_state),
          country_region,
          paste(prov_state, country_region, sep = ", ")
        )
      )
    }
  )

  plot_data <- reactive({
    plot_data <- grouped_data()
    if (input$x_scale != "None") {
      plot_data <- mutate(
        plot_data,
        !!input$x_var := pmax(0, eval(sym(input$x_scale))(.data[[input$x_var]]))
      )
      
    }
    if (input$y_scale != "None") {
      plot_data <- mutate(
        plot_data,
        !!input$y_var := pmax(0, eval(sym(input$y_scale))(.data[[input$y_var]]))
      )
    }
    plot_data
  })

  cum_plot_data <- reactive({
    cum_plot_data <- cum_grouped_data()
    if (input$x_scale != "None") {
      cum_plot_data <- mutate(
        cum_plot_data,
        !!input$x_var := pmax(0, eval(sym(input$x_scale))(.data[[input$x_var]]))
      )
    }
    if (input$y_scale != "None") {
      cum_plot_data <- mutate(cum_plot_data,
        !!input$y_var := pmax(0, eval(sym(input$y_scale))(.data[[input$y_var]]))
      )
    }
    cum_plot_data
  })
  
  ### Plot =====================================================================
  
  axis_label <- function(var) {
    switch(
      var,
      "active"        = "Active",
      "confirmed"     = "New Confirmed",
      "cum_confirmed" = "Confirmed",
      "recovered"     = "New Recovered",
      "cum_recovered" = "Recovered",
      "deaths"        = "New Deaths",
      "cum_deaths"    = "Deaths"
    )
  }

  output$plotly <- renderPlotly(
    tryCatch({
      withProgress(message = "Loading...", value = 0, {
        
        #### ggplot2 Engine ----------------------------------------------------
        
        if (input$engine == "ggplot2") {
          p <- plot_data() %>%
            ggplot(
              aes(
                x     = !!sym(input$x_var),
                y     = !!sym(input$y_var),
                color = !!sym(input$colour_var)
              )
            ) +
            geom_point(
              aes(size = !!sym(input$size_var), text = hover_text), alpha = 0.5
            ) +
            theme_minimal() +
            theme(legend.title = element_blank()) +
            labs(x = axis_label(input$x_var), y = axis_label(input$y_var)) +
            scale_size_area(max_size = 12) +
            scale_x_continuous(labels = label_number_si(accuracy = 0.1)) +
            scale_y_continuous(labels = label_number_si(accuracy = 0.1))
          fig <- ggplotly(p, tooltip = "text")
        }
      
        #### plotly Engine -----------------------------------------------------
        
        if (input$engine == "plotly") {
          
          if (input$animation) {
            fig <- plot_ly(
              cum_plot_data(),
              x         = ~eval(sym(input$x_var)),
              y         = ~eval(sym(input$y_var)),
              size      = ~eval(sym(input$size_var)),
              color     = ~eval(sym(input$colour_var)),
              frame     = ~ date - as.Date("2020-01-22"),
              text      = ~hover_text,
              hoverinfo = "text",
              type      = "scatter",
              mode      = "markers",
              marker    = list(sizeref = 0.1, sizemode = "area")
            )
            fig <- layout(
              fig,
              xaxis = list(title = axis_label(input$x_var)),
              yaxis = list(title = axis_label(input$y_var))
            )
            fig <- animation_opts(fig, frame = 50, transition = 5)
            fig <- animation_slider(
              fig,
              currentvalue = list(prefix = "Days since 22 Jan. 2020: ")
            )  
          } else {
            fig <- plot_ly(
              data      = plot_data(),
              x         = ~eval(sym(input$x_var)),
              y         = ~eval(sym(input$y_var)),
              size      = ~eval(sym(input$size_var)),
              color     = ~eval(sym(input$colour_var)),
              text      = ~hover_text,
              hoverinfo = "text",
              type      = "scatter",
              mode      = "markers",
              marker    = list(sizeref = 0.1, sizemode = "area")
            )
            fig <- layout(
              p     = fig,
              xaxis = list(title = axis_label(input$x_var)),
              yaxis = list(title = axis_label(input$y_var))
            )
          }
        }
        incProgress(1)
      })
      toWebGL(fig)
    }, error = function(e) NULL)
  )
}
