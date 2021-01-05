aboutViewUI <- function(id) {
  ns <- NS(id)
  tagList(
    div(h3("Data"), dataTableOutput(ns("data_table"))), 
    div(
      h3("Data Sources"),
      div(
        a("COVID-19 Data Repository by the Center for Systems Science and
                Engineering (CSSE) at Johns Hopkins University",
          href = "https://github.com/CSSEGISandData/COVID-19"
        )
      ),
      div(
        a("Epidemiological Data from the COVID-19 Outbreak in Canada",
          href = "https://github.com/ishaberry/Covid19Canada/"
        )
      )
    ),
    div(
      h3("Source Code"),
      p("GitHub: ",
        a("github.com/irkaal/covid-19",
          href = "https://github.com/irkaal/covid-19"
        )
      )
    ),
    div(
      h4("Created with Shiny"),
      class = "text-muted",
      style = "padding-top: 3rem;"
    )
  )
}

aboutView <- function(input, output, session, cum_selected_data) {
  output$data_table <- renderDataTable(
    datatable(
      cum_selected_data() %>%
        select(-c(loc_id, continent)) %>%
        arrange(desc(date)),
      options = list(lengthMenu = c(10, 25, 50), pageLength = 10)
    )
  )
}
