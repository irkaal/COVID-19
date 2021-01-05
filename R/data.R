covid_data <- function() {
  url_confirmed_global <- paste0(
    "https://github.com/CSSEGISandData/COVID-19/raw/master/",
    "csse_covid_19_data/csse_covid_19_time_series/",
    "time_series_covid19_confirmed_global.csv"
  )
  url_recovered_global <- paste0(
    "https://github.com/CSSEGISandData/COVID-19/raw/master/",
    "csse_covid_19_data/csse_covid_19_time_series/",
    "time_series_covid19_recovered_global.csv"
  )
  url_deaths_global <- paste0(
    "https://github.com/CSSEGISandData/COVID-19/raw/master/",
    "csse_covid_19_data/csse_covid_19_time_series/",
    "time_series_covid19_deaths_global.csv"
  )
  url_cases_canada <- paste0(
    "https://github.com/ishaberry/Covid19Canada/raw/master/",
    "timeseries_prov/active_timeseries_prov.csv"
  )
  
  ### Global - Canada ==========================================================
  
  #### Confirmed cases ---------------------------------------------------------
  
  df <- read_csv(url_confirmed_global, col_types = cols())
  
  exclude_ps <- c("Grand Princess", "Diamond Princess", "Northwest Territories")
  canada_coord <- df %>%
    filter(`Country/Region` == "Canada", !`Province/State` %in% exclude_ps) %>%
    select(`Province/State`, Lat, Long) %>%
    rename(prov_state = `Province/State`, lat = Lat, lng = Long)
  
  covid <- df %>%
    filter(`Country/Region` != "Canada") %>%
    pivot_longer(
      cols      = -c("Province/State", "Country/Region", "Lat", "Long"),
      names_to  = "date",
      values_to = "cum_confirmed"
    ) %>%
    group_by(`Country/Region`) %>%
    mutate(
      date      = as_date(date, format = "%m/%d/%y"),
      confirmed = cum_confirmed %>%
        difference(default = 0) %>%
        pmax(0)
    ) %>%
    ungroup()
  
  #### Confirmed cases ---------------------------------------------------------
  
  df <- read_csv(url_recovered_global, col_types = cols())
  
  df <- df %>%
    filter(`Country/Region` != "Canada") %>%
    pivot_longer(
      cols      = -c("Province/State", "Country/Region", "Lat", "Long"),
      names_to  = "date",
      values_to = "cum_recovered"
    ) %>%
    group_by(`Country/Region`) %>%
    mutate(
      date      = as_date(date, format = "%m/%d/%y"),
      recovered = cum_recovered %>%
        difference(default = 0) %>%
        pmax(0),
      Lat       = NULL,
      Long      = NULL
    ) %>%
    ungroup()
  
  covid <- covid %>%
    left_join(df, by = c("Province/State", "Country/Region", "date"))
  
  #### Death cases -------------------------------------------------------------
  
  df <- read_csv(url_deaths_global, col_types = cols())
  
  df <- df %>%
    filter(`Country/Region` != "Canada") %>%
    pivot_longer(
      cols      = -c("Province/State", "Country/Region", "Lat", "Long"),
      names_to  = "date",
      values_to = "cum_deaths"
    ) %>%
    group_by(`Country/Region`) %>%
    mutate(
      date   = as_date(date, format = "%m/%d/%y"),
      deaths = cum_deaths %>%
        difference(default = 0) %>%
        pmax(0),
      Lat    = NULL,
      Long   = NULL
    ) %>%
    ungroup()
  
  covid <- covid %>%
    left_join(df, by = c("Province/State", "Country/Region", "date"))
  
  #### Active cases ------------------------------------------------------------
  
  covid <- covid %>%
    mutate(
      active        = cum_confirmed - cum_recovered - cum_deaths,
      active_change = confirmed - recovered - deaths
    )
  
  ### Canada ===================================================================
  
  df <- read_csv(url_cases_canada, col_types = cols())
  
  df <- df %>%
    filter(!province %in% c("Nunavut", "NWT", "Repatriated")) %>%
    rename(
      prov_state    = province,
      date          = date_active,
      cum_confirmed = cumulative_cases,
      cum_recovered = cumulative_recovered,
      cum_deaths    = cumulative_deaths,
      active        = active_cases,
      active_change = active_cases_change
    ) %>%
    mutate(
      prov_state = str_replace_all(
        prov_state,
        c(
          "BC"  = "British Columbia",
          "NL"  = "Newfoundland and Labrador",
          "PEI" = "Prince Edward Island"
        )
      ),
      country_region = "Canada",
      date           = as_date(date, format = "%d-%m-%Y"),
      confirmed      = cum_confirmed %>%
        difference(default = 0) %>%
        pmax(0),
      recovered      = cum_recovered %>%
        difference(default = 0) %>%
        pmax(0),
      deaths         = cum_deaths %>%
        difference(default = 0) %>%
        pmax(0)
    ) %>%
    left_join(canada_coord, by = "prov_state")
  
  ### Global + Canada ==========================================================
  
  continents <- read_feather("data/continents.feather")

  covid %>%
    rename(
      prov_state     = `Province/State`,
      country_region = `Country/Region`,
      lat            = Lat,
      lng            = Long
    ) %>%
    bind_rows(df) %>%
    unite("loc_id", c("prov_state", "country_region"), remove = FALSE) %>%
    mutate(loc_id = as.numeric(as_factor(loc_id))) %>%
    left_join(continents, by = "country_region")
}
