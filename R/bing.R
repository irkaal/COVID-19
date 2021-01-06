library(tidyverse)

covid <- read_csv(
  str_c(
    "https://github.com/microsoft/Bing-COVID-19-Data/raw/master/data/",
    "Bing-COVID19-Data.csv"
  ),
  guess_max = 1e6,
  progress = TRUE
)

# Countries with AdminRegion(s)
covid_latest_1 <- covid %>%
  filter(!is.na(AdminRegion1)) %>%
  group_by(Country_Region, AdminRegion1, AdminRegion2) %>%
  arrange(Updated) %>%
  slice(n()) %>%
  ungroup() %>%
  arrange(ID)

# Countries without AdminRegion(s)
covid_latest_2 <- covid %>%
  filter(
    !Country_Region %in% unique(covid_latest_1$Country_Region),
    Country_Region != "Worldwide"
  ) %>%
  group_by(Country_Region) %>%
  arrange(Updated) %>%
  slice(n()) %>%
  ungroup() %>%
  arrange(ID)

# Bind tibbles
covid_latest <- bind_rows(covid_latest_1, covid_latest_2)

print(covid_latest)

write_csv(covid_latest, "../src/assets/data/data.csv", na = "")
