library(tidyverse)

covid <- read_csv(
  str_c(
    "https://github.com/microsoft/Bing-COVID-19-Data/raw/master/data/",
    "Bing-COVID19-Data.csv"
  ),
  guess_max = 1e6,
  progress = TRUE
)

covid_latest <- covid %>%
  filter(is.na(AdminRegion1)) %>%
  group_by(Country_Region) %>%
  arrange(Updated) %>%
  slice(n()) %>%
  ungroup() %>%
  arrange(ID)

print(covid_latest)

write_csv(covid_latest, "../src/assets/data/data.csv", na = "")
