# New York Times Data Readin

library(readr)
library(tigris)
library(dplyr)

rm(list=ls())
url_2020 <- 'https://raw.githubusercontent.com/nytimes/covid-19-data/master/rolling-averages/us-counties-2020.csv'
url_2021 <- 'https://raw.githubusercontent.com/nytimes/covid-19-data/master/rolling-averages/us-counties-2021.csv'

gid2fips <- function(geoid_str){
  fips_id <- strsplit(geoid_str, split='-')[[1]][2] %>% as.integer()
  return(fips_id)
}
excluded_states <- c("Puerto Rico", "Guam", "Virgin Islands", "Alaska", "Hawaii",
                     "Northern Mariana Islands")



county_2020_data <- read.csv(url_2020)
county_2020_data$date <- as.Date(county_2020_data$date)
county_2020_data$FIPS <- sapply(county_2020_data$geoid, gid2fips)
subset_2020_data <- county_2020_data %>% select(state, date, county, FIPS, cases_avg) %>%
  filter(!(state %in% excluded_states)) %>%
  filter(county != "Unknown") %>%
  arrange(date)



county_2021_data <-  read.csv(url_2021)
county_2021_data$date <- as.Date(county_2021_data$date)
county_2021_data$FIPS <- sapply(county_2021_data$geoid, gid2fips)
subset_2021_data <- county_2021_data %>% select(state, date, county, FIPS, cases_avg) %>%
  filter(!(state %in% excluded_states)) %>%
  filter(county != "Unknown") %>%
  arrange(date)

incidences <- rbind(subset_2020_data, subset_2021_data) %>% arrange(date, state, county)

incidences$cases_avg <- as.integer(incidences$cases_avg)
saveRDS(incidences, file.path('Project', 'processed_data', 'NYT_daily_incidences.rds'))

