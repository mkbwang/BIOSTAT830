
library(dplyr)

rm(list=ls())
# vaccination rate
county_vaccinations <- read.csv('Project/datasheets/COVID-19_Vaccinations_County.csv')

county_vaccinations$Date <- as.Date(county_vaccinations$Date,
                                    format="%m/%d/%Y")


uniq_dates <- unique(county_vaccinations$Date) %>% sort()
county_vaccinations$FIPS <- as.integer(county_vaccinations$FIPS)

selected_vaccination <- county_vaccinations %>%
  select(Date, FIPS, Recip_County, Recip_State, Series_Complete_Pop_Pct) %>%
  na.omit() %>%
  arrange(FIPS, Date)


saveRDS(selected_vaccination, 'Project/processed_data/Vaccination_Rate.rds')


# Metro Status
county_population <- county_vaccinations %>%
  select(FIPS, Recip_County, Recip_State, Metro_status, Census2019) %>%
  na.omit() %>%
  unique()

saveRDS(county_population, 'Project/processed_data/Population.rds')


# SVI
SVI_sheet <- read.csv(file.path('Project', 'datasheets', 'SVI2020_US.csv'))

SVI <- SVI_sheet %>% select(STCNTY, RPL_THEMES) %>% filter(RPL_THEMES >= 0)
colnames(SVI) <- c("FIPS", "SVI")
SVI_mean <- SVI %>% group_by(FIPS) %>% summarise(SVI = mean(SVI))
saveRDS(SVI_mean, 'Project/processed_data/SVI.rds')


# traveling patterns
trip <- read.csv('Project/datasheets/trip_mortality_county_weekly_data.csv')
travel_proportions <- trip %>% select(County.FIPS, State.ggplot, County.Name, Lat, Long_, date, Proportion.not.staying.at.home)
colnames(travel_proportions) <- c('FIPS', 'State', 'County', 'Lat', 'Long_', 'date', 'Travel_Proportion')
travel_proportions$date <- as.Date(travel_proportions$date, format='%Y-%m-%d')
travel_proportions <- travel_proportions %>% arrange(FIPS, date)
saveRDS(travel_proportions, 'Project/processed_data/Travel_Proportions.rds')





