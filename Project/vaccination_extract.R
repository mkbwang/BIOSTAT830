

county_vaccinations <- read.csv('Project/COVID-19_Vaccinations_County.csv')

county_vaccinations$Date <- as.Date(county_vaccinations$Date,
                                    format="%m/%d/%Y")

library(dplyr)

uniq_dates <- unique(county_vaccinations$Date) %>% sort()
county_vaccinations$FIPS <- as.integer(county_vaccinations$FIPS)

selected_vaccination <- county_vaccinations %>%
  select(Date, FIPS, Recip_County, Recip_State, Series_Complete_Pop_Pct, Booster_Doses_Vax_Pct) %>%
  arrange(FIPS, Date)

saveRDS(selected_vaccination, 'Project/Vaccination_Rate.rds')

county_background <- county_vaccinations %>%
  select(FIPS, Recip_County, Recip_State, SVI_CTGY, Metro_status, Census2019) %>%
  unique()

saveRDS(county_background, 'Project/County_Demographics.rds')



