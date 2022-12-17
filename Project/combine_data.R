
library(usmap)
rm(list=ls())

meteorology_data <- readRDS(file.path('Project', 'processed_data', 'Meteorology_weekly.rds'))
colnames(meteorology_data) <- c("State", "County", "FIPS", "Date", "AQI_PM25", "TEMP",
                                "PRESS", "RH_DP", "WIND", "Lat", "Long")
meteorology_data <- meteorology_data %>%
  filter(Date < as.Date('2021-12-01'))

days_count <- meteorology_data %>% group_by(State, County, FIPS) %>%
  summarise(days=n())
days_count_subset <- days_count %>% filter(days == 87)
included_counties <- days_count_subset$FIPS

filtered_meteorology_data <- meteorology_data %>% filter(FIPS %in% included_counties)



reproduction_number <- readRDS(file.path('Project', 'processed_data', 'Weekly_R_t.rds'))


# intersection of meteorology data and reproduction number data
combined_data <- filtered_meteorology_data %>%
  inner_join(reproduction_number, by=c("State", "County", "FIPS", "Date"))


vaccination_rate <- readRDS(file.path('Project', 'processed_data', 'Vaccination_Rate.rds'))
colnames(vaccination_rate) <- c("Date", "FIPS", "County", "State", "Complete_Vax_Rate")
vaccination_rate$State <- abbr2state(vaccination_rate$State)
vaccination_rate$County <- NULL
vaccination_rate$State <- NULL
valid_dates <- as.Date('2021-11-24') - seq(0, 7*86, 7)
vaccination_rate <- vaccination_rate %>% filter(Date %in% valid_dates) %>%
  filter(FIPS %in% combined_data$FIPS)


combined_data <- combined_data %>% full_join(vaccination_rate, by=c("Date", "FIPS"))

# social vulnerability index
SVI <- readRDS(file.path('Project', 'processed_data', 'SVI.rds'))
colnames(SVI) <- c("FIPS", "State", "County", "SVI")
SVI$County <- NULL
SVI$State <- NULL

combined_data <- combined_data %>% left_join(SVI, by="FIPS")

# travel proportions
travel <- readRDS(file.path('Project', 'processed_data', 'Travel_Proportions.rds')) %>%
  select(FIPS, date, Travel_Proportion)
travel$date <- travel$date + 1
colnames(travel)[2] <- "Date"
combined_data <- combined_data %>% left_join(travel, by=c("FIPS", "Date"))


# metro status
metro_status <- readRDS(file.path('Project', 'processed_data', 'Population.rds')) %>%
  select(FIPS, Metro_status, Census2019)

combined_data <- combined_data %>% left_join(metro_status, by='FIPS')


# add census subdivision label
new_england <- c("Connecticut", "Maine", "Massachusetts", "New Hampshire",
                "Rhode Island", "Vermont")

mid_atlantic <- c("New Jersey", "New York", "Pennsylvania")

east_north_central <- c("Indiana", "Illinois", "Michigan", "Ohio", "Wisconsin")

west_north_central <- c("Iowa", "Kansas", "Minnesota", "Missouri",
                        "Nebraska", "North Dakota", "South Dakota")

south_atlantic <- c("Delaware", "Florida", "Georgia", "Maryland", "North Carolina",
                    "South Carolina", "Virginia", "West Virginia")

east_south_central <- c("Alabama", "Kentucky", "Mississippi", "Tennessee")

west_south_central <- c("Arkansas", "Louisiana", "Oklahoma", "Texas")

mountain <- c("Arizona", "Colorado", "Idaho", "New Mexico", "Montana",
              "Utah", "Nevada", "Wyoming")

pacific <- c("Alaska", "California", "Hawaii", "Oregon", "Washington")

states_divisions <- data.frame(State = combined_data$State,
                               Division = NA)

states_divisions$Division[states_divisions$State %in% new_england] <- "New England"
states_divisions$Division[states_divisions$State %in% mid_atlantic] <- "Mid Atlantic"
states_divisions$Division[states_divisions$State %in% east_north_central] <- "East North Central"
states_divisions$Division[states_divisions$State %in% west_north_central] <- "West North Central"
states_divisions$Division[states_divisions$State %in% south_atlantic] <- "South Atlantic"
states_divisions$Division[states_divisions$State %in% east_south_central] <- "East South Central"
states_divisions$Division[states_divisions$State %in% west_south_central] <- "West South Central"
states_divisions$Division[states_divisions$State %in% mountain] <- "Mountain"
states_divisions$Division[states_divisions$State %in% pacific] <- "Pacific"
combined_data <- cbind(states_divisions[, "Division"], combined_data)
colnames(combined_data)[1] <- "Division"


saveRDS(combined_data, file.path('Project', 'processed_data', 'Combined_Data.rds'))

