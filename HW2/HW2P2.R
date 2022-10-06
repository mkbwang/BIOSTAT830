library(readxl)
library(dplyr)
rm(list=ls())

# first process the death counts by weeks
# national weekly
national_deaths <- read.csv(file.path('HW2', 'Provisional_Deaths.csv'))
national_dates <- as.POSIXct(national_deaths$End.Week, format="%m/%d/%Y", tz="UTC")
national_deaths$End.Week <- national_dates

# Michigan daily data
michigan_deaths <- read_excel(file.path('HW2', 'michigan_case_death_county.xlsx'),
                              sheet='Data') %>% filter(!is.na(Date))
michigan_deaths_aggregate <- michigan_deaths %>% group_by(Date) %>%
  summarise(state_cum_deaths = sum(Deaths.Cumulative))


michigan_dates <- michigan_deaths_aggregate$Date
shared_dates <- national_dates[national_dates >= michigan_dates[1]]
national_deaths_subset <- national_deaths %>% filter(End.Week %in% shared_dates)
michigan_deaths_aggregate_subset <- michigan_deaths_aggregate %>%
  filter(Date %in% shared_dates)

michigan_death_weekly <- michigan_deaths_aggregate_subset$state_cum_deaths[2:133] -
  michigan_deaths_aggregate_subset$state_cum_deaths[1:132]

# Michigan weekly data
michigan_death_weekly_df <- list(Date = michigan_deaths_aggregate_subset$Date[2:133],
                                          Counts = michigan_death_weekly) %>%
  as.data.frame()


write.csv(michigan_death_weekly_df,
          file.path('HW2', 'michigan_deaths_by_week.csv'),
          row.names = FALSE)



vaccines <- read.csv(file.path('HW2', 'Provisional_Vaccine.csv'))


