
rm(list=ls())

meteorology <- read.csv(file.path('Project/datasheets/Meteorological_Kriged_Datasheet.csv'))


subset_meteorology <- meteorology %>%
  select(State.Name, County.Name, FIPS, Date.Local, AQI_PM25, TEMP, PRESS, RH_DP, WIND, Lat, Long) %>%
  filter(State.Name != "Alaska" & State.Name != "Hawaii")

subset_meteorology$Date.Local <- as.Date(subset_meteorology$Date.Local,
                                         format="%Y-%m-%d")

start_date <- as.Date('2020-03-29')
end_date <- as.Date('2021-12-04')
num_weeks <- as.integer((end_date - start_date+1)/7)


library(foreach)
library(doParallel)
cl <- makeCluster(4)
registerDoParallel(cl)


weekly_average <- foreach(j=0:(num_weeks-1), .combine=rbind, .packages='dplyr', .inorder=FALSE) %dopar%{
  chosen_week <- start_date + j*7

  one_week_data <- subset_meteorology %>%
    filter(Date.Local >= chosen_week & Date.Local <= chosen_week + 6)

  one_week_summary <- one_week_data %>% group_by(State.Name, County.Name, FIPS) %>%
    summarise(across(everything(), list(mean)))

  one_week_summary$Date.Local_1 <- chosen_week + 3

  return(one_week_summary)
}


days <- weekly_average %>% group_by(State.Name, County.Name, FIPS) %>%
  summarise(days=n())

saveRDS(weekly_average, file.path('Project', 'processed_data', 'Meteorology_weekly.rds'))

