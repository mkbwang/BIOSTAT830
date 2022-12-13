folder ='JHU_counts'
start_date <- as.Date('03-22-2020', format='%m-%d-%Y')
all_dates <- start_date + seq(0, 650)

library(dplyr)
library(doParallel)
cl <- makeCluster(3)
registerDoParallel(cl)
library(foreach)


county_daily_counts <- foreach(i=1:length(all_dates), .combine=rbind, .inorder=FALSE, .packages='dplyr') %dopar%{
  dd <- all_dates[i]
  date_string <- as.character(format(dd, '%m-%d-%Y'))
  file_string <- sprintf('%s.csv', date_string)
  daily_counts <- read.csv(file.path('JHU_Counts', file_string))
  us_daily_counts <- daily_counts %>% filter(Country_Region == "US") %>%
    select(FIPS, Province_State, Lat, Long_, Confirmed, Deaths, Recovered, Active)
  us_daily_counts$Date <- dd
  return(us_daily_counts)

}

saveRDS(county_daily_counts, file=file.path('JHU_Counts', 'Daily_County_Counts.rds'))

uniq_counties <- unique(county_daily_counts$FIPS)

county_daily_incidence <- foreach(j=1:length(uniq_counties), .combine=rbind, .inorder=FALSE, .packages='dplyr') %dopar%{

  selected_county <- uniq_counties[j]
  selected_daily_counts <- county_daily_counts %>% filter(FIPS == selected_county) %>%
    arrange(Date) %>% select(FIPS, Province_State, Date, Lat, Long_, Confirmed)
  selected_daily_counts$Incidence <- 0
  numdays <- nrow(selected_daily_counts)
  selected_daily_counts$Incidence[2:numdays] <- selected_daily_counts$Confirmed[2:numdays] -
    selected_daily_counts$Confirmed[1:(numdays-1)]
  return(selected_daily_counts[-1, ])

}




