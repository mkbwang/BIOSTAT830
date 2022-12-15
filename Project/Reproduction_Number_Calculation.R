
library(dplyr) # data frame manipulation
library(ggplot2) # plot
library(pracma) # triagular moving average
library(zoo)
library(tigris)
theme_set(theme_bw())

rm(list=ls())


# load all incidences
all_incidences <- readRDS(file.path('Project', 'processed_data', 'NYT_daily_incidences.rds'))
unique_locations <- unique(all_incidences$FIPS)

# date range
start_date <- as.Date('2020-04-12')
end_date <- as.Date('2021-11-27')
# time window of seven days
num_weeks <- as.integer((end_date - start_date + 1) / 7)

# serial interval pmf
w_s <- readRDS(file.path('Project', 'processed_data', 'w_s_prob.rds'))


# gamma prior for reproduction number
a_prior <- 2
b_prior <- 3


library(foreach)
library(doParallel)
cl <- makeCluster(4)
registerDoParallel(cl)


R_t_weekly <- foreach(j=1:length(unique_locations), .inorder=FALSE,
                      .packages='dplyr', .combine=rbind) %dopar%{

                        location_code <- unique_locations[j]

                        selected_location_incidences <- all_incidences %>% filter(FIPS == location_code) %>%
                          filter(date>=start_date-14 & date<=end_date) %>%
                          arrange(date)

                        selected_location_incidences$lambda <- 0 #renewal average

                        for (j in seq(15, nrow(selected_location_incidences))){
                          inc_vec <- selected_location_incidences$cases_avg[j-seq(1, 14)]
                          selected_location_incidences$lambda[j] <- sum(inc_vec * w_s)
                        }

                        R_t_df <- data.frame(State = selected_location_incidences$state[1],
                                             County = selected_location_incidences$county[1],
                                             FIPS = location_code,
                                             Date = start_date + seq(4, 4+(num_weeks-1)*7, 7),
                                             R_t=0)

                        for (j in seq(1, num_weeks)){
                          dates_range <- seq((j+2)*7-6, (j+2)*7)
                          R_t_df$R_t[j] <- (a_prior + sum(selected_location_incidences$cases_avg[dates_range]))/
                            (1/b_prior + sum(selected_location_incidences$lambda[dates_range]))
                        }

                        return(R_t_df)

                      }

saveRDS(R_t_weekly, file.path('Project', 'processed_data', 'Weekly_R_t.rds'))

