
library(dplyr)# data frame manipulation
library(ggplot2) # plot
library(pracma) # triagular moving average
library(zoo)
library(tigris)
theme_set(theme_bw())

rm(list=ls())


# load all incidences
all_incidences <- readRDS(file.path('Project', 'processed_data', 'NYT_daily_incidences.rds'))
unique_locations <- unique(all_incidences[, c("state", "county", "FIPS")])

# date range
start_date <- as.Date('2020-03-29')
end_date <- as.Date('2021-11-27')
alldates <- data.frame(date = seq(start_date-14, end_date,by=1))

# time window of seven days
num_weeks <- as.integer((end_date - start_date + 1) / 7)

# serial interval pmf
w_s <- readRDS(file.path('Project', 'processed_data', 'w_s_prob.rds'))


# gamma prior for reproduction number
a_prior <- 2
b_prior <- 2


library(foreach)
library(doParallel)
cl <- makeCluster(4)
registerDoParallel(cl)




reproduction_number <- foreach(j=1:nrow(unique_locations), .inorder=FALSE,
                      .packages='dplyr', .combine=rbind) %dopar%{

                        state_name <- unique_locations$state[j]
                        location_name <- unique_locations$county[j]
                        location_code <- unique_locations$FIPS[j]

                        selected_location_incidences <- all_incidences %>% filter(FIPS == location_code) %>%
                          filter(date>=start_date-14 & date<=end_date) %>%
                          full_join(alldates, by="date") %>% arrange(date)

                        selected_location_incidences$state <- state_name
                        selected_location_incidences$county <- location_name
                        selected_location_incidences$FIPS <- location_code
                        selected_location_incidences$lambda <- 0 #renewal average

                        for (j in seq(15, nrow(selected_location_incidences))){
                          inc_vec <- selected_location_incidences$cases_avg[j-seq(1, 14)]
                          selected_location_incidences$lambda[j] <- sum(inc_vec * w_s)
                        }

                        R_t_df <- data.frame(State = selected_location_incidences$state[1],
                                             County = selected_location_incidences$county[1],
                                             FIPS = location_code,
                                             Date = start_date + seq(3, 3+(num_weeks-1)*7, 7),
                                             R_t=0)

                        for (j in seq(1, num_weeks)){
                          dates_range <- seq((j+2)*7-6, (j+2)*7)
                          R_t_df$R_t[j] <- (a_prior + sum(selected_location_incidences$cases_avg[dates_range]))/
                            (1/b_prior + sum(selected_location_incidences$lambda[dates_range]))
                        }

                        return(R_t_df)

                      }


## NYC contains five counties
NYC_Rt <- reproduction_number %>% filter(FIPS == 36998)
NYC_Rt <- NYC_Rt[rep(seq(1, 87), each=5), ]
NYC_Rt$County <- rep(c("New York", "Bronx", "Kings", "Queens", "Richmond"), 87)
NYC_codes <- fips(state="NY", county=c("New York", "Bronx", "Kings", "Queens", "Richmond")) %>%
  as.integer()
NYC_Rt$FIPS <- rep(NYC_codes, 87)


# Kansas city spans four counties
Kcity_codes <- fips(state="MO", county=c("Cass", "Clay", "Jackson", "Platte")) %>%
  as.integer()
Kcity_Rt <- reproduction_number %>%
  filter(FIPS == 29998)
Kcity_Rt <- Kcity_Rt[rep(seq(1, 87), each=4), ]
Kcity_Rt$County <- rep(c("Cass", "Clay", "Jackson", "Platte"), 87)
Kcity_Rt$FIPS <- rep(Kcity_codes, 87)

temp_reproduction_number <- reproduction_number %>%
  filter(!(FIPS %in% c(36998, 29998, NYC_codes, Kcity_codes)))

new_reproduction_number <- rbind(temp_reproduction_number, NYC_Rt, Kcity_Rt) %>%
  arrange(Date, FIPS)

saveRDS(new_reproduction_number, file.path('Project', 'processed_data', 'Weekly_R_t.rds'))
