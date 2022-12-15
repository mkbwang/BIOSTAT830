
library(dplyr) # data frame manipulation
library(ggplot2) # plot
library(pracma) # triagular moving average
library(zoo)
library(tigris)
theme_set(theme_bw())

rm(list=ls())


# calculate California county R_ts

california_daily_incidences <- readRDS(file.path('Project', 'processed_data', 'California_Daily_Cases.rds'))
california_daily_incidences$FIPS <- as.integer(california_daily_incidences$FIPS)

# select county by FIPS number
selected_county_FIPS <- 6067 # LA county
start_date <- as.Date('2020-04-12')
end_date <- as.Date('2021-11-27')
num_weeks <- as.integer((end_date - start_date + 1) / 7)

# selected_county_counts <- daily_county_counts %>% filter(FIPS == selected_county_FIPS) %>%
#   select(FIPS, Date, Province_State, Confirmed) %>% arrange(Date)
# tot_days <- nrow(selected_county_counts)

selected_county_incidences <- california_daily_incidences %>% filter(FIPS == selected_county_FIPS) %>%
  select(FIPS, date, area, cases) %>% arrange(date)


selected_county_incidences$I <- rollmean(selected_county_incidences$cases,
                                                  k=7, fill=NA) %>% as.integer()

selected_county_range_incidences <- selected_county_incidences %>%
  filter(date>=start_date-14 & date<=end_date) %>% select(date, I)

incidence_plot <- ggplot(selected_county_range_incidences, aes(x=date, y=I)) +
  geom_point(size=1) + geom_line() + xlab("Date") + ylab("Daily Incidence")


selected_county_range_incidences$lambda <- 0
for (j in seq(15, nrow(selected_county_range_incidences))){
  inc_vec <- selected_county_range_incidences$I[j-seq(1, 14)]
  selected_county_range_incidences$lambda[j] <- sum(inc_vec * w_s)
}


a_prior <- 2
b_prior <- 3

R_t_df <- data.frame(Date = start_date + seq(4, 4+(num_weeks-1)*7, 7), R_t=0)

for (j in seq(1, num_weeks)){
  dates_range <- seq((j+2)*7-6, (j+2)*7)
  R_t_df$R_t[j] <- (a_prior + sum(selected_county_range_incidences$I[dates_range]))/
    (1/b_prior + sum(selected_county_range_incidences$lambda[dates_range]))
}


Rt_plot <- ggplot(R_t_df, aes(x=Date, y=R_t)) + geom_point() + geom_line()+
  xlab("Date") + ylab("LA County R_t")

library(cowplot)
plot_grid(incidence_plot, Rt_plot, nrow=2)


