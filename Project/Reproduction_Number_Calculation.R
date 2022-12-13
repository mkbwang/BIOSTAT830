
library(dplyr)
daily_county_counts <- readRDS(file.path('JHU_Counts', 'Daily_County_Counts.rds'))

LA_county_counts <- daily_county_counts %>% filter(FIPS == 6037)
LA_incidences <- LA_county_counts$Confirmed[2:620] - LA_county_counts$Confirmed[1:619]
LA_smoothed_incidences <- LA_incidences
for (j in 2:618){
  LA_smoothed_incidences[j] <- mean(LA_incidences[(j-1):(j+1)]) %>% as.integer()
}

LA_incidence_df <- data.frame(Date=LA_county_counts$Date[3:619],
                              I = LA_smoothed_incidences[2:618])

library(ggplot2)
theme_set(theme_bw())

incidence_plot <- ggplot(LA_incidence_df, aes(x=Date, y=I)) + geom_point() + geom_line()+
  xlab("Date") + ylab("LA County Incidence")


# library(EpiEstim)
# data(Flu2009)
#
# res_parametric_si <- estimate_R(LA_incidence_df,
#                                 method="parametric_si",
#                                 config = make_config(list(
#                                   mean_si = 5,
#                                   std_si = 0.5))
# )


# serial interval probability mass
w_s <- rep(0, 14)
w_s[1] <- plnorm(1.5, meanlog=1.6, sdlog=0.4)
for (j in 2:14){
  w_s[j] <- plnorm(j+0.5, meanlog=1.6, sdlog=0.4) -
    plnorm(j-0.5, meanlog=1.6, sdlog=0.4)
}
w_s <- w_s * 1/(sum(w_s))
w_s <- rev(w_s)

conv_result <- convolve(LA_incidence_df$I, w_s, type='open')
LA_incidence_df$lambda <- 0
LA_incidence_df$lambda[15: 617] <- conv_result[14: 616]
LA_incidence_df$R_t <- 0

a_prior <- 2
b_prior <- 3

for (j in 21:617){
  LA_incidence_df$R_t[j] <- (a_prior + sum(LA_incidence_df$I[(j-6): j]))/
    (1/b_prior + sum(LA_incidence_df$lambda[(j-6):j]))
}

Rt_plot <- ggplot(LA_incidence_df, aes(x=Date, y=R_t)) + geom_point() + geom_line()+
  xlab("Date") + ylab("LA County R_t")

library(cowplot)
plot_grid(incidence_plot, Rt_plot, nrow=2)
mean(LA_incidence_df$R_t[161:283])

plot(res_parametric_si$R$`Mean(R)`, type='l')

