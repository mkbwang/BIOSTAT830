#HW3 P3

library(ggplot2)
library(dplyr)
library(reshape2)
library(cowplot)

rm(list=ls())
michigan_deaths <- read.csv(file.path('HW2', 'michigan_weekly_deaths_by_age.csv'))

michigan_deaths$End.Week <- as.Date(michigan_deaths$End.Week, format="%Y-%m-%d")


knot1 <- as.Date("2020-12-14", format="%Y-%m-%d")
knot2 <- as.Date("2021-06-26", format="%Y-%m-%d")
knot3 <- as.Date("2021-11-29", format="%Y-%m-%d")
knot4 <- as.Date("2022-05-22", format="%Y-%m-%d")
vaccine_available <- michigan_deaths$End.Week > knot1
delta <- michigan_deaths$End.Week > knot2
omicron1 <- michigan_deaths$End.Week > knot3
omicron2 <- michigan_deaths$End.Week > knot4

michigan_deaths$vavailable <- as.numeric(vaccine_available)
michigan_deaths$delta <- as.numeric(delta)
michigan_deaths$omicron1 <- as.numeric(omicron1)
michigan_deaths$omicron2 <- as.numeric(omicron2)

# deaths_by_age_group$End.Week <- as.numeric(as.POSIXct(deaths_by_age_group$End.Week))
michigan_deaths <- michigan_deaths[order(michigan_deaths$End.Week,
                                         michigan_deaths$age_group),]
michigan_deaths$population <- rep(c(2.14e6, 6.05e6, 1.81e6), 132)


# load vaccine csv file
michigan_vaccine <- read.csv(file.path('HW2', 'michigan_complete_vaccination.csv'))
michigan_vaccine$Date <- as.Date(michigan_vaccine$Date, format="%Y-%m-%d")
michigan_vaccine <- michigan_vaccine[order(michigan_vaccine$Date), ]

# matching dates of the vaccine data and the death count data
deaths_alldates <- michigan_deaths$End.Week - 3
vaccines_alldates <- michigan_vaccine$Date
dates_match <- vaccines_alldates[vaccines_alldates %in% deaths_alldates]
vaccines_subset <- michigan_vaccine[michigan_vaccine$Date %in% dates_match,]
vaccines_subset_long <- rep(0, nrow(vaccines_subset)*3)
vaccines_subset_long[seq(1, 274, 3)] <- vaccines_subset$Under18
vaccines_subset_long[seq(2, 275, 3)] <- vaccines_subset$Between19_64
vaccines_subset_long[seq(3, 276, 3)] <- vaccines_subset$Over65
#vwhich(deaths_by_age_group$End.Week-3 == min(vaccines_subset$Date))
start_pos <- min(which(michigan_deaths$End.Week-3 == min(dates_match)))
end_pos <- max(which(michigan_deaths$End.Week-3 == max(dates_match)))
michigan_deaths$fullvc <- 0
michigan_deaths$fullvc[start_pos:end_pos] <- vaccines_subset_long

michigan_deaths$Weeknum <- rep(seq(0, 131), each=3)


## only need population in other two age groups

michigan_deaths <- michigan_deaths %>% filter(age_group %in% c(1, 2))

michigan_deaths$age_group <- michigan_deaths$age_group - 1

library(MASS)
nb_knots_vaccination_age_model <- glm.nb(death_counts ~ offset(log(population)) + Weeknum  +
                                           age_group + fullvc + I(age_group * fullvc)+
                                           I(vavailable * Weeknum) +
                                           I(delta * Weeknum) +
                                           I(omicron1 * Weeknum) +
                                           I(omicron2 * Weeknum) , data=michigan_deaths)


lambda <- nb_knots_vaccination_age_model$theta

coefficients <- nb_knots_vaccination_age_model$coefficients

construct_mat <- nb_knots_vaccination_age_model$model

michigan_deaths$predicted_a_t <- as.integer(nb_knots_vaccination_age_model$fitted.values)




# Kalman filter

a_t_younger <- michigan_deaths[michigan_deaths$age_group == 0, "predicted_a_t"]
a_t_older <- michigan_deaths[michigan_deaths$age_group == 1, "predicted_a_t"]
Y_t_younger <- michigan_deaths[michigan_deaths$age_group == 0, "death_counts"]
Y_t_older <- michigan_deaths[michigan_deaths$age_group == 1, "death_counts"]


KM_calculation <- function(a_t, Y_t, rho, lambda, theta_0){

  m_0 <- theta_0
  C_0 <- 0

  f <- Y_t
  C <- rep(0, length(Y_t))
  # DmBeta <- matrix(0, N,  k)
  # S <- matrix(0,  k,  k )

  Q <- c(rep(0,length(Y_t)))
  m <- rep(1,length(Y_t))
  f[1] <- rho * a_t[1] * m_0 + (1 - rho) * a_t[1]
  u <- rho^2 * C_0 + (1 - rho^2)/lambda  ##u is u_0
  Q[1] <- a_t[1] * a_t[1] * u + a_t[1]
  C[1] <- u/(1 + a_t[1] * u)
  m[1] <- rho*m_0 + 1 - rho + C[1]*(Y_t[1] - f[1])

  # Kalman Filter
  for(n in 2:length(Y_t)){
    f[n] <-  rho * a_t[n] * m[n-1] + (1 - rho) * a_t[n]
    u <- rho^2 * C[n - 1] + (1 - rho^2)/lambda
    ### u is u[n-1]
    Q[n] <- a_t[n] * a_t[n] * u  +  a_t[n]
    C[n] <- u / (1 + a_t[n] * u)
    m[n] <- rho * m[n-1] + 1 - rho + C[n] * (Y_t[n] - f[n])
  }

  m_forward <- m

  # Kalman Smoother
  ms0 <- m_0 #note that C0 = 0 from smoother formula of ms
  ms <- m
  Cs <- C

  for(n in (length(Y_t) - 1):1){
    u <-  C[n] * rho^2 + (1 - rho^2)/lambda
    ms[n] <- m[n] + rho * C[n] * (ms[n + 1] - rho * m[n] - 1 + rho)/u
    Cs[n] <- (1 - rho^2) * C[n] / (u*lambda) + (rho^2)*C[n]^2 * Cs[n + 1]/u^2
  }

  return(list(KMFilter = m_forward, KMSmoother = ms))
}



library(ggplot2)

result_younger <- KM_calculation(a_t = a_t_younger, Y_t = Y_t_younger,
                          rho = 0.5, lambda = lambda, theta_0 = 1)
younger_KMfilter_df <- data.frame(Week = unique(michigan_deaths$End.Week),
                                 Value = result_younger$KMFilter)
younger_KMfilter_df$Type <- "Kalman Filter"
younger_KMsmoother_df <- data.frame(Week = unique(michigan_deaths$End.Week),
                                    Value = result_younger$KMSmoother)
younger_KMsmoother_df$Type <- "Kalman Smoother"
younger_df <- rbind(younger_KMfilter_df, younger_KMsmoother_df)
younger_KMplot <- ggplot(younger_df, aes(x=Week, y=Value)) +
  geom_point() + geom_line() + xlab("Week") + ylab("Theta") +
  facet_wrap(~Type) + theme_bw()



result_older <-  KM_calculation(a_t = a_t_older, Y_t = Y_t_older,
                                rho = 0.5, lambda = lambda, theta_0 = 1)


older_KMfilter_df <- data.frame(Week = unique(michigan_deaths$End.Week),
                                  Value = result_older$KMFilter)
older_KMfilter_df$Type <- "Kalman Filter"
older_KMsmoother_df <- data.frame(Week = unique(michigan_deaths$End.Week),
                                    Value = result_older$KMSmoother)
older_KMsmoother_df$Type <- "Kalman Smoother"
older_df <- rbind(older_KMfilter_df, older_KMsmoother_df)
older_KMplot <- ggplot(older_df, aes(x=Week, y=Value)) +
  geom_point() + geom_line() + xlab("Week") + ylab("Theta") +
  facet_wrap(~Type) + theme_bw()

library(cowplot)

plot_grid(younger_KMplot, older_KMplot,
          nrow=2,
          labels="AUTO")


# predict four future weeks

lastweek <- max(michigan_deaths$End.Week)
futureweeks <- lastweek + c(7, 14, 21, 28)

young_offset <- log(6.05e6)
young_predictors <- cbind(1, construct_mat[rep(263, 5), 3:10]) %>% as.matrix()
young_predictors[, c(2,6:9)] <- seq(131, 135)
younger_future_a_t <- as.vector(young_predictors %*% coefficients + young_offset) %>% exp()

younger_future_theta <- rep(younger_KMsmoother_df$Value[132], 5)
younger_future_Y <- rep(Y_t_younger[132], 5)
set.seed(2021)
Bt <- rbeta(n=5, shape1=0.5*lambda, shape2=(1-0.5)*lambda)
epsilon_t <- rgamma(n=5, shape=(1-0.5)*lambda, scale=1/lambda)

for (j in 2:5){
  younger_future_theta[j] <- Bt[j] * younger_future_theta[j-1] + epsilon_t[j]
  younger_future_Y[j] <- rpois(1, younger_future_theta[j] * younger_future_a_t[j])
}
young_prediction <- data.frame(Week = c(lastweek, futureweeks),
                               Type = c("Old", rep("New", 4)),
                               a_t = younger_future_a_t,
                               theta_t = younger_future_theta,
                               Y_t = younger_future_Y)

young_theta_plot <- ggplot(young_prediction, aes(x=Week, y=theta_t))+
  geom_point(aes(shape=Type), fill="white", size=3) + geom_line() + xlab("Week") + ylab("Theta") +
  scale_x_continuous(breaks=young_prediction$Week) + theme_bw() +
  theme(legend.position = "none") +scale_shape(solid = FALSE)

young_Y_plot <- ggplot(young_prediction, aes(x=Week, y=Y_t))+
  geom_point(aes(shape=Type), fill="white", size=3) + geom_line() + xlab("Week") + ylab("Y") +
  scale_x_continuous(breaks=young_prediction$Week) + theme_bw() +
  theme(legend.position = "none") +scale_shape(solid = FALSE)


# senior population
old_offset <- log(1.81e6)
old_predictors <- cbind(1, construct_mat[rep(264, 5), 3:10]) %>% as.matrix()
old_predictors[, c(2,6:9)] <- seq(131, 135)
old_future_a_t <- as.vector(old_predictors %*% coefficients + old_offset) %>% exp()

old_future_theta <- rep(older_KMsmoother_df$Value[132], 5)
old_future_Y <- rep(Y_t_older[132], 5)
set.seed(2020)
Bt <- rbeta(n=5, shape1=0.5*lambda, shape2=(1-0.5)*lambda)
epsilon_t <- rgamma(n=5, shape=(1-0.5)*lambda, scale=1/lambda)

for (j in 2:5){
  old_future_theta[j] <- Bt[j] * old_future_theta[j-1] + epsilon_t[j]
  old_future_Y[j] <- rpois(1, old_future_theta[j] * old_future_a_t[j])
}
old_prediction <- data.frame(Week = c(lastweek, futureweeks),
                               Type = c("Old", rep("New", 4)),
                               a_t = old_future_a_t,
                               theta_t = old_future_theta,
                               Y_t = old_future_Y)

young_prediction$Agegroup <- "18-64"
old_prediction$Agegroup <- "65+"

combined_prediction <- rbind(young_prediction, old_prediction)

theta_predicton_plot <- ggplot(combined_prediction, aes(x=Week, y=theta_t))+
  geom_point(aes(shape=Type), fill="white", size=3) + geom_line() +
  xlab("Week") + ylab("Theta") + facet_wrap(~Agegroup) +
  scale_x_continuous(breaks=young_prediction$Week, labels=format(young_prediction$Week, format="%m-%d")) + theme_bw() +
  theme(legend.position = "none", axis.text.x = element_text(angle = 90)) +scale_shape(solid = FALSE)


Y_predicton_plot <- ggplot(combined_prediction, aes(x=Week, y=Y_t))+
  geom_point(aes(shape=Type), fill="white", size=3) + geom_line() +
  xlab("Week") + ylab("Y") + facet_wrap(~Agegroup) +
  scale_x_continuous(breaks=young_prediction$Week, labels=format(young_prediction$Week, format="%m-%d")) + theme_bw() +
  theme(legend.position = "none", axis.text.x = element_text(angle = 90)) +scale_shape(solid = FALSE)

combined_prediction_plot <- plot_grid(theta_predicton_plot,
                                      Y_predicton_plot,
                                      labels="AUTO",nrow=2)
