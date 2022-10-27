

rm(list=ls())
folder <- "midterm"

michigan_deaths <- readRDS(file.path(folder, 'MI_death_vaccine_combined_data.RDS'))

knot1 <- as.Date("2020-12-14", format="%Y-%m-%d")
knot2 <- as.Date("2021-06-26", format="%Y-%m-%d")
knot3 <- as.Date("2021-11-29", format="%Y-%m-%d")
knot4 <- as.Date("2022-05-22", format="%Y-%m-%d")
vaccine_available <- michigan_deaths$date > knot1
delta <- michigan_deaths$date > knot2
omicron1 <- michigan_deaths$date > knot3
omicron2 <- michigan_deaths$date > knot4

michigan_deaths$vaccination <- as.numeric(vaccine_available)
michigan_deaths$delta <- as.numeric(delta)
michigan_deaths$omicron1 <- as.numeric(omicron1)
michigan_deaths$omicron2 <- as.numeric(omicron2)

library(dplyr)
# Qa

adults <- michigan_deaths %>% filter(age == "18-64")
adults$date <- seq(0, 132)
adults_lag1 <- adults[2:133, ]
adults_lag1$deaths <- adults_lag1$deaths + 1
adults_lag1$loglagdeath  <- log(adults$deaths[1:132] + 1)


model1_adults <- glm(deaths ~ date  +
                         I(vaccination * date) +
                         I(delta * date) +
                         I(omicron1 * date) +
                         I(omicron2 * date) + vaccine_prop_complete + loglagdeath,
                       family=poisson("log"), data=adults_lag1)



senior <- michigan_deaths %>% filter(age == "65+")
senior$date <- seq(0, 132)
senior_lag1 <- senior[2:133, ]
senior_lag1$deaths <- senior_lag1$deaths + 1
senior_lag1$loglagdeath  <- log(senior$deaths[1:132]+1)

model1_senior <- glm(deaths ~ date  +
                       I(vaccination * date) +
                       I(delta * date) +
                       I(omicron1 * date) +
                       I(omicron2 * date) + vaccine_prop_complete + loglagdeath,
                     family=poisson("log"), data=senior_lag1)

library(MASS)
## negative binomial?
model1_senior_nb <- glm.nb(deaths ~ date  +
                       I(vaccination * date) +
                       I(delta * date) +
                       I(omicron1 * date) +
                       I(omicron2 * date) + vaccine_prop_complete + loglagdeath, data=senior_lag1)


