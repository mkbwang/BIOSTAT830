

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
# Q3a

## extracting deaths lagged by one week
adults <- michigan_deaths %>% filter(age == "18-64")
adults$date <- seq(0, 132)
adults_lag1 <- adults[2:133, ]
adults_lag1$deaths <- adults_lag1$deaths + 1
adults_lag1$loglagdeath  <- log(adults$deaths[1:132] + 1)
adults_lag1$totpop <- 6.05e6

poi_model1_adults <- glm(deaths ~ loglagdeath + vaccine_prop_complete + date+
                         I(vaccination * date) +
                         I(delta * date) +
                         I(omicron1 * date) +
                         I(omicron2 * date)  ,
                       family=poisson("log"), data=adults_lag1)




senior <- michigan_deaths %>% filter(age == "65+")
senior$date <- seq(0, 132)
senior_lag1 <- senior[2:133, ]
senior_lag1$deaths <- senior_lag1$deaths + 1
senior_lag1$loglagdeath  <- log(senior$deaths[1:132]+1)
senior_lag1$totpop <- 1.81e6

model1_senior <- glm(deaths ~ loglagdeath+
                       vaccine_prop_complete+ date+
                       I(vaccination * date) +
                       I(delta * date) +
                       I(omicron1 * date) +
                       I(omicron2 * date),
                     family=poisson("log"), data=senior_lag1)

# Q3B

combined_data <- rbind(adults_lag1, senior_lag1)
combined_data$agegroup <- as.numeric(combined_data$age == "65+")

model2 <- glm(deaths ~ offset(log(totpop)) + loglagdeath+
                vaccine_prop_complete+ I(agegroup * vaccine_prop_complete)+
                date+
                I(vaccination * date) +
                I(delta * date) +
                I(omicron1 * date) +
                I(omicron2 * date),
              family=poisson("log"), data=combined_data)

# Q3C

## vaccination rate lag

adults_lag2 <- adults_lag1[2:132, ]
adults_lag2$vclag1 <- adults_lag1$vaccine_prop_complete[1:131]
adults_lag2$vclag2 <- c(0, adults_lag1$vaccine_prop_complete[1:130])

senior_lag2 <- senior_lag1[2:132, ]
senior_lag2$vclag1 <- senior_lag1$vaccine_prop_complete[1:131]
senior_lag2$vclag2 <- c(0, senior_lag2$vaccine_prop_complete[1:130])


combined_data2 <- rbind(adults_lag2, senior_lag2)
combined_data2$agegroup <-as.numeric(combined_data2$age == "65+")

model3 <- glm(deaths ~ offset(log(totpop)) + loglagdeath+
                vaccine_prop_complete+ I(agegroup * vaccine_prop_complete)+
                vclag1 + I(agegroup*vclag1)+
                vclag2 + I(agegroup*vclag2)+
                date+
                I(vaccination * date) +
                I(delta * date) +
                I(omicron1 * date) +
                I(omicron2 * date),
              family=poisson("log"), data=combined_data2)


