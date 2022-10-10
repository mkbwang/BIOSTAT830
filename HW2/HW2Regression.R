library(ggplot2)
library(dplyr)
library(reshape2)
library(cowplot)

rm(list=ls())
michigan_deaths <- read.csv(file.path('HW2', 'michigan_deaths_by_week.csv'))


adult_deaths <- michigan_deaths %>%
  filter(Sex == "All Sex" & Age.Group %in% c("15-24 Years", "25-34 Years", "35-44 Years", "45-54 Years", "55-64 Years")) %>%
  group_by(End.Week) %>% summarise(death_counts = sum(covid))
adult_deaths$age_group <- 0# adults
adult_deaths$population <- 10050811*(1-0.214-0.181)



senior_deaths <- michigan_deaths %>%
  filter(Sex == "All Sex" & Age.Group %in% c("65-74 Years", "75-84 Years", "85 Years and Over")) %>%
  group_by(End.Week) %>% summarise(death_counts = sum(covid))
senior_deaths$age_group <- 1#senior
senior_deaths$population <- 10050811*0.181

deaths_by_age_group <- rbind(adult_deaths, senior_deaths)
deaths_by_age_group$End.Week <- as.Date(deaths_by_age_group$End.Week, format="%Y-%m-%d")


knot1 <- as.Date("2020-12-14", format="%Y-%m-%d")
knot2 <- as.Date("2021-06-26", format="%Y-%m-%d")
knot3 <- as.Date("2021-11-29", format="%Y-%m-%d")
knot4 <- as.Date("2022-05-22", format="%Y-%m-%d")
vaccine_available <- deaths_by_age_group$End.Week > knot1
delta <- deaths_by_age_group$End.Week > knot2
omicron1 <- deaths_by_age_group$End.Week > knot3
omicron2 <- deaths_by_age_group$End.Week > knot4

deaths_by_age_group$vaccination <- as.numeric(vaccine_available)
deaths_by_age_group$delta <- as.numeric(delta)
deaths_by_age_group$omicron1 <- as.numeric(omicron1)
deaths_by_age_group$omicron2 <- as.numeric(omicron2)

deaths_by_age_group$End.Week <- as.numeric(as.POSIXct(deaths_by_age_group$End.Week))
deaths_by_age_group <- deaths_by_age_group[order(deaths_by_age_group$End.Week,
                                                 deaths_by_age_group$age_group),]
deaths_by_age_group$End.Week <- rep(seq(0, 131), each=2)


library(mgcv)

poi_model <- gam(death_counts ~ offset(log(population)) + s(End.Week, k=20)+ age_group,
                    family=poisson("log"), data=deaths_by_age_group)

nb_model <- gam(death_counts ~ offset(log(population)) + s(End.Week, k=20)+ age_group,
                family=nb(), data=deaths_by_age_group)


poi_knots_model <- gam(death_counts ~ offset(log(population)) + s(End.Week, k=20)+ age_group + vaccination + delta + omicron1 + omicron2,
                       family=poisson("log"), data=deaths_by_age_group)

poi_knots_coefs <- poi_knots_model$coefficients[1:6]
poi_knots_covar <- vcov(poi_knots_model)[1:6, 1:6]

nb_knots_model <- gam(death_counts ~ offset(log(population)) + s(End.Week, k=20)+ age_group + vaccination + delta + omicron1 + omicron2,
                       family=nb(), data=deaths_by_age_group)

