library(ggplot2)
library(dplyr)
library(reshape2)
library(cowplot)

rm(list=ls())
michigan_deaths <- read.csv(file.path('HW2', 'weekly_deaths_by_age.csv'))

michigan_deaths$End.Week <- as.Date(michigan_deaths$End.Week, format="%Y-%m-%d")


knot1 <- as.Date("2020-12-14", format="%Y-%m-%d")
knot2 <- as.Date("2021-06-26", format="%Y-%m-%d")
knot3 <- as.Date("2021-11-29", format="%Y-%m-%d")
knot4 <- as.Date("2022-05-22", format="%Y-%m-%d")
vaccine_available <- michigan_deaths$End.Week > knot1
delta <- michigan_deaths$End.Week > knot2
omicron1 <- michigan_deaths$End.Week > knot3
omicron2 <- michigan_deaths$End.Week > knot4

michigan_deaths$vaccination <- as.numeric(vaccine_available)
michigan_deaths$delta <- as.numeric(delta)
michigan_deaths$omicron1 <- as.numeric(omicron1)
michigan_deaths$omicron2 <- as.numeric(omicron2)

# deaths_by_age_group$End.Week <- as.numeric(as.POSIXct(deaths_by_age_group$End.Week))
michigan_deaths <- michigan_deaths[order(michigan_deaths$End.Week,
                                         michigan_deaths$age_group),]
michigan_deaths$population <- rep(c(2.14e6, 6.05e6, 1.81e6), 132)


# load vaccine csv file
michigan_vaccine <- read.csv(file.path('HW2', 'complete_vaccination.csv'))
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

michigan_deaths$End.Week <- rep(seq(0, 131), each=3)

# library(mgcv)

poi_model <- glm(death_counts ~ offset(log(population)) + End.Week + age_group,
                    family=poisson("log"), data=michigan_deaths)

library(MASS)
nb_model <- glm.nb(death_counts ~ offset(log(population)) + End.Week + age_group,
                   data=michigan_deaths)


poi_knots_model <- glm(death_counts ~ offset(log(population)) + End.Week  +
                         I(vaccination * End.Week) +
                         I(delta * End.Week) +
                         I(omicron1 * End.Week) +
                         I(omicron2 * End.Week) + age_group,
                       family=poisson("log"), data=michigan_deaths)


nb_knots_model <- glm.nb(death_counts ~ offset(log(population)) + End.Week  +
                        I(vaccination * End.Week) +
                        I(delta * End.Week) +
                        I(omicron1 * End.Week) +
                        I(omicron2 * End.Week) + age_group, data=michigan_deaths)


nb_knots_vaccination_model <- glm.nb(death_counts ~ offset(log(population)) + End.Week +  age_group + fullvc+
                                       I(vaccination * End.Week) +
                                       I(delta * End.Week) +
                                       I(omicron1 * End.Week) +
                                       I(omicron2 * End.Week) , data=michigan_deaths)


nb_knots_vaccination_age_model <- glm.nb(death_counts ~ offset(log(population)) + End.Week  +
                                           age_group + fullvc + I(age_group * fullvc)+
                                           I(vaccination * End.Week) +
                                           I(delta * End.Week) +
                                           I(omicron1 * End.Week) +
                                           I(omicron2 * End.Week) , data=michigan_deaths)

