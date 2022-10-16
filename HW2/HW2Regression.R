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

# deaths_by_age_group$End.Week <- as.numeric(as.POSIXct(deaths_by_age_group$End.Week))
deaths_by_age_group <- deaths_by_age_group[order(deaths_by_age_group$End.Week,
                                                 deaths_by_age_group$age_group),]


# load vaccine csv file
michigan_vaccine <- read.csv(file.path('HW2', 'michigan_vaccines.csv'))
michigan_vaccine$Date <- as.Date(michigan_vaccine$Date, format="%m/%d/%Y")
michigan_vaccine <- michigan_vaccine[order(michigan_vaccine$Date), ]
# process the full vaccination percentage for 18-64 age group
michigan_vaccine_percent <- michigan_vaccine %>% mutate(Pop5PlusA = Administered_Dose1_Recip_5Plus / Administered_Dose1_Recip_5PlusPop_Pct,
                                            Pop5PlusS = Series_Complete_5Plus / Series_Complete_5PlusPop_Pct,
                                            Pop18PlusA = Administered_Dose1_Recip_18Plus / Administered_Dose1_Recip_18PlusPop_Pct,
                                            Pop18PlusS = Series_Complete_18Plus / Series_Complete_18PlusPop_Pct,
                                            Pop65PlusA = Administered_Dose1_Recip_65Plus / Administered_Dose1_Recip_65PlusPop_Pct,
                                            Pop65PlusS = Series_Complete_65Plus / Series_Complete_65PlusPop_Pct) %>%
  mutate(Administered_Dose1_Recip_0to17_Pct =
           (Administered_Dose1_Recip_5Plus - Administered_Dose1_Recip_18Plus) /
           (Pop5PlusA - Pop18PlusA),
         Series_Complete_0to17_Pct = (Series_Complete_5Plus - Series_Complete_18Plus) /
           (Pop5PlusS - Pop18PlusS),
         Administered_Dose1_Recip_18to64_Pct =
           (Administered_Dose1_Recip_18Plus - Administered_Dose1_Recip_65Plus) /
           (Pop18PlusA - Pop65PlusA),
         Series_Complete_18to64_Pct = (Series_Complete_18Plus - Series_Complete_65Plus) /
           (Pop18PlusS - Pop65PlusS)) %>%
  dplyr::select(Date, Series_Complete_0to17_Pct, Series_Complete_18to64_Pct, Series_Complete_65PlusPop_Pct)


michigan_vaccine_percent[is.na(michigan_vaccine_percent)] <- 0

# matching dates of the vaccine data and the death count data
deaths_alldates <- deaths_by_age_group$End.Week - 3
vaccines_alldates <- michigan_vaccine_percent$Date
dates_match <- vaccines_alldates[vaccines_alldates %in% deaths_alldates]
vaccines_subset <- michigan_vaccine_percent[michigan_vaccine_percent$Date %in% dates_match,
                                            c("Date", "Series_Complete_18to64_Pct", "Series_Complete_65PlusPop_Pct")]
vaccines_subset_long <- rep(0, nrow(vaccines_subset)*2)
vaccines_subset_long[seq(1, 183, 2)] <- vaccines_subset$Series_Complete_18to64_Pct
vaccines_subset_long[seq(2, 184, 2)] <- vaccines_subset$Series_Complete_65PlusPop_Pct
#vwhich(deaths_by_age_group$End.Week-3 == min(vaccines_subset$Date))
deaths_by_age_group$fullvc <- 0
deaths_by_age_group$fullvc[81:264] <- vaccines_subset_long

deaths_by_age_group$End.Week <- rep(seq(0, 131), each=2)

# library(mgcv)

poi_model <- glm(death_counts ~ offset(log(population)) + End.Week + age_group,
                    family=poisson("log"), data=deaths_by_age_group)

library(MASS)
nb_model <- glm.nb(death_counts ~ offset(log(population)) + End.Week + age_group,
                   data=deaths_by_age_group)


poi_knots_model <- glm(death_counts ~ offset(log(population)) + End.Week  +
                         I(vaccination * End.Week) +
                         I(delta * End.Week) +
                         I(omicron1 * End.Week) +
                         I(omicron2 * End.Week) + age_group,
                       family=poisson("log"), data=deaths_by_age_group)


nb_knots_model <- glm.nb(death_counts ~ offset(log(population)) + End.Week  +
                        I(vaccination * End.Week) +
                        I(delta * End.Week) +
                        I(omicron1 * End.Week) +
                        I(omicron2 * End.Week) + age_group, data=deaths_by_age_group)


nb_knots_vaccination_model <- glm.nb(death_counts ~ offset(log(population)) + End.Week  +
                                       I(vaccination * End.Week) +
                                       I(delta * End.Week) +
                                       I(omicron1 * End.Week) +
                                       I(omicron2 * End.Week) + age_group + fullvc, data=deaths_by_age_group)

nb_knots_vaccination_age_model <- glm.nb(death_counts ~ offset(log(population)) + End.Week  +
                                           I(vaccination * End.Week) +
                                           I(delta * End.Week) +
                                           I(omicron1 * End.Week) +
                                           I(omicron2 * End.Week) + age_group + fullvc + I(age_group * fullvc), data=deaths_by_age_group)


