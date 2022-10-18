library(ggplot2)
library(dplyr)
library(reshape2)
library(cowplot)

rm(list=ls())
michigan_deaths <- read.csv(file.path('HW2', 'michigan_deaths_by_week.csv'))

michigan_vaccines <- read.csv(file.path('HW2', 'michigan_vaccines.csv'))


underage_deaths <- michigan_deaths %>%
  filter(Sex == "All Sex" & Age.Group %in% c("Under 1 year", "1-4 Years", "5-14 Years"))%>%
  group_by(End.Week) %>% summarise(death_counts = sum(covid))
underage_1518 <- michigan_deaths %>% filter(Sex == "All Sex" & Age.Group == "15-24 Years") %>%
  group_by(End.Week) %>% summarise(death_counts = 0.4 * sum(covid))
underage_deaths$death_counts <- underage_deaths$death_counts +
  round(underage_1518$death_counts, digits=0)
underage_deaths$age_group <- "0-18"


adult_1924 <- michigan_deaths %>% filter(Sex == "All Sex" & Age.Group == "15-24 Years") %>%
  group_by(End.Week) %>% summarise(death_counts = 0.6 * sum(covid))
adult_deaths <- michigan_deaths %>%
  filter(Sex == "All Sex" & Age.Group %in% c("25-34 Years", "35-44 Years", "45-54 Years", "55-64 Years")) %>%
  group_by(End.Week) %>% summarise(death_counts = sum(covid))
adult_deaths$death_counts <- adult_deaths$death_counts + round(adult_1924$death_counts, digits=0)
adult_deaths$age_group <- "19-64"


senior_deaths <- michigan_deaths %>%
  filter(Sex == "All Sex" & Age.Group %in% c("65-74 Years", "75-84 Years", "85 Years and Over")) %>%
  group_by(End.Week) %>% summarise(death_counts = sum(covid))
senior_deaths$age_group <- "65+"


male_deaths <- michigan_deaths %>%
  filter(Sex == "Male" & Age.Group=="All Ages") %>%
  group_by(End.Week) %>% summarise(death_counts = sum(covid))
male_deaths$sex <- "Male"

female_deaths <- michigan_deaths %>%
  filter(Sex == "Female" & Age.Group=="All Ages") %>%
  group_by(End.Week) %>% summarise(death_counts = sum(covid))
female_deaths$sex <- "Female"



death_by_age <- rbind(underage_deaths, adult_deaths, senior_deaths)
death_by_age$End.Week <- as.Date(death_by_age$End.Week, format="%Y-%m-%d")
death_by_sex <- rbind(male_deaths, female_deaths)
death_by_sex$End.Week <- as.Date(death_by_sex$End.Week, format="%Y-%m-%d")



age_trajectory <- ggplot(death_by_age, aes(x=End.Week, y=death_counts)) +
  geom_point(size=1, aes(color=age_group), alpha=0.7)+ geom_path(size=0.7, aes(color=age_group), alpha=0.7)+
  scale_color_manual(values=c('#3366ff', '#669900', '#ff3300'))+
  xlab("Week") + ylab("Death Count")+theme_bw()


sex_trajectory <- ggplot(death_by_sex, aes(x=End.Week, y=death_counts))+
  geom_point(size=1, aes(color=sex), alpha=0.7) + geom_path(size=0.7, aes(color=sex), alpha=0.7)+
  scale_color_manual(values=c('#996633', '#cc33ff'))+
  xlab("Week") + ylab("Death Count") +theme_bw()

death_plot <- plot_grid(age_trajectory, sex_trajectory,
                        align="v", ncol=1, labels='AUTO')

death_by_age$age_group <- rep(c(0, 1, 2), each=132)

write.csv(death_by_age,
          "HW2/weekly_deaths_by_age.csv", row.names=FALSE)

# michigan population: https://www.census.gov/quickfacts/MI

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
  mutate(Administered_Dose1_Recip_0to18_Pct =
           (Administered_Dose1_Recip_5Plus - Administered_Dose1_Recip_18Plus) /
           (Pop5PlusA - Pop18PlusA),
         Series_Complete_0to18_Pct = (Series_Complete_5Plus - Series_Complete_18Plus) /
           (Pop5PlusS - Pop18PlusS),
         Administered_Dose1_Recip_19to64_Pct =
           (Administered_Dose1_Recip_18Plus - Administered_Dose1_Recip_65Plus) /
           (Pop18PlusA - Pop65PlusA),
         Series_Complete_19to64_Pct = (Series_Complete_18Plus - Series_Complete_65Plus) /
           (Pop18PlusS - Pop65PlusS)) %>%
  dplyr::select(Date, Administered_Dose1_Recip_0to18_Pct, Administered_Dose1_Recip_19to64_Pct,
                Administered_Dose1_Recip_65PlusPop_Pct, Series_Complete_0to18_Pct,
                Series_Complete_19to64_Pct, Series_Complete_65PlusPop_Pct)


michigan_onedose <- michigan_vaccine_percent %>% select(Date,
                                                        Administered_Dose1_Recip_0to18_Pct,
                                                        Administered_Dose1_Recip_19to64_Pct,
                                                        Administered_Dose1_Recip_65PlusPop_Pct) %>%
  rename(Under18=Administered_Dose1_Recip_0to18_Pct,
         Between19_64=Administered_Dose1_Recip_19to64_Pct,
         Over65=Administered_Dose1_Recip_65PlusPop_Pct)
michigan_onedose[is.na(michigan_onedose)] <- 0



michigan_complete <- michigan_vaccine_percent %>% select(Date,
                                                         Series_Complete_0to18_Pct,
                                                         Series_Complete_19to64_Pct,
                                                         Series_Complete_65PlusPop_Pct) %>%
  rename(Under18=Series_Complete_0to18_Pct,
         Between19_64=Series_Complete_19to64_Pct,
         Over65=Series_Complete_65PlusPop_Pct)

michigan_complete[is.na(michigan_complete)] <- 0

write.csv(michigan_complete, 'HW2/complete_vaccination.csv',
          row.names=FALSE)

michigan_onedose_long <- melt(michigan_onedose, id.vars="Date", variable.name="Age_Group", value.name="Percentage") %>%
  na.omit()
# onedose_long$Category <- "One Dose"
onedose_plot <- ggplot(michigan_onedose_long, aes(x=Date, y=Percentage, color=Age_Group)) +
  geom_path(size=0.8, alpha=0.6) +
  scale_color_manual(values=c('#003300', '#996633', '#003399')) +
  xlab("Date") + ylab("One Dose Proportion") +theme_bw()+ylim(0, 101)



michigan_complete_long <- melt(michigan_complete, id.vars="Date", variable.name="Age_Group",
                      value.name="Percentage") %>% na.omit()

complete_plot <- ggplot(michigan_complete_long, aes(x=Date, y=Percentage, color=Age_Group)) +
  geom_path(size=0.8, alpha=0.6) +
  scale_color_manual(values=c('#003300', '#996633', '#003399')) +
  xlab("Date") + ylab("Full Vaccination Proportion") +theme_bw()+ylim(0, 101)

vaccination_plot <- plot_grid(onedose_plot, complete_plot, align="v",
                              labels="AUTO", nrow=2)
