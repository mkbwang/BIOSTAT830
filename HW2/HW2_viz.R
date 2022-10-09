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
underage_deaths$age_group <- "0-14"


adult_deaths <- michigan_deaths %>%
  filter(Sex == "All Sex" & Age.Group %in% c("15-24 Years", "25-34 Years", "35-44 Years", "45-54 Years", "55-64 Years")) %>%
  group_by(End.Week) %>% summarise(death_counts = sum(covid))
adult_deaths$age_group <- "15-64"


senior_deaths <- michigan_deaths %>%
  filter(Sex == "All Sex" & Age.Group %in% c("65-74 Years", "75-84 Years", "85 Years and Over")) %>%
  group_by(End.Week) %>% summarise(death_counts = sum(covid))
senior_deaths$age_group <- ">65"


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
  scale_color_manual(values=c('#ff3300', '#3366ff', '#669900'))+
  xlab("Week") + ylab("Death Count")+theme_bw()


sex_trajectory <- ggplot(death_by_sex, aes(x=End.Week, y=death_counts))+
  geom_point(size=1, aes(color=sex), alpha=0.7) + geom_path(size=0.7, aes(color=sex), alpha=0.7)+
  scale_color_manual(values=c('#996633', '#cc33ff'))+
  xlab("Week") + ylab("Death Count") +theme_bw()

death_plot <- plot_grid(age_trajectory, sex_trajectory,
                        align="v", ncol=1, labels='AUTO')


# michigan population: https://www.census.gov/quickfacts/MI


michigan_vaccines$Date <- as.Date(michigan_vaccines$Date,
                                  format="%m/%d/%Y")

michigan_vaccines <- michigan_vaccines[order(michigan_vaccines$Date), ]
onedose_columns <- sprintf("Administered_Dose1_Recip_%dPlusPop_Pct",
                           c(5, 12, 18, 65))
onedose <- michigan_vaccines[, c("Date", onedose_columns)]
colnames(onedose) <- c("Date", "5Plus", "12Plus", "18Plus", "65Plus")
onedose_long <- melt(onedose, id.vars="Date", variable.name="Age_Group", value.name="Percentage") %>%
  na.omit()
onedose_long$Category <- "One Dose"
onedose_plot <- ggplot(onedose_long, aes(x=Date, y=Percentage, color=Age_Group)) +
  geom_path(size=0.8, alpha=0.6) +
  scale_color_manual(values=c('#003300', '#996633', '#003399', '#9900cc')) +
  xlab("Date") + ylab("One Dose Proportion") +theme_bw()+ylim(0, 101)



complete_columns <- sprintf("Series_Complete_%dPlusPop_Pct",
                            c(5, 12, 18, 65))
complete <- michigan_vaccines[, c("Date", complete_columns)]
colnames(complete) <- c("Date", "5Plus", "12Plus", "18Plus", "65Plus")
complete_long <- melt(complete, id.vars="Date", variable.name="Age_Group",
                      value.name="Percentage") %>% na.omit()

complete_plot <- ggplot(complete_long, aes(x=Date, y=Percentage, color=Age_Group)) +
  geom_path(size=0.8, alpha=0.6) +
  scale_color_manual(values=c('#003300', '#996633', '#003399', '#9900cc')) +
  xlab("Date") + ylab("Full Vaccination Proportion") +theme_bw()+ylim(0, 101)

vaccination_plot <- plot_grid(onedose_plot, complete_plot, align="v",
                              labels="AUTO", nrow=2)
