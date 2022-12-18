## ----setup, include=FALSE--------------------------------------------------------------
library(CARBayesST)
library(sf)
library(dplyr)
library(spdep)
library(raster)
library(stringr)
library(ggplot2)
library(maps)
library(usmap)
library(tigris)
knitr::opts_chunk$set(echo = TRUE)


## --------------------------------------------------------------------------------------

rm(list=ls())

## --------------------------------------------------------------------------------------
dt <- readRDS("Revised_Combined_Data.rds")

# get rid of four variables(erroneous measurements)
dt$PRESS <- NULL
dt$WIND <- NULL
dt$Metro_status <- NULL
dt$Census2019 <- NULL
dt$log_RT<-log(dt$R_t)

# rescale SVI and traveling proportion
dt$SVI <- dt$SVI * 100
dt$Travel_Proportion <- dt$Travel_Proportion * 100


# central the covariates except the vaccination rate
country_average <- dt %>% summarise(PM25 = mean(AQI_PM25), TEMP=mean(TEMP),
                                    RH_DP=mean(RH_DP), SVI=mean(SVI),
                                    Travel_Proportion = mean(Travel_Proportion))

dt$AQI_PM25 <- dt$AQI_PM25 - country_average$PM25
dt$TEMP <- dt$TEMP - country_average$TEMP
dt$RH_DP <- dt$RH_DP - country_average$RH_DP
dt$SVI <- dt$SVI - country_average$SVI
dt$Travel_Proportion <- dt$Travel_Proportion - country_average$Travel_Proportion


#SORT DATA BY DATE
dt <- dt %>% arrange(FIPS, Date)

#CREATE LAGGED VARIABLES
dt <-dt %>% group_by(FIPS) %>% mutate(TEMP.lag1 = dplyr::lag(TEMP, n = 1, default = NA), TEMP.lag2 = dplyr::lag(TEMP, n = 2, default = NA),
                                     RH_DP.lag1 = dplyr::lag(RH_DP, n=1, default = NA), RH_DP.lag2 = dplyr::lag(RH_DP, n=2, default=NA),
                                     Vax_Rate.lag1 = dplyr::lag(Complete_Vax_Rate, n=1, default = NA), Vax_Rate.lag2 = dplyr::lag(Complete_Vax_Rate, n=2, default=NA),
                                     PM25.lag1 = dplyr::lag(AQI_PM25, n=1, default=NA), PM25.lag2=dplyr::lag(AQI_PM25, n=2, default=NA),
                                     TP.lag1 = dplyr::lag(Travel_Proportion, n=1, default=NA), TP.lag2=dplyr::lag(Travel_Proportion, n=2, default=NA))


uniq_divisions <- unique(dt$Division)
division_id <- as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID")) # change in SLURM
selected_division <- uniq_divisions[division_id]



dt_subset <- dt %>% filter(Division == selected_division) %>% filter(Date >= as.Date('2020-07-08'))


## --------------------------------------------------------------------------------------


formula1<-log_RT~ PM25.lag1 + TEMP.lag1 + RH_DP.lag1 + Vax_Rate.lag1 + SVI + TP.lag1
formula2<-log_RT ~ PM25.lag2 + TEMP.lag2 + RH_DP.lag2 + Vax_Rate.lag2 + SVI + TP.lag2

# model1 <- lm(formula = formula1,data = dt_subset)
# model2 <- lm(formula = formula2,data = dt_subset)
#

## --------------------------------------------------------------------------------------


# sort dates
dt_subset <- dt_subset %>% arrange(Date, FIPS)


# set up W matrix for spatial correlation
uniq_states <- unique(dt_subset$State)
selected_counties <- counties(state=uniq_states) %>% dplyr::select(GEOID, geometry)
selected_counties$GEOID <- as.integer(selected_counties$GEOID)
subset_counties <- selected_counties %>% dplyr::filter(GEOID %in% unique(dt_subset$FIPS)) %>%
  arrange(GEOID)
neighbours <- poly2nb(subset_counties$geometry, queen=TRUE)
adjacency_mat <- matrix(0, nrow=length(neighbours), ncol=length(neighbours))
for (j in 1:length(neighbours)){
  adjacency_mat[j, neighbours[[j]]] <- 1
}


#centroid of each county
xy1<-unique(cbind(dt_subset$Long,dt_subset$Lat))
W.nb <-  knn2nb(knearneigh(xy1, k=1,longlat = TRUE))
for (j in 1:length(W.nb)){
  adjacency_mat[j, W.nb[[j]]] <- 1
  adjacency_mat[W.nb[[j]], j] <- 1
}



set.seed(123)
lag1_model <-ST.CARar(formula1, family="gaussian", data=dt_subset, W=adjacency_mat, burnin = 20000, n.sample = 60000, thin=50, AR=1)

set.seed(123)
lag2_model <-ST.CARar(formula2, family="gaussian", data=dt_subset, W=adjacency_mat, burnin = 20000, n.sample = 60000, thin=50, AR=1)


models <- list(lag1=lag1_model, lag2=lag2_model)


output_file <- sprintf('%s_result.rds', selected_division)

saveRDS(models, output_file)

ne_result <- readRDS('New England_result.rds')
