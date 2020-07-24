#################
# New Zealand Glacial Albedo Research
# Tyler Pantle
# 6/18/20
#################

#set working drive
setwd("/Users/TylerPantle/Documents/NZ_research")

#read in each glacier's data
tas_avg <- read.csv("/Users/TylerPantle/Documents/NZ_research/Tasman_avg.csv")
tas <- read.csv("/Users/TylerPantle/Documents/NZ_research/Tasman_multi.csv")
fj_avg <- read.csv("/Users/TylerPantle/Documents/NZ_research/FJ_avg.csv")
fj <- read.csv("/Users/TylerPantle/Documents/NZ_research/FJ_multi.csv")
fox_avg <- read.csv("/Users/TylerPantle/Documents/NZ_research/Fox_avg.csv")
fox <- read.csv("/Users/TylerPantle/Documents/NZ_research/Fox_multi.csv")

#install packages for data analysis
install.packages(c("dplyr", "stringr", "tidyverse"))
library(dplyr)
library(stringr)
library(tidyverse)

#extract month and day from each data set
#tas avg
new_tas_avg = tas_avg %>% 
  dplyr::mutate(year = lubridate::year(system.time_start),
                month = lubridate::month(system.time_start),
                day = lubridate::day(system.time_start))

head(new_tas_avg) #shows first 6 rows

new_tas_avg #shows all rows

#tas
#reformat date
tas$system.time_start <- as.Date(tas$system.time_start,format = "%b %d,%Y")

new_tas = tas %>% 
  dplyr::mutate(year = lubridate::year(system.time_start),
                month = lubridate::month(system.time_start),
                day = lubridate::day(system.time_start))

head(new_tas) #shows first 6 rows

#fj_avg
#reformat date
fj_avg$system.time_start <- as.Date(fj_avg$system.time_start,format = "%b %d,%Y")
#extract dates
new_fj_avg = fj_avg %>% 
  dplyr::mutate(year = lubridate::year(system.time_start),
                month = lubridate::month(system.time_start),
                day = lubridate::day(system.time_start))

head(new_fj_avg) #shows first 6 rows

#fj
#reformat date
fj$system.time_start <- as.Date(fj$system.time_start,format = "%b %d,%Y")
#extract dates
new_fj = fj %>% 
  dplyr::mutate(year = lubridate::year(system.time_start),
                month = lubridate::month(system.time_start),
                day = lubridate::day(system.time_start))

head(new_fj) #shows first 6 rows

#fox_avg
#reformat date
fox_avg$system.time_start <- as.Date(fox_avg$system.time_start,format = "%b %d,%Y")
#extract dates
new_fox_avg = fox_avg %>% 
  dplyr::mutate(year = lubridate::year(system.time_start),
                month = lubridate::month(system.time_start),
                day = lubridate::day(system.time_start))

head(new_fox_avg) #shows first 6 rows

#fox
#reformat date
fox$system.time_start <- as.Date(fox$system.time_start,format = "%b %d,%Y")
#extract dates
new_fox = fox %>% 
  dplyr::mutate(year = lubridate::year(system.time_start),
                month = lubridate::month(system.time_start),
                day = lubridate::day(system.time_start))

head(new_fox) #shows first 6 rows

#Franz Josph Avg Albedo over season
new_fj_avg$season[new_fj_avg$month==12]<-"Summer"
new_fj_avg$season[new_fj_avg$month>=1 & new_fj_avg$month<=2]<-"Summer"
new_fj_avg$season[new_fj_avg$month>=3 & new_fj_avg$month<=5]<-"Autumn"
new_fj_avg$season[new_fj_avg$month>=6 & new_fj_avg$month<=8]<-"Winter"
new_fj_avg$season[new_fj_avg$month>=9 & new_fj_avg$month<=11]<-"Spring"
#boxplot of albedo over season
boxplot(
  new_fj_avg$fj~new_fj_avg$season, 
  new_fj_avg,
  main = "Franz Joseph Avg Albedo vs. Season (2000-2020)",
  xlab = "Season",
  ylab = "Average Albedo")

#Fox  Avg Albedo over season
new_fox_avg$season[new_fox_avg$month==12]<-"Summer"
new_fox_avg$season[new_fox_avg$month>=1 & new_fox_avg$month<=2]<-"Summer"
new_fox_avg$season[new_fox_avg$month>=3 & new_fox_avg$month<=5]<-"Autumn"
new_fox_avg$season[new_fox_avg$month>=6 & new_fox_avg$month<=8]<-"Winter"
new_fox_avg$season[new_fox_avg$month>=9 & new_fox_avg$month<=11]<-"Spring"
#boxplot of albedo over season
boxplot(
  new_fox_avg$fox~new_fox_avg$season, 
  new_fox_avg,
  main = "Fox Glacier Avg Albedo vs. Season (2000-2020)",
  xlab = "Season",
  ylab = "Average Albedo")

#Tasman Avg Albedo over season
new_tas_avg$season[new_tas_avg$month==12]<-"Summer"
new_tas_avg$season[new_tas_avg$month>=1 & new_tas_avg$month<=2]<-"Summer"
new_tas_avg$season[new_tas_avg$month>=3 & new_tas_avg$month<=5]<-"Autumn"
new_tas_avg$season[new_tas_avg$month>=6 & new_tas_avg$month<=8]<-"Winter"
new_tas_avg$season[new_tas_avg$month>=9 & new_tas_avg$month<=11]<-"Spring"
#boxplot of albedo over season
boxplot(
  new_tas_avg$tas~new_tas_avg$season, 
  new_tas_avg,
  main = "Tasman Glacier Avg Albedo vs. Season (2000-2020)",
  xlab = "Season",
  ylab = "Average Albedo")


