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
install.packages(c("dplyr", "stringr", "tidyverse","ggplot2","tidyr"))
library(dplyr)
library(stringr)
library(tidyverse)
library(ggplot2)
library(tidyr)

###################################
#
#extract month and day from each data set
#

#tasman avg
tas_avg$system.time_start <- as.Date(tas_avg$system.time_start,format = "%m/%d/%y")

new_tas_avg = tas_avg %>% 
  dplyr::mutate(year = lubridate::year(system.time_start),
                month = lubridate::month(system.time_start),
                day = lubridate::day(system.time_start))

head(new_tas_avg) #shows first 6 rows

new_tas_avg #shows all rows

#tasman
#reformat date
tas$system.time_start <- as.Date(tas$system.time_start,format = "%b %d,%Y")

new_tas = tas %>% 
  dplyr::mutate(year = lubridate::year(system.time_start),
                month = lubridate::month(system.time_start),
                day = lubridate::day(system.time_start))

head(new_tas) #shows first 6 rows

#franz joseph avg
fj_avg$system.time_start <- as.Date(fj_avg$system.time_start,format = "%b %d,%Y")

new_fj_avg = fj_avg %>% 
  dplyr::mutate(year = lubridate::year(system.time_start),
                month = lubridate::month(system.time_start),
                day = lubridate::day(system.time_start))

head(new_fj_avg) #shows first 6 rows

#franz joseph
fj$system.time_start <- as.Date(fj$system.time_start,format = "%b %d,%Y")

new_fj = fj %>% 
  dplyr::mutate(year = lubridate::year(system.time_start),
                month = lubridate::month(system.time_start),
                day = lubridate::day(system.time_start))

head(new_fj) #shows first 6 rows

#fox avg
fox_avg$system.time_start <- as.Date(fox_avg$system.time_start,format = "%b %d,%Y")

new_fox_avg = fox_avg %>% 
  dplyr::mutate(year = lubridate::year(system.time_start),
                month = lubridate::month(system.time_start),
                day = lubridate::day(system.time_start))

head(new_fox_avg) #shows first 6 rows

#fox
fox$system.time_start <- as.Date(fox$system.time_start,format = "%b %d,%Y")

new_fox = fox %>% 
  dplyr::mutate(year = lubridate::year(system.time_start),
                month = lubridate::month(system.time_start),
                day = lubridate::day(system.time_start))

head(new_fox) #shows first 6 rows

###################################
#
# add column with season
#

#Franz Josph Avg Albedo 
new_fj_avg$season[new_fj_avg$month==12]<-"Summer"
new_fj_avg$season[new_fj_avg$month>=1 & new_fj_avg$month<=2]<-"Summer"
new_fj_avg$season[new_fj_avg$month>=3 & new_fj_avg$month<=5]<-"Autumn"
new_fj_avg$season[new_fj_avg$month>=6 & new_fj_avg$month<=8]<-"Winter"
new_fj_avg$season[new_fj_avg$month>=9 & new_fj_avg$month<=11]<-"Spring"

#May-Oct
new_fj_avg$snow[new_fj_avg$month>=5 & new_fj_avg$month<=10]<-"On"
#Nov-Apr
new_fj_avg$snow[new_fj_avg$month>=1 & new_fj_avg$month<=4]<-"Off"
new_fj_avg$snow[new_fj_avg$month>=11 & new_fj_avg$month<=12]<-"Off"
#boxplot of albedo throughout seasons
boxplot(
  new_fj_avg$fj~new_fj_avg$season, 
  new_fj_avg,
  main = "Franz Joseph Avg Albedo vs. Season (2000-2020)",
  xlab = "Season",
  ylab = "Average Albedo")

#Fox Avg Albedo 
new_fox_avg$season[new_fox_avg$month==12]<-"Summer"
new_fox_avg$season[new_fox_avg$month>=1 & new_fox_avg$month<=2]<-"Summer"
new_fox_avg$season[new_fox_avg$month>=3 & new_fox_avg$month<=5]<-"Autumn"
new_fox_avg$season[new_fox_avg$month>=6 & new_fox_avg$month<=8]<-"Winter"
new_fox_avg$season[new_fox_avg$month>=9 & new_fox_avg$month<=11]<-"Spring"

#May-Oct
new_fox_avg$snow[new_fox_avg$month>=5 & new_fox_avg$month<=10]<-"On"
#Nov-Apr
new_fox_avg$snow[new_fox_avg$month>=1 & new_fox_avg$month<=4]<-"Off"
new_fox_avg$snow[new_fox_avg$month>=11 & new_fox_avg$month<=12]<-"Off"
#boxplot of albedo throughout seasons
boxplot(
  new_fox_avg$fox~new_fox_avg$season, 
  new_fox_avg,
  main = "Fox Glacier Avg Albedo vs. Season (2000-2020)",
  xlab = "Season",
  ylab = "Average Albedo")

#Tasman Avg Albedo
new_tas_avg$season[new_tas_avg$month==12]<-"Summer"
new_tas_avg$season[new_tas_avg$month>=1 & new_tas_avg$month<=2]<-"Summer"
new_tas_avg$season[new_tas_avg$month>=3 & new_tas_avg$month<=5]<-"Autumn"
new_tas_avg$season[new_tas_avg$month>=6 & new_tas_avg$month<=8]<-"Winter"
new_tas_avg$season[new_tas_avg$month>=9 & new_tas_avg$month<=11]<-"Spring"

#May-Oct
new_tas_avg$snow[new_tas_avg$month>=5 & new_tas_avg$month<=10]<-"On"
#Nov-Apr
new_tas_avg$snow[new_tas_avg$month>=1 & new_tas_avg$month<=4]<-"Off" 
new_tas_avg$snow[new_tas_avg$month>=11 & new_tas_avg$month<=12]<-"Off"
#boxplot of albedo throughout seasons
boxplot(
  new_tas_avg$tasman~new_tas_avg$season, 
  new_tas_avg,
  main = "Tasman Glacier Avg Albedo vs. Season (2000-2020)",
  xlab = "Season",
  ylab = "Average Albedo")

###################################
#
# subset for snow on/off months (avg datasets only)
# 

fj_snow_on <- subset(new_fj_avg,snow=='On',na.rm=TRUE)
fj_snow_off <- subset(new_fj_avg,snow=='Off',na.rm=TRUE)

tas_snow_on <- subset(new_tas_avg,snow=='On',na.rm=TRUE)
tas_snow_off <- subset(new_tas_avg,snow=='Off',na.rm=TRUE)

fox_snow_on <- subset(new_fox_avg,snow=='On',na.rm=TRUE)
fox_snow_off <- subset(new_fox_avg,snow=='Off',na.rm=TRUE)

###################################
#
# plots albedo vs calendar month by year, snow on and snow off
# 

# Franz Joseph Snow-On
plot(
  fj_snow_on$system.time_start,
  fj_snow_on$fj,
  ylim = c(40,100),
  main = 'Franz Joseph Albedo of Snow-On Months (May-Oct)',
  xlab = 'Date',
  ylab = 'Albedo',
  type='p',
  pch = 16,
)
abline(lm(fj_snow_on$fj ~ fj_snow_on$system.time_start, col='red'))


# Franz Joseph Snow-Off
plot(
  fj_snow_off$system.time_start,
  fj_snow_off$fj,
  ylim = c(40,100),
  main = 'Franz Joseph Albedo of Snow-Off Months (Nov-Apr)',
  xlab = 'Date',
  ylab = 'Albedo',
  type='p',
  pch = 16,
)
abline(lm(fj_snow_off$fj ~ fj_snow_off$system.time_start, col='red'))

# FJ snow on with ggplot2
#ggplot(data = fj_snow_on$fj, aes(x = fj_snow_on$system.time_start, y = fj_snow_on$fj)) +
#  geom_point(size = 2) +  
#  stat_smooth(method = "lm", col = "red", alpha = .25) # this will fit the trend line



### write for loops for snow on and snow off months
### this will give us 12 plots--one of each month--of each glacier

par(mfrow=c(2,3)) #set plot window to show 6 plots at a time

# double-check month values being pulled with UNIQUE function
unique(fj_snow_on$month) #checked with fj datasets, results apply to all
unique(fj_snow_off$month)

# FJ SNOW ON months for loop
for(i in unique(fj_snow_on$month)) {
  plot(fj_snow_on$year[fj_snow_on$month==i],
       fj_snow_on$fj[fj_snow_on$month==i],
       xlim = range(fj_snow_on$year),
       ylim = range(fj_snow_on$fj),
       xlab = 'Date',
       ylab = 'Albedo',
       main = paste('FJ Snow-On Albedo: Month', i),
       pch=16
       )
  #include trendline
  abline(lm(fj_snow_on$fj[fj_snow_on$month==i] ~
              fj_snow_on$year[fj_snow_on$month==i]))
}

# FJ SNOW OFF months for loop
for(i in unique(fj_snow_off$month)) {
  plot(fj_snow_off$year[fj_snow_off$month==i],
       fj_snow_off$fj[fj_snow_off$month==i],
       xlim = range(fj_snow_off$year),
       ylim = range(fj_snow_off$fj),
       xlab = 'Date',
       ylab = 'Albedo',
       main = paste('FJ Snow-Off Albedo: Month', i),
       pch=16
  )
  #include trendline
  abline(lm(fj_snow_off$fj[fj_snow_off$month==i] ~
              fj_snow_off$year[fj_snow_off$month==i]))
}

# FOX SNOW ON months for loop
for(i in unique(fox_snow_on$month)) {
  plot(fox_snow_on$year[fox_snow_on$month==i],
       fox_snow_on$fox[fox_snow_on$month==i],
       xlim = range(fox_snow_on$year),
       ylim = range(fox_snow_on$fox),
       xlab = 'Date',
       ylab = 'Albedo',
       main = paste('Fox Snow-On Albedo: Month', i),
       pch=16
  )
  #include trendline
  abline(lm(fox_snow_on$fox[fox_snow_on$month==i] ~
              fox_snow_on$year[fox_snow_on$month==i]))
}

# FOX SNOW OFF months for loop
for(i in unique(fox_snow_off$month)) {
  plot(fox_snow_off$year[fox_snow_off$month==i],
       fox_snow_off$fox[fox_snow_off$month==i],
       xlim = range(fox_snow_off$year),
       ylim = range(15:73),
       xlab = 'Date',
       ylab = 'Albedo',
       main = paste('Fox Snow-Off Albedo: Month', i),
       pch=16
  )
  #include trendline
  abline(lm(fox_snow_off$fox[fox_snow_off$month==i] ~
              fox_snow_off$year[fox_snow_off$month==i]))
}

# TASMAN SNOW ON months for loop
for(i in unique(tas_snow_on$month)) {
  plot(tas_snow_on$year[tas_snow_on$month==i],
       tas_snow_on$tasman[tas_snow_on$month==i],
       xlim = range(tas_snow_on$year),
       ylim = range(tas_snow_on$tasman),
       xlab = 'Date',
       ylab = 'Albedo',
       main = paste('Tasman Snow-On Albedo: Month', i),
       pch=16
  )
  #include trendline
  abline(lm(tas_snow_on$tasman[tas_snow_on$month==i] ~
              tas_snow_on$year[tas_snow_on$month==i]))
}
# TASMAN SNOW OFF months for loop
for(i in unique(tas_snow_off$month)) {
  plot(tas_snow_off$year[tas_snow_off$month==i],
       tas_snow_off$tasman[tas_snow_off$month==i],
       xlim = range(tas_snow_off$year),
       ylim = range(tas_snow_off$tasman),
       xlab = 'Date',
       ylab = 'Albedo',
       main = paste('Tasman Snow-Off Albedo: Month', i),
       pch=16
  )
  #include trendline
  abline(lm(tas_snow_off$tasman[tas_snow_off$month==i] ~
              tas_snow_off$year[tas_snow_off$month==i]))
}

####
# combine all glacier data into 12 monthly plots, highlighting each glacier

# merge all SNOW ON
merge1 <- merge(fj_snow_on,fox_snow_on, by='system.time_start')
snow_on <- merge(merge1,tas_snow_on,by='system.time_start')
snow_on[9:13]<-list(NULL) #remove repeating columns
snow_on[10:14]<-list(NULL) #remove repeating columns

# tidyr clean up SNOW ON dataset for easier plotting
# create Glacier and Albedo columns
snow_on <- snow_on %>%
  pivot_longer(c('fj','fox','tasman'), names_to = "Glacier", values_to = "Albedo")
print(snow_on)

# plot SNOW ON
# change month in line 332:37 for desired month plot
ggplot(subset(snow_on, month.x %in% 5), aes(x=system.time_start, y=Albedo, colour = Glacier)) +
  facet_wrap(vars(Glacier), ncol = 3) + 
  geom_point() +
  geom_smooth(method=lm) +
  labs(x = "Date", y = "Albedo") +
  ggtitle("Franz Joseph, Fox, and Tasman Glacial Albedos during May (2000-2020)") 
  

#merge all SNOW OFF
merge2 <- merge(fj_snow_off,fox_snow_off, by='system.time_start')
snow_off <- merge(merge2,tas_snow_off, by='system.time_start')
snow_off[9:13]<-list(NULL) #remove repeating columns
snow_off[10:14]<-list(NULL) #remove repeating columns

# tidyr clean up SNOW OFF dataset for easier plotting
# create Glacier and Albedo columns
snow_off <- snow_off %>%
  pivot_longer(c('fj','fox','tasman'), names_to = "Glacier", values_to = "Albedo")
print(snow_off)

# plot SNOW OFF
# change month in line 354:38 for desired month plot
ggplot(subset(snow_off, month.x %in% 11), aes(x=system.time_start, y=Albedo, colour = Glacier)) +
  facet_wrap(vars(Glacier), ncol = 3) + 
  geom_point() +
  geom_smooth(method=lm) +
  labs(x = "Date", y = "Albedo") +
  ggtitle("Franz Joseph, Fox, and Tasman Glacial Albedos during Snow-Off Months (2000-2020)") 

# merge SNOW_ON and SNOW_OFF
months <- rbind(snow_on,snow_off)
# edit data for cleaner plot labels
# replace glacier column with proper names for plots
months$Glacier[months$Glacier=='fj'] <- 'Franz Joseph'
months$Glacier[months$Glacier=='fox'] <- 'Fox'
months$Glacier[months$Glacier=='tasman'] <- 'Tasman'
# replace month number with names 
months$month.x <- month.name[months$month.x]

# now we have all glacier data together so we can write 1 for loop...
# ...instead of 2 for loops for snow on and snow off
# use for loop to create 12 graphs
unique(months$month.x) # double-check month values you'll be attaching to 1
for(i in unique(months$month.x)) {
  print(ggplot(subset(months, month.x==i), aes(x=system.time_start, y=Albedo, colour = Glacier)) +
    facet_wrap(vars(Glacier), ncol = 3) + 
    geom_point() +
    geom_smooth(method=lm) +
    labs(x = "Date", y = "Albedo") +
    ggtitle("2000-2020 Glacial Albedos during ",i)) 
}


###
# compare this season to "normal"
# find trend of "normal"...
    # 1) expected values for Nov'19-Apr/May'20
    # 2) note slope of "normal" for avg change per year
# difference in expected vs measured
# avergae monthly albedo vs most recent montly albedo Nov'19-Apr/May'20

# 
