# clear all data
remove(list=ls())

# load libraries
library(tidyverse)
library(dplyr)
library(ggplot2)
# install.packages("lme4")
install.packages("lmerTest")
#library(lme4)  # for linear mixed models
library(lmerTest)  # and testing their significance

insect<-read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vTq7-xuTeasMbBQ366icvMvTduhKvMTsuk89mFN0lR_UzHqs67CdXmuDgs-NEuZNeBHZsmMy9LpM4Sz/pub?output=csv")
snowmelt<-read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQsQW1IpTBbS7PK4z8e3BvXjWHCJYST2puqkeq0cOgILjYtCHj3JNV5DfwgfcJxpktqnFo-PveIs49T/pub?output=csv")

##################################################
#Calculates average snow melt date for each year
snowpeak<- snowmelt %>% group_by(Year)%>%
  summarize(AveregeSnowMeltday = mean(Snowmelt))
#Calculates the cumulative sum of insect biomass for each year
output <- insect %>%
  filter(year >= 2019) %>%
  ungroup() %>%
  group_by(year, doy) %>%
  summarize(DailyBiomass = sum(Biomass), .groups = "drop") %>%
  group_by(year) %>%
  arrange(doy) %>%
  mutate(runningtotal = cumsum(DailyBiomass)) %>%
  select(year, InsectPeakday = doy, runningtotal)
# calculates what half the insect biomass is yearly
halfvalue <- insect %>%
  filter(year >= 2019) %>%
  group_by(year) %>%
  summarize(
    YearlyBiomass = sum(Biomass),
    HalfYearlyBiomass = YearlyBiomass / 2
  )
#combines the above 2 data frames
combined <- output %>%
  inner_join(halfvalue, by = "year")
#finds the data when half the insect biomass has emerged (peak insect emergence)
result <- combined %>%
  filter(runningtotal < HalfYearlyBiomass) %>%
  rename(Year = year)%>%
  group_by(Year) %>%
  slice_max(runningtotal)

#creates a dataframe with the snowmelt data
plotting <- result %>%
  inner_join(snowpeak, by = "Year")%>%
  mutate(Difference = InsectPeakday - AveregeSnowMeltday)
                   
########## Trying some models ##########
<<<<<<< HEAD
m1<- lm(InsectPeakday ~ AveregeSnowMeltday, data = plotting)
summary(m1)
=======
m1<- 

>>>>>>> 3ef592df4ada53e4511f87cfdffcdf3d379fd1d2

m2<-glm(InsectPeakday ~ AveregeSnowMeltday, data = plotting, family = "poisson")
summary(m2)


                  