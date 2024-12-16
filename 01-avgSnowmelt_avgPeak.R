# clear all data
remove(list=ls())

# load libraries
library(tidyverse)
library(dplyr)
library(ggplot2)

#load data
insect<-read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vTq7-xuTeasMbBQ366icvMvTduhKvMTsuk89mFN0lR_UzHqs67CdXmuDgs-NEuZNeBHZsmMy9LpM4Sz/pub?output=csv")
snowmelt<-read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQsQW1IpTBbS7PK4z8e3BvXjWHCJYST2puqkeq0cOgILjYtCHj3JNV5DfwgfcJxpktqnFo-PveIs49T/pub?output=csv")

#Calculates average snow melt date for each year
snowpeak<- snowmelt %>% group_by(Year)%>%
  summarize(AveregeSnowMeltday = mean(Snowmelt))

#Plots average snow melt date for each year with 1 average for plots
ggplot(snowpeak, aes( x = Year, y = AveregeSnowMeltday))+
  geom_line()+
  geom_point()+
  theme_classic()

#Plots average snow melt date for each year and for each plot
ggplot(snowpeak, aes( x = Year, y = AveregeSnowMeltday))+
  geom_line(data = snowmelt, aes( x = Year, y = Snowmelt, color = Plot.ID))+
  geom_point(data = snowmelt, aes( x = Year, y = Snowmelt, color = Plot.ID))+
  geom_line()+
  geom_point()+
  theme_classic()

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

#Plotting against year
ggplot(plotting, aes( x = Year))+
  geom_line(aes(y = AveregeSnowMeltday),color = "black")+
  geom_point(aes(y = AveregeSnowMeltday), color = "black")+
  geom_line(aes(y = InsectPeakday), color = "blue")+
  geom_point(aes(y = InsectPeakday), color = "blue")+
  theme_classic()
#Plotting insect peak vs snowmelt

ggplot(plotting, aes( x = AveregeSnowMeltday, y = Difference))+
  geom_line()+
  geom_point()+
  theme_classic()
