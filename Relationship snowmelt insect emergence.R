library(tidyverse)
insect<-read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vTq7-xuTeasMbBQ366icvMvTduhKvMTsuk89mFN0lR_UzHqs67CdXmuDgs-NEuZNeBHZsmMy9LpM4Sz/pub?output=csv")
snowmelt<-read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQsQW1IpTBbS7PK4z8e3BvXjWHCJYST2puqkeq0cOgILjYtCHj3JNV5DfwgfcJxpktqnFo-PveIs49T/pub?output=csv")
                   
########## Trying some models ##########
# install.packages("lme4")
# install.packages("lmerTest")
library(lme4)  # for linear mixed models
library(lmerTest)  # and testing their significance

m1<- 




                  