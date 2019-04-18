#Make a generalized linear model (preferably with more than one variable) for one of your hypotheses. Articulate which hypothesis you are testing.
#Explain what the R output is telling you about your data, in relation to your hypothesis.
#(Hint: you can use lsmeans, effects, relevel, or predict to help you.) You should include this explanation in either your README or in your code.
#Plot your model (e.g. using predict) and overlay the model on top of the underlying data. Remember that you will need to use “type=response”.
#Write a results statement (as you would in a scientific paper). If you need to reference a statistical table, you can include this result statement and table as a separate word doc that you upload to canvas titled “LASTNAME_week10_results”
#You will turn in this assignment in two weeks with model comparisons.
#  Week 11
#1. Use likelihood ratio tests and one other model selection approach to test at least 3 models of your data.
#The models can be LMs or GLMs that you have already tested.
#2. Explain what the results are telling you for each approach.
#3. Include a synthesis statement on how the output of each approach is similar or different in your code. Remember to update your README and annotate your code.
#
#clear workspace
rm(list=ls())
########################################
#Make a univariate linear model for one of your hypotheses
#Examine the assumptions of linearity (using tests or diagnostic plots)
#Plot the relationship in ggplot using stat_smooth
#install packages 
#install.packages("ggplot2")
#install.packages("tidyverse")
#install.packages("dplyr")
#install.packages("magrittr")
#install.packages("reshape2")
#install.packages("reshape")
#install.packages("gridExtra")
#install.packages("knitr")
#install.packages("officer")
#install.packages("plotly")
#install.packages("lubridate")
#install.packages("MASS")                                                                                 
###Library Call###
library(MASS)
library(tidyverse)
library(dplyr)
library(magrittr)
library(reshape)
library(reshape2)
library(gridExtra)
library(knitr)
library(officer)
library(plotly)
library(lubridate)

#Read in CSV files.
Habitat <-read.csv("subsite.attributes.csv")
RedKnot <-read.csv("REKN_J.data.2016.data.2014.2016.csv")

#changing RedKnot Subsite and column and to character
RedKnot$Subsite <- as.character(RedKnot$Subsite)
Habitat$Subsite <- as.character(Habitat$Subsite)

#Account for subsites that have been renamed or changed in RedKnot and Survey.Density files 
RedKnot$Subsite[RedKnot$Subsite == "HOOK"] <- "DMP"
RedKnot$Subsite[RedKnot$Subsite == "THL"] <- "DMP"
RedKnot$Subsite[RedKnot$Subsite == "HAZ"] <- "NBE"
RedKnot$Subsite[RedKnot$Subsite == "TOR"] <- "FS"
RedKnot$Subsite[RedKnot$Subsite == "NBO"] <- "NBW"
RedKnot$Subsite[RedKnot$Subsite == "MUS"] <- "FS"
RedKnot$Subsite[RedKnot$Subsite == "LFS"] <- "FS"
RedKnot$Subsite[RedKnot$Subsite == "BONS"] <- "CMF"
RedKnot$Subsite[RedKnot$Subsite == "MWPF"] <- "NMA"

Habitat$Subsite[Habitat$Subsite == "HOOK"] <- "DMP"
Habitat$Subsite[Habitat$Subsite == "THL"] <- "DMP"
Habitat$Subsite[Habitat$Subsite == "HAZ"] <- "NBE"
Habitat$Subsite[Habitat$Subsite == "TOR"] <- "FS"
Habitat$Subsite[Habitat$Subsite == "NBO"] <- "NBW"
Habitat$Subsite[Habitat$Subsite == "MUS"] <- "FS"
Habitat$Subsite[Habitat$Subsite == "LFS"] <- "FS"
Habitat$Subsite[Habitat$Subsite == "BONS"] <- "CMF"
Habitat$Subsite[Habitat$Subsite == "MWPF"] <- "NMA"

###Add column of subsite length
###Write function in R to say where habitat column AND subsite column is = to x and x, populate column "length" with "y"
##Length values pulled from habitat.attributes CSV/google earth measure tool
RedKnot["Habitat.length"] <- NA
#RedKnot["Date"] <- as.Date(NA)

##Modifying Habitat to match data set attribute## 
RedKnot[which(RedKnot$Subsite == "CSB"),7] = "Cupsogue Flood Shoals"
RedKnot[which(RedKnot$Subsite == "FS"),7] = "Old Inlet Flood Shoals"
#RedKnot[which(RedKnot$Subsite == "CMF"),7] = "Cupsogue Mudflat"
###Add column of subsite length
RedKnot["Habitat.length"] <- NA
###Write function in R to say where habitat column AND subsite column is = to x and x, populate column "length" with "y"
##Length values pulled from habitat.attributes CSV/google earth measure tool

RedKnot[which(RedKnot$Subsite == "CSB" & RedKnot$Habitat == "Cupsogue Flood Shoals"),9] = 764.59
RedKnot[which(RedKnot$Subsite ==  "OIW" & RedKnot$Habitat == "BOTH"),9] = 1278.0264
RedKnot[which(RedKnot$Subsite ==  "CMF"),9] = 774.43
RedKnot[which(RedKnot$Subsite == "CSE" & RedKnot$Habitat == "Bay"),9] = 906.78
RedKnot[which(RedKnot$Subsite == "CSW" & RedKnot$Habitat == "Bay"),9] = 427.6344
RedKnot[which(RedKnot$Subsite == "CSW" & RedKnot$Habitat == "BOTH"),9] = 1164.6408
RedKnot[which(RedKnot$Subsite == "FS"),9] = 1052.34
RedKnot[which(RedKnot$Subsite == "CMF"),9] = 774.4
RedKnot[which(RedKnot$Subsite == "NBE" & RedKnot$Habitat == "Backshore"),9] = 803.7576
RedKnot[which(RedKnot$Subsite == "NMA" & RedKnot$Habitat == "Bay"),9] = 165.5064
RedKnot[which(RedKnot$Subsite == "OIE" & RedKnot$Habitat == "Ocean"),9] = 1217.371
RedKnot[which(RedKnot$Subsite == "OIW" & RedKnot$Habitat == "Bay"),9] = 674.8272
RedKnot[which(RedKnot$Subsite == "OIW" & RedKnot$Habitat == "BOTH"),9] = 1278.0264
RedKnot[which(RedKnot$Subsite == "PSO" & RedKnot$Habitat == "Bay"),9] = 1876.044
RedKnot[which(RedKnot$Subsite == "PSO" & RedKnot$Habitat == "Ocean"),9] = 1036.015
RedKnot[which(RedKnot$Subsite == "SPTE" & RedKnot$Habitat == "Ocean"),9] = 1850.136
RedKnot[which(RedKnot$Subsite == "SPTE" & RedKnot$Habitat == "BOTH"),9] = 3831.0312
RedKnot[which(RedKnot$Subsite == "SPTE" & RedKnot$Habitat == "Backshore"),9] = 1850.136


##utility function for pretty printing (thanks Kate this is handy)
pr <- function(m) printCoefmat(coef(summary(m)),
                               digits=3,signif.stars=FALSE)

## Simple models
head(RedKnot)
RedKnot$Year <- as.factor(RedKnot$Year)
RedKnot$Year

#Create Julian date period for surveys based on 2016 survey dates.
#Filter for rows with "2016"
#Densities <- filter(RedKnot, Year == "2016")
#Make date column date format
#library(lubridate)
#Match survey dates with survey numbers#
#str(Densities$Date)
#Densities$Date <- as.POSIXct(Densities$Date, tz = 'EST', '%m/%d/%Y')
#str(Densities$Date)
#Densities$Jdate <- yday(Densities$Date)
#Create min and max jdate for each survey period
#a1 = aggregate(Jdate~Survey, FUN=min, data=Densities)                                                        
#a2 = aggregate(Jdate~Survey, FUN=max, data=Densities)
#cbind(a1, a2)
#need to try to get min and max and extrapolate them to data set to say, each survey period fell between x and x date
#so I can show in figure##STILL WORKING ON 

##Seperating Spring and fall

RedKnot <- RedKnot %>%
  mutate(Period = if_else(Survey >= 11, 'Fall', 'Spring'))

###Densities### 
#Get 2016, 2015, 2014 max Densities
##Filtering data by year, arranging by survey, adding a column with mutate of the densities, dropping species site and year 
##renaming columns in order to track them individially after leftjoin function of all 3 years.
rd_2016 <- filter(RedKnot, Year == "2016") %>% 
  arrange(Survey) %>% 
  mutate(density_16 = Total / (Habitat.length/1000)) %>%
  select(-c(Species, Site, Year)) %>%
  rename(date_16 = Date) %>%
  rename(subsite_16 = Subsite) %>%
  rename(total_16 = Total) %>%
  rename(habitat_length_16 = Habitat.length)

rd_2015 <- filter(RedKnot, Year == "2015") %>% 
  arrange(Survey) %>% 
  mutate(density_15 = Total / (Habitat.length/1000)) %>%
  select(-c(Species, Site, Year)) %>%
  rename(date_15 = Date) %>%
  rename(subsite_15 = Subsite) %>%
  rename(total_15 = Total) %>%
  rename(habitat_length_15 = Habitat.length)

rd_2014 <- filter(RedKnot, Year == "2014") %>% 
  arrange(Survey) %>% 
  mutate(density_14 = Total / (Habitat.length/1000)) %>%
  select(-c(Species, Site, Year)) %>%
  rename(date_14 = Date) %>%
  rename(subsite_14 = Subsite) %>%
  rename(total_14 = Total) %>%
  rename(habitat_length_14 = Habitat.length)

##Trying full join function
rd_all <- full_join(rd_2016, rd_2015, by = c("Survey","Habitat"))
rd_all <- full_join(rd_all, rd_2014, by = c("Survey","Habitat"))

#Applying left join by Survey and Habitat ##Basically need all combinations of Habitat and Subsite to be represented in each survey period, if no observations, NA==0!
#rd_all <- left_join(rd_2015, rd_2016, by = c("Survey", "Habitat"))
#rd_all <- left_join(rd_all, rd_2014, by = c("Survey", "Habitat"))

hist(RedKnot$Total)
var(RedKnot$Total)

###AIC first##
#the formula for AIC is very simple
#2*number of parameters - 2 ln(lik)
h1 = glm(Total~Habitat*Habitat.length,data = RedKnot, family = poisson)#is habitat type*habitat length 
h2 = glm(Total~Habitat+Habitat.length,data = RedKnot, family = poisson)
h3 = glm(Total~Habitat*Year,data = RedKnot, family = poisson)
h4 = glm(Total~1,data = RedKnot, family = poisson)
#simple version
AIC(h1,h2,h3,h4)

#tabular
aictab(cand.set=list(h1,h2,h3,h4),modnames=c("h1","h2","h3","h4"))#AIC table
summary(h3)
#this function will give a nice AIC table, but calculating weights and delta AIC is very straightforward