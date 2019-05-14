#clear workspace
rm(list=ls())
########################################
#Install packages
########################################
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

#####################CSV#######################
Habitat <-read.csv("subsite.attributes.csv")
RedKnot <-read.csv("REKN_J.data.2016.data.2014.2016.csv")
#RedKnot data is pulled directly from our Master Database, sorted for REKN.

#####################CLEANING THE DATA#######################
#changing RedKnot Subsite and column and to character
RedKnot$Subsite <- as.character(RedKnot$Subsite)
RedKnot$Habitat <- as.character(RedKnot$Habitat)
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

#Modifying Habitat to match data set attributes, calling CSB and FS as Habitat = Flood Shoals  
RedKnot[which(RedKnot$Subsite == "CSB"),7] = "Flood Shoals"
RedKnot[which(RedKnot$Subsite == "FS"),7] = "Flood Shoals"
RedKnot[which(RedKnot$Subsite == "CMF"),7] = "Mudflat"

#Add column of subsite length
RedKnot["Habitat.length"] <- NA

#Write function in R to say where habitat column AND subsite column is = to x and x, populate column "length" with "y"
#Length values pulled from habitat.attributes CSV/google earth measure tool
RedKnot[which(RedKnot$Subsite == "CSB" & RedKnot$Habitat == "Flood Shoals"),9] = 764.59
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

#Convert year to numeric
RedKnot$year = as.numeric(as.character(RedKnot$Year))
#New data frame in case of imminent disaster 
RedKnot2 <- RedKnot
head(RedKnot2)

#Example data for expanding 
#surveys = c(1, 1, 3, 1, 2, 4, 5)  
#year = c(2010, 2010, 2010, 2011, 2011, 2011, 2011)
#habitat = c("a", "b", "c", "a", "b", "d", "e")
#df = tibble(surveys, year, habitat)

#Expanding data to account for each survey, and habitat type covered each survey, for each year of the study  
RedKnotFull = RedKnot2 %>% complete(year = full_seq(year, 1), Survey = 1:30, Habitat)

#Mutate to add column of Spring/Fall surveys based on peaks, and, add density column
RedKnotFull <- RedKnotFull %>%
  mutate(density_all = Total / (Habitat.length/1000))
#Mutate to add column of Spring/Fall surveys based on peaks, and, add density column
RedKnotFull <- RedKnotFull %>%
  mutate(Period = if_else(Survey >= 11, 'Fall', 'Spring'))

#####################FIGURES#######################
#Plot Density by survey and habitat
RedKnotFull$year <- as.factor(RedKnotFull$year)

ggplot(RedKnotFull, aes(Habitat, density_all, color = year))+
  geom_point(cex = 2, pch=19)+
  geom_jitter()+
  ylab("Red Knot Density")+
  xlab("Habitat")+
  theme_classic()

#Plot Counts by survey
ggplot(data=RedKnotFull,aes(x=Survey,y=Total))+
  geom_bar(stat = "identity")+
  facet_wrap(~year, ncol=3)+ #this is creating multiple "panels" for site
  scale_x_continuous(breaks = c(1 , 5, 10, 15, 20, 25, 30), labels = c("1", "5", "10", "15", "20", "25", "30"))+
  #scale_x_continuous(breaks = c(121,152,182,213,244,274),labels = c("May 1","June 1","July 1","August 1", "September 1", "October 1")) +
  xlab("Survey Periods by Year")+
  ylab("Red Knot Counts")+
  theme_classic()
#Plot Counts by survey
#ggplot(data=RedKnotFull,aes(x=Survey,y=Total))+
 # geom_bar(stat = "identity")+
  #facet_wrap(~year, ncol=3)+ #this is creating multiple "panels" for site
  #cale_x_discrete(breaks = c(6, 12, 18, 24, 30), labels = c("May","June","July","August", "September"))+
  #scale_x_continuous(breaks = c(121,152,182,213,244,274),labels = c("May 1","June 1","July 1","August 1", "September 1")) +
  #xlab("Month")+
  #ylab("Red Knot Counts")+
  #theme_classic()

#####################TESTS#######################
#Populating NA's correctly for analysis 
#example code x[is.na(x)] <- 0
RedKnotFull$Total[is.na(RedKnotFull$Total)] <- 0
RedKnotFull$density_all[is.na(RedKnotFull$density_all)] <- 0

#Shapiro-Wilk Test
#Are our data normally distributed?
#The null hypothesis is that the data are normally distributed
#P<0.05 indicates NOT normal
swt<-shapiro.test(RedKnotFull$Total)
swt

#A P-value < 0.05 indicates that data are not normally distributed
swt_d<-shapiro.test(RedKnotFull$density_all)
swt_d

#both datasets I want to analyze are either poisson or negative binomial distributed.
hist(RedKnotFull$Total)
hist(RedKnotFull$density_all)

#####################MODELS#######################
#Looking to examine any significant differences between survey and totals, and habitat type and totals,
#survey and density, and habitat type and density
head(RedKnotFull)
RedKnotFull$Survey <- as.factor(RedKnotFull$Survey)

#t test between fall and spring
tt <- t.test(Total~Period,data=RedKnotFull)
tt
#so we can see significance between fall and spring surveys, but with 95% confidence intervals, overlap.
#plot 
ggplot(RedKnotFull, aes(x = Period, y = Total))+
  geom_boxplot(fill=NA)+
  scale_size_area(breaks=1:2,max_size=4)

#ANOVA on Survey and Tukey analysis
library(car)
aov.h1 = aov(Total~Period, data = RedKnotFull)
summary(aov.h1)
TukeyHSD(aov.h1)

aov.h2 = aov(density_all~Period, data = RedKnotFull)
summary(aov.h2)

#note: zero inflated models ? Might have to run next time as 0 data is...abundant in data?.(14)
#try using calendar day to determine date for each survey
#This is the part I'm having a hard time understanding/interpreting 
library(MASS)
h1 <- glm.nb(density_all ~ Period + Habitat, data = RedKnotFull)
h2 <- glm.nb(density_all ~ Habitat, data=RedKnotFull)
h3 <- glm.nb(density_all ~ Survey, data = RedKnotFull)
h4 <- glm.nb(density_all ~ 1, data= RedKnotFull)

summary(h1)
summary(h2)
summary(h3)
summary(h4)

emmeans(h1, pairwise~Habitat, type="response")

#AIC first#
#the formula for AIC is very simple
#2*number of parameters - 2 ln(lik)
AIC(h1,h2,h3, h4)

#tabular
library(AICcmodavg)
aictab(cand.set=list(h1,h2,h3,h4),modnames=c("h1","h2","h3","h4"))

#showing first model is highest* ranked AIC.(with lowest score)
