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

########################################
#Read in CSV files.
########################################
Habitat <-read.csv("subsite.attributes.csv")
RedKnot <-read.csv("REKN_J.data.2016.data.2014.2016.csv")

########################################
#Cleaning the data
########################################
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

#Populating NA's correctly, Site = FNS, keep NA's for Totals
#x[is.na(x)] <- 0
RedKnotFull$Total[is.na(RedKnotFull$Total)] <- 0

#Mutate to add column of Spring/Fall surveys based on peaks, and, add density column
RedKnotFull <- RedKnotFull %>%
  mutate(Period = if_else(Survey >= 11, 'Fall', 'Spring'))
RedKnotFull <- RedKnotFull %>%
  mutate(density_all = Total / (Habitat.length/1000))

#####################TESTS#######################
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
###AIC first##
#the formula for AIC is very simple
#2*number of parameters - 2 ln(lik)
h1 = glm(Total~Habitat, data = RedKnotFull, family = poisson)
h2 = glm(Total~Survey,data = RedKnotFull, family = poisson)
h3 = glm(Total~1,data = RedKnotFull, family = poisson)
summary(h1)
summary(h2)
summary(h3)
AIC(h1,h2,h3)
h4 = glm(density_all ~Habitat, data = RedKnotFull, family = poisson)
h5 = glm(density_all ~Survey, data = RedKnotFull, family = poisson)
h6 = glm(density_all~1,data = RedKnotFull, family = poisson)
#simple version
AIC(h4,h5,h6)

#tabular
aictab(cand.set=list(h1,h2,h3,h4),modnames=c("h1","h2","h3","h4"))#AIC table
summary(h3)


#summary(RedKnotFull$year)
#rekn.nb <- glmer.nb(Total ~ Survey, (1|Subsite), data=RedKnotFull)
#rekn.nb1 <- glmer.nb(density_all ~ Survey, (1|Subsite), data=RedKnotFull)
#rekn.nb2 <- glmer.nb(Total ~ Habitat, (1|Subsite), data=RedKnotFull)
#rekn.nb3 <- glmer.nb(density_all ~ Habitat, (1|Subsite), data=RedKnotFull)

#look at mixed model
#summary(m.nb)
#t.value = 13.02
#p.value = 2*pt(t.value, df = 161, lower=FALSE)

#####################FIGURES#######################
#Plot Density by survey and habitat
ggplot(RedKnotFull, aes(Survey, density_all, color = Habitat))+
  geom_point(cex = 2, pch=19)+
  ylab("Red Knot Density")+
  xlab("Survey")+
  theme_classic()
#Plot Counts by survey
ggplot(data=RedKnotFull,aes(x=Survey,y=Total))+
  geom_bar(stat = "identity")+
  facet_wrap(~year, ncol=3)+ #this is creating multiple "panels" for site
  scale_x_continuous(breaks = c(1 , 5, 10, 15, 20, 25, 30), labels = c("1", "5", "10", "15", "20", "25", "30"))+
  #scale_x_continuous(breaks = c(121,152,182,213,244,274),labels = c("May 1","June 1","July 1","August 1", "September 1", "October 1")) +
  xlab("Survey")+
  ylab("Red Knot Counts")+
  theme_classic()

#Finding duplicates 
#which(duplicated(RedKnotFull[,1:3]) == TRUE)
#RedKnotFull[which(duplicated(RedKnotFull[,1:3]) == TRUE),]


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

##Seperating Spring and fall

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

#####################FIGURES#######################
#Plot Density by survey and habitat
ggplot(rd_all, aes(Survey, density_16, color = Habitat))+
  geom_point(cex = 6, pch=21)+
  geom_point(aes(Survey, density_15, color = Habitat), cex = 6, pch=5)+
  geom_point(aes(Survey, density_14, color = Habitat), cex = 6, pch=3)+
  ylab("Red Knot Density")+
  xlab("Survey")+
  theme_classic()
#Plot Counts by survey
ggplot(data=RedKnot,aes(x=Survey,y=Total))+
  geom_bar(stat = "identity")+
  facet_wrap(~Year, ncol=3)+ #this is creating multiple "panels" for site
  scale_x_continuous(breaks = c(1 , 5, 10, 15, 20, 25, 30), labels = c("1", "5", "10", "15", "20", "25", "30"))+
  #scale_x_continuous(breaks = c(121,152,182,213,244,274),labels = c("May 1","June 1","July 1","August 1", "September 1", "October 1")) +
  xlab("Survey")+
  ylab("Red Knot Counts")+
  theme_classic()

###ANOVA on Survey Period###
#fitting the data to figure out which tests I can run I know my data is negative binomial distribution --not normally distributed
RedKnot$Survey <- as.factor(RedKnotFull$Survey)
rd_2014$Survey <- as.factor(rd_2014$Survey)
# Compute the analysis of variance on whether survey has effect on total number seen. By year or by combined?
survey.glm <- glm(Total ~ Survey, family=poisson, data = RedKnotFull)
summary(survey.glm)

survey.all.aov.total <- aov(Total ~ Survey, data = RedKnotFull)

survey.aov <- aov(total_14 ~ Survey, data = rd_2014)
# Summary of the analysis
summary(survey.all.aov.total)
summary(survey.aov)
#Check residuals vs fitted
plot(survey.aov, 1)

density.aov <- aov(density_all~Survey, data = RedKnot)
summary(density.aov)
