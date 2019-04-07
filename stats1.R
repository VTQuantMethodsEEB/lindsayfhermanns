#clear workspace
rm(list=ls())
########################################
# Testing if there are significant differences between REKN counts on the bay habitat vs. counts on the ocean.
#Null= There is no significant difference between bay and ocean counts.
#H1=There is a significant difference between bay and ocean counts.
#H2=ANOVA to test if there is a significant difference between counts at all habitat types.
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
#read csv file in
RedKnot<-read.csv(file="RK_Shorebird_Count_2010.csv")
Habitat <-read.csv("subsite.attributes.csv")
Survey.Density <- read.csv("Habitat.Densities.csv")
Dates.Survey <- read.csv("Shorebird_Survey_Form.csv")

#changing RedKnot Subsite and column and to character
RedKnot$Subsite <- as.character(RedKnot$Subsite)
Survey.Density$Subsite <- as.character(Survey.Density$Subsite)
#changing Red.Knot Habitat.birds.seen.in to character
RedKnot$Habitat.birds.seen.in <- as.character(RedKnot$Habitat.birds.seen.in)
Survey.Density$Habitat <- as.character(Survey.Density$Habitat)

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

Survey.Density$Subsite[Survey.Density$Subsite == "HOOK"] <- "DMP"
Survey.Density$Subsite[Survey.Density$Subsite == "THL"] <- "DMP"
Survey.Density$Subsite[Survey.Density$Subsite == "HAZ"] <- "NBE"
Survey.Density$Subsite[Survey.Density$Subsite == "TOR"] <- "FS"
Survey.Density$Subsite[Survey.Density$Subsite == "NBO"] <- "NBW"
Survey.Density$Subsite[Survey.Density$Subsite == "MUS"] <- "FS"
Survey.Density$Subsite[Survey.Density$Subsite == "LFS"] <- "FS"
Survey.Density$Subsite[Survey.Density$Subsite == "BONS"] <- "CMF"
Survey.Density$Subsite[Survey.Density$Subsite == "MWPF"] <- "NMA"

####Start here####
####Get total counts by survey period####

####Get total counts per year~survey period ~ hab####
####Get total counts per year~survey period ~subsite~habitat####
####Get density by survey period####
####Get density per year~survey period ~ hab####
####Get density per year~survey period ~subsite~habitat####

######Getting total counts per year, subsite, habitat type.######
##Defining SE function##
std <- function(x) sd(x)/sqrt(length(x))

#total count per year
year.count1 <- RedKnot%>%
  group_by(Year) %>% 
  summarize(Number.of.birds.seen = sum(Number.of.birds.seen, na.rm = T))


#total count per habitat
habitat.count1 <- RedKnot%>%
  group_by(Habitat.birds.seen.in)%>%
  summarize(Number.of.birds.seen = sum(Number.of.birds.seen, na.rm = T))
StandErr1 <- std(habitat.count1$Number.of.birds.seen)

#total count per subsite
subsite.count <- data.frame(rowsum(RedKnot$Number.of.birds.seen, reorder = T, RedKnot$Subsite, na.rm = T, ~RedKnot$Year))

#total count per subsite per year
year.subsite.count <- RedKnot%>%
  group_by(Year, Subsite) %>% 
  summarize(Number.of.birds.seen = sum(Number.of.birds.seen, na.rm = T))

#total count per habitat per year
year.habitat.count <- RedKnot%>%
  group_by(Habitat.birds.seen.in, Year) %>% 
  summarize(Number.of.birds.seen = sum(Number.of.birds.seen, na.rm = T))

#examining SPTE to determine what habitat/why high count through years
SPTE.Hab <- filter(RedKnot, Subsite == "SPTE")

####Stats Summary for Red Knots on FI####
mean <- mean(year.count1$Number.of.birds.seen)
std <- function(x) sd(x)/sqrt(length(x))
StandErr2 <- std(year.count1$Number.of.birds.seen)

##Finding peak survey period(s) of red knot abundance on FI##
peak.survey <- Survey.Density%>%
  group_by(Survey)%>%
  summarize(Count = sum(Count, na.rm = T))

library(lubridate)
#Match survey dates with survey numbers#
str(Dates.Survey$Date)
Dates.Survey$Date <- as.POSIXct(Dates.Survey$Date, tz = 'EST', '%m/%d/%Y')
str(Dates.Survey$Date)
Dates.Survey$jdate <- yday(Dates.Survey$Date)

#Looking for min and max Jdate to make export table of peak counts over survey periods#
date.julian <- select(Dates.Survey, Survey, Date, jdate)
a1 = aggregate(jdate~Survey, FUN=min, data=date.julian)                                                        
a2 = aggregate(jdate~Survey, FUN=max, data=date.julian)
#could bind but matched dates in excel
#cbind(function)

ggplot(peak.survey, aes(Survey, Count))+
barplot(peak.survey$Count)

# Write CSV in R to add "real dates" to CSV file based on min and max julian dates per survey
write.csv(peak.survey, file = "peak.survey.csv")

###Densities By Habitat Types###
###Write function in R to say where habitat column AND subsite column is = to x and x, populate column "length" with "y"
##Length values pulled from habitat.attributes CSV/google earth measure tool
Survey.Density[which(Survey.Density$Subsite == "CSB" & Survey.Density$Habitat == "Bay"),5] = "Cupsogue Flood Shoals"

Survey.Density[which(Survey.Density$Subsite == "CSB" & Survey.Density$Habitat == "Cupsogue Flood Shoals"),6] = 764.59
Survey.Density[which(Survey.Density$Subsite == "OIW" & Survey.Density$Habitat == "BOTH"),6] = 1278.0264
Survey.Density[which(Survey.Density$Subsite == "CMF"),6] = 774.43
Survey.Density[which(Survey.Density$Subsite == "CSE" & Survey.Density$Habitat == "Bay"),6] = 906.78
Survey.Density[which(Survey.Density$Subsite == "CSW" & Survey.Density$Habitat == "Bay"),6] = 427.6344
Survey.Density[which(Survey.Density$Subsite == "CSW" & Survey.Density$Habitat == "BOTH"),6] = 1164.6408
Survey.Density[which(Survey.Density$Subsite == "FS"),6] = 1052.34
Survey.Density[which(Survey.Density$Subsite == "CMF"),6] = 774.4
Survey.Density[which(Survey.Density$Subsite == "NBE" & Survey.Density$Habitat == "Backshore"),6] = 803.7576
Survey.Density[which(Survey.Density$Subsite == "NMA" & Survey.Density$Habitat == "Bay"),6] = 165.5064
Survey.Density[which(Survey.Density$Subsite == "OIE" & Survey.Density$Habitat == "Ocean"),6] = 1217.371
Survey.Density[which(Survey.Density$Subsite == "OIW" & Survey.Density$Habitat == "Bay"),6] = 674.8272
Survey.Density[which(Survey.Density$Subsite == "OIW" & Survey.Density$Habitat == "BOTH"),6] = 1278.0264
Survey.Density[which(Survey.Density$Subsite == "PSO" & Survey.Density$Habitat == "Bay"),6] = 1876.044
Survey.Density[which(Survey.Density$Subsite == "PSO" & Survey.Density$Habitat == "Ocean"),6] = 1036.015
Survey.Density[which(Survey.Density$Subsite == "SPTE" & Survey.Density$Habitat == "Ocean"),6] = 1850.136
Survey.Density[which(Survey.Density$Subsite == "SPTE" & Survey.Density$Habitat == "BOTH"),6] = 3831.0312
Survey.Density[which(Survey.Density$Subsite == "SPTE" & Survey.Density$Habitat == "Backshore"),6] = 1850.136

##Create new column "Density" which is Total/Habitat.length##
Survey.Density$Density.per.m <- Survey.Density$Count/Survey.Density$Habitat.Length

##Examine densities per subsite, habitat and subsite habitats##
##Density per subsite habitat##
Density.subhab.survey <- Survey.Density %>%
  group_by(Survey, Subsite, Habitat) %>% 
  summarize(Density.per.km = sum(Density.per.m*1000, na.rm = T))

Density.hab.survey <- Survey.Density %>%
  group_by(Survey, Habitat) %>% 
  summarize(Density.per.km = sum(Density.per.m*1000, na.rm = T))

Density.sub.survey <- Survey.Density %>%
  group_by(Survey, Subsite) %>% 
  summarize(Density.per.km = sum(Density.per.m*1000, na.rm = T))

###ANOVA on Survey Period###
# Compute the analysis of variance
survey.aov <- aov(Count ~ Survey , data = peak.survey)
# Summary of the analysis
summary(survey.aov)
#Check residuals vs fitted
plot(survey.aov, 1)
