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
RedKnot[which(RedKnot$Subsite == "CMF"),7] = "1"
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

RedKnot <- RedKnot %>%
mutate(Period = if_else(Survey >= 11, 'Fall', 'Spring'))

RedKnot <- RedKnot %>%
mutate(density_all = Total / (Habitat.length/1000))
  
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

rd_all <- full_join(rd_2016, rd_2015, by = c("Survey","Habitat"))
rd_all <- full_join(rd_all, rd_2014, by = c("Survey","Habitat"))

##################################################
# GGPLOT Count and Density Plots 
##################################################

#Plotting counts by survey per year
#Setting theme

ggplot(data=RedKnot,aes(x=Survey,y=Total))+
  geom_bar(stat = "identity")+
  facet_wrap(~Year, ncol=3)+ #this is creating multiple "panels" for site
  scale_x_continuous(breaks = c(1 , 5, 10, 15, 20, 25, 30), labels = c("1", "5", "10", "15", "20", "25", "30"))+
#scale_x_continuous(breaks = c(121,152,182,213,244,274),labels = c("May 1","June 1","July 1","August 1", "September 1", "October 1")) +
  xlab("Survey")+
  ylab("Red Knot Counts")+
  theme_classic()
#color spring migration surveys differently than fall migration surveys

#Plotting density by habitat
ggplot(data=rd_all,aes(x=Habitat,y=density_16, color = Habitat))+
  geom_point(cex = 6, pch=21)+
  geom_point(aes(Survey, density_15, color = Habitat), cex = 6, pch=5)+
  geom_point(aes(Survey, density_14, color = Habitat), cex = 6, pch=3)
 
  

#####STILL WORKING ON BELOW#####


   facet_wrap(~Year, ncol=3)+ #this is creating multiple "panels" for site
  #scale_x_continuous(breaks = c(1 , 5, 10, 15, 20, 25, 30), labels = c("1", "5", "10", "15", "20", "25", "30"))+
  #scale_x_continuous(breaks = c(121,152,182,213,244,274),labels = c("May 1","June 1","July 1","August 1", "September 1", "October 1")) +
  xlab("Habitat")+
  ylab("Red Knot Density")+
  theme_classic()

ggplot(RedKnot, aes(Survey, density_16, color = Habitat))+
  geom_point(cex = 6, pch=21)+
  geom_point(aes(Survey, density_15, color = Habitat), cex = 6, pch=5)+
  geom_point(aes(Survey, density_14, color = Habitat), cex = 6, pch=3)


ggplot(data=RedKnot,aes(x=Julian.date, fill = Habitat.birds.seen.in)) +
  geom_histogram(binwidth = bin) +
  facet_wrap(~Year)+ #this is creating multiple "panels" for site
  scale_x_continuous(breaks = c(121,152,182,213,244,274),labels = c("May 1","June 1","July 1","Aug 1", "Sept 1", "Oct 1")) +
  xlab("Date")+
  ylab("Density")+ ###NEED to change Y axis to reflect data conveyed and relabel ticks###
  ggtitle("Habitat Densities") +
  labs(legend = "Habitat")+
  theme_bw()+
theme(legend.position="bottom",
legend.title = element_blank(),
legend.text = element_text(size=20),
legend.background = element_blank(),
legend.key=element_rect(fill="white",color="white"),
 plot.title = element_text(hjust = 0.5))

#Plotting density by subsite
ggplot(data=RedKnot,aes(x=Julian.date, fill = Subsite))+
  geom_histogram(binwidth = bin) +
  facet_wrap(~Year, ncol=2)+ #this is creating multiple "panels" for site
  scale_x_continuous(breaks = c(121,152,182,213,244,274),labels = c("May 1","June 1","July 1","August 1", "September 1", "October 1")) +
  #relabel y ticks##
  xlab("Date")+
  ylab("Red Knot Counts")

##########################################
# Bar plots with base plotting
#########################################

# Plotting counts by subsite
# get counts by subsite and store as object called 'subsite.n'
subsite.n = as.numeric(rowsum(RedKnot$Number.of.birds.seen, reorder = T, RedKnot$Subsite, na.rm = T))
# get names of subsites
subsites = names(table(RedKnot$Subsite))

# plot bars of counts by subsite, but do not plot axes  
barplot(subsite.n, las = 1, xlab = " ", ylab = " ", col = "grey", cex.lab = 1.7, cex.main = 1.5, axes = FALSE)
# add x axis with subsite labels
axis(1, c(0.8, 2, 3.2, 4.4, 5.6, 6.8, 8, 9.2, 10.4, 11.6, 12.8, 14), subsites, cex.axis = 1.2)
# add y axis
axis(2,cex.axis = 1.3, las = 1)
# label x axis
mtext("Subsite", side = 1, line = 2.5, cex = 1.5, font = 2)
# label y axis (side is x or y, line = distance from edge, cex is relative font size, font=2 is bold)
mtext("Count", side = 2, line = 3, cex = 1.5, font = 2)

###############
# plot densities of counts; color and split by year
ggplot(data=RedKnot,aes(x=Julian.date))+
  geom_density(aes(fill = factor(Year), alpha = 0.4)) +
  facet_wrap(~Year, ncol=2)+ #this is creating multiple "panels" for site
  scale_x_continuous(breaks = c(121,152,182,213,244,274),labels = c("May 1","June 1","July 1","August 1", "September 1", "October 1")) +
  #relabel y ticks##
  xlab("Date")+
  ylab("Red Knot Counts")

# plot densities of counts; split by year
ggplot(data=RedKnot,aes(x=Julian.date))+
  geom_density(aes(fill = Study.area, alpha = 0.4)) +
  facet_wrap(~Year, ncol=2)+ #this is creating multiple "panels" for site
  scale_x_continuous(breaks = c(121,152,182,213,244,274),labels = c("May 1","June 1","July 1","August 1", "September 1", "October 1")) +
  #relabel y ticks##
  xlab("Date")+
  ylab("Red Knot Counts")
