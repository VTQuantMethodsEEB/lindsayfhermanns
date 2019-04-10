#clear workspace
rm(list=ls())

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
install.packages("plotly")
#library call
library(ggplot2)
library(tidyverse)
library(dplyr)
library(magrittr)
library(reshape)
library(reshape2)
library(gridExtra)
library(knitr)
library(officer)
library(plotly)

#read csv file in
RedKnot<-read.csv(file="RK_ShorebirdSurveys.csv",head=TRUE,sep=",")

#Adjusting date column
RedKnot$Date <- as.POSIXct(RedKnot$Date, tz = 'EST', '%m/%d/%Y')

#basic calls and calcs for data
unique(RedKnot$Subsite)
#str(RedKnot)
#head(RedKnot)
#tail(RedKnot)
#log10(RedKnot$Unbanded)
#dim(RedKnot)
#names(RedKnot)

#tallying number of REKN seen each date surveyed
#RedKnot$total = RedKnot$Ocean+RedKnot$BayBackshore
RedKnot$total = rowSums(RedKnot[,c("Ocean","BayBackshore")],na.rm=T)

#changing Red Knot Subsite to character
RedKnot$Subsite <- as.character(RedKnot$Subsite)
#Account for subsites that have been renamed or changed
RedKnot$Subsite[RedKnot$Subsite == "HOOK"] <- "DMP"
RedKnot$Subsite[RedKnot$Subsite == "THL"] <- "DMP"
RedKnot$Subsite[RedKnot$Subsite == "HAZ"] <- "NBE"
RedKnot$Subsite[RedKnot$Subsite == "TOR"] <- "FS"
RedKnot$Subsite[RedKnot$Subsite == "NBO"] <- "NBW"
RedKnot$Subsite[RedKnot$Subsite == "MUS"] <- "FS"
RedKnot$Subsite[RedKnot$Subsite == "LFS"] <- "FS"
RedKnot$Subsite[RedKnot$Subsite == "BONS"] <- "CMF"
RedKnot$Subsite[RedKnot$Subsite == "MWPF"] <- "NMA"

#row sum script
#rowSums(dat[,c("b", "c")], na.rm=TRUE)

#changing RedKnot data to lowercase redknot
#redknot=RedKnot
#View(redknot)

##want to know how many redknots for each date per subsite
sum.red.knots = aggregate(total~Subsite+Date, FUN=sum, data=RedKnot)

#sum.red.knots
#View(sum.red.knots)
#plot(sum.red.knots)

#spreading data by subsite
#REKN.wide<-spread(sum.red.knots, Subsite, total, convert = T)
#View(REKN.wide)
#plot(REKN.wide)

#Mean of REKN at each subsite throughout the study period 
sum.red.knots %>%
  group_by(Subsite) %>%
  summarise(total=mean(total, na.rm = TRUE))

sum.red.knots$Date <- as.Date(sum.red.knots$Date, "%y%m%d")

#Rearranging data to be consectutive
sum.red.knots %>%
  group_by(Date=2013)%>%
  arrange(Date)

#making year column
sum.red.knots$year <- substr(sum.red.knots$Date, 1,4)
sum.red.knots$year <- as.factor(sum.red.knots$year)

#creating Julian date column
sum.red.knots$jdate <- as.numeric(format(sum.red.knots$Date, "%j"))

#Plotting #redknots per month per year by subsite to examine trends and patterns. 
#Had to change to Julian date, then split each year by facet wrap, added new tick marks and labels.
#Also would like to change the x and y axis labels AND is there a way to change the aesthetics so points are simply black, and line colors represent different subsites
ggplot(sum.red.knots, aes(jdate, total)) + 
  geom_line(aes(group = Subsite), colour = "grey50") + 
  geom_point(aes(colour = Subsite)) +
  scale_x_continuous(breaks = c(121,152,182,213,244,274),labels = c("May 1","June 1","July 1","August 1", "September 1", "October 1")) +
  facet_wrap(~ year, ncol=2) +
  xlab("Date")+
  ylab("Red Knot Counts")
#this is a bit hard to see - logging might make easier!

#Population data may be better represented by box plot##in progress##
ggplot(sum.red.knots, aes(jdate, total))+
  

scale_x_continuous(breaks = c(121,152,182,213,244,274),labels = c("May 1","June 1","July 1","August 1", "September 1", "October 1")) +
  facet_wrap(~ year, ncol=2) +
  xlab("Date")+
  ylab("Red Knot Counts")

#should this show something?

####Now to plot REKN seen on BaySide vs OceanSide habitats
####want to know how many redknots for each date per habitat

hab.red.knots = aggregate(total~BayBackshore+Ocean+Date, FUN=sum, data=RedKnot)

####Rearranging data to be consectutive
hab.red.knots %>%
  group_by(Date=2013)%>%
  arrange(Date)

#making year column
hab.red.knots$year <- substr(hab.red.knots$Date, 1,4)
hab.red.knots$year <- as.factor(hab.red.knots$year)

#creating Julian date column
hab.red.knots$jdate <- as.numeric(format(hab.red.knots$Date, "%j"))  

#Need to create 2 different plots; one showing ocean counts and one showing bay counts
#I want to see two lines on one graph...not two seperate ones! Also thinking I would like to show histogram instead of line graph to represent the data better. Thoughts?
ggplot(hab.red.knots) + 
  geom_line(data=hab.red.knots, aes(x=jdate, y=Ocean, color='Ocean')) +
  geom_line(data=hab.red.knots, aes(x=jdate, y=BayBackshore, color='BayBackshore')) +
  scale_x_continuous(breaks = c(121,152,182,213,244,274),labels = c("May 1","June 1","July 1","August 1", "September 1", "October 1")) +
  facet_wrap(~ year, ncol=2) +
  xlab("Date")+
  ylab("Red Knot Counts") +
  labs(colour="Habitat")


##create ocean dataframe
names(RedKnot)
bay = RedKnot[-13]
ocean = RedKnot[-14]
dim(bay)
dim(ocean)
bay$habitat = "BayBackshore"
ocean$habitat = "Ocean"
names(ocean[13]) = "Count"
names(bay[13]) = "Count"

RedKnot2 = rbind(bay,ocean)

#create a new column with lubridate, pull out year from column date, then facet_wrap by year free_x to remove year axis title.
#library(lubridate)
#sum.red.knots$year = year(sum.red.knots$Date)
#sum.red.knots$month = month(sum.red.knots$Date)
#sum.red.knots$day = day(sum.red.knots$Date)
#partial.days =  sum.red.knots$day/31
#sum.red.knots$mday = sum.red.knots$month + partial.days
