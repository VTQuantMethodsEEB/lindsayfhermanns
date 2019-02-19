---
title: "README"
output: github_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

###FROM KEL: See updated handout and instructions for README. This file should just contain a week by week log of what files you used and generally what you did. You don't need to include any code. Can you update this with those changes? Thanks!

## Red Knot Abundance on Fire Island

This is a project examining if there are seasonal patterns within the rufa subspecies of red knot, and if there is preference for red knots using habitat, on Fire Island (bayside habitat vs. oceanside habitat). I would also like to use resight data to describe mean stopover time at Fire Island. Data was collected from Virginia Tech Shorebird lab 2013-2016 by conducting transect surveys and counting different observed shorebird species. Resight data was collected by Virginia Tech Shorebird lab from 2013-2018.

####WEEK 1####
Basic R commands, library calls, functions, data import. Read csv file in and manipulated some data.
Using file RK Shorebird Surveys. 

#read csv file in
RedKnot<-read.csv(file="RK_ShorebirdSurveys.csv",head=TRUE,sep=",")

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

#view data
RedKnot

#summarize data
summary(RedKnot)

#calculations with data using $ to call out column
RedKnot$Unbanded2<-2*(RedKnot$Unbanded)
                 
#aggregate / mean of Unbanded Redknots at each subsite (?) 
RedKnot %>%
  subset(Unbanded > 10) %>%
  aggregate(. ~ Subsite, ., FUN=mean)

#Tried to make a table of all RedKnots seen at Subsites
tab1 <- data.frame(RedKnot$Subsite,RedKnot$Date, RedKnot$Banded.Unknown+RedKnot$Unbanded+RedKnot$Unknown)
tab1$Total<-1*(tab1$RedKnot.Banded.Unknown...RedKnot.Unbanded...RedKnot.Unknown)                   

#And wanted to see a summary of it...wonder if it can be plotted too?
summary(tab1)
plot(tab1)

#Plot the table of Redknots seen by date but it's not plotted consecutively...I just wanted to play around with it
plot(RedKnot$Date, tab1$Total, type = "l", color = "dodgerblue4")

###WEEK 2###
Using file RK Shorebird Surveys. 
Mutate/aggregate/summarize, calculated redknot counts/subsite. Trying to arrange by date and plot by year to look at seasonal peaks and determine high peak count per year.


#clear workspace
rm(list=ls())
#read csv file in
RedKnot<-read.csv(file="RK_ShorebirdSurveys.csv",head=TRUE,sep=",")

names(RedKnot)
#reformatting date as YEAR MONTH DAY.(Isn't working for some reason, taking out this chunk)
#RedKnot$Date <- as.POSIXct(strptime(RedKnot$Date, '%Y-%m-%d', tz = 'EST'))

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

#basic calls and calcs for data
unique(RedKnot$Subsite)
str(RedKnot)
head(RedKnot)
tail(RedKnot)
log10(RedKnot$Unbanded)
dim(RedKnot)

#tallying number of REKN seen each date surveyed, row sum script example
#rowSums(dat[,c("b", "c")], na.rm=TRUE)
RedKnot$total = rowSums(RedKnot[,c("Ocean","BayBackshore")],na.rm=T)

##want to know how many redknots for each date per subsite
sum.red.knots = aggregate(total~Subsite+Date, FUN=sum, data=RedKnot)

View(sum.red.knots)

#spreading data by subsite
REKN.wide<-spread(sum.red.knots, Subsite, total, convert = T)
View(REKN.wide)

#Mean of REKN at each subsite throughout the study period, summarize creates new dataframe
  mean.by.subsite = sum.red.knots %>%
  group_by(Subsite) %>%
    summarise(total=mean(total, na.rm = TRUE))
  
#Mutate to add column and create new dataframe
  total.mean.bay = RedKnot %>%
    group_by(Subsite, Date, BayBackshore) %>%
    mutate(total.mean.bay=mean(BayBackshore, na.rm = TRUE))

###WEEK 3###
Using file "RK_ShorebirdSurveys"", code=Homework3.R
Plotted 2 graphs showing 1) the counts of REKN throughout each year by Subsite, and 2) the counts of REKN throughout each year by habitat.
I had some help from my lab to parse out problems with Date columns...we converted Date to Julian Date to work around the whole continuous problem...I think I would like to change line graphs for histograms I think? If that would portray the data better...

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

#Account for subsites that have been renamed or changed
RedKnot$Subsite[RedKnot$Subsite == "HOOK"] <- "DMP"
RedKnot$Subsite[RedKnot$Subsite == "THL"] <- "DMP"
RedKnot$Subsite[RedKnot$Subsite == "HAZ"] <- "NBE"
RedKnot$Subsite[RedKnot$Subsite == "TOR"] <- "FS"
RedKnot$Subsite[RedKnot$Subsite == "NBO"] <- "NBW"
RedKnot$Subsite[RedKnot$Subsite == "MUS"] <- "FS"
RedKnot$Subsite[RedKnot$Subsite == "LFS"] <- "FS"
RedKnot$Subsite[RedKnot$Subsite == "BONS"] <- "CMF"
#RedKnot$Subsite[RedKnot$Subsite == "MWPF"] <- "NMA" ***NOT RUNNING***

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
  
#Now to plot REKN seen on BaySide vs OceanSide habitats
##want to know how many redknots for each date per habitat

hab.red.knots = aggregate(total~BayBackshore+Ocean+Date, FUN=sum, data=RedKnot)

#Rearranging data to be consectutive
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
    

###WEEK 4###