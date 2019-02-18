---
title: "README"
output: github_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

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
Using file RK Shorebird Surveys. 
IN PROGRESS -- Plotting data! 

###WEEK 4###