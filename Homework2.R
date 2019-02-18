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

  