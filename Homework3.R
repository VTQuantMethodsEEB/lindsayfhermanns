#clear workspace
rm(list=ls())

#read csv file in
RedKnot<-read.csv(file="RK_ShorebirdSurveys.csv",head=TRUE,sep=",")

names(RedKnot)
RedKnot$Date <- as.POSIXct(RedKnot$Date, tz = 'EST', '%m/%d/%Y')
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

#tallying number of REKN seen each date surveyed
RedKnot$total = RedKnot$Ocean+RedKnot$BayBackshore
RedKnot$total = rowSums(RedKnot[,c("Ocean","BayBackshore")],na.rm=T)

#row sum script
rowSums(dat[,c("b", "c")], na.rm=TRUE)

#changing RedKnot data to lowercase redknot
redknot=RedKnot
View(redknot)
Dates <- as.Date(redknot$Date, "%y%m%d")

##want to know how many redknots for each date per subsite
sum.red.knots = aggregate(total~Subsite+Date, FUN=sum, data=RedKnot)
sum.red.knots
View(sum.red.knots)
plot(sum.red.knots)

#spreading data by subsite
REKN.wide<-spread(sum.red.knots, Subsite, total, convert = T)
View(REKN.wide)
plot(REKN.wide)

#Mean of REKN at each subsite throughout the study period
sum.red.knots %>%
  group_by(Subsite) %>%
  summarise(total=mean(total, na.rm = TRUE))

#Plotting total REKN counts on the y by date on the x.
sum.red.knots %>%
  group_by(Date=2013)%>%
arrange(Date)

#wanting to take the lines out or, just have them per year rather than throughout all study years
sum.red.knots %>%
  arrange(Date)
ggplot(sum.red.knots, aes (Date, total)) + 
  geom_line(aes(group = Subsite), colour = "grey50") + 
  geom_point(aes(colour=Subsite))

#create a new column with lubridate, pull out year from column date, then facet_wrap by year free_x to remove year axis title.
library(lubridate)
sum.red.knots$year = year(sum.red.knots$Date)
sum.red.knots$month = month(sum.red.knots$Date)
sum.red.knots$day = day(sum.red.knots$Date)
partial.days =  sum.red.knots$day/31
sum.red.knots$mday = sum.red.knots$month + partial.days

ggplot(sum.red.knots, aes (mday, total)) +
  facet_wrap(~year)+
  geom_line(aes(group = Subsite), colour = "grey50") + 
  geom_point(aes(colour=Subsite))

#wanting to take the lines out or, just have them per year rather than throughout all study years
sum.red.knots %>%
  arrange(Date)
ggplot(sum.red.knots, aes (mday, total)) + 
  geom_line(aes(group = Subsite), colour = "grey50") + 
  geom_point(aes(colour=Subsite))






