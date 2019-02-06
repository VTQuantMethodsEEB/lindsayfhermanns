#clear workspace
rm(list=ls())

#read csv file in
RedKnot<-read.csv(file="RK_ShorebirdSurveys.csv",head=TRUE,sep=",")

names(RedKnot)
RedKnot$Date <- as.POSIXct(strptime(RedKnot$Date, '%Y-%m-%d', tz = 'EST'))
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

#still trying to plot 
sum.red.knots %>%
  arrange(Date)
ggplot(sum.red.knots, aes (Date, total)) + 
  geom_line(aes(group = Subsite), colour = "grey50") + 
  geom_point(aes(colour=Subsite))

sum.red.knots%>%
  arrange(Date)
g1=ggplot(data=sum.red.knots,aes(x=Date(Y),y=total))+
  geom_boxplot()+
  geom_point(aes(color=Subsite))



