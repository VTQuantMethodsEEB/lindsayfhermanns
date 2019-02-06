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


