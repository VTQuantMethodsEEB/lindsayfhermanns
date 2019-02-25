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
#install.packages("plotly")
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
RedKnot<-read.csv(file="RK_Shorebird_Count_2010.csv")

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

#Adjust Date column
RedKnot$Date=as.Date(RedKnot$Date, "%m/%d/%y")

###LET'S look at counts over time

ggplot(data=RedKnot,aes(x=Julian.date,y=Number.of.birds.seen, fill = Habitat.birds.seen.in))+
  geom_bar(stat = "identity")+
  facet_wrap(~Year, ncol=2)+ #this is creating multiple "panels" for site
scale_x_continuous(breaks = c(121,152,182,213,244,274),labels = c("May 1","June 1","July 1","August 1", "September 1", "October 1")) +
  xlab("Date")+
  ylab("Red Knot Counts")
###############  
  
 