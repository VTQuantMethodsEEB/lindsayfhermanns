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
#changing Red Knot Habitat.birds.seen.in to character
RedKnot$Habitat.birds.seen.in <- as.character(RedKnot$Habitat.birds.seen.in)
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

#Relabel habitat types
RedKnot$Habitat.birds.seen.in[RedKnot$Habitat.birds.seen.in == "backshore"] <- "Backshore"
RedKnot$Habitat.birds.seen.in[RedKnot$Habitat.birds.seen.in == "bay"] <- "Bay"
RedKnot$Habitat.birds.seen.in[RedKnot$Habitat.birds.seen.in == "bay_backshore"] <- "Bay/Backshore"
RedKnot$Habitat.birds.seen.in[RedKnot$Habitat.birds.seen.in == "ocean"] <- "Ocean"
RedKnot$Habitat.birds.seen.in[RedKnot$Habitat.birds.seen.in == "mudflat"] <- "Mudflat"
RedKnot$Habitat.birds.seen.in[RedKnot$Habitat.birds.seen.in == "sandflat"] <- "Sandflat"

#Adjust Date column
RedKnot$Date=as.Date(RedKnot$Date, "%m/%d/%y")

##################################################
# GGPLOT Count and Density Plots 
##################################################

#Plotting counts by habitat
ggplot(data=RedKnot,aes(x=Julian.date,y=Number.of.birds.seen, fill = Habitat.birds.seen.in))+
  geom_bar(stat = "identity")+
  facet_wrap(~Year, ncol=2)+ #this is creating multiple "panels" for site
scale_x_continuous(breaks = c(121,152,182,213,244,274),labels = c("May 1","June 1","July 1","August 1", "September 1", "October 1")) +
  xlab("Date")+
  ylab("Red Knot Counts")

#Plotting density by habitat
#chose bin widths (7 is by week)
bin <- 7

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
