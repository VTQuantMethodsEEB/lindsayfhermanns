#clear workspace
rm(list=ls())
########################################
# Testing if there are significant differences between REKN counts on the bay habitat vs. counts on the ocean.
#Null= There is no significant difference between bay and ocean counts.
#H1=There is a significant difference between bay and ocean counts.
#H2=ANOVA to test if there is a significant difference between counts at all habitat types.
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

#read csv file in
RedKnot<-read.csv(file="RK_Shorebird_Count_2010.csv")
Habitat <-read.csv("subsite.attributes.csv")

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

######Getting total counts per year, subsite, habitat type.######
##Defining SE function##
std <- function(x) sd(x)/sqrt(length(x))

#total year counts per year as data frame year count
year.count <- data.frame(as.numeric(rowsum(RedKnot$Number.of.birds.seen, reorder = T, RedKnot$Year, na.rm = T)))
year <- c("2014", "2015", "2016")

#total count per year
year.count1 <- RedKnot%>%
  group_by(Year) %>% 
  summarize(Number.of.birds.seen = sum(Number.of.birds.seen, na.rm = T))
StandErr <- std(year.count1$Number.of.birds.seen)

#total count per habitat
habitat.count1 <- RedKnot%>%
  group_by(Habitat.birds.seen.in)%>%
  summarize(Number.of.birds.seen = sum(Number.of.birds.seen, na.rm = T))

#total count per subsite
subsite.count <- data.frame(rowsum(RedKnot$Number.of.birds.seen, reorder = T, RedKnot$Subsite, na.rm = T, ~RedKnot$Year))

#total count per subsite per year
year.subsite.count <- RedKnot%>%
  group_by(Year, Subsite) %>% 
  summarize(Number.of.birds.seen = sum(Number.of.birds.seen, na.rm = T))

#total count per habitat per year
year.habitat.count <- RedKnot%>%
  group_by(Habitat.birds.seen.in, Year) %>% 
  summarize(Number.of.birds.seen = sum(Number.of.birds.seen, na.rm = T))

#examining SPTE to determine what habitat/why high count through years
SPTE.Hab <- filter(RedKnot, Subsite == "SPTE")

####Stats Summary for Red Knots on FI####
mean <- mean(year.count1$Number.of.birds.seen)
std <- function(x) sd(x)/sqrt(length(x))
StandErr <- std(year.count1$Number.of.birds.seen)

#Finding peak day(s) of red knot abundance on FI
jdate.peak.count <- RedKnot%>%
  group_by(Julian.date)%>%
  summarize(Number.of.birds.seen = sum(Number.of.birds.seen, na.rm = T))
  
