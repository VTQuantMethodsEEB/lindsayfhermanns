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
#library call
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

#Adjust Date column
RedKnot$Date=as.Date(RedKnot$Date, "%m/%d/%y")

#Adjust Number.of.birds.seen to numeric, drop NAs
RedKnot$Number.of.birds.seen=as.numeric(RedKnot$Number.of.birds.seen, na.rm = T)
#Adjust Habitat.birds.seen.in to factor
RedKnot$Habitat.birds.seen.in=as.factor(RedKnot$Habitat.birds.seen.in)

#total counts by habitat type, just to see what the data look like
Habitat.Totals <- aggregate(RedKnot$Number.of.birds.seen ~RedKnot$Habitat.birds.seen.in, FUN = sum) 

#Creating seperate frame for permutation test
Counts <- data.frame(RedKnot$Habitat.birds.seen.in, RedKnot$Number.of.birds.seen)
na.omit(Counts, cols=2)
#need to make dataframe to run for loops. Creating dataframe where each count occurence is seperated by habitat type
#assigning second column of data (counts) to habitat dataframes
mudflat <- Counts[which(Counts$RedKnot.Habitat.birds.seen.in=="mudflat"),2]
ocean <- Counts[which(Counts$RedKnot.Habitat.birds.seen.in=="ocean"),2]
sandflat <- Counts[which(Counts$RedKnot.Habitat.birds.seen.in=="sandflat"),2]
bay <- Counts[which(Counts$RedKnot.Habitat.birds.seen.in=="bay"),2]
backshore <- Counts[which(Counts$RedKnot.Habitat.birds.seen.in=="backshore"),2]
bay.backshore <- Counts[which(Counts$RedKnot.Habitat.birds.seen.in=="bay_backshore"),2] 

#remove NA from dataframes
mudflat <- na.omit(mudflat)
backshore <- na.omit(backshore)
bay <- na.omit(bay)
bay.backshore <- na.omit(bay.backshore)
ocean <- na.omit(ocean)
sandflat <- na.omit(sandflat)

#what is the mean difference without permutation?
mean(bay) - mean(ocean)

########################################
##Permutation## 
#set.seed will set R's random number generator to start at the same place
#this ensures that when you, and I, and anyone else, does the test, we will all get the same results
set.seed(101)

## set aside space for results
res <- NA 

for (i in 1:1000) {
  habitatboot <- sample(c(bay,ocean)) ## scramble
  ## pick out bay and ocean samples
  bayboot <- habitatboot[1:length(bay)] 
  oceanboot <- habitatboot[(length(ocean)+1):length(habitatboot)] 
  ## compute & store difference in means
  res[i] <- mean(bayboot)-mean(oceanboot) #calculate the difference in the bay means and the ocean means
  #[i] says "where i", and i is a counter, after running this loop, i should be 1000
}

#what is our observed mean difference?
obs <- mean(bay)-mean(ocean)

#histogram of differences
hist(res,col="gray",las=1,main="")
abline(v=obs,col="red")

##Obtain p value
res[res>=obs]
length(res[res>=obs])
180/1000
mean(res>=obs)        
#if our p value is significant at p< 0.05,and our p value is 0.18, we can fail to reject H0 an conclude there are no significant differences between counts at bay vs counts at ocean. 

###############################################
#T test for ocean vs mudflat counts
###############################################

names(Counts)

#Only can perform t-test on 2 variables, lets examine mudflat counts compared to ocean counts; using welches as data is normalized but not equal variance..
#H0=There is not a significant difference between counts at the mudflat and counts at the ocean. 
#H1=There is a siginificant difference in counts of REKN at the mudflat and counts at the ocean.
#creating df where only mudflats and ocean habitats are considered from df "Counts"
ttrekn <- Counts[Counts$RedKnot.Habitat.birds.seen.in == "mudflat" | Counts$RedKnot.Habitat.birds.seen.in == "ocean", ]
na.omit(ttrekn)
tt <- t.test(RedKnot.Number.of.birds.seen~RedKnot.Habitat.birds.seen.in, data = ttrekn)
tt

#data:  RedKnot.Number.of.birds.seen by RedKnot.Habitat.birds.seen.in
#t = 3.2286, df = 92.177, p-value = 0.001724
##So we conclude that we can reject the null hypothesis and that there is significant difference in the mean REKN counts between ocean and mudflats.

