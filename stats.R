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

#total year counts per year as data frame year count
year.count <- data.frame(as.numeric(rowsum(RedKnot$Number.of.birds.seen, reorder = T, RedKnot$Year, na.rm = T)))
#total mean of all counts over years
mean(year.count$as.numeric.rowsum.RedKnot.Number.of.birds.seen..reorder...T..)
#total count per subsite
subsite.count <- data.frame(rowsum(RedKnot$Number.of.birds.seen, reorder = T, RedKnot$Subsite, na.rm = T, ~RedKnot$Year))
#total count per subsite per year

year.subsite.count <- RedKnot%>%
  group_by(Year, Subsite)                            # multiple group columns


left_join(RedKnot)