#Describe a response variable (something interested in predicting) in your dataset. Make a histogram of this variable. What distribution might you use to model your data?

#Draw from a random distribution of this type to create a pseudo-dataset. (How many times should you draw?) You can get an estimate of parameters using the “fitdistr” function in the MASS package. See example or ?”fitdistr”
#Plot your pseudo-data. Is it exactly the same? Why or why not?
#Using your pseudo-data, ask some reasonable questions about percentiles, and/or probability. What does the output tell you about your question?
#Remember to update your README and commit and push to GitHub once you have completed the assignment. Make sure to type your github repository name into canvas when you are done. 

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
#RedKnot$Habitat.birds.seen.in=as.factor(RedKnot$Habitat.birds.seen.in)

#total counts by habitat type, just to see what the data look like
Habitat.Totals <- aggregate(RedKnot$Number.of.birds.seen ~RedKnot$Habitat.birds.seen.in, FUN = sum) 

#histogram
hist(RedKnot$Number.of.birds.seen)
rk.nar <- na.omit(RedKnot$Number.of.birds.seen)

#take the mean of RedKnot$Number.of.birds.seen
mean(RedKnot$Number.of.birds.seen, trim = 0, na.rm = TRUE)

####figuring out parameters ####
set.seed(123)
nb.fit = fitdistr(rk.nar, densfun = "negative binomial", start = list(size = 243, mu = 10.30))
nb.fit$estimate
str(nb.fit)
?fitdistr
######Example

#Negative Binomial
?rnbinom
#here, mu is the mean, and size is the dispersion parameter (k)
#note - this is the preferred parameterization in ecology
x = rnbinom(243, size = 0.244, mu = 10.309)
hist(x)



##fitting a distribution to actual data
liz = read.csv("lizards.csv")
head(liz)
hist(liz$N)

library(MASS)
set.seed(123)
nb.fit = fitdistr(liz$N,densfun = "negative binomial" )
nb.fit

##if you need to supply start list
nb.fit = fitdistr(liz$N, densfun = "negative binomial", start = list(size = 1, mu = 15))


length(liz$N)

hist(rnbinom(23,size = 1.38, mu = 24.52))

new.liz = rnbinom(23,size = 1.38, mu = 24.52)
hist(new.liz)

#what is the 80 percentile of number of lizards perching?
qnbinom(p = .8, mu = 24.52, size = 1.38)

#what is the probability of exactly 1 lizard perching?
pnbinom(q=1,mu = 24.52, size = 1.38)
