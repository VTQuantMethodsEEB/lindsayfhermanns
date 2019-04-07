#clear workspace
rm(list=ls())
########################################
#Make a univariate linear model for one of your hypotheses
#Examine the assumptions of linearity (using tests or diagnostic plots)
#Plot the relationship in ggplot using stat_smooth
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
#install.packages("lubridate")
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
library(lubridate)

#Read in CSV files.
Habitat <-read.csv("subsite.attributes.csv")
RedKnot <-read.csv("REKN_J.data.2016.data.2014.2016.csv")

#changing RedKnot Subsite and column and to character
RedKnot$Subsite <- as.character(RedKnot$Subsite)
Habitat$Subsite <- as.character(Habitat$Subsite)

#Account for subsites that have been renamed or changed in RedKnot and Survey.Density files 
RedKnot$Subsite[RedKnot$Subsite == "HOOK"] <- "DMP"
RedKnot$Subsite[RedKnot$Subsite == "THL"] <- "DMP"
RedKnot$Subsite[RedKnot$Subsite == "HAZ"] <- "NBE"
RedKnot$Subsite[RedKnot$Subsite == "TOR"] <- "FS"
RedKnot$Subsite[RedKnot$Subsite == "NBO"] <- "NBW"
RedKnot$Subsite[RedKnot$Subsite == "MUS"] <- "FS"
RedKnot$Subsite[RedKnot$Subsite == "LFS"] <- "FS"
RedKnot$Subsite[RedKnot$Subsite == "BONS"] <- "CMF"
RedKnot$Subsite[RedKnot$Subsite == "MWPF"] <- "NMA"

Habitat$Subsite[Habitat$Subsite == "HOOK"] <- "DMP"
Habitat$Subsite[Habitat$Subsite == "THL"] <- "DMP"
Habitat$Subsite[Habitat$Subsite == "HAZ"] <- "NBE"
Habitat$Subsite[Habitat$Subsite == "TOR"] <- "FS"
Habitat$Subsite[Habitat$Subsite == "NBO"] <- "NBW"
Habitat$Subsite[Habitat$Subsite == "MUS"] <- "FS"
Habitat$Subsite[Habitat$Subsite == "LFS"] <- "FS"
Habitat$Subsite[Habitat$Subsite == "BONS"] <- "CMF"
Habitat$Subsite[Habitat$Subsite == "MWPF"] <- "NMA"

###Add column of subsite length
###Write function in R to say where habitat column AND subsite column is = to x and x, populate column "length" with "y"
##Length values pulled from habitat.attributes CSV/google earth measure tool
RedKnot["Habitat.length"] <- NA
#RedKnot["Date"] <- as.Date(NA)

##Modifying Habitat to match data set attribute## 
RedKnot[which(RedKnot$Subsite == "CSB"),7] = "Cupsogue Flood Shoals"
RedKnot[which(RedKnot$Subsite == "FS"),7] = "Old Inlet Flood Shoals"

###Add column of subsite length
RedKnot["Habitat.length"] <- NA
###Write function in R to say where habitat column AND subsite column is = to x and x, populate column "length" with "y"
##Length values pulled from habitat.attributes CSV/google earth measure tool

RedKnot[which(RedKnot$Subsite == "CSB" & RedKnot$Habitat == "Cupsogue Flood Shoals"),9] = 764.59
RedKnot[which(RedKnot$Subsite ==  "OIW" & RedKnot$Habitat == "BOTH"),9] = 1278.0264
RedKnot[which(RedKnot$Subsite ==  "CMF"),9] = 774.43
RedKnot[which(RedKnot$Subsite == "CSE" & RedKnot$Habitat == "Bay"),9] = 906.78
RedKnot[which(RedKnot$Subsite == "CSW" & RedKnot$Habitat == "Bay"),9] = 427.6344
RedKnot[which(RedKnot$Subsite == "CSW" & RedKnot$Habitat == "BOTH"),9] = 1164.6408
RedKnot[which(RedKnot$Subsite == "FS"),9] = 1052.34
RedKnot[which(RedKnot$Subsite == "CMF"),9] = 774.4
RedKnot[which(RedKnot$Subsite == "NBE" & RedKnot$Habitat == "Backshore"),9] = 803.7576
RedKnot[which(RedKnot$Subsite == "NMA" & RedKnot$Habitat == "Bay"),9] = 165.5064
RedKnot[which(RedKnot$Subsite == "OIE" & RedKnot$Habitat == "Ocean"),9] = 1217.371
RedKnot[which(RedKnot$Subsite == "OIW" & RedKnot$Habitat == "Bay"),9] = 674.8272
RedKnot[which(RedKnot$Subsite == "OIW" & RedKnot$Habitat == "BOTH"),9] = 1278.0264
RedKnot[which(RedKnot$Subsite == "PSO" & RedKnot$Habitat == "Bay"),9] = 1876.044
RedKnot[which(RedKnot$Subsite == "PSO" & RedKnot$Habitat == "Ocean"),9] = 1036.015
RedKnot[which(RedKnot$Subsite == "SPTE" & RedKnot$Habitat == "Ocean"),9] = 1850.136
RedKnot[which(RedKnot$Subsite == "SPTE" & RedKnot$Habitat == "BOTH"),9] = 3831.0312
RedKnot[which(RedKnot$Subsite == "SPTE" & RedKnot$Habitat == "Backshore"),9] = 1850.136


##utility function for pretty printing (thanks Kate this is handy)
pr <- function(m) printCoefmat(coef(summary(m)),
                               digits=3,signif.stars=FALSE)

## Simple models
head(RedKnot)
RedKnot$Year <- as.factor(RedKnot$Year)
RedKnot$Year

#examining Total count of red knot by Habitat Type.
#H0: there are not any significant effects of habitat on REKN totals
#H1: there are sig effects of Hab length on totals.
lm1 <- lm(Total~Habitat, data = RedKnot)
summary(lm1)

pr(lm1 <- lm(Total~Habitat, data = RedKnot))

pr(lm0 <- lm(Total~Habitat-1,data=RedKnot))
#now, these models are the values of the variables rather than the differences
##How should I interpret my output?
##Predict
#my go to:
predict(lm1,newdata=data.frame(Habitat=c("Bay", "Backshore", "Ocean", "Cupsogue Flood Shoals", "Old Inlet Flood Shoals")),
        interval="confidence")
plot(lm1)
#install.packages("emmeans")
#####CONFUSED about what all this is doing to my data####
library(effects)
summary(allEffects(lm1))
library(multcomp)
library(emmeans)
emmeans(lm1,specs = ~Habitat)

#plot the effects.....This is rad##
plot(allEffects(lm1))

#Now, looking at survey effect (essentially, time of year) on total.
pr(lm3 <- lm(Total~Survey,data=RedKnot))
plot(allEffects(lm3))

#can you say then, there is a positive relationship between survey and total? Later in the year, higher counts?

#plot the data
library("ggplot2"); theme_set(theme_bw()+
                                theme(panel.spacing=grid::unit(0,"lines")))

ggplot(RedKnot,aes(x=Habitat,y=Total))+
  geom_boxplot(fill="lightgray")+
  facet_wrap(~Year,scale="free_x",nrow=1)+
  geom_hline(yintercept=mean(RedKnot$Total),colour="red",lwd=1,alpha=0.4) #line is at mean number of redknot per survey

## Interactive Models ##
##Examining if total is effected by habitat and year
Redknot <- mutate(RedKnot,
                  Year=factor(Year,
                              levels=c("2014","2015","2016")))
pr(lmRK2 <- lm(Total~Habitat*Year,data=RedKnot))

## Releveling interactive models, to the Year
RedKnot$Year <- relevel(RedKnot$Year, ref = "2016" )
pr(lmRK2 <- lm(Total~Habitat*Year,data=RedKnot))

## Using lsmeans
# we can add the interaction for all pairwise comparisons
lsm2<-emmeans(lmRK2,pairwise~Habitat*Year)
lsm2

#get back to earlier levels
RedKnot <- mutate(RedKnot,
                  Year=factor(Year,
                              levels=c("2014","2015","2016")))

##plotting the interactive model

pp <- with(Redknot,
           expand.grid(year=levels(time),habitat=levels(habitat)))

pp2 <- pp
pp2$grahami <- predict(lmTL2,newdata=pp2)


ggplot(pp2,aes(x=time,y=grahami,colour=light))+geom_point()+
  geom_line(aes(group=light))

###ADD RAW data to plot###
###ALWAYS ADD RAW DATA!###
ggplot(pp2,aes(x=time,y=grahami,colour=light))+ #pp2 is the dataframe with our predictions
  geom_point(size=4)+ #the large solid points show our predictions
  geom_line(aes(group=light))+ #the lines connect our predictions
  geom_point(data=lizards, aes(x=time,y=grahami,colour = light), shape=1) #this shows our RAW data
#lizards contains our raw data frame and we plot that on top on the predictions in open circles
# we specify open circles with "shape = 1"

##more graphics

lizards <- mutate(lizards,
                  time=factor(time,
                              levels=c("early","midday","late")))

pr(lmTL2 <- lm(grahami~time*light,data=lizards))

plot(allEffects(lmTL2))

####Start here####
####Get total counts by survey period####
####Get total counts per year~survey period ~ hab####
####Get total counts per year~survey period ~subsite~habitat####
####Get density by survey period####
####Get density per year~survey period ~ hab####
####Get density per year~survey period ~subsite~habitat####
