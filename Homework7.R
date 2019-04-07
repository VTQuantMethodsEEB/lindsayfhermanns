####Make a univariate linear model for one of your hypotheses
#Examine the assumptions of linearity (using tests or diagnostic plots)
#Plot the relationship in ggplot using stat_smooth
#You can hold off on submitting this assignment (which will be done via github) until after next weeks assignment
#Remember to update your README file - we will combine this week and next week so you can indicate that in your file

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

#read csv file in
RedKnot<-read.csv(file="RK_Shorebird_Count_2010.csv")
Habitat <-read.csv("subsite.attributes.csv")
Survey.Density <- read.csv("Habitat.Densities.csv")
Dates.Survey <- read.csv("Shorebird_Survey_Form.csv")

#changing RedKnot Subsite and column and to character
RedKnot$Subsite <- as.character(RedKnot$Subsite)
Survey.Density$Subsite <- as.character(Survey.Density$Subsite)
#changing Red.Knot Habitat.birds.seen.in to character
RedKnot$Habitat.birds.seen.in <- as.character(RedKnot$Habitat.birds.seen.in)
Survey.Density$Habitat <- as.character(Survey.Density$Habitat)

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

Survey.Density$Subsite[Survey.Density$Subsite == "HOOK"] <- "DMP"
Survey.Density$Subsite[Survey.Density$Subsite == "THL"] <- "DMP"
Survey.Density$Subsite[Survey.Density$Subsite == "HAZ"] <- "NBE"
Survey.Density$Subsite[Survey.Density$Subsite == "TOR"] <- "FS"
Survey.Density$Subsite[Survey.Density$Subsite == "NBO"] <- "NBW"
Survey.Density$Subsite[Survey.Density$Subsite == "MUS"] <- "FS"
Survey.Density$Subsite[Survey.Density$Subsite == "LFS"] <- "FS"
Survey.Density$Subsite[Survey.Density$Subsite == "BONS"] <- "CMF"
Survey.Density$Subsite[Survey.Density$Subsite == "MWPF"] <- "NMA"

#H0 there is 
#univariate linear model exploring relationship between habitat and red knot count
l1 <- lm(Number.of.birds.seen~Julian.date, data = RedKnot)
lm(Number.of.birds.seen~Julian.date, data = RedKnot)
summary(l1)
plot(l1)

hist(resid(l1))
shapiro.test(resid(l1))

##log transform?
#Data is not normally distributed, because of count data?
#Trying with count of REKN by sub

###Anova###
l2 = aov(Number.of.birds.seen~Habitat.birds.seen.in, data = RedKnot);summary(l2)
#Is this showing that there is no significant effect of habitat on #of REKN observed
TukeyHSD(l2)

##Plotting number of birds seen by date, continuous variable##
library(ggplot2)
r=ggplot(data=RedKnot, aes(x=Julian.date, y=Number.of.birds.seen))+ 
  geom_point()+
  stat_smooth(method = "lm")+
  theme_bw() + 
  theme(axis.title=element_text(size=20),axis.text=element_text(size=10),panel.grid = element_blank(), axis.line=element_line(),legend.position="top",legend.title=element_blank())
print(r)


###Part 2###
#H0 No significant differences of habitat type and habitat length on number of red knots
#H1 there is a significant difference of habitat type and habitat length on number of red knots
#H2 there is a significant difference of habitat type and 






#####PART 2

###1. Make a linear model (with more than one variable) for one of your hypotheses. Articulate which hypothesis you are testing.

###2. Use an interactive model and an additive model. Explain what hypothesis each of these is testing, and what the R output is telling you about your data.
#(Hint: you can use lsmeans, effects, relevel, or predict to help you.) You should include this explanation in either your README or in your code.

###3. Plot your model (e.g. using predict) and overlay the model on top of the underlying data. See code for example to plot both model and data.