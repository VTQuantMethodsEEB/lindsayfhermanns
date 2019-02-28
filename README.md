---
title: "README"
output: github_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```


## Red Knot Abundance on Fire Island

This is a project examining if there are seasonal patterns within the rufa subspecies of red knot, and if there is preference for red knots using habitat, on Fire Island (bayside habitat vs. oceanside habitat). I would also like to use resight data to describe mean stopover time at Fire Island. Data was collected from Virginia Tech Shorebird lab 2013-2016 by conducting transect surveys and counting different observed shorebird species. Resight data was collected by Virginia Tech Shorebird lab from 2013-2018.

####WEEK 1####
Basic R commands, library calls, functions, data import. Read csv file in and manipulated some data.
Using file RK Shorebird Surveys. 


###WEEK 2###
Using file RK Shorebird Surveys. 
Mutate/aggregate/summarize, calculated redknot counts/subsite. Trying to arrange by date and plot by year to look at seasonal peaks and determine high peak count per year.

###WEEK 3###
Using file "RK_ShorebirdSurveys"", code=Homework3.R
Plotted 2 graphs showing 1) the counts of REKN throughout each year by Subsite, and 2) the counts of REKN throughout each year by habitat.
I had some help from my lab to parse out problems with Date columns...we converted Date to Julian Date to work around the whole continuous problem...I think I would like to change line graphs for histograms I think? If that would portray the data better...

###WEEK 4###
#Statistical Philosophy and Hypotheses about data#
I hypothesize that there is a significant pattern within red knot abundance on Fire and Westhampton Islands both among and between years, and that the data will indicate a peak migration time. I also hypothesize that red knots prefer one type of habitat over others during this peak migration time, and think that they may prefer bayside habitats over ocean side habitats (more red knots will be counted in bayside habitats vs. ocean side habitats over the course of data collection, May-September, 2013-2016).
Seasonal patterns can be tested through stating the mean high counts between years, and, the two high peaks of counts throughout each year. I don’t know that there is a test to run for this specifically. I would most likely use ANOVA 2 variance test, or, a paired T-test, to determine habitat preference each year, and compared among years. 
I’m not certain, as my statistics background isn’t strong, what other statistical tests I could do in order to distinguish significance in habitat preference, but I would be interested in learning more about Bayesian approaches and applying that, if I could and in addition to frequentist methods, to ensure the data was being portrayed in the best way (most real way?) possible. I don’t see why there is anything wrong with testing your data in multiple ways, if that is a viable option. I view statistics as a tool to make your data make the most sense, not necessarily based solely off finding significant p-value results.

###Week 5###
Using file "RK_Shorebird_Count_2010.csv", code=Week 5 Homework.R
Testing two hypotheses on data, specifically looking at habitat types and count data associated with habitat types.
#Hypotheses 1
Testing if there are significant differences between REKN counts on the bay habitat vs. counts on the ocean.
Null= There is no significant difference between bay and ocean counts.
H1=There is a significant difference between bay and ocean counts.
My prediction was that there would be a difference. I seperated my data into individual 2 column data frames by habitat with count occurences. This allows me to go back and test other hypotheses about habitat and counts. After using a for loop to complete a permutation test, we failed to reject the Null finding there were no significant differences between bay and ocean counts. I wanted to examine if there were differences between mudflat (which, in the study site, is opposite the ocean side habitat,as mudflats are typically deposited on bayside habitat on Fire Island) and ocean counts. H0=There is not a significant difference between counts at the mudflat and counts at the ocean. 
H1=There is a siginificant difference in counts of REKN at the mudflat and counts at the ocean.
I combined my data from my Counts data frame to extract only counts associated with mudflat and ocean habitats by using a "where this column is "x", or "|" this is "y", include all rows for these instances."
Used a welches t test because my data are normally distributed but the variance of counts is unequal.
We got a p-value = 0.001724, p<0.05, so we can reject our null and conclude there is a significant difference in counts at the mudflats vs. counts at the ocean. 

###Week 6###

