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
RK_Shorebird_Count_2010.csv code=Week 6 Homework.R
Psuedo data and determining which 
I used the same parameters as my data -- 243 obs, and after fitting the data,found my k parameter
#and the mu of my data 10.309, and used those to make a psuedo-data set.The data somewhat has a similar trend as my data, but not exactly the same.
#The data doesn't look the same although the more iterations or random draws I performed using my initial data paramaters the histogram would look more similar to my initial data (with more repitions)

###Week 8&&9###

Linear models (lm and glm) functions. REKN_J.data.2016.data.2014.2016.csv code=Homework8.9.R
Attempted to plot iteractive models and us lsmeans to examine any significant differences between habitat types and year (modeling count througout each) and also examining habitat length effect on total REKN observed. Flood shoals showed a significant effect on total counts. I had a difficult time plotting my interractive model... 

###Week 10&11###
GLM and Model Selection REKN_J.data.2016.data.2014.2016.csv and subsite.attributes.csv code=Homework10.11.R
I attempted to run GLM to examine the effects Total number of red knots observed by Period and Year,and, Total number of birds observed by Habitat type and survey period (aka, spring migration period or fall migration period). H0: Total number of birds was not significantly effected between years and periods. H1: There was an effect of year and period on total number of birds. For this circumstance we rejected the null as there was a significant effect of year on total number of birds seen. I also compared total counts of redknots between Habitat types  and periods. H0: No significant effect between habitat types and periods on total number of birds. H1: There is an effect on totals by habitat types and survey periods. We rejected the null in this instance as well. I had some difficulties understanding and interpretting the data, although I did get my models to run. Maybe you can give me some feedback as to how to best examine the results? Also expand grid, although I got it to work, and added the new data to my redknot dataframe, I wasn't sure I was plotting the correct predicted data over the entirety of my data.
For model selections, the top model was Habitat*Period. I think the GLM and AIC approach both show similar results: that Habitat*Period have significance within the model. The AIC and anova model comparisons ranking Habitat*Period to be the best fitting model, and, when I run this model, I get significant results as well of effect of habitat and year on totals.

Week 12
GLM and Model Selection REKN_J.data.2016.data.2014.2016.csv and subsite.attributes.csv code=Homework10.11.R
Looked at fixed and random effects within my data set. I was interested in exploring the hypothesis that survey and habitat type had significant effects on total birds seen. I attempted to set my fixed effects and random effects. My subsites were set as a random effect, because I wanted to account for variability among subsites. My fixed effects were Habitat and Survey, because I was interested in the effects of these (accounting for my random effect). I was able to run my first model, but not my second. 
Within my first model, it would seem that Survey and Habitat both had significant effects on Total number of birds, with p values being 0.0031051 and 0.0001072 respectively.   
Survey  47.270 24  0.0031051 ** 
Habitat 23.363  4  0.0001072 ***
Also, Habitat Cupsogue flood shoals was the only habitat that had any significant effect on totals (which is what I have been finding consistently) with intercept of 1.054e+00 and a p-value of 0.03278.


