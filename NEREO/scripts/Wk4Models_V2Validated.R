#Nereo Models - Week 4

rm(list = ls())

#Set WD
setwd("~/Desktop/Nereo/NereoAnalysis/NEREO")

#Packages Needed
library(readxl)
library(lme4)
library(nlme)
library(tidyverse)
library(car)
library(glmmTMB)
library(glmm)

# Base model - run for each week lmer(Size/Number~ Temp + pH  + Temp:pH +  (1|Dish))

#Uploading and Cleaning the Week 4 data

#pulling data
data <- read_csv("Wk3Wk4DataAnalysis/wk3wk4_Nereo_counts.csv")
View(data)

head(data)
tail(data)
str(data)

#data cleaning: 
#1. convert all variables into factors, except for number which should be continuous
data$Week = as.factor(data$Week)
data$Temp = as.factor(data$Temp)
data$pH = as.factor(data$pH)
data$Trt_Rep = as.factor(data$Trt_Rep)
data$Photo_Rep = as.factor(data$Photo_Rep)
data$Type = as.factor(data$Type)
data$Number = as.numeric(data$Number)

#hmmm. NAs were introduced for that last one  let's isolate the NAs to see where they were.
#NAdt = data %>% 
#  filter(is.na(Number))
#ok this does make sense - Wk4_AB1_x4_2 is a duplicate photo of AB1_x4_3

#2. need to split data set in two - into week3 and week 4 separately. need to think a little more about how to compare across weeks
pdt = data %>% 
  filter(!is.na(Number)) %>% 
  filter(Week == "Week4") %>% 
  spread(Type, Number)
#creating a new column for Trt_trtRep which will serve as our random variable
pdt$FullTrt = paste(pdt$Treatment,"_",pdt$Trt_Rep)

# FEMALES -----------------------------

#quickly graph data
boxplot(Females ~ Treatment, pdt)
#hmm a lot of overlap in errors
#generally looks like low temps have higher mean numbe of females than high temps
#not really sure theres a pH effect

#data exploration
#purpose: plot the histogram and dotchart for each continuous response and fixed variable - if there are random variables, check those impacts on each of the main variables. Histograms - looking for a nice even normal distrbution, no skews. Dotcharts - looking for random spread, no clusters. 
hist(pdt$Females) #actually pretty normalish, maybe a bit of skew to the left
dotchart(pdt$Females, color=pdt$Trt_Rep) #a bit clustered to the left and the top, lie along a curve almost

#TESTING ASSUMPTIONS AND CHOOSING TRANSFORMATIONS
#build main model lme argument (allows for random effects as opposed to lm)
f.mod.noT = lme(Females ~ Temp*pH,random= ~1|FullTrt, data = pdt) #modeling the effects of Temp and pH with random effects of Dish (Trt_Rep)
f.res =as.numeric(residuals(f.mod.noT)) #create a value for the residuals to do tests of LMM assumptions

#testing normality
hist(f.res) #normalish but kinda skewed to left
dotchart(f.res) #relatively scattered but a bit clustered on the left
qqnorm(f.res)
qqline(f.res) #ok until high end
shapiro.test(f.res) #p<<<0.05 so didn't pass test of normality
#testing homogeneity of variances
leveneTest(Females ~ Temp, data=pdt) #p=0.01381
leveneTest(Females ~ pH, data=pdt) #p=0.3442
leveneTest(Females ~ Temp:pH, data=pdt) #p=0.02249
leveneTest(Females ~ Temp*pH, data=pdt) #p=0.02249, ok so we can def say variances all homogenous
plot(f.mod.noT) #residuals clustered in two areas? skew hp

#ok so without a transformation, the residual variances are mostly homogenous, but the residuals are def not normal. So let's try a sqrt transformation and see what that does
#build main model lme argument (allows for random effects as opposed to lm)
f.mod.sqrt = lme(sqrt(Females) ~ Temp*pH,random= ~1|FullTrt, data = pdt) #modeling the effects of Temp and pH with random effects of Dish (Trt_Rep)
f.res.sqrt =as.numeric(residuals(f.mod.sqrt)) #create a value for the residuals to do tests oof LMM assumptions
#testing normality
hist(f.res.sqrt) #def looking more normal, less skewed
dotchart(f.res.sqrt) #well scattered, more centered
qqnorm(f.res.sqrt)
qqline(f.res.sqrt) #much better
shapiro.test(f.res.sqrt) #p=0.08796 so passes test of normality!
#let's make sure variances are still homogenous
leveneTest(sqrt(Females) ~ Temp, data=pdt) #p=0.6178
leveneTest(sqrt(Females) ~ pH, data=pdt) #p=0.5243
leveneTest(sqrt(Females) ~ Temp:pH, data=pdt) #p=0.09334
leveneTest(sqrt(Females) ~ Temp*pH, data=pdt) #p=0.09334, ok so we still have homogeneity of variances
plot(f.mod.sqrt) #residuals still kinda clustered in two areas? 

#let's try a log transformation now and just see what that does
#build main model lme argument (allows for random effects as opposed to lm)
f.mod.log = lme(log(Females) ~ Temp*pH,random= ~1|FullTrt, data = pdt) #modeling the effects of Temp and pH with random effects of Dish (Trt_Rep)
f.res.log =as.numeric(residuals(f.mod.log)) #create a value for the residuals to do tests oof LMM assumptions
#testing normality
hist(f.res.log) #ooh way more normal
dotchart(f.res.log) #def more spread out
qqnorm(f.res.log)
qqline(f.res.log) #good
shapiro.test(f.res.log) #p=.8475 so really passes test of normality! 
#let's make sure variances are still homogenous
leveneTest(log(Females) ~ Temp, data=pdt) #p=0.05818 so worse
leveneTest(log(Females) ~ pH, data=pdt) #p=0.8368
leveneTest(log(Females) ~ Temp:pH, data=pdt) #p=0.006006
leveneTest(log(Females) ~ Temp*pH, data=pdt) #p=0.006006, ok so we now lost homogeneity of variances :/
plot(f.mod.log) #residuals kinda clustered in two areas? interesting - not sure what to do about that


#We will choose sqrt transformation because it allows residual to be both normal and keep homogeneity of variances. Also this data is count data, which a sqrt transformation is particularly appropriate for



#MODEL VALIDATION
#From K. Laskowski: "This "nesting" process is how you get the overall significance of whatever fixed/random effect you are interested in. This process of comparing two models (one with the effect, one without the effect) is performing a Log Likelihood Ratio test. Basically you run two models, and then compare the log likelihoods of each model to see whether the two models are different from each other. If they are significantly different then this means that the effect that you removed is having a big effect (i.e. it is significant). If you remember from class, you should always start by testing the effects of your higher-order effects first. So test whether the interaction is significant (using a log likelihood ratio test). If it is, then basically you're done as it doesn't make statistical sense to then test for the main effects (i.e. temp) if your interaction is significant (temp x ph)"
#"the anova(mod1, mod2) is what is performing the LLR test. The LLR test is essentially equivalent to an F-test in a classic ANOVA and so tells you the "overall" effect of a particular fixed effect. The p-values that are reported in the summary are from t-tests which are testing whether the parameter estimate itself is different from zero. IF you only have continuous effects or categorical effects with only two levels, then the t-tests and LLR tests will largely be the same. BUT if you have a categorical variable with MORE than two levels, then you'll see in your summary that you have multiple t-tests for each level of the effect and then you can't tell what the "overall" effect is anymore - this is why you need the LLR test.

#Starting to go through the nesting process
f.mod.main = lme(sqrt(Females) ~ Temp + pH + Temp:pH, method="REML",random= ~1|FullTrt, data = pdt)

#Determine proper random structure using REML (REML = restricted maximum likelihood) - remove 1 random effect at a time and finally all random effects
f.mod.rtest = gls(sqrt(Females) ~ Temp + pH + Temp:pH, data = pdt)
#compare models with and without random effects
anova(f.mod.main, f.mod.rtest)
#p value =0.005 so Dish(Trt_Rep) does explain variation???



#Determine proper fixed structure using ML (ML = maximum likelihood)

#First, we check the Interactions
f.mod.ml = lme(sqrt(Females) ~ Temp + pH + Temp:pH, method="ML",random= ~1|FullTrt, data = pdt)
#checking Temp:pH intxn
f.mod.txp = lme(sqrt(Females) ~ Temp + pH, method="ML",random= ~1|FullTrt, data = pdt)
anova(f.mod.ml, f.mod.txp)
#p value = 0.3671 so looks like Temp x pH is not significant


#Next we check the individual effects
#new full model without interaction terms
f.mod.nox = lme(sqrt(Females) ~ Temp + pH, method="ML",random= ~1|FullTrt, data = pdt)
#checking Temp
f.mod.temp = lme(sqrt(Females) ~ pH, method="ML",random= ~1|FullTrt, data = pdt)
anova(f.mod.nox, f.mod.temp)
#pvalue is <.0001 so significant
#checking pH
f.mod.pH = lme(sqrt(Females) ~ Temp, method="ML",random= ~1|FullTrt, data = pdt)
anova(f.mod.nox, f.mod.pH)
#pvalue is 0.4717 so not significant


#rerun model without terms we don't think are significant
f.mod.final = lme(log(Females) ~ Temp, method="REML",random= ~1|FullTrt, data = pdt)
summary(f.mod.final)

#rerun full model to report
f.mod.finalfull = lme(sqrt(Females) ~ Temp + pH + Temp:pH, method="REML",random= ~1|FullTrt, data = pdt)
summary(f.mod.finalfull)

REest = (0.2862774)^2 #from Random Effect Intercept in summary
res.est = (0.4612656)^2 #from random effect residual in summary

#Validate Original Model Assumptions
qqnorm(resid(f.mod.finalfull))
qqline(resid(f.mod.finalfull)) #majority of QQ pts lie along line, so looks good! there is some skew along the ends tho

#plotting residuals against 2 predictors in the model
#for continuous variables: want to see a nice starry night of random noise
#for categorical variables: want to see even box plots

plot(resid(f.mod.finalfull)~pdt$Temp) #bar means pretty even, high temp has more outliers
plot(resid(f.mod.finalfull)~pdt$pH) #bar means pretty even, high pH has more outliers
#overall, looks like we aren't violating any assumptions


# MALES -----------------------------

#quickly graph data
boxplot(Males ~ Treatment, pdt)
#hmm a lot of overlap in errors
#generally looks like low temps have more males than high temps
#generally looks the same for pH - higher number of males in low pH than high pH
#v interesting

#data exploration
#purpose: plot the histogram and dotchart for each continuous response and fixed variable - if there are random variables, check those impacts on each of the main variables. Histograms - looking for a nice even normal distrbution, no skews. Dotcharts - looking for random spread, no clusters. 
hist(pdt$Males) #normalish, skew to left
dotchart(pdt$Males, color=pdt$Trt_Rep) #a bit clustered to the left

#TESTING ASSUMPTIONS AND CHOOSING TRANSFORMATIONS
#build main model lme argument (allows for random effects as opposed to lm)
m.mod.noT = lme(Males ~ Temp*pH,random= ~1|FullTrt, data = pdt) #modeling the effects of Temp and pH with random effects of Dish (Trt_Rep)
m.res =as.numeric(residuals(f.mod.noT)) #create a value for the residuals to do tests oof LMM assumptions

#testing normality
hist(m.res) #normalish but kinda skewed to left
dotchart(m.res) #relatively scattered but a bit clustered on the left
qqnorm(m.res)
qqline(m.res) #ok until high end
shapiro.test(m.res) #p<<<0.05 so didn't pass test of normality
#testing homogeneity of variances
leveneTest(Males ~ Temp, data=pdt) #p=0.003219
leveneTest(Males ~ pH, data=pdt) #p=0.3487
leveneTest(Males ~ Temp:pH, data=pdt) #p=0.04944
leveneTest(Males ~ Temp*pH, data=pdt) #p=0.04944, ok so we can def say variances all homogenous
plot(m.mod.noT) #residuals clustered in two areas? skew high

#ok so without a transformation, the residual variances are not homogenous, nor are they normal. So let's try a sqrt transformation and see what that does
#build main model lme argument (allows for random effects as opposed to lm)
m.mod.sqrt = lme(sqrt(Males) ~ Temp*pH,random= ~1|FullTrt, data = pdt) #modeling the effects of Temp and pH with random effects of Dish (Trt_Rep)
m.res.sqrt =as.numeric(residuals(m.mod.sqrt)) #create a value for the residuals to do tests oof LMM assumptions
#testing normality
hist(m.res.sqrt) #def looking more normal, less skewed
dotchart(m.res.sqrt) #well scattered, more centered
qqnorm(m.res.sqrt)
qqline(m.res.sqrt) #idk if that's better
shapiro.test(m.res.sqrt) #p<<<0.05 so still not normal
#let's make sure variances are still homogenous
leveneTest(sqrt(Males) ~ Temp, data=pdt) #p=0.545
leveneTest(sqrt(Males) ~ pH, data=pdt) #p=0.8873
leveneTest(sqrt(Males) ~ Temp:pH, data=pdt) #p=0.7019
leveneTest(sqrt(Males) ~ Temp*pH, data=pdt) #p=0.7019, ok so we have homogeneity of variances, but not normality
plot(m.mod.sqrt) #residuals still kinda clustered in two areas? now trend low

#let's try a log transformation now and see what that does
#build main model lme argument (allows for random effects as opposed to lm)
m.mod.log = lme(log(Males+1) ~ Temp*pH,random= ~1|FullTrt, data = pdt) #modeling the effects of Temp and pH with random effects of Dish (Trt_Rep)
#need to do Males+1 bc error arising
m.res.log =as.numeric(residuals(m.mod.log)) #create a value for the residuals to do tests oof LMM assumptions
#testing normality
hist(m.res.log) #more normal, but skewed right
dotchart(m.res.log) #skewed more right
qqnorm(m.res.log)
qqline(m.res.log) #worse :/
shapiro.test(m.res.log) #p<<<0.05 so still not normal
#let's make sure variances are still homogenous
leveneTest(log(Males+1) ~ Temp, data=pdt) #p=0.02137 so worse
leveneTest(log(Males+1) ~ pH, data=pdt) #p=0.6542
leveneTest(log(Males+1) ~ Temp:pH, data=pdt) #error
leveneTest(log(Males+1) ~ Temp*pH, data=pdt) #p=error, ok so we now lost homogeneity of variances :/
plot(m.mod.log) #residuals kinda clustered in two areas? really tredn low

#We will choose sqrt transformation because it at least has homogeneity of variances

#MODEL VALIDATION
#Starting to go through the nesting process
m.mod.main = lme(sqrt(Males) ~ Temp + pH + Temp:pH, method="REML",random= ~1|FullTrt, data = pdt)

#Determine proper random structure using REML - remove 1 random effect at a time and finally all random effects
m.mod.rtest = gls(sqrt(Males) ~ Temp + pH + Temp:pH, data = pdt)
#compare models with and without random effects
anova(m.mod.main, m.mod.rtest)
#p value =0.2982 so Dish(Trt_Rep) does not explain variation


#Determine proper fixed structure using ML

#First, we check the Interactions
m.mod.ml = lme(sqrt(Males) ~ Temp + pH + Temp:pH, method="ML",random= ~1|FullTrt, data = pdt)
#checking Temp:pH intxn
m.mod.txp = lme(sqrt(Males) ~ Temp + pH, method="ML",random= ~1|FullTrt, data = pdt)
anova(m.mod.ml, m.mod.txp)
#p value = 0.9007 so looks like Temp x pH is not significant


#Next we check the individual effects
#new full model without interaction terms
m.mod.nox = lme(sqrt(Males) ~ Temp + pH, method="ML",random= ~1|FullTrt, data = pdt)
#checking Temp
m.mod.temp = lme(sqrt(Males) ~ pH, method="ML",random= ~1|FullTrt, data = pdt)
anova(m.mod.nox, m.mod.temp)
#pvalue is <.0001 so significant
#checking pH
m.mod.pH = lme(sqrt(Males) ~ Temp, method="ML",random= ~1|FullTrt, data = pdt)
anova(m.mod.nox, m.mod.pH)
#pvalue is 0.0033 so yes significant


#rerun model without terms we don't think are significant
m.mod.final = lme(sqrt(Males) ~ Temp, method="REML",random= ~1|FullTrt, data = pdt)
summary(m.mod.final)
#HONESTLY NOT USING THIS MODEL SO IDK WHY WE NEED THIS

#rerun full model to report
m.mod.finalfull = lme(sqrt(Males) ~ Temp + pH + Temp:pH, method="REML",random= ~1|FullTrt, data = pdt)
summary(m.mod.finalfull)

REest = (0.2162877)^2 #from Random Effect Intercept in summary
res.est = (0.6452484)^2 #from random effect residual in summary

#Validate Original Model Assumptions
qqnorm(resid(m.mod.finalfull))
qqline(resid(m.mod.finalfull)) #hmm not super great looking - lot of end skew

#plotting residuals against 2 predictors in the model
#for continuous variables: want to see a nice starry night of random noise
#for categorical variables: want to see even box plots

plot(resid(m.mod.finalfull)~pdt$Temp) #bar means evenish, high temp has more low outliers but low temp has a high outlier
plot(resid(m.mod.finalfull)~pdt$pH) #bar means pretty even, high pH has more outliers
#overall, looks like we aren't violating any assumptions

# JUVENILES -----------------------------

#quickly graph data
boxplot(Juveniles ~ Treatment, pdt)
#hmm a lot of overlap in errors
#generally looks like low temps have higher number of  gams than high temps
#generally looks the same for pH -  low pH has lower number than high pH
#v interesting

#data exploration
#purpose: plot the histogram and dotchart for each continuous response and fixed variable - if there are random variables, check those impacts on each of the main variables. Histograms - looking for a nice even normal distrbution, no skews. Dotcharts - looking for random spread, no clusters. 
hist(pdt$Juveniles) #pretty high leftwards skew
dotchart(pdt$Juveniles, color=pdt$Trt_Rep) #a bit clustered to the left, but relatively even

#TESTING ASSUMPTIONS AND CHOOSING TRANSFORMATIONS
#build main model lme argument (allows for random effects as opposed to lm)
j.mod.noT = lme(Juveniles ~ Temp*pH,random= ~1|FullTrt, data = pdt) #modeling the effects of Temp and pH with random effects of Dish (Trt_Rep)
j.res =as.numeric(residuals(j.mod.noT)) #create a value for the residuals to do tests oof LMM assumptions

#testing normality
hist(j.res) #normalish but kinda skewed to left
dotchart(j.res) #relatively scattered but a bit clustered on the left
qqnorm(j.res)
qqline(j.res) #ok until high end
shapiro.test(j.res) #p=0.000244 so didn't pass test of normality
#testing homogeneity of variances
leveneTest(Juveniles ~ Temp, data=pdt) #p=0.008726
leveneTest(Juveniles ~ pH, data=pdt) #p=0.8272
leveneTest(Juveniles ~ Temp:pH, data=pdt) #p=0.05267
leveneTest(Juveniles ~ Temp*pH, data=pdt) #p=0.05267, ok so we can def say variances all homogenous
plot(j.mod.noT) #residuals clustered in two areas? skew high

#ok so without a transformation, the residual variances are mostly homogenous, but the residuals are def not normal. So let's try a sqrt transformation and see what that does
#build main model lme argument (allows for random effects as opposed to lm)
j.mod.sqrt = lme(sqrt(Juveniles) ~ Temp*pH,random= ~1|FullTrt, data = pdt) #modeling the effects of Temp and pH with random effects of Dish (Trt_Rep)
j.res.sqrt =as.numeric(residuals(j.mod.sqrt)) #create a value for the residuals to do tests oof LMM assumptions
#testing normality
hist(j.res.sqrt) #def looking more normal, less skewed
dotchart(j.res.sqrt) #well scattered, more centered
qqnorm(j.res.sqrt)
qqline(j.res.sqrt) #much better
shapiro.test(j.res.sqrt) #p=0.07891 so passes test of normality!
#let's make sure variances are still homogenous
leveneTest(sqrt(Juveniles) ~ Temp, data=pdt) #p=0.2108
leveneTest(sqrt(Juveniles) ~ pH, data=pdt) #p=0.6945
leveneTest(sqrt(Juveniles) ~ Temp:pH, data=pdt) #p=0.3583
leveneTest(sqrt(Juveniles) ~ Temp*pH, data=pdt) #p=0.3583, ok so we still have homogeneity of variances
plot(j.mod.sqrt) #residuals still kinda clustered in two areas? but less high skew

#We will choose sqrt transformation because it allows residual to be both normal and keep homogeneity of variances. Also this data is count data, which a sqrt transformation is particularly appropriate for



#MODEL VALIDATION

#Starting to go through the nesting process
j.mod.main = lme(sqrt(Juveniles) ~ Temp + pH + Temp:pH, method="REML",random= ~1|FullTrt, data = pdt)

#Determine proper random structure using REML - remove 1 random effect at a time and finally all random effects
j.mod.rtest = gls(sqrt(Juveniles) ~ Temp + pH + Temp:pH, data = pdt)
#compare models with and without random effects
anova(j.mod.main, j.mod.rtest)
#p value =0.0805 so Dish(Trt_Rep) almost explain variations but doesnt actually


#Determine proper fixed structure using ML

#First, we check the Interactions
j.mod.ml = lme(sqrt(Juveniles) ~ Temp + pH + Temp:pH, method="ML",random= ~1|FullTrt, data = pdt)
#checking Temp:pH intxn
j.mod.txp = lme(sqrt(Juveniles) ~ Temp + pH, method="ML",random= ~1|FullTrt, data = pdt)
anova(j.mod.ml, j.mod.txp)
#p value = 0.6558 so looks like Temp x pH is not significant


#Next we check the individual effects
#new full model without interaction terms
j.mod.nox = lme(sqrt(Juveniles) ~ Temp + pH, method="ML",random= ~1|FullTrt, data = pdt)
#checking Temp
j.mod.temp = lme(sqrt(Juveniles) ~ pH, method="ML",random= ~1|FullTrt, data = pdt)
anova(j.mod.nox, j.mod.temp)
#pvalue is <.0001 so significant
#checking pH
j.mod.pH = lme(sqrt(Juveniles) ~ Temp, method="ML",random= ~1|FullTrt, data = pdt)
anova(j.mod.nox, j.mod.pH)
#pvalue is 0.3009 so not significant


#rerun full model to report
j.mod.finalfull = lme(sqrt(Juveniles) ~ Temp + pH + Temp:pH, method="REML",random= ~1|FullTrt, data = pdt)
summary(j.mod.finalfull)

REest = (0.2838499)^2 #from Random Effect Intercept in summary
res.est = (0.6229719)^2 #from random effect residual in summary

#Validate Original Model Assumptions
qqnorm(resid(j.mod.finalfull))
qqline(resid(j.mod.finalfull)) #ooh v close to the line, very nice

#plotting residuals against 2 predictors in the model
#for continuous variables: want to see a nice starry night of random noise
#for categorical variables: want to see even box plots

plot(resid(j.mod.finalfull)~pdt$Temp) #bar means evenish, high temp has more low outliers but low temp has a high outlier
plot(resid(j.mod.finalfull)~pdt$pH) #bar means pretty even, high pH has more outliers
#overall, looks like we aren't violating any assumptions

# EGGS -----------------------------

#quickly graph data
boxplot(Eggs ~ Treatment, pdt)
#hmm a lot of overlap in errors
#generally looks like low temps have higher number of  gams than high temps
#generally looks the same for pH -  low pH has higher number than high pH
#v interesting

#data exploration
#purpose: plot the histogram and dotchart for each continuous response and fixed variable - if there are random variables, check those impacts on each of the main variables. Histograms - looking for a nice even normal distrbution, no skews. Dotcharts - looking for random spread, no clusters. 
hist(pdt$Eggs) #hot damn, pure poisson dist
dotchart(pdt$Eggs, color=pdt$Trt_Rep) #very clustered to the left

#TESTING ASSUMPTIONS AND CHOOSING TRANSFORMATIONS
#build main model lme argument (allows for random effects as opposed to lm)
e.mod.noT = lme(Eggs ~ Temp*pH,random= ~1|FullTrt, data = pdt) #modeling the effects of Temp and pH with random effects of Dish (Trt_Rep)
e.res =as.numeric(residuals(e.mod.noT)) #create a value for the residuals to do tests oof LMM assumptions

#testing normality
hist(e.res) #normalish but kinda skewed to left
dotchart(e.res) #relatively scattered but a bit clustered on the left
qqnorm(e.res)
qqline(e.res) #not bad actually - only seems to be one outlier
shapiro.test(e.res) #p=0.0003736 so didn't quite pass test of normality
#testing homogeneity of variances
leveneTest(Eggs ~ Temp, data=pdt) #p=0.0004139
leveneTest(Eggs ~ pH, data=pdt) #p=0.1478
leveneTest(Eggs ~ Temp:pH, data=pdt) #p=0.0002985
leveneTest(Eggs ~ Temp*pH, data=pdt) #p=0.0002985, ok so we can def say variances all homogenous
plot(e.mod.noT) #residuals clustered in two areas? skew left a bit

#ok so without a transformation, the residual variances are not homogenous, and the residuals are def not normal. So let's try a sqrt transformation and see what that does
#build main model lme argument (allows for random effects as opposed to lm)
e.mod.sqrt = lme(sqrt(Eggs) ~ Temp*pH,random= ~1|FullTrt, data = pdt) #modeling the effects of Temp and pH with random effects of Dish (Trt_Rep)
e.res.sqrt =as.numeric(residuals(e.mod.sqrt)) #create a value for the residuals to do tests oof LMM assumptions
#testing normality
hist(e.res.sqrt) #def looking more normal, less skewed
dotchart(e.res.sqrt) #well scattered, more centered
qqnorm(e.res.sqrt)
qqline(e.res.sqrt) #pretty good, points just slightly below (but parallel to) line
shapiro.test(e.res.sqrt) #p=0.2878 so passes test of normality!
#let's make sure variances are still homogenous
leveneTest(sqrt(Eggs) ~ Temp, data=pdt) #p=0.8282
leveneTest(sqrt(Eggs) ~ pH, data=pdt) #p=0.6248
leveneTest(sqrt(Eggs) ~ Temp:pH, data=pdt) #p=0.2739
leveneTest(sqrt(Eggs) ~ Temp*pH, data=pdt) #p=0.2739, ok so we now have homogeneity of variances and normality
plot(e.mod.sqrt) #residuals much better and more evenly scattered

#We will choose sqrt transformation because it allows residual to be both normal and keep homogeneity of variances. Also this data is count data, which a sqrt transformation is particularly appropriate for



#MODEL VALIDATION

#Starting to go through the nesting process
e.mod.main = lme(sqrt(Eggs) ~ Temp + pH + Temp:pH, method="REML",random= ~1|FullTrt, data = pdt)

#Determine proper random structure using REML - remove 1 random effect at a time and finally all random effects
e.mod.rtest = gls(sqrt(Eggs) ~ Temp + pH + Temp:pH, data = pdt)
#compare models with and without random effects
anova(e.mod.main, e.mod.rtest)
#p value =0.0225 so Dish(Trt_Rep) does explain variation


#Determine proper fixed structure using ML

#First, we check the Interactions
e.mod.ml = lme(sqrt(Eggs) ~ Temp + pH + Temp:pH, method="ML",random= ~1|FullTrt, data = pdt)
#checking Temp:pH intxn
e.mod.txp = lme(sqrt(Eggs) ~ Temp + pH, method="ML",random= ~1|FullTrt, data = pdt)
anova(e.mod.ml, e.mod.txp)
#p value = 0.6633 so looks like Temp x pH is not significant


#Next we check the individual effects
#new full model without interaction terms
e.mod.nox = lme(sqrt(Eggs) ~ Temp + pH, method="ML",random= ~1|FullTrt, data = pdt)
#checking Temp
e.mod.temp = lme(sqrt(Eggs) ~ pH, method="ML",random= ~1|FullTrt, data = pdt)
anova(e.mod.nox, e.mod.temp)
#pvalue is <.0001 so significant
#checking pH
e.mod.pH = lme(sqrt(Eggs) ~ Temp, method="ML",random= ~1|FullTrt, data = pdt)
anova(e.mod.nox, e.mod.pH)
#pvalue is 0.0360 so significant


#rerun model without terms we don't think are significant
e.mod.final = lme(sqrt(Eggs) ~ Temp, method="REML",random= ~1|FullTrt, data = pdt)
summary(e.mod.final)
#HONESTLY NOT USING THIS MODEL SO IDK WHY WE NEED THIS

#rerun full model to report
e.mod.finalfull = lme(sqrt(Eggs) ~ Temp + pH + Temp:pH, method="REML",random= ~1|FullTrt, data = pdt)
summary(e.mod.finalfull)

REest = (0.5352344)^2 #from Random Effect Intercept in summary
res.est = (0.9908117)^2 #from random effect residual in summary

#Validate Original Model Assumptions
qqnorm(resid(e.mod.finalfull))
qqline(resid(e.mod.finalfull)) #hmm close to the line, a bit low tho

#plotting residuals against 2 predictors in the model
#for continuous variables: want to see a nice starry night of random noise
#for categorical variables: want to see even box plots

plot(resid(e.mod.finalfull)~pdt$Temp) #bar means evenish, high temp has more low outliers but low temp has a high outlier
plot(resid(e.mod.finalfull)~pdt$pH) #bar means pretty even, high pH has more outliers
#overall, looks like we aren't violating any assumptions


# # PROPORTION PRODUCTIVE FEMALES-----------------------------

#create new column with calculation of the proportion of productive females
pdt$Prop_prod <- pdt$Productive/pdt$Females
#quickly graph data
boxplot(Prop_prod ~ Treatment, pdt)
#ok interesting. so basically, here we are seeing 100% productivity at high temps, but not at low temps.

#data exploration
#purpose: plot the histogram and dotchart for each continuous response and fixed variable - if there are random variables, check those impacts on each of the main variables. Histograms - looking for a nice even normal distrbution, no skews. Dotcharts - looking for random spread, no clusters.
hist(pdt$Prop_prod) #v skewed to the right
dotchart(pdt$Prop_prod, color=pdt$Trt_Rep) #v skewed to the right

#Calculcate the means for each treatment
pp_means = pdt %>%
  group_by(Treatment) %>% 
  summarise(ave = mean(Prop_prod))

#calculate the SDs for each treatment
pp_sds = pdt %>%
  group_by(Treatment) %>% 
  summarise(ave = sd(Prop_prod))

#MODEL VALIDATION

#RECHECK VALIDATION OF RANDOM EFFECTS
#Starting to go through the nesting process
pp.mod.main = glmmTMB((Prop_prod-0.00001) ~ Temp*pH +(1|FullTrt), family = beta_family(link="logit"), data = pdt)

#BUILDING/VALIDATING THE MODEL
#lme argument (allows for random effects as opposed to lm)
pp.mod.full = glmmTMB((Prop_prod-0.00001) ~ Temp*pH +(1|FullTrt), family = beta_family(link="logit"), data = pdt) #modeling the effects of Temp and pH with random effects of Dish (Trt_Rep)

#Determine proper fixed structure using ML

#First, we check the Interactions
pp.mod.ml = glmmTMB((Prop_prod-0.00001) ~ Temp*pH +(1|FullTrt), family = beta_family(link="logit"), data = pdt)
#checking Temp:pH intxn
pp.mod.txp = glmmTMB((Prop_prod-0.00001) ~ Temp+pH +(1|FullTrt), family = beta_family(link="logit"), data = pdt)
anova(pp.mod.ml, pp.mod.txp)
#p value = 0.4744 so looks like Temp x pH is not significant


#Next we check the individual effects
#new full model without interaction terms
pp.mod.nox = glmmTMB((Prop_prod-0.00001) ~ Temp+pH +(1|FullTrt), family = beta_family(link="logit"), data = pdt)
#checking Temp
pp.mod.temp = glmmTMB((Prop_prod-0.00001) ~ pH +(1|FullTrt), family = beta_family(link="logit"), data = pdt)
anova(pp.mod.nox, pp.mod.temp)
#pvalue is <.0001 so significant
#checking pH
pp.mod.pH = glmmTMB((Prop_prod-0.00001) ~ Temp +(1|FullTrt), family = beta_family(link="logit"), data = pdt)
anova(pp.mod.nox, pp.mod.pH)
#pvalue is 0.2811 so not significant


#rerun full model to report
pp.mod.main = glmmTMB((Prop_prod-0.00001) ~ Temp*pH +(1|FullTrt), family = beta_family(link="logit"), data = pdt)
summary(pp.mod.main)

REest = (0.1350155)^2 #from Random Effect Intercept in summary
res.est = (0.3167536)^2 #from random effect residual in summary




# EGGS PER FEMALE -----------------------------

# calculate eggs per female per dish
pdt$eggperF <- pdt$Eggs/pdt$Females

#quickly graph data
boxplot(pdt$eggperF ~ Treatment, pdt)
#hmm a lot of overlap in errors
#generally looks like low temps have more eggs per f than high temps
#generally looks the same for pH - more eggs per F in low pH than high pH
#v interesting

#data exploration
#purpose: plot the histogram and dotchart for each continuous response and fixed variable - if there are random variables, check those impacts on each of the main variables. Histograms - looking for a nice even normal distrbution, no skews. Dotcharts - looking for random spread, no clusters. 
hist(pdt$eggperF) #not normal, leftward skew
dotchart(pdt$eggperF, color=pdt$Trt_Rep) #a bit clustered to the left

#TESTING ASSUMPTIONS AND CHOOSING TRANSFORMATIONS
#build main model lme argument (allows for random effects as opposed to lm)
epf.mod.noT = lme(eggperF ~ Temp*pH,random= ~1|FullTrt, data = pdt) #modeling the effects of Temp and pH with random effects of Dish (Trt_Rep)
epf.res =as.numeric(residuals(epf.mod.noT)) #create a value for the residuals to do tests oof LMM assumptions

#testing normality
hist(epf.res) #normal looking
dotchart(epf.res) #relatively scattered 
qqnorm(epf.res)
qqline(epf.res) #wow very nice actually
shapiro.test(epf.res) #p=0.2153 so passed test of normality!
#testing homogeneity of variances
leveneTest(eggperF ~ Temp, data=pdt) #p=0.8854
leveneTest(eggperF ~ pH, data=pdt) #p=0.006727
leveneTest(eggperF ~ Temp:pH, data=pdt) #p=0.008049
leveneTest(eggperF ~ Temp*pH, data=pdt) #p=0.008049, ok so we can def say variances not all homogenous
plot(epf.mod.noT) #residuals even, maybe trend a bit high

#ok so without a transformation, the residual variances are not homogenous, but the residuals def not normal. So let's try a sqrt transformation and see what that does
#build main model lme argument (allows for random effects as opposed to lm)
epf.mod.sqrt = lme(sqrt(eggperF) ~ Temp*pH,random= ~1|FullTrt, data = pdt) #modeling the effects of Temp and pH with random effects of Dish (Trt_Rep)
epf.res.sqrt =as.numeric(residuals(epf.mod.sqrt)) #create a value for the residuals to do tests oof LMM assumptions
#testing normality
hist(epf.res.sqrt) #def looking more normal, less skewed
dotchart(epf.res.sqrt) #well scattered, more centered
qqnorm(epf.res.sqrt)
qqline(epf.res.sqrt) #pretty good, almost a bit worse tbh
shapiro.test(epf.res.sqrt) #p=0.02896 so almost passes test of normality
#let's make sure variances are still homogenous
leveneTest(sqrt(eggperF) ~ Temp, data=pdt) #p=0.006691
leveneTest(sqrt(eggperF) ~ pH, data=pdt) #p=0.343
leveneTest(sqrt(eggperF) ~ Temp:pH, data=pdt) #p=0.05481
leveneTest(sqrt(eggperF) ~ Temp*pH, data=pdt) #p=0.05481, ok so we now have not quite homogeneity of variances and normality
plot(epf.mod.sqrt) #residuals grouped in 4 clumps

#ok let's try a log transformation and see what that does
#build main model lme argument (allows for random effects as opposed to lm)
epf.mod.log = lme(log(eggperF+1) ~ Temp*pH,random= ~1|FullTrt, data = pdt) #modeling the effects of Temp and pH with random effects of Dish (Trt_Rep)
epf.res.log =as.numeric(residuals(epf.mod.log)) #create a value for the residuals to do tests oof LMM assumptions
#testing normality
hist(epf.res.log) #def looking more normal, less skewed
dotchart(epf.res.log) #well scattered, more centered
qqnorm(epf.res.log)
qqline(epf.res.log) #alight
shapiro.test(epf.res.log) #p=0.554 so passes test of normality!
#let's make sure variances are still homogenous
leveneTest(log(eggperF+1) ~ Temp, data=pdt) #p=0.1224
leveneTest(log(eggperF+1) ~ pH, data=pdt) #p=0.07792
leveneTest(log(eggperF+1) ~ Temp:pH, data=pdt) #ERROR
leveneTest(log(eggperF+1) ~ Temp*pH, data=pdt) #ERROR, ok so we now kinda have homogeneity of variances and normality
plot(epf.mod.log) #residuals grouped in 4 clumps

#We will choose LOG(X+1) transformation because it allows residual to be both normal and keep homogeneity of variances. 



#MODEL VALIDATION


#Starting to go through the nesting process
epf.mod.main = lme(log(eggperF+1) ~ Temp + pH + Temp:pH, method="REML",random= ~1|FullTrt, data = pdt)

#Determine proper random structure using REML - remove 1 random effect at a time and finally all random effects
epf.mod.rtest = gls(log(eggperF+1) ~ Temp + pH + Temp:pH, data = pdt)
#compare models with and without random effects
anova(epf.mod.main, epf.mod.rtest)
#p value =0.4525 so Dish(Trt_Rep) does not explain variation


#Determine proper fixed structure using ML

#First, we check the Interactions
epf.mod.ml = lme(log(eggperF+1) ~ Temp + pH + Temp:pH, method="ML",random= ~1|FullTrt, data = pdt)
#checking Temp:pH intxn
epf.mod.txp = lme(log(eggperF+1) ~ Temp + pH, method="ML",random= ~1|FullTrt, data = pdt)
anova(epf.mod.ml, epf.mod.txp)
#p value = 0.7547 so looks like Temp x pH is not significant


#Next we check the individual effects
#new full model without interaction terms
epf.mod.nox = lme(log(eggperF+1) ~ Temp + pH, method="ML",random= ~1|FullTrt, data = pdt)
#checking Temp
epf.mod.temp = lme(log(eggperF+1) ~ pH, method="ML",random= ~1|FullTrt, data = pdt)
anova(epf.mod.nox, epf.mod.temp)
#pvalue is 0.0003 so significant
#checking pH
epf.mod.pH = lme(log(eggperF+1) ~ Temp, method="ML",random= ~1|FullTrt, data = pdt)
anova(epf.mod.nox, epf.mod.pH)
#pvalue is 0.0022 so significant


#rerun model without terms we don't think are significant
epf.mod.final = lme(log(eggperF+1) ~ Temp, method="REML",random= ~1|FullTrt, data = pdt)
summary(epf.mod.final)
#HONESTLY NOT USING THIS MODEL SO IDK WHY WE NEED THIS

#rerun full model to report
epf.mod.finalfull = lme(log(eggperF+1) ~ Temp + pH + Temp:pH, method="REML",random= ~1|FullTrt, data = pdt)
summary(epf.mod.finalfull)

REest = (0.067462)^2 #from Random Effect Intercept in summary
res.est = (0.2410126)^2 #from random effect residual in summary

#Validate Original Model Assumptions
qqnorm(resid(epf.mod.finalfull))
qqline(resid(epf.mod.finalfull)) #ooh v close to the line, very nice

#plotting residuals against 2 predictors in the model
#for continuous variables: want to see a nice starry night of random noise
#for categorical variables: want to see even box plots

plot(resid(epf.mod.finalfull)~pdt$Temp) #bar means evenish, high temp has more low outliers but low temp has a high outlier
plot(resid(epf.mod.finalfull)~pdt$pH) #bar means pretty even, high pH has more outliers
#overall, looks like we aren't violating any assumptions


# JUVENILES PER FEMALE -----------------------------

# calculate juvs per female per dish
pdt$juvperF <- pdt$Juveniles/pdt$Females


#quickly graph data
boxplot(pdt$juvperF ~ Treatment, pdt)
#hmm a lot of overlap in errors
#generally looks like low temps have more eggs per f than high temps
#generally looks the same for pH - more eggs per F in low pH than high pH
#v interesting

#data exploration
#purpose: plot the histogram and dotchart for each continuous response and fixed variable - if there are random variables, check those impacts on each of the main variables. Histograms - looking for a nice even normal distrbution, no skews. Dotcharts - looking for random spread, no clusters. 
hist(pdt$juvperF) #normalish
dotchart(pdt$juvperF, color=pdt$Trt_Rep) #a bit clustered in center

#TESTING ASSUMPTIONS AND CHOOSING TRANSFORMATIONS
#build main model lme argument (allows for random effects as opposed to lm)
jpf.mod.noT = lme(juvperF ~ Temp*pH,random= ~1|FullTrt, data = pdt) #modeling the effects of Temp and pH with random effects of Dish (Trt_Rep)
jpf.res =as.numeric(residuals(jpf.mod.noT)) #create a value for the residuals to do tests oof LMM assumptions

#testing normality
hist(jpf.res) #normalish, slightly skewed to left
dotchart(jpf.res) #relatively evenly scattered 
qqnorm(jpf.res)
qqline(jpf.res) #ok until high end
shapiro.test(jpf.res) #p=0.009118 so almost passed test of normality
#testing homogeneity of variances
leveneTest(juvperF ~ Temp, data=pdt) #p=0.8272
leveneTest(juvperF ~ pH, data=pdt) #p=0.4094
leveneTest(juvperF ~ Temp:pH, data=pdt) #p=0.4316
leveneTest(juvperF ~ Temp*pH, data=pdt) #p=0.4316, ok so we can def say variances all homogenous
plot(jpf.mod.noT) #residuals in 2 clusters, maybe trend a bit high

#ok so without a transformation, the residual variances are  homogenous, but the residuals not quite normal. So let's try a sqrt transformation and see what that does
#build main model lme argument (allows for random effects as opposed to lm)
jpf.mod.sqrt = lme(sqrt(juvperF) ~ Temp*pH,random= ~1|FullTrt, data = pdt) #modeling the effects of Temp and pH with random effects of Dish (Trt_Rep)
jpf.res.sqrt =as.numeric(residuals(jpf.mod.sqrt)) #create a value for the residuals to do tests oof LMM assumptions
#testing normality
hist(jpf.res.sqrt) #def looking more normal, less skewed
dotchart(jpf.res.sqrt) #well scattered, more centered
qqnorm(jpf.res.sqrt)
qqline(jpf.res.sqrt) #oooh v nice
shapiro.test(jpf.res.sqrt) #p=0.3005 so passes test of normality!
#let's make sure variances are still homogenous
leveneTest(sqrt(juvperF) ~ Temp, data=pdt) #p=0.4552
leveneTest(sqrt(juvperF) ~ pH, data=pdt) #p=0.2831
leveneTest(sqrt(juvperF) ~ Temp:pH, data=pdt) #p=0.2244
leveneTest(sqrt(juvperF) ~ Temp*pH, data=pdt) #p=0.2244, ok so we now have both homogeneity of variances and normality
#WHAT DOES THIS PLOT ACTUALLY SHOW
plot(jpf.mod.sqrt) #residuals scattered, skew right

#We will choose sqrt transformation because it allows residual to be both normal and keep homogeneity of variances. Also this data is calculated from count data, which a sqrt transformation is particularly appropriate for


#MODEL VALIDATION


#Starting to go through the nesting process
jpf.mod.main = lme(sqrt(juvperF) ~ Temp + pH + Temp:pH, method="REML",random= ~1|FullTrt, data = pdt)

#Determine proper random structure using REML - remove 1 random effect at a time and finally all random effects
jpf.mod.rtest = gls(sqrt(juvperF) ~ Temp + pH + Temp:pH, data = pdt)
#compare models with and without random effects
anova(jpf.mod.main, jpf.mod.rtest)
#p value =0.2847 so Dish(Trt_Rep) does not explain variation


#Determine proper fixed structure using ML

#First, we check the Interactions
jpf.mod.ml = lme(sqrt(juvperF) ~ Temp + pH + Temp:pH, method="ML",random= ~1|FullTrt, data = pdt)
#checking Temp:pH intxn
jpf.mod.txp = lme(sqrt(juvperF) ~ Temp + pH, method="ML",random= ~1|FullTrt, data = pdt)
anova(jpf.mod.ml, jpf.mod.txp)
#p value = 0.7513 so looks like Temp x pH is not significant


#Next we check the individual effects
#new full model without interaction terms
jpf.mod.nox = lme(sqrt(juvperF) ~ Temp + pH, method="ML",random= ~1|FullTrt, data = pdt)
#checking Temp
jpf.mod.temp = lme(sqrt(juvperF) ~ pH, method="ML",random= ~1|FullTrt, data = pdt)
anova(jpf.mod.nox, jpf.mod.temp)
#pvalue is 0.0024 so significant
#checking pH
jpf.mod.pH = lme(sqrt(juvperF) ~ Temp, method="ML",random= ~1|FullTrt, data = pdt)
anova(jpf.mod.nox, jpf.mod.pH)
#pvalue is 0.7225 so not significant


#rerun model without terms we don't think are significant
jpf.mod.final = lme(sqrt(juvperF) ~ Temp, method="REML",random= ~1|FullTrt, data = pdt)
summary(jpf.mod.final)
#HONESTLY NOT USING THIS MODEL SO IDK WHY WE NEED THIS

#rerun full model to report
jpf.mod.finalfull = lme(sqrt(juvperF) ~ Temp + pH + Temp:pH, method="REML",random= ~1|FullTrt, data = pdt)
summary(jpf.mod.finalfull)

REest = (0.05088352)^2 #from Random Effect Intercept in summary
res.est = (0.149379)^2 #from random effect residual in summary

#Validate Original Model Assumptions
qqnorm(resid(jpf.mod.finalfull))
qqline(resid(jpf.mod.finalfull)) #v nice

#plotting residuals against 2 predictors in the model
#for continuous variables: want to see a nice starry night of random noise
#for categorical variables: want to see even box plots

plot(resid(jpf.mod.finalfull)~pdt$Temp) #bar means evenish, high temp has more low outliers but low temp has a high outlier
plot(resid(jpf.mod.finalfull)~pdt$pH) #bar means pretty even, high pH has more outliers
#overall, looks like we aren't violating any assumptions


# EGGS/JUVENILES PER FEMALE -----------------------------

# calculate eggs per female per dish
pdt$egg_juv <- pdt$Eggs+pdt$Juveniles
pdt$EJperF <- pdt$egg_juv/pdt$Females

#quickly graph data
boxplot(pdt$EJperF ~ Treatment, pdt)
#hmm a lot of overlap in errors
#generally looks like low temps have more eggs per f than high temps
#generally looks the same for pH - more eggs per F in low pH than high pH
#v interesting

#data exploration
#purpose: plot the histogram and dotchart for each continuous response and fixed variable - if there are random variables, check those impacts on each of the main variables. Histograms - looking for a nice even normal distrbution, no skews. Dotcharts - looking for random spread, no clusters. 
hist(pdt$EJperF) #pretty normal
dotchart(pdt$EJperF, color=pdt$Trt_Rep) #a bit clustered in middle

#TESTING ASSUMPTIONS AND CHOOSING TRANSFORMATIONS
#build main model lme argument (allows for random effects as opposed to lm)
opf.mod.noT = lme(EJperF ~ Temp*pH,random= ~1|FullTrt, data = pdt) #modeling the effects of Temp and pH with random effects of Dish (Trt_Rep)
opf.res =as.numeric(residuals(opf.mod.noT)) #create a value for the residuals to do tests oof LMM assumptions

#testing normality
hist(opf.res) #normal looking
dotchart(opf.res) #relatively scattered 
qqnorm(opf.res)
qqline(opf.res) #wow very nice actually
shapiro.test(opf.res) #p=0.6079 so passed test of normality!
#testing homogeneity of variances
leveneTest(EJperF ~ Temp, data=pdt) #p=0.2298
leveneTest(EJperF ~ pH, data=pdt) #p=0.01457
leveneTest(EJperF ~ Temp:pH, data=pdt) #p=0.04349
leveneTest(EJperF ~ Temp*pH, data=pdt) #p=0.04349, ok so we can def say variances not all homogenous
plot(opf.mod.noT) #residuals pretty even

#ok so without a transformation, the residual variances are not homogenous, but the residuals aro normal. So let's try a sqrt transformation and see what that does
#build main model lme argument (allows for random effects as opposed to lm)
opf.mod.sqrt = lme(sqrt(EJperF) ~ Temp*pH,random= ~1|FullTrt, data = pdt) #modeling the effects of Temp and pH with random effects of Dish (Trt_Rep)
opf.res.sqrt =as.numeric(residuals(epf.mod.sqrt)) #create a value for the residuals to do tests oof LMM assumptions
#testing normality
hist(opf.res.sqrt) #def looking normal
dotchart(opf.res.sqrt) #well scattered
qqnorm(opf.res.sqrt)
qqline(opf.res.sqrt) #pretty good, almost a bit worse near low end tho
shapiro.test(opf.res.sqrt) #p=0.02627 so almost passes test of normality
#let's make sure variances are still homogenous
leveneTest(sqrt(EJperF) ~ Temp, data=pdt) #p=0.2353
leveneTest(sqrt(EJperF) ~ pH, data=pdt) #p=0.02973
leveneTest(sqrt(EJperF) ~ Temp:pH, data=pdt) #p=0.08229
leveneTest(sqrt(EJperF) ~ Temp*pH, data=pdt) #p=0.08229, ok so we now almost homogeneity of variances and normality
plot(opf.mod.sqrt) #residuals maybe a bit low

#ok let's try a log transformation and see what that does
#build main model lme argument (allows for random effects as opposed to lm)
opf.mod.log = lme(log(EJperF) ~ Temp*pH,random= ~1|FullTrt, data = pdt) #modeling the effects of Temp and pH with random effects of Dish (Trt_Rep)
opf.res.log =as.numeric(residuals(opf.mod.log)) #create a value for the residuals to do tests oof LMM assumptions
#testing normality
hist(opf.res.log) #normal, slightly skewed to right
dotchart(opf.res.log) #well scattered, slightly skewed to right
qqnorm(opf.res.log)
qqline(opf.res.log) #pretty good
shapiro.test(opf.res.log) #p=0.2424 so passes test of normality!
#let's make sure variances are still homogenous
leveneTest(log(EJperF) ~ Temp, data=pdt) #p=0.2454
leveneTest(log(EJperF) ~ pH, data=pdt) #p=0.06177
leveneTest(log(EJperF) ~ Temp:pH, data=pdt) #p=0.1533
leveneTest(log(EJperF) ~ Temp*pH, data=pdt) #p=0.1533, ok so we now kinda have homogeneity of variances and normality
plot(opf.mod.log) #residuals well scattered, but one low outlier

#We will choose LOG transformation because it allows residual to be both normal and keep homogeneity of variances. 



#MODEL VALIDATION

#Starting to go through the nesting process
opf.mod.main = lme(log(EJperF) ~ Temp + pH + Temp:pH, method="REML",random= ~1|FullTrt, data = pdt)

#Determine proper random structure using REML - remove 1 random effect at a time and finally all random effects
opf.mod.rtest = gls(log(EJperF) ~ Temp + pH + Temp:pH, data = pdt)
#compare models with and without random effects
anova(opf.mod.main, opf.mod.rtest)
#p value =0.3048 so Dish(Trt_Rep) does not explain variation


#Determine proper fixed structure using ML

#First, we check the Interactions
opf.mod.ml = lme(log(EJperF) ~ Temp + pH + Temp:pH, method="ML",random= ~1|FullTrt, data = pdt)
#checking Temp:pH intxn
opf.mod.txp = lme(log(EJperF) ~ Temp + pH, method="ML",random= ~1|FullTrt, data = pdt)
anova(opf.mod.ml, opf.mod.txp)
#p value = 0.9373 so looks like Temp x pH is not significant


#Next we check the individual effects
#new full model without interaction terms
opf.mod.nox = lme(log(EJperF) ~ Temp + pH, method="ML",random= ~1|FullTrt, data = pdt)
#checking Temp
opf.mod.temp = lme(log(EJperF) ~ pH, method="ML",random= ~1|FullTrt, data = pdt)
anova(opf.mod.nox, opf.mod.temp)
#pvalue is 0.6331 so not significant
#checking pH
opf.mod.pH = lme(log(EJperF) ~ Temp, method="ML",random= ~1|FullTrt, data = pdt)
anova(opf.mod.nox, opf.mod.pH)
#pvalue is 0.0221 so significant


#rerun model without terms we don't think are significant
opf.mod.final = lme(log(EJperF) ~ Temp, method="REML",random= ~1|FullTrt, data = pdt)
summary(opf.mod.final)
#HONESTLY NOT USING THIS MODEL SO IDK WHY WE NEED THIS

#rerun full model to report
opf.mod.finalfull = lme(log(EJperF) ~ Temp + pH + Temp:pH, method="REML",random= ~1|FullTrt, data = pdt)
summary(opf.mod.finalfull)

REest = (0.07822096)^2 #from Random Effect Intercept in summary
res.est = (0.234586)^2 #from random effect residual in summary

#Validate Original Model Assumptions
qqnorm(resid(opf.mod.finalfull))
qqline(resid(opf.mod.finalfull)) #ooh v close to the line, very nice

#plotting residuals against 2 predictors in the model
#for continuous variables: want to see a nice starry night of random noise
#for categorical variables: want to see even box plots

plot(resid(opf.mod.finalfull)~pdt$Temp) #bar means evenish, high temp has more low outliers but low temp has a high outlier
plot(resid(opf.mod.finalfull)~pdt$pH) #bar means pretty even, high pH has more outliers
#overall, looks like we aren't violating any assumptions

# CALCULATE MEANS AND SDS --------------

#first, we have a lot of columns, so select the columns desired
pdt2 = pdt[,c(4,8:11,15:19)]

#create a new df with means(sd) for each variable
means_sd = pdt2 %>%
  group_by(Treatment) %>% 
  summarise_all(list(~ str_c(round(mean(.), 2), " (", round(sd(.), 2), ")")))


