#Nereo Week 3 Model Comparison

rm(list = ls())

#Set WD
setwd("~/Desktop/Nereo/NereoAnalysis/NEREO")

#Packages Needed
library(readxl)
library(lme4)
library(nlme)
library(tidyverse)
library(car)


# Base model - run for each week lmer(Size/Number~ Temp + pH  + Temp:pH +  (1|Dish))

#pulling data
library(readxl)
data <- read_excel("Wk3Wk4DataAnalysis/wk3wk4_Nereo_counts.xlsx")
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

#create new variable for random effect
data$FullTrt = paste(data$Treatment,"_",data$Trt_Rep)

#hmmm. NAs were introduced for that last one  let's isolate the NAs to see where they were.
#library(tidyverse)
#NAdt = data %>% 
  #filter(is.na(Number))
#ok this does make sense - Wk4_AB1_x4_2 is a duplicate photo of AB1_x4_3 - all in Week 4 so don't worry about here

#2. need to split data set in two - into week3 and week 4 separately. need to think a little more about how to compare across weeks

library(tidyverse)
wk3dt = data %>% 
  select(Week, Temp, pH, Treatment, Trt_Rep, Photo_Rep, FullTrt, File_name,Type,Number) %>%
  filter(Week == "Week3") %>% 
  spread(Type, Number)

#3. I don't think we are including "type" as a variable in the model - rather each type should be analyzed separately, with the exception of productive vs. non-productive? Those can be analyzed together and be a variable in the model I believe. There may be some other model we have to do as well looking at # eggs and # juveniles as a fuction of # females, but we will think more about that one later. 


# WEEK 3 FEMALE DATA -------------------------------------------------------------------

#prospective model: lmer(Number~ Temp + pH  + Temp:pH +  (1|Trt_Rep), data=wk3dt)

#DATA EXPLORATION
#quickly graph data by Treatment
boxplot(Females ~ Treatment, wk3dt)
#hmm a lot of overlap in errors
#generally looks like low temps have higher # of females than high temps
#no strong trend for pH

#purpose: plot the histogram and dotchart for each continuous response and fixed variable - if there are random variables, check those impacts on each of the main variables. Histograms - looking for a nice even normal distrbution, no skews. Dotcharts - looking for random spread, no clusters. 
hist(wk3dt$Females) #normalish but kinda skewed to left
dotchart(wk3dt$Females, color=wk3dt$Trt_Rep) #oof v clustered on the left

#TESTING ASSUMPTIONS AND CHOOSING TRANSFORMATIONS
#build main model lme argument (allows for random effects as opposed to lm)
f.mod.noT = lme(Females ~ Temp*pH,random= ~1|FullTrt, data = wk3dt) #modeling the effects of Temp and pH with random effects of Dish (Trt_Rep)
f.res =as.numeric(residuals(f.mod.noT)) #create a value for the residuals to do tests oof LMM assumptions

#testing normality
hist(f.res) #normalish but kinda skewed to left
dotchart(f.res) #v clustered on the left
qqnorm(f.res)
qqline(f.res) #not great alignment tbh
shapiro.test(f.res) #p<<<0.05 so didn't pass test of normality
#testing homogeneity of variances
leveneTest(Females ~ Temp, data=wk3dt) #p=0.5257
leveneTest(Females ~ pH, data=wk3dt) #p=0.2559
leveneTest(Females ~ Temp:pH, data=wk3dt) #p=0.2509
leveneTest(Females ~ Temp*pH, data=wk3dt) #p=0.2509, ok so we can def say variances all homogenous
plot(f.mod.noT) #residuals clustered in two areas? interesting - not sure what to do about that

#ok so without a transformation, the residual variances are all homogenous, but the residuals are def not normal. So let's try a sqrt transformation and see what that does
#build main model lme argument (allows for random effects as opposed to lm)
f.mod.sqrt = lme(sqrt(Females) ~ Temp*pH,random= ~1|FullTrt, data = wk3dt) #modeling the effects of Temp and pH with random effects of Dish (Trt_Rep)
f.res.sqrt =as.numeric(residuals(f.mod.sqrt)) #create a value for the residuals to do tests oof LMM assumptions
#testing normality
hist(f.res.sqrt) #def looking more normal, less skewed
dotchart(f.res.sqrt) #still kinda clustered on the left, but more spread out
qqnorm(f.res.sqrt)
qqline(f.res.sqrt) #not great alignment but a bit better
shapiro.test(f.res.sqrt) #p<<<0.05 so still didn't pass test of normality
#let's make sure variances are still homogenous
leveneTest(sqrt(Females) ~ Temp, data=wk3dt) #p=0.08395
leveneTest(sqrt(Females) ~ pH, data=wk3dt) #p=0.2718
leveneTest(sqrt(Females) ~ Temp:pH, data=wk3dt) #p=0.08509
leveneTest(sqrt(Females) ~ Temp*pH, data=wk3dt) #p=0.08509, ok so we still have homogeneity of variances
plot(f.mod.sqrt) #residuals kinda clustered in two areas? interesting - not sure what to do about that

summary(f.mod.sqrt)

#let's try a log transformation now and see what that does
#build main model lme argument (allows for random effects as opposed to lm)
f.mod.log = lme(log(Females) ~ Temp*pH,random= ~1|FullTrt, data = wk3dt) #modeling the effects of Temp and pH with random effects of Dish (Trt_Rep)
f.res.log =as.numeric(residuals(f.mod.log)) #create a value for the residuals to do tests oof LMM assumptions
#testing normality
hist(f.res.log) #ooh way more normal
dotchart(f.res.log) #def more spread out
qqnorm(f.res.log)
qqline(f.res.log) #way better
shapiro.test(f.res.log) #p=.07418 so passes test of normality! 
#let's make sure variances are still homogenous
leveneTest(log(Females) ~ Temp, data=wk3dt) #p=0.0017
leveneTest(log(Females) ~ pH, data=wk3dt) #p=0.332
leveneTest(log(Females) ~ Temp:pH, data=wk3dt) #p=0.004708
leveneTest(log(Females) ~ Temp*pH, data=wk3dt) #p=0.004708, ok so we now lost homogeneity of variances :/
plot(f.mod.log) #residuals kinda clustered in two areas? interesting - not sure what to do about that


#We will choose sqrt transformation because it is better to achieve homogeneity of variances than normality of residuals


#MODEL VALIDATION

#build the full model we will be validating with lme argument (allows for random effects as opposed to lm)
f.mod.full = lme(sqrt(Females) ~ Temp + pH + Temp:pH,random= ~1|FullTrt, data = wk3dt) #modeling the effects of Temp and pH with random effects of Dish (Trt_Rep)

#Starting to go through the nesting process
#build main model with all terms
f.mod.main = lme(sqrt(Females) ~ Temp + pH + Temp:pH, method="REML",random= ~1|FullTrt, data = wk3dt)
#Determine proper random structure using REML - remove 1 random effect at a time and finally all random effects
f.mod.rtest = gls(sqrt(Females) ~ Temp + pH + Temp:pH, data = wk3dt)
#compare models with and without random effects
anova(f.mod.main, f.mod.rtest)
#p value =0.0246 so Dish(Trt_Rep) does kinda explain variation???



#Determine proper fixed structure using ML

#First, we check the Interactions
f.mod.ml = lme(sqrt(Females) ~ Temp + pH + Temp:pH, method="ML",random= ~1|FullTrt, data = wk3dt)
#checking Temp:pH intxn
f.mod.txp = lme(sqrt(Females) ~ Temp + pH, method="ML",random= ~1|FullTrt, data = wk3dt)
anova(f.mod.ml, f.mod.txp)
#p value = 0.6265 so looks like Temp x pH is not significant


#Next we check the individual effects
#new full model without interaction terms
f.mod.nox = lme(sqrt(Females) ~ Temp + pH, method="ML",random= ~1|FullTrt, data = wk3dt)
#checking Temp
f.mod.temp = lme(sqrt(Females) ~ pH, method="ML",random= ~1|FullTrt, data = wk3dt)
anova(f.mod.nox, f.mod.temp)
#pvalue is <.0001 so significant
#checking pH
f.mod.pH = lme(sqrt(Females) ~ Temp, method="ML",random= ~1|FullTrt, data = wk3dt)
anova(f.mod.nox, f.mod.pH)
#pvalue is 0.8795 so not significant


#rerun model without terms we don't think are significant
f.mod.final = lme(sqrt(Females) ~ Temp, method="REML",random= ~1|FullTrt, data = wk3dt)
summary(f.mod.final)

#rerun full model to report
f.mod.finalfull = lme(sqrt(Females) ~ Temp + pH + Temp:pH, method="REML",random= ~1|FullTrt, data = wk3dt)
summary(f.mod.finalfull)

REest = (0.3253689)^2 #from Random Effect Intercept in summary
res.est = (0.6095301)^2 #from random effect residual in summary

#Validate Original Model Assumptions
qqnorm(resid(f.mod.finalfull))
qqline(resid(f.mod.finalfull)) #majority of QQ pts lie along line, so looks good! there is some skew along the ends tho

#plotting residuals against 2 predictors in the model
#for continuous variables: want to see a nice starry night of random noise
#for categorical variables: want to see even box plots

plot(resid(f.mod.finalfull)~wk3dt$Temp) #bar means pretty even, high temp has more outliers
plot(resid(f.mod.finalfull)~wk3dt$pH) #bar means pretty even, high pH has more outliers
#overall, looks like we aren't violating any assumptions



# WEEK 3 MALE DATA -------------------------------------------------------------------

#prospective model: lmer(Number~ Temp + pH  + Temp:pH +  (1|Trt_Rep), data=wk3dt)

#quickly graph data by Treatment
boxplot(Males ~ Treatment, wk3dt)
#hmm a lot of overlap in errors
#generally looks like low temps have higher # of males than high temps
#pH only has trend at low temp - low pH has more #s than high pH

#data exploration
#purpose: plot the histogram and dotchart for each continuous response and fixed variable - if there are random variables, check those impacts on each of the main variables. Histograms - looking for a nice even normal distrbution, no skews. Dotcharts - looking for random spread, no clusters. 
hist(wk3dt$Males) #normalish but kinda skewed to left
dotchart(wk3dt$Males, color=wk3dt$Trt_Rep) #clustered on the left

#TESTING ASSUMPTIONS AND CHOOSING TRANSFORMATIONS
#build main model lme argument (allows for random effects as opposed to lm)
m.mod.noT = lme(Males ~ Temp*pH,random= ~1|FullTrt, data = wk3dt) #modeling the effects of Temp and pH with random effects of Dish (Trt_Rep)
m.res =as.numeric(residuals(m.mod.noT)) #create a value for the residuals to do tests oof LMM assumptions

#testing normality
hist(m.res) #quite normal looking
dotchart(m.res) #nice spread honestly, not that clustered
qqnorm(m.res)
qqline(m.res) #pretty good at center, just weird at high end
shapiro.test(m.res) #p<<<0.05 so didn't pass test of normality
#testing homogeneity of variances
leveneTest(Males ~ Temp, data=wk3dt) #p=0.6513
leveneTest(Males ~ pH, data=wk3dt) #p=0.2175
leveneTest(Males ~ Temp:pH, data=wk3dt) #p=0.437
leveneTest(Males ~ Temp*pH, data=wk3dt) #p=0.437, ok so we can def say variances all homogenous
plot(m.mod.noT) #residuals look pretty even except on right side

#ok so without a transformation, the residual variances are all homogenous, but the residuals are def not normal. So let's try a sqrt transformation and see what that does
#build main model lme argument (allows for random effects as opposed to lm)
m.mod.sqrt = lme(sqrt(Males) ~ Temp*pH,random= ~1|FullTrt, data = wk3dt) #modeling the effects of Temp and pH with random effects of Dish (Trt_Rep)
m.res.sqrt =as.numeric(residuals(m.mod.sqrt)) #create a value for the residuals to do tests oof LMM assumptions
#testing normality
hist(m.res.sqrt) #still normal
dotchart(m.res.sqrt) #still kinda clustered in center
qqnorm(m.res.sqrt)
qqline(m.res.sqrt) #worse on lower end, but better on higher end
shapiro.test(m.res.sqrt) #p=0.000255 so still didn't pass test of normality, but closer?
#let's make sure variances are still homogenous
leveneTest(sqrt(Males) ~ Temp, data=wk3dt) #p=0.2857
leveneTest(sqrt(Males) ~ pH, data=wk3dt) #p=0.5363
leveneTest(sqrt(Males) ~ Temp:pH, data=wk3dt) #p=0.2219
leveneTest(sqrt(Males) ~ Temp*pH, data=wk3dt) #p=0.2219, ok so we still have homogeneity of variances
plot(m.mod.sqrt) #residuals kinda clustered in two areas? interesting - not sure what to do about that

#let's try a log transformation now and see what that does
#build main model lme argument (allows for random effects as opposed to lm)
m.mod.log = lme(log(Males+1) ~ Temp*pH,random= ~1|FullTrt, data = wk3dt) #modeling the effects of Temp and pH with random effects of Dish (Trt_Rep)
#need to do Males+1 bc error arising
m.res.log =as.numeric(residuals(m.mod.log)) #create a value for the residuals to do tests oof LMM assumptions
#testing normality
hist(m.res.log) #still pretty normal looking, just shifted slightly to the right
dotchart(m.res.log) #shifted to the right
qqnorm(m.res.log)
qqline(m.res.log) #oof way worse
shapiro.test(m.res.log) #p<<<0.05 so still not passing test of normality
#let's make sure variances are still homogenous
leveneTest(log(Males+1) ~ Temp, data=wk3dt) #p=0.04537
leveneTest(log(Males+1) ~ pH, data=wk3dt) #p=0.7854
leveneTest(log(Males+1) ~ Temp:pH, data=wk3dt) #error
leveneTest(log(Males+1) ~ Temp*pH, data=wk3dt) #error, ok so we now lost homogeneity of variances :/
plot(m.mod.log) #residuals v clustered in two areas

#We will choose sqrt transformation because better to have homogeneity of variance than normality of resids - also we never actually achieved normality 

#MODEL VALIDATION
#Starting to go through the nesting process
m.mod.main = lme(sqrt(Males) ~ Temp + pH + Temp:pH, method="REML",random= ~1|FullTrt, data = wk3dt)

#Determine proper random structure using REML - remove 1 random effect at a time and finally all random effects
m.mod.rtest = gls(sqrt(Males) ~ Temp + pH + Temp:pH, data = wk3dt)
#compare models with and without random effects
anova(m.mod.main, m.mod.rtest)
#p value 0.3966 so Dish(Trt_Rep) doesnt explain variation


#Determine proper fixed structure using ML

#First, we check the Interactions
m.mod.ml = lme(sqrt(Males) ~ Temp + pH + Temp:pH, method="ML",random= ~1|FullTrt, data = wk3dt)
#checking Temp:pH intxn
m.mod.txp = lme(sqrt(Males) ~ Temp + pH, method="ML",random= ~1|FullTrt, data = wk3dt)
anova(m.mod.ml, m.mod.txp)
#p value = 0.147 so looks like Temp x pH is not significant


#Next we check the individual effects
#new full model without interaction terms
m.mod.nox = lme(sqrt(Males) ~ Temp + pH, method="ML",random= ~1|FullTrt, data = wk3dt)
#checking Temp
m.mod.temp = lme(sqrt(Males) ~ pH, method="ML",random= ~1|FullTrt, data = wk3dt)
anova(m.mod.nox, m.mod.temp)
#pvalue is <.0001 so significant
#checking pH
m.mod.pH = lme(sqrt(Males) ~ Temp, method="ML",random= ~1|FullTrt, data = wk3dt)
anova(m.mod.nox, m.mod.pH)
#pvalue is 0.2296 so not significant


#rerun model without terms we don't think are significant 
#(REALLY DOESNT MAKE SENSE TO RUN  CODE WITH AN INTXN THAT DOESNT INCLUDE ONE OF THE INTERACTING TERMS)
m.mod.final = lme(sqrt(Males) ~ Temp + Temp:pH, method="REML",random= ~1|FullTrt, data = wk3dt)
summary(m.mod.final)

#rerun full model to report
m.mod.finalfull = lme(sqrt(Males) ~ Temp + pH + Temp:pH, method="REML",random= ~1|FullTrt, data = wk3dt)
summary(m.mod.finalfull)

REest = ( 0.2305993)^2
res.est = (0.7718666)^2

#Validate Original Model Assumptions
qqnorm(resid(m.mod.finalfull))
qqline(resid(m.mod.finalfull)) #majority of QQ pts lie along line, so looks good! there is some skew along the ends tho

#plotting residuals against 2 predictors in the model
#for continuous variables: want to see a nice starry night of random noise
#for categorical variables: want to see even box plots

plot(resid(m.mod.finalfull)~wk3dt$Temp) #bar means pretty even, high temp has more outliers
plot(resid(m.mod.finalfull)~wk3dt$pH) #bar means pretty even, high pH has more outliers
#overall, looks like we aren't violating any assumptions




# WEEK 3 JUVENILE DATA -------------------------------------------------------------------

#prospective model: lmer(Number~ Temp + pH  + Temp:pH +  (1|Trt_Rep), data=wk3dt)

#quickly graph data by Treatment
boxplot(Juveniles ~ Treatment, wk3dt)
#hmm a lot of overlap in errors
#generally looks like low temps have lower # of juveniles than high temps
#generally looks like low pH has lower # of juveniles than high pH

#data exploration
#purpose: plot the histogram and dotchart for each continuous response and fixed variable - if there are random variables, check those impacts on each of the main variables. Histograms - looking for a nice even normal distrbution, no skews. Dotcharts - looking for random spread, no clusters. 
hist(wk3dt$Juveniles) #normalish but kinda skewed to left
dotchart(wk3dt$Juveniles, color=wk3dt$Trt_Rep) #oof v clustered on the left

#TESTING ASSUMPTIONS AND CHOOSING TRANSFORMATIONS
#build main model lme argument (allows for random effects as opposed to lm)
j.mod.noT = lme(Juveniles ~ Temp*pH,random= ~1|FullTrt, data = wk3dt) #modeling the effects of Temp and pH with random effects of Dish (Trt_Rep)
j.res =as.numeric(residuals(j.mod.noT)) #create a value for the residuals to do tests oof LMM assumptions

#testing normality
hist(j.res) #normalish, but skewed to left
dotchart(j.res) #kinda clustered on the left
qqnorm(j.res)
qqline(j.res) #not great alignment tbh
shapiro.test(j.res) #p<<<0.05 so didn't pass test of normality
#testing homogeneity of variances
leveneTest(Juveniles ~ Temp, data=wk3dt) #p=0.06067
leveneTest(Juveniles ~ pH, data=wk3dt) #p=0.08935
leveneTest(Juveniles ~ Temp:pH, data=wk3dt) #p=0.02339
leveneTest(Juveniles ~ Temp*pH, data=wk3dt) #p=0.02339, ok so residuals are neither normal nor have homogeneity of variances
plot(j.mod.noT) #residuals clustered toward left

#ok So let's try a sqrt transformation and see what that does
#build main model lme argument (allows for random effects as opposed to lm)
j.mod.sqrt = lme(sqrt(Juveniles) ~ Temp*pH,random= ~1|FullTrt, data = wk3dt) #modeling the effects of Temp and pH with random effects of Dish (Trt_Rep)
j.res.sqrt =as.numeric(residuals(j.mod.sqrt)) #create a value for the residuals to do tests oof LMM assumptions
#testing normality
hist(j.res.sqrt) #def looking more normal
dotchart(j.res.sqrt) # more spread out
qqnorm(j.res.sqrt)
qqline(j.res.sqrt) #ooh v nice alignment
shapiro.test(j.res.sqrt) #p=0.924 so passed test of normality!
#let's make sure variances are still homogenous
leveneTest(sqrt(Juveniles) ~ Temp, data=wk3dt) #p=0.3388
leveneTest(sqrt(Juveniles) ~ pH, data=wk3dt) #p=0.6969
leveneTest(sqrt(Juveniles) ~ Temp:pH, data=wk3dt) #p=0.2995
leveneTest(sqrt(Juveniles) ~ Temp*pH, data=wk3dt) #p=0.2995, ok so we still have homogeneity of variances
plot(j.mod.sqrt) #residuals nicely spread out

#We will choose sqrt transformation because it easily improved both normality and homogeneity of variances for residuals. Also the data is count data, so this is a reasonable transformation choice.


#MODEL VALIDATION

#Starting to go through the nesting process
j.mod.main = lme(sqrt(Juveniles) ~ Temp + pH + Temp:pH, method="REML",random= ~1|FullTrt, data = wk3dt)

#Determine proper random structure using REML - remove 1 random effect at a time and finally all random effects
j.mod.rtest = gls(sqrt(Juveniles) ~ Temp + pH + Temp:pH, data = wk3dt)
#compare models with and without random effects
anova(j.mod.main, j.mod.rtest)
#p value 0.0263 so Dish(Trt_Rep) does  explain some variation



#Determine proper fixed structure using ML

#First, we check the Interactions
j.mod.ml = lme(sqrt(Juveniles) ~ Temp + pH + Temp:pH, method="ML",random= ~1|FullTrt, data = wk3dt)
#checking Temp:pH intxn
j.mod.txp = lme(sqrt(Juveniles) ~ Temp + pH, method="ML",random= ~1|FullTrt, data = wk3dt)
anova(j.mod.ml, j.mod.txp)
#p value = 0.6736 so looks like Temp x pH is not significant


#Next we check the individual effects
#new full model without interaction terms
j.mod.nox = lme(sqrt(Juveniles) ~ Temp + pH, method="ML",random= ~1|FullTrt, data = wk3dt)
#checking Temp
j.mod.temp = lme(sqrt(Juveniles) ~ pH, method="ML",random= ~1|FullTrt, data = wk3dt)
anova(j.mod.nox, j.mod.temp)
#pvalue is 0.0175 so significant
#checking pH
j.mod.pH = lme(sqrt(Juveniles) ~ Temp, method="ML",random= ~1|FullTrt, data = wk3dt)
anova(j.mod.nox, j.mod.pH)
#pvalue is 0.0015 so significant


#rerun model without terms we don't think are significant
j.mod.final = lme(sqrt(Juveniles) ~ Temp +pH, method="REML",random= ~1|FullTrt, data = wk3dt)
summary(j.mod.final)

#rerun full model to report
j.mod.finalfull = lme(sqrt(Juveniles) ~ Temp + pH + Temp:pH, method="REML",random= ~1|FullTrt, data = wk3dt)
summary(j.mod.finalfull)

#taking values from residuals line at top of summary and squaring them to convert SD to variance
REest = (0.3575791)^2
res.est = (0.6751944)^2

#Validate Original Model Assumptions
qqnorm(resid(j.mod.finalfull))
qqline(resid(j.mod.finalfull)) #majority of QQ pts lie along line, so looks good! there is some skew along the ends tho

#plotting residuals against 2 predictors in the model
#for continuous variables: want to see a nice starry night of random noise
#for categorical variables: want to see even box plots

plot(resid(j.mod.finalfull)~wk3dt$Temp) #bar means pretty even, high temp has smaller range
plot(resid(j.mod.finalfull)~wk3dt$pH) #bar means pretty even, high pH has smaller range
#overall, looks like we aren't violating any assumptions




# WEEK 3 EGG DATA -------------------------------------------------------------------

#prospective model: lmer(Number~ Temp + pH  + Temp:pH +  (1|Trt_Rep), data=wk3dt)

#quickly graph data by Treatment
boxplot(Eggs ~ Treatment, wk3dt)
#hmm a lot of overlap in errors
#generally looks like low temps have higher # of eggs than high temps
#no strong trend for pH

#data exploration
#purpose: plot the histogram and dotchart for each continuous response and fixed variable - if there are random variables, check those impacts on each of the main variables. Histograms - looking for a nice even normal distrbution, no skews. Dotcharts - looking for random spread, no clusters. 
hist(wk3dt$Eggs) #oooh v skewed to left
dotchart(wk3dt$Eggs, color=wk3dt$Trt_Rep) #oof v clustered on the left


#TESTING ASSUMPTIONS AND CHOOSING TRANSFORMATIONS
#build main model lme argument (allows for random effects as opposed to lm)
e.mod.noT = lme(Eggs ~ Temp*pH,random= ~1|FullTrt, data = wk3dt) #modeling the effects of Temp and pH with random effects of Dish (Trt_Rep)
e.res =as.numeric(residuals(e.mod.noT)) #create a value for the residuals to do tests oof LMM assumptions

#testing normality
hist(e.res) #normalish but kinda skewed to left
dotchart(e.res) #v clustered on the left
qqnorm(e.res)
qqline(e.res) #not great alignment tbh
shapiro.test(e.res) #p<<<0.05 so didn't pass test of normality
#testing homogeneity of variances
leveneTest(Eggs ~ Temp, data=wk3dt) #p=0.9859
leveneTest(Eggs ~ pH, data=wk3dt) #p=0.423
leveneTest(Eggs ~ Temp:pH, data=wk3dt) #p=0.9894
leveneTest(Eggs ~ Temp*pH, data=wk3dt) #p=0.9894, ok so we can def say variances all homogenous
plot(e.mod.noT) #residuals kinda trend high

#ok so without a transformation, the residual variances are all homogenous, but the residuals are def not normal. So let's try a sqrt transformation and see what that does
#build main model lme argument (allows for random effects as opposed to lm)
e.mod.sqrt = lme(sqrt(Eggs) ~ Temp*pH,random= ~1|FullTrt, data = wk3dt) #modeling the effects of Temp and pH with random effects of Dish (Trt_Rep)
e.res.sqrt =as.numeric(residuals(e.mod.sqrt)) #create a value for the residuals to do tests oof LMM assumptions
#testing normality
hist(e.res.sqrt) #def looking more normal, less skewed
dotchart(e.res.sqrt) #still kinda clustered on the left, but more spread out
qqnorm(e.res.sqrt)
qqline(e.res.sqrt) #ooh v nice actually
shapiro.test(e.res.sqrt) #p=0.6942 so passes test of normality!
#let's make sure variances are still homogenous
leveneTest(sqrt(Eggs) ~ Temp, data=wk3dt) #p=0.1876
leveneTest(sqrt(Eggs) ~ pH, data=wk3dt) #p=0.1355
leveneTest(sqrt(Eggs) ~ Temp:pH, data=wk3dt) #p=0.5949
leveneTest(sqrt(Eggs) ~ Temp*pH, data=wk3dt) #p=0.5949, ok so we still have homogeneity of variances
plot(e.mod.sqrt) #residuals much more scattered

#We will choose sqrt transformation because it gets residuals normal while keeping homogeneity of variance. Also the data is count data, so this is a reasonable transformation choice.


#MODEL VALIDATION

#Starting to go through the nesting process
e.mod.main = lme(sqrt(Eggs) ~ Temp + pH + Temp:pH, method="REML",random= ~1|FullTrt, data = wk3dt)

#Determine proper random structure using REML - remove 1 random effect at a time and finally all random effects
e.mod.rtest = gls(sqrt(Eggs) ~ Temp + pH + Temp:pH, data = wk3dt)
#compare models with and without random effects
anova(e.mod.main, e.mod.rtest)
#p value .3824 so Dish(Trt_Rep) does not explain a lot of variation



#Determine proper fixed structure using ML

#First, we check the Interactions
e.mod.ml = lme(sqrt(Eggs) ~ Temp + pH + Temp:pH, method="ML",random= ~1|FullTrt, data = wk3dt)
#checking Temp:pH intxn
e.mod.txp = lme(sqrt(Eggs) ~ Temp + pH, method="ML",random= ~1|FullTrt, data = wk3dt)
anova(e.mod.ml, e.mod.txp)
#p value = 0.5173 so looks like Temp x pH is not significant


#Next we check the individual effects
#new full model without interaction terms
e.mod.nox = lme(sqrt(Eggs) ~ Temp + pH, method="ML",random= ~1|FullTrt, data = wk3dt)
#checking Temp
e.mod.temp = lme(sqrt(Eggs) ~ pH, method="ML",random= ~1|FullTrt, data = wk3dt)
anova(e.mod.nox, e.mod.temp)
#pvalue is 0.01 so significant
#checking pH
e.mod.pH = lme(sqrt(Eggs) ~ Temp, method="ML",random= ~1|FullTrt, data = wk3dt)
anova(e.mod.nox, e.mod.pH)
#pvalue is 0.2772 so notsignificant


#rerun model without terms we don't think are significant
e.mod.final = lme(sqrt(Eggs) ~ Temp, method="REML",random= ~1|FullTrt, data = wk3dt)
summary(e.mod.final)

#rerun full model to report
e.mod.finalfull = lme(sqrt(Eggs) ~ Temp + pH + Temp:pH, method="REML",random= ~1|FullTrt, data = wk3dt)
summary(e.mod.finalfull)

#taking values from residuals line at top of summary and squaring them to convert SD to variance
REest = (0.3485701)^2
res.est = (1.147534)^2

#Validate Original Model Assumptions
qqnorm(resid(e.mod.finalfull))
qqline(resid(e.mod.finalfull)) #majority of QQ pts have some skew along the ends tho

#plotting residuals against 2 predictors in the model
#for continuous variables: want to see a nice starry night of random noise
#for categorical variables: want to see even box plots

plot(resid(e.mod.finalfull)~wk3dt$Temp) #bar means pretty even, high temp has smaller range
plot(resid(e.mod.finalfull)~wk3dt$pH) #bar means pretty even, high pH has smaller range
#overall, looks like we aren't violating any assumptions



# # WEEK 3 PRODUCTIVITY DATA -------------------------------------------------------------------
#== A. proportion gametophytes productive
#creat new column with calculation of the proportion of productive females
wk3dt$Prop_prod <- wk3dt$Productive/wk3dt$Females
#quickly graph data
boxplot(Prop_prod ~ Treatment, wk3dt)

#data exploration
#purpose: plot the histogram and dotchart for each continuous response and fixed variable - if there are random variables, check those impacts on each of the main variables. Histograms - looking for a nice even normal distrbution, no skews. Dotcharts - looking for random spread, no clusters.
hist(wk3dt$Prop_prod) #of awful skew to right
dotchart(wk3dt$Prop_prod, color=wk3dt$Trt_Rep) #v clustered to the right

#TESTING ASSUMPTIONS AND CHOOSING TRANSFORMATIONS
#build main model lme argument (allows for random effects as opposed to lm)
pp.mod.noT = lme(Prop_prod ~ Temp*pH,random= ~1|FullTrt, data = wk3dt) #modeling the effects of Temp and pH with random effects of Dish (Trt_Rep)
pp.res =as.numeric(residuals(pp.mod.noT)) #create a value for the residuals to do tests oof LMM assumptions

#testing normality
hist(pp.res) #normalish but kinda skewed to right
dotchart(pp.res) #kinda clustered on right but still a bit scattered
qqnorm(pp.res)
qqline(pp.res) #not great alignment tbh
shapiro.test(pp.res) #p<<<0.05 so didn't pass test of normality
#testing homogeneity of variances
leveneTest(Prop_prod ~ Temp, data=wk3dt) #p=0.00511
leveneTest(Prop_prod ~ pH, data=wk3dt) #p=0.02228
leveneTest(Prop_prod ~ Temp:pH, data=wk3dt) #p=0.0005003
leveneTest(Prop_prod ~ Temp*pH, data=wk3dt) #p=0.0005003, ok so we can def say variances all very NOT homogenous
plot(pp.mod.noT) #residuals trend low

#ok so without a transformation, the residual variances are all homogenous, but the residuals are def not normal. So let's try an arcsin sqrt transformation (because this is proportion data) and see what that does
#build main model lme argument (allows for random effects as opposed to lm)
pp.mod.acsr = lme(asin(sqrt(Prop_prod)) ~ Temp*pH,random= ~1|FullTrt, data = wk3dt) #modeling the effects of Temp and pH with random effects of Dish (Trt_Rep)
pp.res.acsr =as.numeric(residuals(pp.mod.acsr)) #create a value for the residuals to do tests oof LMM assumptions
#testing normality
hist(pp.res.acsr) #def looking more normal, less skewed
dotchart(pp.res.acsr) # more spread out
qqnorm(pp.res.acsr)
qqline(pp.res.acsr) #much much better but still a bit wiggly
shapiro.test(pp.res.acsr) #p=0.006003 so didn't really pass test of normality, but not terribly far
#let's make sure variances are still homogenous
leveneTest(asin(sqrt(Prop_prod)) ~ Temp, data=wk3dt) #p=0.7805
leveneTest(asin(sqrt(Prop_prod)) ~ pH, data=wk3dt) #p=0.3299
leveneTest(asin(sqrt(Prop_prod)) ~ Temp:pH, data=wk3dt) #p=0.1898
leveneTest(asin(sqrt(Prop_prod)) ~ Temp*pH, data=wk3dt) #p=0.1898, ok so we have homogeneity of variances
plot(pp.mod.acsr) #residuals much more scattered

#We will not condct a linear model of this data - rather we will just calculate means and SDs

#Calculcate the means for each treatment
pp_means = wk3dt %>%
  group_by(Treatment) %>% 
  summarise(ave = mean(Prop_prod))

#calculate the SDs for each treatment
pp_sds = wk3dt %>%
  group_by(Treatment) %>% 
  summarise(ave = sd(Prop_prod))

# #MODEL VALIDATION - NOT USING THIS PROCESS
# 
# #Starting to go through the nesting process
# prod.mod.main = lme(asin(sqrt(Prop_prod)) ~ Temp + pH + Temp:pH, method="REML",random= ~1|FullTrt, data = wk3dt)
# 
# #Determine proper random structure using REML - remove 1 random effect at a time and finally all random effects
# prod.mod.rtest = gls(asin(sqrt(Prop_prod)) ~ Temp + pH + Temp:pH, data = wk3dt)
# #compare models with and without random effects
# anova(prod.mod.main, prod.mod.rtest)
# #p value .1593 so Dish(Trt_Rep) really does not explain a lot of variation
# 
# 
# 
# #Determine proper fixed structure using ML
# 
# #First, we check the Interactions
# prod.mod.ml = lme(asin(sqrt(Prop_prod)) ~ Temp + pH + Temp:pH, method="ML",random= ~1|FullTrt, data = wk3dt)
# #checking Temp:pH intxn
# prod.mod.txp = lme(asin(sqrt(Prop_prod)) ~ Temp + pH, method="ML",random= ~1|FullTrt, data = wk3dt)
# anova(prod.mod.ml, prod.mod.txp)
# #p value = 0.5221 so looks like Temp x pH is not significant
# 
# 
# #Next we check the individual effects
# #new full model without interaction terms
# prod.mod.nox = lme(asin(sqrt(Prop_prod)) ~ Temp + pH, method="ML",random= ~1|FullTrt, data = wk3dt)
# #checking Temp
# prod.mod.temp = lme(asin(sqrt(Prop_prod)) ~ pH, method="ML",random= ~1|FullTrt, data = wk3dt)
# anova(prod.mod.nox, prod.mod.temp)
# #pvalue is <0.0001 so significant
# #checking pH
# prod.mod.pH = lme(asin(sqrt(Prop_prod)) ~ Temp, method="ML",random= ~1|FullTrt, data = wk3dt)
# anova(prod.mod.nox, prod.mod.pH)
# #pvalue is 0.0067 so significant
# 
# 
# #rerun model without terms we don't think are significant
# prod.mod.final = lme(asin(sqrt(Prop_prod)) ~ Temp +pH, method="REML",random= ~1|FullTrt, data = wk3dt)
# summary(prod.mod.final)
# 
# #rerun full model to report
# prod.mod.finalfull = lme(asin(sqrt(Prop_prod)) ~ Temp + pH + Temp:pH, method="REML",random= ~1|FullTrt, data = wk3dt)
# summary(prod.mod.finalfull)
# 
# #taking values from residuals line at top of summary and squaring them to convert SD to variance
# REest = ( 0.08408908)^2
# res.est = (0.2106407)^2
# 
# #Validate Original Model Assumptions
# qqnorm(resid(prod.mod.finalfull)) 
# qqline(resid(prod.mod.finalfull)) #pretty good
# 
# #plotting residuals against 2 predictors in the model
# #for continuous variables: want to see a nice starry night of random noise
# #for categorical variables: want to see even box plots
# 
# plot(resid(prod.mod.finalfull)~wk3dt$Temp) #bar means not even, but ranges align more or less
# plot(resid(prod.mod.finalfull)~wk3dt$pH) #bar means not even, but ranges align more or less
# #overall, looks like we aren't violating any assumptions



#WEEK 3 EGGS PER FEMALE DATA ---------------

wk3dt$eggperF <- wk3dt$Eggs/wk3dt$Females
boxplot(eggperF ~ Treatment, wk3dt)

summary(
  lmer(eggperF ~ Temp + pH + (1|Trt_Rep), wk3dt)) 
# high temp low pH trt only one that seems diff from the rest

#TESTING ASSUMPTIONS AND CHOOSING TRANSFORMATIONS
#build main model lme argument (allows for random effects as opposed to lm)
epf.mod.noT = lme(eggperF ~ Temp*pH,random= ~1|FullTrt, data = wk3dt) #modeling the effects of Temp and pH with random effects of Dish (Trt_Rep)
epf.res =as.numeric(residuals(epf.mod.noT)) #create a value for the residuals to do tests oof LMM assumptions

#testing normality
hist(epf.res) #pretty normal
dotchart(epf.res) #pretty scattered
qqnorm(epf.res)
qqline(epf.res) #pretty ok alignment tbh
shapiro.test(epf.res) #p=0.3376 so passes test of normality
#testing homogeneity of variances
leveneTest(eggperF ~ Temp, data=wk3dt) #p=0.0374
leveneTest(eggperF ~ pH, data=wk3dt) #p=0.3836
leveneTest(eggperF ~ Temp:pH, data=wk3dt) #p=0.1507
leveneTest(eggperF ~ Temp*pH, data=wk3dt) #p=0.1507, ok so we can def say variances all homogenous
plot(epf.mod.noT) #residuals trend a teensy bit high

#ok so without a transformation, the data is normal and the residual variances are mostly homogenous, but I just want to try a sqrt transformation and see what that does - does it make all variances homogenous?pf
#build main model lme argument (allows for random effects as opposed to lm)
epf.mod.sqrt = lme(sqrt(eggperF) ~ Temp*pH,random= ~1|FullTrt, data = wk3dt) #modeling the effects of Temp and pH with random effects of Dish (Trt_Rep)
epf.res.sqrt =as.numeric(residuals(epf.mod.sqrt)) #create a value for the residuals to do tests oof LMM assumptions
#testing normality
hist(epf.res.sqrt) #normalish, slightly skewed to right
dotchart(epf.res.sqrt) #kinda clustered on the right, but still spread out
qqnorm(epf.res.sqrt)
qqline(epf.res.sqrt) #worse actually
shapiro.test(epf.res.sqrt) #p=0.003714 so doesn't pass test of normality
#let's make sure variances are still homogenous
leveneTest(sqrt(eggperF) ~ Temp, data=wk3dt) #p=0.0326
leveneTest(sqrt(eggperF) ~ pH, data=wk3dt) #p=0.6925
leveneTest(sqrt(eggperF) ~ Temp:pH, data=wk3dt) #p=0.3208
leveneTest(sqrt(eggperF) ~ Temp*pH, data=wk3dt) #p=0.3208, ok so we still miss one homogeneity of variances, but now not normal either
plot(epf.mod.sqrt) #residuals clustered in 2 groups

#We will choose NO transformation because it gets residuals normal while keeping homogeneity of variance (for everything but temp alone). 

#MODEL VALIDATION

#Starting to go through the nesting process
epf.mod.main = lme(eggperF ~ Temp + pH + Temp:pH, method="REML",random= ~1|FullTrt, data = wk3dt)

#Determine proper random structure using REML - remove 1 random effect at a time and finally all random effects
epf.mod.rtest = gls(eggperF ~ Temp + pH + Temp:pH, data = wk3dt)
#compare models with and without random effects
anova(epf.mod.main, epf.mod.rtest)
#p value .6302 so Dish(Trt_Rep) really does not explain a lot of variation



#Determine proper fixed structure using ML

#First, we check the Interactions
epf.mod.ml = lme(eggperF ~ Temp + pH + Temp:pH, method="ML",random= ~1|FullTrt, data = wk3dt)
#checking Temp:pH intxn
epf.mod.txp = lme(eggperF ~ Temp + pH, method="ML",random= ~1|FullTrt, data = wk3dt)
anova(epf.mod.ml, epf.mod.txp)
#p value = 0.0521 so looks like Temp x pH is an edge of significant


#Next we check the individual effects
#new full model without interaction terms
epf.mod.nox = lme(eggperF ~ Temp + pH, method="ML",random= ~1|FullTrt, data = wk3dt)
#checking Temp
epf.mod.temp = lme(eggperF ~ pH, method="ML",random= ~1|FullTrt, data = wk3dt)
anova(epf.mod.nox, epf.mod.temp)
#pvalue is 0.7077 so notsignificant
#checking pH
epf.mod.pH = lme(eggperF ~ Temp, method="ML",random= ~1|FullTrt, data = wk3dt)
anova(epf.mod.nox, epf.mod.pH)
#pvalue is 0.0533 so almost significant


#rerun model without terms we don't think are significant
epf.mod.final = lme(eggperF ~ Temp +pH, method="REML",random= ~1|FullTrt, data = wk3dt)
summary(epf.mod.final)

#rerun full model to report
epf.mod.finalfull = lme(eggperF ~ Temp + pH + Temp:pH, method="REML",random= ~1|FullTrt, data = wk3dt)
summary(epf.mod.finalfull)

#taking values from residuals line at top of summary and squaring them to convert SD to variance
REest = (0.09619716)^2
res.est = (0.4373658)^2

#Validate Original Model Assumptions
qqnorm(resid(epf.mod.finalfull)) 
qqline(resid(epf.mod.finalfull)) #ugh so normal

#plotting residuals against 2 predictors in the model
#for continuous variables: want to see a nice starry night of random noise
#for categorical variables: want to see even box plots

plot(resid(epf.mod.finalfull)~wk3dt$Temp) #bar means pretty even, high temp has smaller range
plot(resid(epf.mod.finalfull)~wk3dt$pH) #bar means pretty even, high pH has smaller range
#overall, looks like we aren't violating any assumptions




#WEEK 3 JUVENILES PER FEMALE DATA ---------------

wk3dt$juvperF <- wk3dt$Juveniles/wk3dt$Females
boxplot(juvperF ~ Treatment, wk3dt)

summary(
  lmer(juvperF ~ Temp + pH + (1|Trt_Rep), wk3dt)) 
# high temp low pH trt only one that seems diff from the rest

#TESTING ASSUMPTIONS AND CHOOSING TRANSFORMATIONS
#build main model lme argument (allows for random effects as opposed to lm)
jpf.mod.noT = lme(juvperF ~ Temp*pH,random= ~1|FullTrt, data = wk3dt) #modeling the effects of Temp and pH with random effects of Dish (Trt_Rep)
jpf.res =as.numeric(residuals(jpf.mod.noT)) #create a value for the residuals to do tests oof LMM assumptions

#testing normality
hist(jpf.res) #pretty normal
dotchart(jpf.res) #pretty scattered
qqnorm(jpf.res)
qqline(jpf.res) #pretty ok alignment tbh
shapiro.test(jpf.res) #p=0.02311 so almost passes test of normality
#testing homogeneity of variances
leveneTest(juvperF ~ Temp, data=wk3dt) #p=0.004021
leveneTest(juvperF ~ pH, data=wk3dt) #p=0.05724
leveneTest(juvperF ~ Temp:pH, data=wk3dt) #p=0.1996
leveneTest(juvperF ~ Temp*pH, data=wk3dt) #p=0.1996, ok so not all variances homogenous
plot(jpf.mod.noT) #residuals trend a teensy bit high

#ok so without a transformation, the data is not normal and the residual variances are not all homogenous, so ! want to try a sqrt transformation and see what that does 
#build main model lme argument (allows for random effects as opposed to lm)
jpf.mod.sqrt = lme(sqrt(juvperF) ~ Temp*pH,random= ~1|FullTrt, data = wk3dt) #modeling the effects of Temp and pH with random effects of Dish (Trt_Rep)
jpf.res.sqrt =as.numeric(residuals(jpf.mod.sqrt)) #create a value for the residuals to do tests oof LMM assumptions
#testing normality
hist(jpf.res.sqrt) #normal looking
dotchart(jpf.res.sqrt) #pretty scattered
qqnorm(jpf.res.sqrt)
qqline(jpf.res.sqrt) #v nice
shapiro.test(jpf.res.sqrt) #p=0.5414 so passes test of normality!
#let's make sure variances are still homogenous
leveneTest(sqrt(juvperF) ~ Temp, data=wk3dt) #p=0.6765
leveneTest(sqrt(juvperF) ~ pH, data=wk3dt) #p=0.6876
leveneTest(sqrt(juvperF) ~ Temp:pH, data=wk3dt) #p=0.8521
leveneTest(sqrt(juvperF) ~ Temp*pH, data=wk3dt) #p=0.8521, ok so variances all homogenous!
plot(jpf.mod.sqrt) #residuals clustered in 4 groups

#We will choose sqrt transformation because it gets residuals normal and gets homogeneity of variance. Also data is based on count data, so this transformation seems appropriate

#MODEL VALIDATION

#Starting to go through the nesting process
jpf.mod.main = lme(sqrt(juvperF) ~ Temp + pH + Temp:pH, method="REML",random= ~1|FullTrt, data = wk3dt)

#Determine proper random structure using REML - remove 1 random effect at a time and finally all random effects
jpf.mod.rtest = gls(sqrt(juvperF) ~ Temp + pH + Temp:pH, data = wk3dt)
#compare models with and without random effects
anova(jpf.mod.main, jpf.mod.rtest)
#p value .7313 so Dish(Trt_Rep) really does not explain a lot of variation



#Determine proper fixed structure using ML

#First, we check the Interactions
jpf.mod.ml = lme(sqrt(juvperF) ~ Temp + pH + Temp:pH, method="ML",random= ~1|FullTrt, data = wk3dt)
#checking Temp:pH intxn
jpf.mod.txp = lme(sqrt(juvperF) ~ Temp + pH, method="ML",random= ~1|FullTrt, data = wk3dt)
anova(jpf.mod.ml, jpf.mod.txp)
#p value = 0.7955 so looks like Temp x pH is not significant


#Next we check the individual effects
#new full model without interaction terms
jpf.mod.nox = lme(sqrt(juvperF) ~ Temp + pH, method="ML",random= ~1|FullTrt, data = wk3dt)
#checking Temp
jpf.mod.temp = lme(sqrt(juvperF) ~ pH, method="ML",random= ~1|FullTrt, data = wk3dt)
anova(jpf.mod.nox, jpf.mod.temp)
#pvalue is <0.0001 so v significant
#checking pH
jpf.mod.pH = lme(sqrt(juvperF) ~ Temp, method="ML",random= ~1|FullTrt, data = wk3dt)
anova(jpf.mod.nox, jpf.mod.pH)
#pvalue is <0.0001 so significant


#rerun model without terms we don't think are significant
jpf.mod.final = lme(sqrt(juvperF) ~ Temp +pH, method="REML",random= ~1|FullTrt, data = wk3dt)
summary(jpf.mod.final)

#rerun full model to report
jpf.mod.finalfull = lme(sqrt(juvperF) ~ Temp + pH + Temp:pH, method="REML",random= ~1|FullTrt, data = wk3dt)
summary(jpf.mod.finalfull)

#taking values from residuals line at top of summary and squaring them to convert SD to variance
REest = (0.03556958)^2
res.est = (0.1931486)^2

#Validate Original Model Assumptions
qqnorm(resid(jpf.mod.finalfull)) 
qqline(resid(jpf.mod.finalfull)) #ugh so normal

#plotting residuals against 2 predictors in the model
#for continuous variables: want to see a nice starry night of random noise
#for categorical variables: want to see even box plots

plot(resid(jpf.mod.finalfull)~wk3dt$Temp) #bar means pretty even, high temp has smaller range
plot(resid(jpf.mod.finalfull)~wk3dt$pH) #bar means pretty even, high pH has smaller range
#overall, looks like we aren't violating any assumptions



#WEEK 3 OFFSPRING (J+E) PER FEMALE DATA ---------------

wk3dt$offperF <- (wk3dt$Juveniles + wk3dt$Eggs)/wk3dt$Females
boxplot(offperF ~ Treatment, wk3dt)

summary(
  lmer(offperF ~ Temp + pH + (1|Trt_Rep), wk3dt)) 
# high temp low pH trt only one that seems diff from the rest

##TESTING ASSUMPTIONS AND CHOOSING TRANSFORMATIONS
#build main model lme argument (allows for random effects as opposed to lm)
opf.mod.noT = lme(offperF ~ Temp*pH,random= ~1|FullTrt, data = wk3dt) #modeling the effects of Temp and pH with random effects of Dish (Trt_Rep)
opf.res =as.numeric(residuals(opf.mod.noT)) #create a value for the residuals to do tests oof LMM assumptions

#testing normality
hist(opf.res) #pretty normal
dotchart(opf.res) #pretty scattered
qqnorm(opf.res)
qqline(opf.res) #pretty ok alignment tbh
shapiro.test(opf.res) #p=0.3129 so passes test of normality
#testing homogeneity of variances
leveneTest(offperF ~ Temp, data=wk3dt) #p=0.2992
leveneTest(offperF ~ pH, data=wk3dt) #p=0.19
leveneTest(offperF ~ Temp:pH, data=wk3dt) #p=0.397
leveneTest(offperF ~ Temp*pH, data=wk3dt) #p=0.397, ok so all variances homogenous
plot(opf.mod.noT) #residuals even but in 2 clusters 

#We will choose NO transformation because residuals already normal and have homogeneity of variance

#MODEL VALIDATION


#Starting to go through the nesting process
opf.mod.main = lme(offperF ~ Temp + pH + Temp:pH, method="REML",random= ~1|FullTrt, data = wk3dt)

#Determine proper random structure using REML - remove 1 random effect at a time and finally all random effects
opf.mod.rtest = gls(offperF ~ Temp + pH + Temp:pH, data = wk3dt)
#compare models with and without random effects
anova(opf.mod.main, opf.mod.rtest)
#p value .7986 so Dish(Trt_Rep) really does not explain a lot of variation



#Determine proper fixed structure using ML

#First, we check the Interactions
opf.mod.ml = lme(offperF ~ Temp + pH + Temp:pH, method="ML",random= ~1|FullTrt, data = wk3dt)
#checking Temp:pH intxn
opf.mod.txp = lme(offperF ~ Temp + pH, method="ML",random= ~1|FullTrt, data = wk3dt)
anova(opf.mod.ml, opf.mod.txp)
#p value = 0.1911 so looks like Temp x pH is not significant


#Next we check the individual effects
#new full model without interaction terms
opf.mod.nox = lme(offperF ~ Temp + pH, method="ML",random= ~1|FullTrt, data = wk3dt)
#checking Temp
opf.mod.temp = lme(offperF ~ pH, method="ML",random= ~1|FullTrt, data = wk3dt)
anova(opf.mod.nox, opf.mod.temp)
#pvalue is <0.0001 so very significant
#checking pH
opf.mod.pH = lme(offperF ~ Temp, method="ML",random= ~1|FullTrt, data = wk3dt)
anova(opf.mod.nox, opf.mod.pH)
#pvalue is 0.2432 so not significant


#rerun model without terms we don't think are significant
opf.mod.final = lme(offperF ~ Temp +pH, method="REML",random= ~1|FullTrt, data = wk3dt)
summary(opf.mod.final)

#rerun full model to report
opf.mod.finalfull = lme(offperF ~ Temp + pH + Temp:pH, method="REML",random= ~1|FullTrt, data = wk3dt)
summary(opf.mod.finalfull)

#taking values from residuals line at top of summary and squaring them to convert SD to variance
REest = (0.07466348)^2
res.est = (0.4729499)^2

#Validate Original Model Assumptions
qqnorm(resid(opf.mod.finalfull)) 
qqline(resid(opf.mod.finalfull)) #ugh so normal

#plotting residuals against 2 predictors in the model
#for continuous variables: want to see a nice starry night of random noise
#for categorical variables: want to see even box plots

plot(resid(opf.mod.finalfull)~wk3dt$Temp) #bar means pretty even, high temp has smaller range
plot(resid(opf.mod.finalfull)~wk3dt$pH) #bar means pretty even, high pH has smaller range
#overall, looks like we aren't violating any assumptions???


# CALCULATE MEANS AND SDS --------------

#first, we have a lot of columns, so select the columns desired
wk3dt2 = wk3dt[,c(4,9:12,15:18)]

#create a new df with means(sd) for each variable
means_sd = wk3dt2 %>%
  group_by(Treatment) %>% 
  summarise_all(list(~ str_c(round(mean(.), 2), " (", round(sd(.), 2), ")")))






