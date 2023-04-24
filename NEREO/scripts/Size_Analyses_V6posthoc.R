# Nereo Sizes

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
library(emmeans)


#CALCULATE AREA CONVERSION FACTOR -------------------------------------------
# took sizes of 4 0.0625mm^2 cells on a hemocytometer using image J
#average those sizes to get our area conversion factor
IJvals = c(70602,70872,72201,71645)
Area_con = mean(IJvals) # = 71330
#so theres 71330 pixels per 0.0625mm^2

#upload data:
main_data <- read_csv("Wk3Wk4DataAnalysis/wk3wk4_juvsizes.csv")


#covert area in pixels to square mm
main_data$area_mm2 = main_data$Area*(0.0625/Area_con)

#convert square mm to square um
main_data$area_um2 = main_data$area_mm2*1000000

#IMAGE SIZE CALCULATIONS ---------------------------------
#quick calculation of overall size of each photo

#so theres 71330 pixels per 0.0625mm^2

#full photo
#each image 1280x960 pixels
1280*960 #=1228800 pixels per image
71330/0.0625 = 1228800/x
71330*X =1228800*0.0625
X=(1228800*0.0625)/71330 #~1.08 mm^2

#egg photo
#each image 304x289 pixels
304*289 #=87856 pixels per image
E=(87856*0.0625)/71330 #~0.077 mm^2

#juvenile photo
#each image 281x266 pixels
281*266 #=74746 pixels per image
J=(74746*0.0625)/71330 #~0.065 mm^2



#female photo
#each image 281x266 pixels
281*266 #=74746 pixels per image
J=(74746*0.0625)/71330 #~0.065 mm^2

#male photo
#each image 279x266 pixels
279*266 #=74214 pixels per image
M=(74214*0.0625)/71330 #~0.065 mm^2

#spore photo
#each image 279x258 pixels
279*258 #=71982 pixels per image
S=(71982*0.0625)/71330 #~0.063 mm^2

# PULLING COVARIATES -----------------------

#OK so the main difference between our size models and our count models is that they will have covariates - essentially, we are accounting for the fact that the number of gams present may affect the size of the juveniles. As such, we need to expand our base model to look like: 

# Base model - run for each week lmer(Size/Number~ ShockTemp + pH  + ShockTemp:pH + (1|Dish) + i + i:ShockTemp + i:pH + i:ShockTemp:pH)
#or, in shorthand (with our actual variable names): 
#lmer(Juveniles/Eggs ~ Heat_Trt*pH*gammean+ (1|Dish))

#so lets pull up the week 4 count data, and make a new column for i in our files called "dish_ave_c" that contains the average gametophyte count data for that dish that week

wk3wk4 <- read_csv("Wk3Wk4DataAnalysis/wk3wk4_Nereo_counts.csv")
View(wk3wk4)
#checking variable designations
wk3wk4$Trt_Rep = as.factor(wk3wk4$Trt_Rep)
wk3wk4$Photo_Rep = as.factor(wk3wk4$Photo_Rep)
wk3wk4$Type = as.factor(wk3wk4$Type)
wk3wk4$Number = as.numeric(wk3wk4$Number)
#filtering out desired data and taking the sum of gametophytes for each photo
wk3wk4cov = wk3wk4 %>% 
  filter(Type == c("Females","Males")) %>% 
  filter(!is.na(Number)) %>% 
  group_by(Week, Treatment,Trt_Rep,Photo_Rep) %>% 
  summarise(wk3wk4gams = sum(Number))

#so, we now have the total gametophyte count for each photo, but let's calculate the average number of gams photographed in each dish
wk3wk4cov2 = wk3wk4cov %>% 
  group_by(Week,Treatment,Trt_Rep) %>% 
  summarise(gammean = mean(wk3wk4gams))

#hard to have half of a gametophyte so lets make that an integer
wk3wk4cov2$gammean = as.integer(wk3wk4cov2$gammean)

#creating a new column in both w4cov and hsdt that will have matching values, thus allowing us to match the covariate data more appropriately
wk3wk4cov2$FullTrt = paste(wk3wk4cov2$Week,"_",wk3wk4cov2$Treatment,"_",wk3wk4cov2$Trt_Rep)
main_data$FullTrt = paste(main_data$Week,"_",main_data$Treatment,"_",main_data$Trt_Rep)  

#cool, now we try to merge our 2 tables
main_data = merge(main_data, wk3wk4cov2, by="FullTrt")
#dope, it worked! 

#we just have a lot of extra columns now, so let's choose the ones we want
main_data = main_data[,c(1,5:13,17,18,22)] #noice


#so, we have the size of each gametophyte count for each photo, but let's calculate the average size of gams photographed in each dish so that we can do data exploration on the relationship of the covariate
wk3wk4nonas = main_data %>% 
  filter(!is.na(area_um2)) 
wk3wk4ave_um = wk3wk4nonas %>%
  group_by(FullTrt) %>% 
  summarise(ave_um = mean(area_um2))
#cool, now we try to merge our 2 tables
main_data2 = merge(main_data, wk3wk4ave_um, by="FullTrt")



# WEEK 3 SIZES ----------------------------

#pull week 3 data
wk3dt = main_data2 %>% 
  filter(Week.x == "Week3") %>% 
  filter(!is.na(area_um2)) %>% 
  filter(File_name != "AA3_x4_3") #gets rid of one 0 value in data set

#data cleaning: 
#1. convert all variables into factors, except for number which should be continuous
wk3dt$FullTrt = as.factor(wk3dt$FullTrt)
wk3dt$Temp = as.factor(wk3dt$Temp)
wk3dt$pH = as.factor(wk3dt$pH)
wk3dt$Trt_Rep = as.factor(wk3dt$Trt_Rep)
wk3dt$Week.x = as.factor(wk3dt$Week.x)
wk3dt$Photo_Rep = as.factor(wk3dt$Photo_Rep)
wk3dt$gammean = as.numeric(wk3dt$gammean)
wk3dt$area_um2 = as.numeric(wk3dt$area_um2)

#data exploration
#purpose: plot the histogram and dotchart for each continuous response and fixed variable - if there are random variables, check those impacts on each of the main variables. Histograms - looking for a nice even normal distrbution, no skews. Dotcharts - looking for random spread, no clusters. 
hist(wk3dt$area_um2) #hot damn big skew to left
dotchart(wk3dt$area_um2, color=wk3dt$Trt_Rep) #V clustered to the left and the top, lie along a curve almost

#calculate means and SDs
#first, we have a lot of columns, so select the columns desired
wk3dt2 = wk3dt[,c(3,12,13)]
wk3dt2$FullTrt = as.factor(wk3dt2$FullTrt)
wk3dt2$Temp = as.factor(wk3dt2$Temp)
wk3dt2$pH = as.factor(wk3dt2$pH)
wk3dt2$Trt_Rep = as.factor(wk3dt2$Trt_Rep)
wk3dt2$Week.x = as.factor(wk3dt2$Week.x)
wk3dt2$Photo_Rep = as.factor(wk3dt2$Photo_Rep)
wk3dt2$gammean = as.numeric(wk3dt2$gammean)
wk3dt2$area_um2 = as.numeric(wk3dt2$area_um2)
#create a new df with means(sd) for each variable
means_sd3 = wk3dt2 %>%
  group_by(Treatment.x) %>% 
  summarise_all(list(~ str_c(round(mean(.), 2), " (", round(sd(.), 2), ")")))



#lets look at relationship between raw sizes and the covariate

plot(wk3dt$area_um2~wk3dt$gammean) 
plot(log(wk3dt$area_um2)~wk3dt$gammean)
#doesn't seem to be a particularly significant relationship? but also maybe there is a trend of larger sizes with fewer gams? very vague tho
#need to redo this with avg size vs gammmean for each dish
plot(wk3dt$ave_um~wk3dt$gammean) 
plot(log(wk3dt$ave_um)~wk3dt$gammean)
#ope yep, there's the trend - the more gams, the smaller the average sizes

#TESTING ASSUMPTIONS AND CHOOSING TRANSFORMATIONS
#build main model lme argument (allows for random effects as opposed to lm)
wk3.mod.noT = glmer(I(area_um2/100) ~ Temp*pH*gammean +(1|FullTrt), family = Gamma(link="inverse"), data = wk3dt) #modeling the effects of Temp and pH with random effects of Dish (Trt_Rep)


#MODEL VALIDATION

#RECHECK VALIDATION OF RANDOM EFFECTS
#Starting to go through the nesting process
wk3.mod.main = glmer(I(area_um2/100) ~ Temp*pH*gammean +(1|FullTrt), family = Gamma(link="inverse"), data = wk3dt)

#Determine proper random structure using REML (REML = restricted maximum likelihood) - remove 1 random effect at a time and finally all random effects
wk3.mod.rtest = glmer(I(area_um2/100) ~ Temp*pH*gammean, family = Gamma(link="inverse"), data = wk3dt)
#compare models with and without random effects
anova(wk3.mod.main, wk3.mod.rtest)
#p value =??? so Dish(Trt_Rep) does/not kinda explain variation???



#Determine proper fixed structure using ML (ML = maximum likelihood)

#First, we check the Interactions
wk3.mod.ml = glmer(I(area_um2/100) ~ Temp + pH + Temp:pH + gammean + gammean:Temp + gammean:pH + gammean:pH:Temp +(1|FullTrt), family = Gamma(link="inverse"), data = wk3dt)
#Start with the 3-way interaction
wk3.mod.txpxg = glmer(I(area_um2/100) ~ Temp + pH + Temp:pH + gammean + gammean:Temp + gammean:pH +(1|FullTrt), family = Gamma(link="inverse"), data = wk3dt)
anova(wk3.mod.ml, wk3.mod.txpxg)
#p value = 0.05182 so barely not significant. - also this is now giving chisq outputs instead of log lik?

#then move onto the 2 way interactions
#checking gam:Temp intxn
wk3.mod.txg = glmer(I(area_um2/100) ~ Temp + pH + Temp:pH + gammean + gammean:pH +(1|FullTrt), family = Gamma(link="inverse"), data = wk3dt)
anova(wk3.mod.txpxg, wk3.mod.txg)
#p value = 0.4521 so not significant
#checking gam:pH inxn
wk3.mod.pxg = glmer(I(area_um2/100) ~ Temp + pH + Temp:pH + gammean + gammean:Temp +(1|FullTrt), family = Gamma(link="inverse"), data = wk3dt)
anova(wk3.mod.txpxg, wk3.mod.pxg)
#p=0.5605 so really not significant
#checking Temp:pH intxn
wk3.mod.txp = glmer(I(area_um2/100) ~ Temp + pH + gammean + gammean:Temp + gammean:pH +(1|FullTrt), family = Gamma(link="inverse"), data = wk3dt)
anova(wk3.mod.txpxg, wk3.mod.txp)
#p value = 0.6915 so looks like Temp x pH is not significant


#Next check the covariate without the gammean interaction terms
wk3.mod.ml = glmer(I(area_um2/100) ~ Temp + pH + gammean +(1|FullTrt), family = Gamma(link="inverse"), data = wk3dt)
#checking Temp:pH intxn
wk3.mod.cov = glmer(I(area_um2/100) ~ Temp + pH +(1|FullTrt), family = Gamma(link="inverse"), data = wk3dt)
anova(wk3.mod.ml, wk3.mod.cov)
#p value = 0.01676 so looks like gammean is quite significant


#Next we check the individual effects
#new full model without interaction terms
wk3.mod.nox = glmer(I(area_um2/100) ~ Temp + pH + gammean +(1|FullTrt), family = Gamma(link="inverse"), data = wk3dt)
#checking Temp
wk3.mod.temp = glmer(I(area_um2/100) ~ pH + gammean +(1|FullTrt), family = Gamma(link="inverse"), data = wk3dt)
anova(wk3.mod.nox, wk3.mod.temp)
#pvalue is <0.001 so significant
#checking pH
wk3.mod.pH = glmer(I(area_um2/100) ~ Temp + gammean +(1|FullTrt), family = Gamma(link="inverse"), data = wk3dt)
anova(wk3.mod.nox, wk3.mod.pH)
#pvalue is 0.9408 so not significant


#rerun full model to report
wk3.mod.finalfull = glmer(I(area_um2/100) ~ Temp*pH*gammean +(1|FullTrt), family = Gamma(link="inverse"), data = wk3dt)
summary(wk3.mod.finalfull)

#Validate Original Model Assumptions
#DO THIS FOR GLMMS???

# WEEK 4 SIZES ----------------------------

#pull week 4 data
wk4dt = main_data %>% 
  filter(Week.x == "Week4") %>% 
  filter(!is.na(area_um2))

#data cleaning: 
#convert all variables into factors, except for number which should be continuous
wk4dt$FullTrt = as.factor(wk4dt$FullTrt)
wk4dt$Temp = as.factor(wk4dt$Temp)
wk4dt$pH = as.factor(wk4dt$pH)
wk4dt$Trt_Rep = as.factor(wk4dt$Trt_Rep)
wk4dt$Week.x = as.factor(wk4dt$Week.x)
wk4dt$Photo_Rep = as.factor(wk4dt$Photo_Rep)
wk4dt$gammean = as.numeric(wk4dt$gammean)
wk4dt$area_um2 = as.numeric(wk4dt$area_um2)

#calculate means and SDs
#first, we have a lot of columns, so select the columns desired
wk4dt2 = wk4dt[,c(3,12,13)]
#create a new df with means(sd) for each variable
means_sd4 = wk4dt2 %>%
  group_by(Treatment.x) %>% 
  summarise_all(list(~ str_c(round(mean(.), 2), " (", round(sd(.), 2), ")")))

#data exploration
#purpose: plot the histogram and dotchart for each continuous response and fixed variable - if there are random variables, check those impacts on each of the main variables. Histograms - looking for a nice even normal distrbution, no skews. Dotcharts - looking for random spread, no clusters. 
hist(wk4dt$area_um2) #hot damn big skew to left
hist(wk4dt2$ave_um)
dotchart(wk4dt$area_um2, color=wk4dt$Trt_Rep) #V clustered to the left and the top, lie along a curve almost

#lets look at relationship between raw sizes and the covariate
wk4dt2 = main_data2 %>% 
  filter(Week.x == "Week4") %>% 
  filter(!is.na(area_um2))

plot(wk4dt2$area_um2~wk4dt$gammean) 
plot(log(wk4dt2$area_um2)~wk4dt$gammean)
#doesn't seem to be a particularly significant relationship? but also maybe there is a trend of larger sizes with fewer gams? very vague tho
#need to redo this with avg size vs gammmean for each dish

plot(wk4dt2$ave_um~wk4dt2$gammean) 
plot(log(wk4dt2$ave_um)~wk4dt2$gammean)
#ope yep, there's the trend - the more gams, the smaller the average sizes

#ok lets run a quick lm to see how significant the covariate is in influencing size
global = lm(ave_um ~ gammean, data = wk4dt2)
summary(global)


#MODEL VALIDATION

#RECHECK VALIDATION OF RANDOM EFFECTS
#Starting to go through the nesting process
wk4.mod.main = glmer(I(area_um2/100) ~ Temp*pH*gammean +(1|FullTrt), family = Gamma(link="inverse"), data = wk4dt)

#Determine proper fixed structure using ML (ML = maximum likelihood)

#First, we check the Interactions
wk4.mod.ml = glmer(I(area_um2/100) ~ Temp*pH*gammean +(1|FullTrt), family = Gamma(link="inverse"), data = wk4dt)
#Start with the 3-way interaction
wk4.mod.txpxg = glmer(I(area_um2/100) ~ Temp*pH + Temp*gammean +pH*gammean +(1|FullTrt), family = Gamma(link="inverse"), data = wk4dt)
anova(wk4.mod.ml, wk4.mod.txpxg)
#p value = 0.01181 so  significant. - also this is now giving chisq outputs instead of log lik?

#ok, so because the 3-way interaction is significant, we now have to do post-hoc tests to get to the individual effects. So we are going to subset data to look at effects within each of the levels: 1) subset low pH, 2) subset high pH, 3) subset low temp, 4) subset high temp

#SUBSETTING LOW PH
wk4lph = wk4dt %>% 
  filter(pH == "low")

#build main model
lph.mod.main = glmer(I(area_um2/100) ~ Temp*gammean +(1|FullTrt), family = Gamma(link="inverse"), data = wk4lph)

#First, we check the Interactions

#checking gam:Temp intxn
lph.mod.txg = glmer(I(area_um2/100) ~ Temp + gammean +(1|FullTrt), family = Gamma(link="inverse"), data = wk4lph)
anova(lph.mod.main, lph.mod.txg)
#p value = 0.2044 so not significant

#Next check the covariate
lph.mod.g = glmer(I(area_um2/100) ~ Temp +(1|FullTrt), family = Gamma(link="inverse"), data = wk4lph)
anova(lph.mod.txg, lph.mod.g)
#p value = 0.5721 so not significant

#finally, check temp
lph.mod.t = glmer(I(area_um2/100) ~ gammean +(1|FullTrt), family = Gamma(link="inverse"), data = wk4lph)
anova(lph.mod.txg, lph.mod.t)
#p value = 0.001523 so significant - expected

#get final values for rest of model
summary(lph.mod.main)

#SUBSETTING HIGH PH
wk4hph = wk4dt %>% 
  filter(pH == "high")

#build main model
hph.mod.main = glmer(I(area_um2/100) ~ Temp*gammean +(1|FullTrt), family = Gamma(link="inverse"), data = wk4hph)

#First, we check the Interactions

#checking gam:Temp intxn
hph.mod.txg = glmer(I(area_um2/100) ~ Temp + gammean +(1|FullTrt), family = Gamma(link="inverse"), data = wk4hph)
anova(hph.mod.main, hph.mod.txg)
#p value = 0.02132 so significant - ugh but I still want to know how temp and gammeans do here so we are gonna keep going. Need to think of other post hoc test maybe?

#Next check the covariate
hph.mod.g = glmer(I(area_um2/100) ~ Temp +(1|FullTrt), family = Gamma(link="inverse"), data = wk4hph)
anova(hph.mod.txg, hph.mod.g)
#p value = 0.07449 so marginally significant

#finally, check temp
hph.mod.t = glmer(I(area_um2/100) ~ gammean +(1|FullTrt), family = Gamma(link="inverse"), data = wk4hph)
anova(hph.mod.txg, hph.mod.t)
#p value = 0.1491 so not significant - interesting

#get final values for rest of model
summary(hph.mod.main)


#SUBSETTING LOW TEMP
wk4ltemp = wk4dt %>% 
  filter(Temp == "cold")

#build main model
ltemp.mod.main = glmer(I(area_um2/100) ~ pH*gammean +(1|FullTrt), family = Gamma(link="inverse"), data = wk4ltemp)

#First, we check the Interactions

#checking gam:pH intxn
ltemp.mod.pxg = glmer(I(area_um2/100) ~ pH + gammean +(1|FullTrt), family = Gamma(link="inverse"), data = wk4ltemp)
anova(ltemp.mod.main, ltemp.mod.pxg)
#p value = 0.1322 so not significant

#Next check the covariate
ltemp.mod.g = glmer(I(area_um2/100) ~ pH +(1|FullTrt), family = Gamma(link="inverse"), data = wk4ltemp)
anova(ltemp.mod.pxg, ltemp.mod.g)
#p value = 0.2788 so not significant

#finally, check ph
ltemp.mod.p = glmer(I(area_um2/100) ~ gammean +(1|FullTrt), family = Gamma(link="inverse"), data = wk4ltemp)
anova(ltemp.mod.pxg, ltemp.mod.p)
#p value = 0.04673 so significant - expected

#get final values for rest of model
summary(ltemp.mod.main)

#SUBSETTING HIGH TEMP
wk4htemp = wk4dt %>% 
  filter(Temp == "hot")

#build main model
htemp.mod.main = glmer(I(area_um2/100) ~ pH*gammean +(1|FullTrt), family = Gamma(link="inverse"), data = wk4htemp)

#First, we check the Interactions

#checking gam:Temp intxn
htemp.mod.pxg = glmer(I(area_um2/100) ~ pH + gammean +(1|FullTrt), family = Gamma(link="inverse"), data = wk4htemp)
anova(htemp.mod.main, htemp.mod.pxg)
#p value = 0.01399 so significant - ugh but I still want to know how temp and gammeans do here so we are gonna keep going. Need to think of other post hoc test maybe?

#Next check the covariate
htemp.mod.g = glmer(I(area_um2/100) ~ pH +(1|FullTrt), family = Gamma(link="inverse"), data = wk4htemp)
anova(htemp.mod.pxg, htemp.mod.g)
#p value = 0.3202 so not significant

#finally, check pH
htemp.mod.p = glmer(I(area_um2/100) ~ gammean +(1|FullTrt), family = Gamma(link="inverse"), data = wk4htemp)
anova(htemp.mod.pxg, htemp.mod.p)
#p value = 0.7758 so not significant - interesting

#get final values for rest of model
summary(htemp.mod.main)

