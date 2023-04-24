#DATA VIZ FOR ESA

rm(list = ls())

#Set WD
setwd("~/Desktop/Nereo/NereoAnalysis/NEREO")

#Packages Needed
library(readxl)
library(lme4)
library(nlme)
library(tidyverse)
library(car)

#GRAPHS TO MAKE IN GGPLOT
# 1. WEEK 4 #F
# 2. WEEK 4 #M
# 3. WEEK 4 #E
# 4. WEEK 4 #J
# 5. Post Heat #E
# 6. Post Heat #J
# 7. WEEK 4 J Sizes
# 8. Post Heat J Size
# 9. WEEK 4 E/F
# 10. WEEK 4 % Prod

# LOAD AND CLEAN DATA FOR WEEK 4 AND PRODUCTIVITY -----------------------------------------

rm(list = ls())

#pulling data
data <- read_csv("Wk3Wk4DataAnalysis/wk3wk4_Nereo_counts.csv")

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

#1b. Need to change trt names for Ambient so...
data$Temp1 <- ifelse(data$Temp =="Low", "Ambient", "High")
data$pH1 <- ifelse(data$pH =="High", "Ambient", "Low")

#2. need to split data set in two - into week3 and week 4 separately. need to think a little more about how to compare across weeks
pdt = data %>% 
  filter(!is.na(Number)) %>% 
  filter(Week == "Week4") %>% 
  spread(Type, Number)
#creating a new column for Trt_trtRep which will serve as our random variable
pdt$FullTrt = paste(pdt$Treatment,"_",pdt$Trt_Rep)

#3.  create new column with calculation of the proportion of productive females
pdt$Prop_prod <- pdt$Productive/pdt$Females

#4. calculate eE/F, J/F, E+J/F per photo
pdt$eggperF <- pdt$Eggs/pdt$Females
pdt$juvperF <- pdt$Juveniles/pdt$Females
pdt$egg_juv <- pdt$Eggs+pdt$Juveniles
pdt$EJperF <- pdt$egg_juv/pdt$Females


# WEEK 4 COUNT GRAPHS ----------------------------------------------------------

###FEMALES###
#Model = full.f.mod = lme(log(Females) ~ Temp + pH + Temp:pH, method="REML",random= ~1|FullTrt, data = pdt)

#base code box plot
boxplot(Females ~ Treatment, pdt)

#let's plot it with ggplot now
ggplot(pdt, aes(x=Treatment, y=Females, fill=Treatment, group=Treatment)) + 
  ggtitle("Week 4")+
  ylab("Number of Females") +
  geom_boxplot(show.legend = FALSE) +
  scale_x_discrete(limits=c("AA","AB","BA","BB"), labels=str_wrap(c("Ambient Temp, Low pH", "Ambient Temp, Ambient pH", "High Temp, Low pH","High Temp, Ambient pH"), width = 13)) +
  scale_fill_manual(values=c("#0099FF","#0000FF","#FF0000","#990033","#9900CC","#330066")) +
  theme(plot.title = element_text(face="bold", hjust = 0.9, size = 24),axis.title.x = element_blank(),axis.title.y = element_text(size = 20, margin = margin(t = 0, r = 20, b = 0, l = 0)),axis.text.x = element_text(size = 20),axis.text.y = element_text(size = 20),panel.grid.major = element_blank (), panel.grid.minor = element_blank(),panel.background = element_rect(fill='transparent'),plot.background = element_rect(fill='transparent', color=NA), axis.line = element_line(colour = "black"))

###MALES###
#Model = full.m.mod = lme(sqrt(Males) ~ Temp + pH + Temp:pH, method="REML",random= ~1|FullTrt, data = pdt)

#base code box plot
boxplot(Males ~ Treatment, pdt)

#let's plot it with ggplot now
ggplot(pdt, aes(x=Treatment, y=Males, fill=Treatment, group=Treatment)) + 
  ggtitle("Week 4")+
  ylab("Number of Males") +
  geom_boxplot(show.legend = FALSE) +
  scale_x_discrete(limits=c("AA","AB","BA","BB"), labels=str_wrap(c("Ambient Temp, Low pH", "Ambient Temp, Ambient pH", "High Temp, Low pH","High Temp, Ambient pH"), width = 13)) +
  scale_fill_manual(values=c("#0099FF","#0000FF","#FF0000","#990033","#9900CC","#330066")) +
  theme(plot.title = element_text(face="bold", hjust = 0.9, size = 24),axis.title.x = element_blank(),axis.title.y = element_text(size = 20, margin = margin(t = 0, r = 20, b = 0, l = 0)),axis.text.x = element_text(size = 20),axis.text.y = element_text(size = 20),panel.grid.major = element_blank (), panel.grid.minor = element_blank(),panel.background = element_rect(fill='transparent'),plot.background = element_rect(fill='transparent', color=NA), axis.line = element_line(colour = "black"))

###EGGS###
#Model = full.e.mod = lme(log(Eggs+1) ~ Temp + pH + Temp:pH, method="REML",random= ~1|FullTrt, data = pdt)

#base code box plot
boxplot(Eggs ~ Treatment, pdt)

#let's plot it with ggplot now
ggplot(pdt, aes(x=Treatment, y=Eggs, fill=Treatment, group=Treatment)) + 
  ggtitle("Week 4")+
  ylab("Number of Eggs") +
  geom_boxplot(show.legend = FALSE) +
  scale_x_discrete(limits=c("AA","AB","BA","BB"), labels=str_wrap(c("Ambient Temp, Low pH", "Ambient Temp, Ambient pH", "High Temp, Low pH","High Temp, Ambient pH"), width = 13)) +
  scale_fill_manual(values=c("#0099FF","#0000FF","#FF0000","#990033","#9900CC","#330066")) +
  theme(plot.title = element_text(face="bold", hjust = 0.9, size = 24),axis.title.x = element_blank(),axis.title.y = element_text(size = 20, margin = margin(t = 0, r = 20, b = 0, l = 0)),axis.text.x = element_text(size = 20),axis.text.y = element_text(size = 20),panel.grid.major = element_blank (), panel.grid.minor = element_blank(),panel.background = element_rect(fill='transparent'),plot.background = element_rect(fill='transparent', color=NA), axis.line = element_line(colour = "black"))

###JUVENILES###
#Model = full.j.mod = lme(log(Juveniles) ~ Temp + pH + Temp:pH, method="REML",random= ~1|FullTrt, data = pdt)

#base code box plot
boxplot(Juveniles ~ Treatment, pdt)

#let's plot it with ggplot now
ggplot(pdt, aes(x=Treatment, y=Juveniles, fill=Treatment, group=Treatment)) + 
  ggtitle("Week 4")+
  ylab("Number of Juveniles") +
  geom_boxplot(show.legend = FALSE) +
  scale_x_discrete(limits=c("AA","AB","BA","BB"), labels=str_wrap(c("Ambient Temp, Low pH", "Ambient Temp, Ambient pH", "High Temp, Low pH","High Temp, Ambient pH"), width = 13)) +
  scale_fill_manual(values=c("#0099FF","#0000FF","#FF0000","#990033","#9900CC","#330066")) +
  theme(plot.title = element_text(face="bold", hjust = 0.9, size = 24),axis.title.x = element_blank(),axis.title.y = element_text(size = 20, margin = margin(t = 0, r = 20, b = 0, l = 0)),axis.text.x = element_text(size = 20),axis.text.y = element_text(size = 20),panel.grid.major = element_blank (), panel.grid.minor = element_blank(),panel.background = element_rect(fill='transparent'),plot.background = element_rect(fill='transparent', color=NA), axis.line = element_line(colour = "black"))

# Productivity GRAPHS ----------------------------------------------------------

### % PRODUCTIVE F ### (Only one with properly normal data)
#Model = full.prod.mod = lme(Prop_prod ~ Temp + pH + Temp:pH, method="REML",random= ~1|FullTrt, data = pdt)

#base code box plot
boxplot(Prop_prod ~ Treatment, pdt)

#let's plot it with ggplot now
ggplot(pdt, aes(x=Treatment, y=Prop_prod, fill=Treatment, group=Treatment)) + 
  ggtitle("Week 4")+
  ylab("Proportion of Productive Females") +
  geom_boxplot(show.legend = FALSE) +
  scale_x_discrete(limits=c("AA","AB","BA","BB"), labels=str_wrap(c("Ambient Temp, Low pH", "Ambient Temp, Ambient pH", "High Temp, Low pH","High Temp, Ambient pH"), width = 13)) +
  scale_fill_manual(values=c("#0099FF","#0000FF","#FF0000","#990033","#9900CC","#330066")) +
  theme(plot.title = element_text(face="bold", hjust = 0.9, size = 24),axis.title.x = element_blank(),axis.title.y = element_text(size = 20, margin = margin(t = 0, r = 20, b = 0, l = 0)),axis.text.x = element_text(size = 20),axis.text.y = element_text(size = 20),panel.grid.major = element_blank (), panel.grid.minor = element_blank(),panel.background = element_rect(fill='transparent'),plot.background = element_rect(fill='transparent', color=NA), axis.line = element_line(colour = "black"))

### EGGS PER F ###
#Model = full.epf.mod = lme(log(eggperF+0.5) ~ Temp + pH + Temp:pH, method="REML",random= ~1|FullTrt, data = pdt)

#base code box plot
boxplot(pdt$eggperF ~ Treatment, pdt)

#let's plot it with ggplot now
ggplot(pdt, aes(x=Treatment, y=eggperF, fill=Treatment, group=Treatment)) + 
  ggtitle("Week 4")+
  ylab("Eggs per Female") +
  geom_boxplot(show.legend = FALSE) +
  scale_x_discrete(limits=c("AA","AB","BA","BB"), labels=str_wrap(c("Ambient Temp, Low pH", "Ambient Temp, Ambient pH", "High Temp, Low pH","High Temp, Ambient pH"), width = 13)) +
  scale_fill_manual(values=c("#0099FF","#0000FF","#FF0000","#990033","#9900CC","#330066")) +
  theme(plot.title = element_text(face="bold", hjust = 0.9, size = 24),axis.title.x = element_blank(),axis.title.y = element_text(size = 20, margin = margin(t = 0, r = 20, b = 0, l = 0)),axis.text.x = element_text(size = 20),axis.text.y = element_text(size = 20),panel.grid.major = element_blank (), panel.grid.minor = element_blank(),panel.background = element_rect(fill='transparent'),plot.background = element_rect(fill='transparent', color=NA), axis.line = element_line(colour = "black"))

### JUVS PER F ###
#Model = full.epf.mod = lme(log(eggperF+0.5) ~ Temp + pH + Temp:pH, method="REML",random= ~1|FullTrt, data = pdt)

#base code box plot
boxplot(pdt$juvperF ~ Treatment, pdt)

#let's plot it with ggplot now
ggplot(pdt, aes(x=Treatment, y=juvperF, fill=Treatment, group=Treatment)) + 
  ggtitle("Week 4")+
  ylab("Juveniles per Female") +
  geom_boxplot(show.legend = FALSE) +
  scale_x_discrete(limits=c("AA","AB","BA","BB"), labels=str_wrap(c("Ambient Temp, Low pH", "Ambient Temp, Ambient pH", "High Temp, Low pH","High Temp, Ambient pH"), width = 13)) +
  scale_fill_manual(values=c("#0099FF","#0000FF","#FF0000","#990033","#9900CC","#330066")) +
  theme(plot.title = element_text(face="bold", hjust = 0.9, size = 24),axis.title.x = element_blank(),axis.title.y = element_text(size = 20, margin = margin(t = 0, r = 20, b = 0, l = 0)),axis.text.x = element_text(size = 20),axis.text.y = element_text(size = 20),panel.grid.major = element_blank (), panel.grid.minor = element_blank(),panel.background = element_rect(fill='transparent'),plot.background = element_rect(fill='transparent', color=NA), axis.line = element_line(colour = "black"))

### EGGS+JUVS PER F ###
#Model = full.epf.mod = lme(log(eggperF+0.5) ~ Temp + pH + Temp:pH, method="REML",random= ~1|FullTrt, data = pdt)

#base code box plot
boxplot(pdt$EJperF ~ Treatment, pdt)

#let's plot it with ggplot now
ggplot(pdt, aes(x=Treatment, y=EJperF, fill=Treatment, group=Treatment)) + 
  ggtitle("Week 4")+
  ylab("Mean Number of Offspring per Female") +
  geom_boxplot(show.legend = FALSE) +
  scale_x_discrete(limits=c("AA","AB","BA","BB"), labels=str_wrap(c("Ambient Temp, Low pH", "Ambient Temp, Ambient pH", "High Temp, Low pH","High Temp, Ambient pH"), width = 13)) +
  scale_fill_manual(values=c("#0099FF","#0000FF","#FF0000","#990033","#9900CC","#330066")) +
  theme(plot.title = element_text(face="bold", hjust = 0.9, size = 24),axis.title.x = element_blank(),axis.title.y = element_text(size = 20, margin = margin(t = 0, r = 20, b = 0, l = 0)),axis.text.x = element_text(size = 20),axis.text.y = element_text(size = 20),panel.grid.major = element_blank (), panel.grid.minor = element_blank(),panel.background = element_rect(fill='transparent'),plot.background = element_rect(fill='transparent', color=NA), axis.line = element_line(colour = "black"))

# WEEK 3 COUNT GRAPHS ----------------------------------------------------------

#2. need to split data set into week3  separately
pdt3 = data %>% 
  filter(!is.na(Number)) %>% 
  filter(Week == "Week3") %>% 
  spread(Type, Number)
#creating a new column for Trt_trtRep which will serve as our random variable
pdt3$FullTrt = paste(pdt3$Treatment,"_",pdt3$Trt_Rep)

#3.  create new column with calculation of the proportion of productive females
pdt3$Prop_prod <- pdt3$Productive/pdt3$Females

#4. calculate eE/F, J/F, E+J/F per photo
pdt3$eggperF <- pdt3$Eggs/pdt3$Females
pdt3$juvperF <- pdt3$Juveniles/pdt3$Females
pdt3$egg_juv <- pdt3$Eggs+pdt3$Juveniles
pdt3$EJperF <- pdt3$egg_juv/pdt3$Females

###FEMALES###
#Model = full.f.mod = lme(log(Females) ~ Temp + pH + Temp:pH, method="REML",random= ~1|FullTrt, data = pdt)

#base code box plot
boxplot(Females ~ Treatment, pdt3)

#let's plot it with ggplot now
ggplot(pdt3, aes(x=Treatment, y=Females, fill=Treatment, group=Treatment)) + 
  ggtitle("Week 3")+
  ylab("Number of Females") +
  geom_boxplot(show.legend = FALSE) +
  scale_x_discrete(limits=c("AA","AB","BA","BB"), labels=str_wrap(c("Ambient Temp, Low pH", "Ambient Temp, Ambient pH", "High Temp, Low pH","High Temp, Ambient pH"), width = 13)) +
  scale_fill_manual(values=c("#0099FF","#0000FF","#FF0000","#990033","#9900CC","#330066")) +
  theme(plot.title = element_text(face="bold", hjust = 0.9, size = 24),axis.title.x = element_blank(),axis.title.y = element_text(size = 20, margin = margin(t = 0, r = 20, b = 0, l = 0)),axis.text.x = element_text(size = 20),axis.text.y = element_text(size = 20),panel.grid.major = element_blank (), panel.grid.minor = element_blank(),panel.background = element_rect(fill='transparent'),plot.background = element_rect(fill='transparent', color=NA), axis.line = element_line(colour = "black"))

###MALES###
#Model = full.m.mod = lme(sqrt(Males) ~ Temp + pH + Temp:pH, method="REML",random= ~1|FullTrt, data = pdt)

#base code box plot
boxplot(Males ~ Treatment, pdt3)

#let's plot it with ggplot now
ggplot(pdt3, aes(x=Treatment, y=Males, fill=Treatment, group=Treatment)) + 
  ggtitle("Week 3")+
  ylab("Number of Males") +
  geom_boxplot(show.legend = FALSE) +
  scale_x_discrete(limits=c("AA","AB","BA","BB"), labels=str_wrap(c("Ambient Temp, Low pH", "Ambient Temp, Ambient pH", "High Temp, Low pH","High Temp, Ambient pH"), width = 13)) +
  scale_fill_manual(values=c("#0099FF","#0000FF","#FF0000","#990033","#9900CC","#330066")) +
  theme(plot.title = element_text(face="bold", hjust = 0.9, size = 24),axis.title.x = element_blank(),axis.title.y = element_text(size = 20, margin = margin(t = 0, r = 20, b = 0, l = 0)),axis.text.x = element_text(size = 20),axis.text.y = element_text(size = 20),panel.grid.major = element_blank (), panel.grid.minor = element_blank(),panel.background = element_rect(fill='transparent'),plot.background = element_rect(fill='transparent', color=NA), axis.line = element_line(colour = "black"))

###EGGS###
#Model = full.e.mod = lme(log(Eggs+1) ~ Temp + pH + Temp:pH, method="REML",random= ~1|FullTrt, data = pdt)

#base code box plot
boxplot(Eggs ~ Treatment, pdt3)

#let's plot it with ggplot now
ggplot(pdt3, aes(x=Treatment, y=Eggs, fill=Treatment, group=Treatment)) + 
  ggtitle("Week 3")+
  ylab("Number of Eggs") +
  geom_boxplot(show.legend = FALSE) +
  scale_x_discrete(limits=c("AA","AB","BA","BB"), labels=str_wrap(c("Ambient Temp, Low pH", "Ambient Temp, Ambient pH", "High Temp, Low pH","High Temp, Ambient pH"), width = 13)) +
  scale_fill_manual(values=c("#0099FF","#0000FF","#FF0000","#990033","#9900CC","#330066")) +
  theme(plot.title = element_text(face="bold", hjust = 0.9, size = 24),axis.title.x = element_blank(),axis.title.y = element_text(size = 20, margin = margin(t = 0, r = 20, b = 0, l = 0)),axis.text.x = element_text(size = 20),axis.text.y = element_text(size = 20),panel.grid.major = element_blank (), panel.grid.minor = element_blank(),panel.background = element_rect(fill='transparent'),plot.background = element_rect(fill='transparent', color=NA), axis.line = element_line(colour = "black"))

###JUVENILES###
#Model = full.j.mod = lme(log(Juveniles) ~ Temp + pH + Temp:pH, method="REML",random= ~1|FullTrt, data = pdt)

#base code box plot
boxplot(Juveniles ~ Treatment, pdt3)

#let's plot it with ggplot now
ggplot(pdt3, aes(x=Treatment, y=Juveniles, fill=Treatment, group=Treatment)) + 
  ggtitle("Week 3")+
  ylab("Number of Juveniles") +
  geom_boxplot(show.legend = FALSE) +
  scale_x_discrete(limits=c("AA","AB","BA","BB"), labels=str_wrap(c("Ambient Temp, Low pH", "Ambient Temp, Ambient pH", "High Temp, Low pH","High Temp, Ambient pH"), width = 13)) +
  scale_fill_manual(values=c("#0099FF","#0000FF","#FF0000","#990033","#9900CC","#330066")) +
  theme(plot.title = element_text(face="bold", hjust = 0.9, size = 24),axis.title.x = element_blank(),axis.title.y = element_text(size = 20, margin = margin(t = 0, r = 20, b = 0, l = 0)),axis.text.x = element_text(size = 20),axis.text.y = element_text(size = 20),panel.grid.major = element_blank (), panel.grid.minor = element_blank(),panel.background = element_rect(fill='transparent'),plot.background = element_rect(fill='transparent', color=NA), axis.line = element_line(colour = "black"))

# Productivity GRAPHS ----------------------------------------------------------

### % PRODUCTIVE F ### (Only one with properly normal data)
#Model = full.prod.mod = lme(Prop_prod ~ Temp + pH + Temp:pH, method="REML",random= ~1|FullTrt, data = pdt)

#base code box plot
boxplot(Prop_prod ~ Treatment, pdt3)

#let's plot it with ggplot now
ggplot(pdt3, aes(x=Treatment, y=Prop_prod, fill=Treatment, group=Treatment)) + 
  ggtitle("Week 3")+
  ylab("Proportion of Productive Females") +
  geom_boxplot(show.legend = FALSE) +
  scale_x_discrete(limits=c("AA","AB","BA","BB"), labels=str_wrap(c("Ambient Temp, Low pH", "Ambient Temp, Ambient pH", "High Temp, Low pH","High Temp, Ambient pH"), width = 13)) +
  scale_fill_manual(values=c("#0099FF","#0000FF","#FF0000","#990033","#9900CC","#330066")) +
  theme(plot.title = element_text(face="bold", hjust = 0.9, size = 24),axis.title.x = element_blank(),axis.title.y = element_text(size = 20, margin = margin(t = 0, r = 20, b = 0, l = 0)),axis.text.x = element_text(size = 20),axis.text.y = element_text(size = 20),panel.grid.major = element_blank (), panel.grid.minor = element_blank(),panel.background = element_rect(fill='transparent'),plot.background = element_rect(fill='transparent', color=NA), axis.line = element_line(colour = "black"))


### EGGS PER F ###
#Model = full.epf.mod = lme(log(eggperF+0.5) ~ Temp + pH + Temp:pH, method="REML",random= ~1|FullTrt, data = pdt)

#base code box plot
boxplot(pdt$eggperF ~ Treatment, pdt3)

#let's plot it with ggplot now
ggplot(pdt3, aes(x=Treatment, y=eggperF, fill=Treatment, group=Treatment)) + 
  ggtitle("Week 3")+
  ylab("Eggs per Female") +
  geom_boxplot(show.legend = FALSE) +
  scale_x_discrete(limits=c("AA","AB","BA","BB"), labels=str_wrap(c("Ambient Temp, Low pH", "Ambient Temp, Ambient pH", "High Temp, Low pH","High Temp, Ambient pH"), width = 13)) +
  scale_fill_manual(values=c("#0099FF","#0000FF","#FF0000","#990033","#9900CC","#330066")) +
  theme(plot.title = element_text(face="bold", hjust = 0.9, size = 24),axis.title.x = element_blank(),axis.title.y = element_text(size = 20, margin = margin(t = 0, r = 20, b = 0, l = 0)),axis.text.x = element_text(size = 20),axis.text.y = element_text(size = 20),panel.grid.major = element_blank (), panel.grid.minor = element_blank(),panel.background = element_rect(fill='transparent'),plot.background = element_rect(fill='transparent', color=NA), axis.line = element_line(colour = "black"))

### JUVS PER F ###
#Model = full.epf.mod = lme(log(eggperF+0.5) ~ Temp + pH + Temp:pH, method="REML",random= ~1|FullTrt, data = pdt)

#base code box plot
boxplot(pdt$juvperF ~ Treatment, pdt3)

#let's plot it with ggplot now
ggplot(pdt3, aes(x=Treatment, y=juvperF, fill=Treatment, group=Treatment)) + 
  ggtitle("Week 3")+
  ylab("Juveniles per Female") +
  geom_boxplot(show.legend = FALSE) +
  scale_x_discrete(limits=c("AA","AB","BA","BB"), labels=str_wrap(c("Ambient Temp, Low pH", "Ambient Temp, Ambient pH", "High Temp, Low pH","High Temp, Ambient pH"), width = 13)) +
  scale_fill_manual(values=c("#0099FF","#0000FF","#FF0000","#990033","#9900CC","#330066")) +
  theme(plot.title = element_text(face="bold", hjust = 0.9, size = 24),axis.title.x = element_blank(),axis.title.y = element_text(size = 20, margin = margin(t = 0, r = 20, b = 0, l = 0)),axis.text.x = element_text(size = 20),axis.text.y = element_text(size = 20),panel.grid.major = element_blank (), panel.grid.minor = element_blank(),panel.background = element_rect(fill='transparent'),plot.background = element_rect(fill='transparent', color=NA), axis.line = element_line(colour = "black"))

### EGGS+JUVS PER F ###
#Model = full.epf.mod = lme(log(eggperF+0.5) ~ Temp + pH + Temp:pH, method="REML",random= ~1|FullTrt, data = pdt)

#base code box plot
boxplot(pdt$EJperF ~ Treatment, pdt3)

#let's plot it with ggplot now
ggplot(pdt3, aes(x=Treatment, y=EJperF, fill=Treatment, group=Treatment)) + 
  ggtitle("Week 3")+
  ylab("Mean Number of Offspring per Female") +
  geom_boxplot(show.legend = FALSE) +
  scale_x_discrete(limits=c("AA","AB","BA","BB"), labels=str_wrap(c("Ambient Temp, Low pH", "Ambient Temp, Ambient pH", "High Temp, Low pH","High Temp, Ambient pH"), width = 13)) +
  scale_fill_manual(values=c("#0099FF","#0000FF","#FF0000","#990033","#9900CC","#330066")) +
  theme(plot.title = element_text(face="bold", hjust = 0.9, size = 24),axis.title.x = element_blank(),axis.title.y = element_text(size = 20, margin = margin(t = 0, r = 20, b = 0, l = 0)),axis.text.x = element_text(size = 20),axis.text.y = element_text(size = 20),panel.grid.major = element_blank (), panel.grid.minor = element_blank(),panel.background = element_rect(fill='transparent'),plot.background = element_rect(fill='transparent', color=NA), axis.line = element_line(colour = "black"))




# LOAD AND CLEAN DATA FOR SIZES -----------------------------------------

rm(list = ls())

#CALCULATE AREA CONVERSION FACTOR
# took sizes of 4 0.0625mm^2 cells on a hemocytometer using image J
#average those sizes to get our area conversion factor
IJvals = c(70602,70872,72201,71645)
Area_con = mean(IJvals) # = 71330
#so theres 71330 pixels per 0.0625mm^2


#upload data:
main_data <- read_csv("Wk3Wk4DataAnalysis/wk3wk4_juvsizes.csv")
heat_data <- read_csv("HeatTrtAnalysis/heat_trt_juvsizes.csv")

#covert area in pixels to square mm
main_data$area_mm2 = main_data$Area*(0.0625/Area_con)
heat_data$area_mm2 = heat_data$Area*(0.0625/Area_con)
#convert square mm to square um
main_data$area_um2 = main_data$area_mm2*1000000
heat_data$area_um2 = heat_data$area_mm2*1000000

#creating a new column for frandom effect
main_data$FullTrt = paste(main_data$Treatment,"_",main_data$Trt_Rep) 
heat_data$FullTrt = paste(heat_data$pH,"_",heat_data$Heat_Trt,"_",heat_data$Trt_Rep)
#creating a new column with combo trtXph for graphing purposes
heat_data$pHTemp = paste(heat_data$pH,"_",heat_data$Heat_Trt)

#make these new variables factors
main_data$FullTrt = as.factor(main_data$FullTrt)
heat_data$FullTrt = as.factor(heat_data$FullTrt)
heat_data$pHTemp = as.factor(heat_data$pHTemp)

#pull week 3 data
wk3dt = main_data %>% 
  filter(Week == "Week3") %>% 
  filter(!is.na(area_um2)) %>% 
  filter(File_name != "AA3_x4_3") #gets rid of one 0 value in data set

#pull week 4 data
wk4dt = main_data %>% 
  filter(Week == "Week4") %>% 
  filter(!is.na(area_um2))


# Size GRAPHS ----------------------------------------------------------

###WEEK 3###
#Model = full.wk4.mod = lme(log(area_um2) ~ Temp*pH*gammean, method="REML",random= ~1|FullTrt, data = wk4dt)

#base code box plot
boxplot(area_um2 ~ Temp + pH, wk3dt)

ggplot(wk3dt, aes(x=Treatment, y=area_um2, fill=Treatment, group=Treatment)) + 
  ggtitle("Week 3")+
  ylab(bquote("Juvenile Size" (mu*"m"^2))) +
  geom_boxplot(show.legend = FALSE) +
  scale_x_discrete(limits=c("AA","AB","BA","BB"), labels=str_wrap(c("Ambient Temp, Low pH", "Ambient Temp, Ambient pH", "High Temp, Low pH","High Temp, Ambient pH"), width = 13)) +
  scale_fill_manual(values=c("#0099FF","#0000FF","#FF0000","#990033","#9900CC","#330066")) +
  theme(plot.title = element_text(face="bold", hjust = 0.9, size = 24),axis.title.x = element_blank(),axis.title.y = element_text(size = 20, margin = margin(t = 0, r = 20, b = 0, l = 0)),axis.text.x = element_text(size = 20),axis.text.y = element_text(size = 20),panel.grid.major = element_blank (), panel.grid.minor = element_blank(),panel.background = element_rect(fill='transparent'),plot.background = element_rect(fill='transparent', color=NA), axis.line = element_line(colour = "black"))

###WEEK 4###
#Model = full.wk4.mod = lme(log(area_um2) ~ Temp*pH*gammean, method="REML",random= ~1|FullTrt, data = wk4dt)

#base code box plot
boxplot(area_um2 ~ Temp + pH, wk4dt)

ggplot(wk4dt, aes(x=Treatment, y=area_um2, fill=Treatment, group=Treatment)) + 
  ggtitle("Week 4")+
  ylab(bquote("Juvenile Size" (mu*"m"^2))) +
  geom_boxplot(show.legend = FALSE) +
  scale_x_discrete(limits=c("AA","AB","BA","BB"), labels=str_wrap(c("Ambient Temp, Low pH", "Ambient Temp, Ambient pH", "High Temp, Low pH","High Temp, Ambient pH"), width = 13)) +
  scale_fill_manual(values=c("#0099FF","#0000FF","#FF0000","#990033","#9900CC","#330066")) +
  theme(plot.title = element_text(face="bold", hjust = 0.9, size = 24),axis.title.x = element_blank(),axis.title.y = element_text(size = 20, margin = margin(t = 0, r = 20, b = 0, l = 0)),axis.text.x = element_text(size = 20),axis.text.y = element_text(size = 20),panel.grid.major = element_blank (), panel.grid.minor = element_blank(),panel.background = element_rect(fill='transparent'),plot.background = element_rect(fill='transparent', color=NA), axis.line = element_line(colour = "black"))

# COVARIATE RELATIONSHIPS ------------------------------------------

#so, we have the size of each gametophyte count for each photo, but let's calculate the average size of gams photographed in each dish so that we can do data exploration on the relationship of the covariate
wk3wk4nonas = main_data %>% 
  filter(!is.na(area_um2)) 
wk3wk4ave_um = wk3wk4nonas %>%
  group_by(FullTrt) %>% 
  summarise(ave_um = mean(area_um2))
#cool, now we try to merge our 2 tables
main_data2 = merge(main_data, wk3wk4ave_um, by="FullTrt")

#WEEK 3

#lets look at relationship between raw sizes and the covariate
wk3dt2 = main_data2 %>% 
  filter(Week.x == "Week3") %>% 
  filter(!is.na(area_um2)) %>% 
  filter(File_name != "AA3_x4_3") #gets rid of one 0 value in data set

#calculate r^2
w3_r2 = cor(wk3dt2$ave_um,wk3dt2$gammean)^2

#basic plot we are trying to make
plot(wk3dt2$ave_um~wk3dt2$gammean) 
plot(log(wk3dt2$ave_um)~wk3dt2$gammean)

#now plot with ggplot
ggplot(wk3dt2, aes(x=gammean, y=ave_um, color = Treatment.x)) + 
  ggtitle("Week 3")+
  xlab("Mean Number of Gametophytes") +
  ylab(bquote("Mean Juvenile Size" (mu*"m"^2))) +
  geom_point(show.legend = FALSE) +
  scale_color_manual(name = "Treatment", labels=c("Ambient Temp, Low pH", "Ambient Temp, Ambient pH", "High Temp, Low pH","High Temp, Ambient pH"), values = c("AA" = "#0099FF",
                                "AB" ="#0000FF",
                                "BA"="#FF0000",
                                "BB"="#990033")) +
  geom_smooth(method=lm, se=FALSE) + #adds the trendline
  theme(plot.title = element_text(face="bold", hjust = 0.9, size = 24),axis.title.x = element_text(size = 20, margin = margin(t = 20, r = 20, b = 0, l = 0)),axis.title.y = element_text(size = 20, margin = margin(t = 0, r = 20, b = 0, l = 0)),axis.text.x = element_text(size = 20),axis.text.y = element_text(size = 20),panel.grid.major = element_blank (), panel.grid.minor = element_blank(),panel.background = element_rect(fill='transparent'),plot.background = element_rect(fill='transparent', color=NA), axis.line = element_line(colour = "black"), legend.position=c(0.8,0.8), legend.title = element_text(size=18, face="bold"), legend.text = element_text(size=18))

#WEEK 4
#lets look at relationship between raw sizes and the covariate
wk4dt2 = main_data2 %>% 
  filter(Week.x == "Week4") %>% 
  filter(!is.na(area_um2)) 

#calculate r^2
w4_r2 = cor(wk4dt2$ave_um,wk4dt2$gammean)^2

#basic plot we are trying to make
plot(wk4dt2$ave_um~wk4dt2$gammean) 
plot(log(wk4dt2$ave_um)~wk4dt2$gammean)

#now plot with ggplot
ggplot(wk4dt2, aes(x=gammean, y=ave_um, color = Treatment.x)) + 
  ggtitle("Week 4")+
  xlab("Mean Number of Gametophytes") +
  ylab(bquote("Mean Juvenile Size" (mu*"m"^2))) +
  geom_point(show.legend = FALSE) +
  scale_color_manual(name = "Treatment", labels=c("Ambient Temp, Low pH", "Ambient Temp, Ambient pH", "High Temp, Low pH","High Temp, Ambient pH"),values = c("AA" = "#0099FF",
                                "AB" ="#0000FF",
                                "BA"="#FF0000",
                                "BB"="#990033")) +
  geom_smooth(method=lm, se=FALSE) + #adds the trendline
  theme(plot.title = element_text(face="bold", hjust = 0.9, size = 24),axis.title.x = element_text(size = 20, margin = margin(t = 20, r = 20, b = 0, l = 0)),axis.title.y = element_text(size = 20, margin = margin(t = 0, r = 20, b = 0, l = 0)),axis.text.x = element_text(size = 20),axis.text.y = element_text(size = 20),panel.grid.major = element_blank (), panel.grid.minor = element_blank(),panel.background = element_rect(fill='transparent'),plot.background = element_rect(fill='transparent', color=NA), axis.line = element_line(colour = "black"), legend.position=c(0.8,0.8), legend.title = element_text(size=18, face="bold"), legend.text = element_text(size=18))




