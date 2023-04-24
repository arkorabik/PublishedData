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
library(cowplot)


# LOAD AND CLEAN DATA  -----------------------------------------

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


####WEEK 4 DATA ####
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



#### WEEK 3 DATA ####

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


#### SIZES DATA ####

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

#pull week 3 data
wk3dt = main_data2 %>% 
  filter(Week.x == "Week3") %>% 
  filter(!is.na(area_um2)) %>% 
  filter(File_name != "AA3_x4_3") #gets rid of one 0 value in data set

#pull week 4 data
wk4dt = main_data2 %>% 
  filter(Week.x == "Week4") %>% 
  filter(!is.na(area_um2))

#### COVARIATE RELATIONSHIPS ####

#so, we have the size of each gametophyte count for each photo, but let's calculate the average size of gams photographed in each dish so that we can do data exploration on the relationship of the covariate
wk3wk4nonas = main_data %>% 
  filter(!is.na(area_um2)) 
wk3wk4ave_um = wk3wk4nonas %>%
  group_by(FullTrt) %>% 
  summarise(ave_um = mean(area_um2))
#cool, now we try to merge our 2 tables
main_data2 = merge(main_data, wk3wk4ave_um, by="FullTrt")

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

# GROUPING GRAPHS -----------------------------------------------

#run graphs separately first to make sure they work then move down here to set mfrow

####Male and female gametophyte numbers####
#set dev


#Week 4 Females
FNo4 = ggplot(pdt, aes(x=Treatment, y=Females, fill=Treatment, group=Treatment)) + 
  ggtitle("Week 4")+
  ylab("Number of Females") +
  geom_boxplot(show.legend = FALSE) +
  stat_summary(fun.y=mean, geom="point", shape=18, size=8, color="black", fill="black") +
  scale_x_discrete(limits=c("AA","AB","BA","BB"), labels=str_wrap(c("Ambient Temp, Low pH", "Ambient Temp, Ambient pH", "High Temp, Low pH","High Temp, Ambient pH"), width = 13)) +
  scale_y_continuous(limits = c(0,40), breaks=seq(from=0,to=35,by=5))+
  scale_fill_manual(values=c("#0099FF","#0000FF","#FF0000","#990033","#9900CC","#330066")) +
  theme(plot.title = element_text(face="bold", hjust = 0.9, size = 20),axis.title.x = element_blank(),axis.title.y = element_text(face = "bold",size = 20, margin = margin(t = 0, r = 20, b = 0, l = 0)),axis.text.x = element_blank(),axis.text.y = element_text(size = 16),panel.grid.major = element_blank (), panel.grid.minor = element_blank(),panel.background = element_rect(fill='transparent'),plot.background = element_rect(fill='transparent', color=NA), axis.line = element_line(colour = "black"))


#Week 4 Males
MNo4 = ggplot(pdt, aes(x=Treatment, y=Males, fill=Treatment, group=Treatment)) + 
  ggtitle("Week 4")+
  ylab("Number of Males") +
  geom_boxplot(show.legend = FALSE) +
  stat_summary(fun.y=mean, geom="point", shape=18, size=8, color="black", fill="black") +
  scale_x_discrete(limits=c("AA","AB","BA","BB"), labels=str_wrap(c("Ambient Temp, Low pH", "Ambient Temp, Ambient pH", "High Temp, Low pH","High Temp, Ambient pH"), width = 13)) +
  scale_y_continuous(limits = c(0,40), breaks=seq(from=0,to=35,by=5))+
  scale_fill_manual(values=c("#0099FF","#0000FF","#FF0000","#990033","#9900CC","#330066")) +
  theme(plot.title = element_blank (),axis.title.x = element_blank(),axis.title.y = element_text(face = "bold",size = 20, margin = margin(t = 0, r = 20, b = 0, l = 0)),axis.text.x = element_text(size = 16),axis.text.y = element_text(size = 16),panel.grid.major = element_blank (), panel.grid.minor = element_blank(),panel.background = element_rect(fill='transparent'),plot.background = element_rect(fill='transparent', color=NA), axis.line = element_line(colour = "black"))

MNo4

plot_grid(
  FNo4,MNo4,
  ncol = 1)


####Juvenile and egg numbers ####

#Week 4 Eggs
ENo4 = ggplot(pdt, aes(x=Treatment, y=Eggs, fill=Treatment, group=Treatment)) + 
  ggtitle("Week 4")+
  ylab("Number of Eggs") +
  geom_boxplot(show.legend = FALSE) +
  stat_summary(fun=mean, geom="point", shape=18, size=8, color="black", fill="black") +
  scale_x_discrete(limits=c("AA","AB","BA","BB"), labels=str_wrap(c("Ambient Temp, Low pH", "Ambient Temp, Ambient pH", "High Temp, Low pH","High Temp, Ambient pH"), width = 13)) +
  scale_y_continuous(limits = c(0,40), breaks=seq(from=0,to=35,by=5))+
  scale_fill_manual(values=c("#0099FF","#0000FF","#FF0000","#990033","#9900CC","#330066")) +
  theme(plot.title = element_text(face="bold", hjust = 0.9, size = 20),axis.title.x = element_blank(),axis.title.y = element_text(face = "bold",size = 20, margin = margin(t = 0, r = 20, b = 0, l = 0)),axis.text.x = element_blank(),axis.text.y = element_text(size = 16),panel.grid.major = element_blank (), panel.grid.minor = element_blank(),panel.background = element_rect(fill='transparent'),plot.background = element_rect(fill='transparent', color=NA), axis.line = element_line(colour = "black"))

#Week 4 Juvs
JNo4 = ggplot(pdt, aes(x=Treatment, y=Juveniles, fill=Treatment, group=Treatment)) +
  ggtitle("Week 4")+
  ylab("Number of Sporelings") +
  geom_boxplot(show.legend = FALSE) +
  stat_summary(fun=mean, geom="point", shape=18, size=8, color="black", fill="black") +
  scale_x_discrete(limits=c("AA","AB","BA","BB"), labels=str_wrap(c("Ambient Temp, Low pH", "Ambient Temp, Ambient pH", "High Temp, Low pH","High Temp, Ambient pH"), width = 13)) +
  scale_y_continuous(limits = c(0,40), breaks=seq(from=0,to=35,by=5))+
  scale_fill_manual(values=c("#0099FF","#0000FF","#FF0000","#990033","#9900CC","#330066")) +
  theme(plot.title = element_blank (),axis.title.x = element_blank(),axis.title.y = element_text(face = "bold",size = 20, margin = margin(t = 0, r = 20, b = 0, l = 0)),axis.text.x = element_text(size = 16),axis.text.y = element_text(size = 16),panel.grid.major = element_blank (), panel.grid.minor = element_blank(),panel.background = element_rect(fill='transparent'),plot.background = element_rect(fill='transparent', color=NA), axis.line = element_line(colour = "black"))


plot_grid(
  ENo4, JNo4,
  ncol = 1)





####offspring per female####


#Week 3 EPF
EPF3 = ggplot(pdt3, aes(x=Treatment, y=eggperF, fill=Treatment, group=Treatment)) + 
  ggtitle("Week 3")+
  ylab("Eggs per Female") +
  geom_boxplot(show.legend = FALSE) +
  stat_summary(fun.y=mean, geom="point", shape=18, size=5, color="black", fill="black") +
  scale_x_discrete(limits=c("AA","AB","BA","BB"), labels=str_wrap(c("Ambient Temp, Low pH", "Ambient Temp, Ambient pH", "High Temp, Low pH","High Temp, Ambient pH"), width = 13)) +
  scale_y_continuous(limits = c(0,4), breaks=seq(from=0,to=4,by=0.5))+
  scale_fill_manual(values=c("#0099FF","#0000FF","#FF0000","#990033","#9900CC","#330066")) +
  theme(plot.title = element_text(face="bold", hjust = 0.9, size = 12),axis.title.x = element_blank(),axis.title.y = element_text(face = "bold",size = 12, margin = margin(t = 0, r = 20, b = 0, l = 0)),axis.text.x = element_text(size = 9),axis.text.y = element_text(size = 9),panel.grid.major = element_blank (), panel.grid.minor = element_blank(),panel.background = element_rect(fill='transparent'),plot.background = element_rect(fill='transparent', color=NA), axis.line = element_line(colour = "black"))

#Week 3 JPF
JPF3 = ggplot(pdt3, aes(x=Treatment, y=juvperF, fill=Treatment, group=Treatment)) + 
  ggtitle("Week 3")+
  ylab("Sporelings per Female") +
  geom_boxplot(show.legend = FALSE) +
  stat_summary(fun.y=mean, geom="point", shape=18, size=5, color="black", fill="black") +
  scale_x_discrete(limits=c("AA","AB","BA","BB"), labels=str_wrap(c("Ambient Temp, Low pH", "Ambient Temp, Ambient pH", "High Temp, Low pH","High Temp, Ambient pH"), width = 13)) +
  scale_y_continuous(limits = c(0,4), breaks=seq(from=0,to=4,by=0.5))+
  scale_fill_manual(values=c("#0099FF","#0000FF","#FF0000","#990033","#9900CC","#330066")) +
  theme(plot.title = element_blank (),axis.title.x = element_blank(),axis.title.y = element_text(face = "bold",size = 12, margin = margin(t = 0, r = 20, b = 0, l = 0)),axis.text.x = element_text( size = 9),axis.text.y = element_text(size = 9),panel.grid.major = element_blank (), panel.grid.minor = element_blank(),panel.background = element_rect(fill='transparent'),plot.background = element_rect(fill='transparent', color=NA), axis.line = element_line(colour = "black"))

#Week 3 OPF
OPF3 = ggplot(pdt3, aes(x=Treatment, y=EJperF, fill=Treatment, group=Treatment)) + 
  ggtitle("Week 3")+
  ylab("Offspring per Female") +
  geom_boxplot(show.legend = FALSE) +
  stat_summary(fun.y=mean, geom="point", shape=18, size=5, color="black", fill="black") +
  scale_x_discrete(limits=c("AA","AB","BA","BB"), labels=str_wrap(c("Ambient Temp, Low pH", "Ambient Temp, Ambient pH", "High Temp, Low pH","High Temp, Ambient pH"), width = 13)) +
  scale_y_continuous(limits = c(0,4), breaks=seq(from=0,to=4,by=0.5))+
  scale_fill_manual(values=c("#0099FF","#0000FF","#FF0000","#990033","#9900CC","#330066")) +
  theme(plot.title = element_blank (),axis.title.x = element_blank(),axis.title.y = element_text(face = "bold",size = 12, margin = margin(t = 0, r = 20, b = 0, l = 0)),axis.text.x = element_text( size = 9),axis.text.y = element_text(size = 9),panel.grid.major = element_blank (), panel.grid.minor = element_blank(),panel.background = element_rect(fill='transparent'),plot.background = element_rect(fill='transparent', color=NA), axis.line = element_line(colour = "black"))

#Week 4 EPF
EPF4 = ggplot(pdt, aes(x=Treatment, y=eggperF, fill=Treatment, group=Treatment)) + 
  ggtitle("Week 4")+
  ylab("Eggs per Female") +
  geom_boxplot(show.legend = FALSE) +
  stat_summary(fun.y=mean, geom="point", shape=18, size=5, color="black", fill="black") +
  scale_x_discrete(limits=c("AA","AB","BA","BB"), labels=str_wrap(c("Ambient Temp, Low pH", "Ambient Temp, Ambient pH", "High Temp, Low pH","High Temp, Ambient pH"), width = 13)) +
  scale_y_continuous(limits = c(0,4), breaks=seq(from=0,to=4,by=0.5))+
  scale_fill_manual(values=c("#0099FF","#0000FF","#FF0000","#990033","#9900CC","#330066")) +
  theme(plot.title = element_text(face="bold", hjust = 0.9, size = 12),axis.title.x = element_blank(),axis.title.y = element_blank (),axis.text.x = element_text(size = 9),axis.text.y = element_text(size = 9),panel.grid.major = element_blank (), panel.grid.minor = element_blank(),panel.background = element_rect(fill='transparent'),plot.background = element_rect(fill='transparent', color=NA), axis.line = element_line(colour = "black"))

#Week 4 JPF
JPF4 = ggplot(pdt, aes(x=Treatment, y=juvperF, fill=Treatment, group=Treatment)) + 
  ggtitle("Week 4")+
  ylab("Sporelings per Female") +
  geom_boxplot(show.legend = FALSE) +
  stat_summary(fun.y=mean, geom="point", shape=18, size=5, color="black", fill="black") +
  scale_x_discrete(limits=c("AA","AB","BA","BB"), labels=str_wrap(c("Ambient Temp, Low pH", "Ambient Temp, Ambient pH", "High Temp, Low pH","High Temp, Ambient pH"), width = 13)) +
  scale_y_continuous(limits = c(0,4), breaks=seq(from=0,to=4,by=0.5))+
  scale_fill_manual(values=c("#0099FF","#0000FF","#FF0000","#990033","#9900CC","#330066")) +
  theme(plot.title = element_blank (),axis.title.x = element_blank(),axis.title.y = element_blank (),axis.text.x = element_text(size = 9),axis.text.y = element_text(size = 9),panel.grid.major = element_blank (), panel.grid.minor = element_blank(),panel.background = element_rect(fill='transparent'),plot.background = element_rect(fill='transparent', color=NA), axis.line = element_line(colour = "black"))

#Week 4 OPF
OPF4 = ggplot(pdt, aes(x=Treatment, y=EJperF, fill=Treatment, group=Treatment)) + 
  ggtitle("Week 4")+
  ylab("Offspring per Female") +
  geom_boxplot(show.legend = FALSE) +
  stat_summary(fun.y=mean, geom="point", shape=18, size=5, color="black", fill="black") +
  scale_x_discrete(limits=c("AA","AB","BA","BB"), labels=str_wrap(c("Ambient Temp, Low pH", "Ambient Temp, Ambient pH", "High Temp, Low pH","High Temp, Ambient pH"), width = 13)) +
  scale_y_continuous(limits = c(0,4), breaks=seq(from=0,to=4,by=0.5))+
  scale_fill_manual(values=c("#0099FF","#0000FF","#FF0000","#990033","#9900CC","#330066")) +
  theme(plot.title = element_blank (),axis.title.x = element_blank(),axis.title.y = element_blank (),axis.text.x = element_text(size = 9),axis.text.y = element_text(size = 9),panel.grid.major = element_blank (), panel.grid.minor = element_blank(),panel.background = element_rect(fill='transparent'),plot.background = element_rect(fill='transparent', color=NA), axis.line = element_line(colour = "black"))


plot_grid(
  EPF3, EPF4, JPF3, JPF4,OPF3, OPF4,
  ncol = 2)



####sizes + Size by covariate ####

#Week 4 Sizes
Size4 = ggplot(wk4dt, aes(x=Treatment.x, y=area_um2, fill=Treatment.x, group=Treatment.x)) + 
  ggtitle("Week 4")+
  ylab(bquote("Mean Sporeling Size" (mu*"m"^2))) +
  geom_boxplot(show.legend = FALSE) +
  stat_summary(fun=mean, geom="point", shape=18, size=8, color="black", fill="black") +
  scale_x_discrete(limits=c("AA","AB","BA","BB"), labels=str_wrap(c("Ambient Temp, Low pH", "Ambient Temp, Ambient pH", "High Temp, Low pH","High Temp, Ambient pH"), width = 13)) +
  scale_y_continuous(limits = c(0,15000), breaks=seq(from=0,to=15000,by=5000))+
  scale_fill_manual(values=c("#0099FF","#0000FF","#FF0000","#990033","#9900CC","#330066")) +
  theme(plot.title = element_text(face="bold", hjust = 0.9, size = 20),axis.title.x = element_blank(),axis.title.y = element_text(size = 16, margin = margin(t = 0, r = 20, b = 0, l = 0)),axis.text.x = element_text(size = 16),axis.text.y = element_text(size = 16),panel.grid.major = element_blank (), panel.grid.minor = element_blank(),panel.background = element_rect(fill='transparent'),plot.background = element_rect(fill='transparent', color=NA), axis.line = element_line(colour = "black"))


#Week 4 Cov
Cov4 = ggplot(wk4dt, aes(x=gammean, y=ave_um, color = Treatment.x)) + 
  ggtitle("Week 4")+
  xlab("Mean Number of Gametophytes") +
  ylab(bquote("Mean Sporeling Size" (mu*"m"^2))) +
  geom_point(show.legend = FALSE) +
  scale_color_manual(name = "Treatment", labels=c("Ambient Temp, Low pH", "Ambient Temp, Ambient pH", "High Temp, Low pH","High Temp, Ambient pH"),values = c("AA" = "#0099FF","AB" ="#0000FF","BA"="#FF0000","BB"="#990033")) +
  scale_x_continuous(limits = c(5,40), breaks=seq(from=10,to=40,by=10))+
  geom_smooth(method=lm, se=FALSE) + #adds the trendline
  geom_smooth(method = lm,se=F,aes(group=1),color='black', linetype="dotted")+
  theme(plot.title = element_text(face="bold", hjust = 0.9, size = 20),axis.title.x = element_text(size = 16, margin = margin(t = 20, r = 20, b = 0, l = 0)),axis.title.y = element_text(size = 16, margin = margin(t = 0, r = 20, b = 0, l = 0)),axis.text.x = element_text(size = 16),axis.text.y = element_text(size = 16),panel.grid.major = element_blank (), panel.grid.minor = element_blank(),panel.background = element_rect(fill='transparent'),plot.background = element_rect(fill='transparent', color=NA), axis.line = element_line(colour = "black"), legend.position=c(0.7,0.8), legend.title = element_text(size=14, face="bold"), legend.text = element_text(size=14))


plot_grid(
  Size4,Cov4,
  ncol = 2)


#### proportion productive females ####

#Week 4 PP
ggplot(pdt, aes(x=Treatment, y=Prop_prod, fill=Treatment, group=Treatment)) + 
  ggtitle("Week 4")+
  ylab("Proportion of Productive Females") +
  geom_boxplot(show.legend = FALSE) +
  stat_summary(fun.y=mean, geom="point", shape=18, size=8, color="black", fill="black") +
  scale_x_discrete(limits=c("AA","AB","BA","BB"), labels=str_wrap(c("Ambient Temp, Low pH", "Ambient Temp, Ambient pH", "High Temp, Low pH","High Temp, Ambient pH"), width = 13)) +
  scale_fill_manual(values=c("#0099FF","#0000FF","#FF0000","#990033","#9900CC","#330066")) +
  theme(plot.title = element_text(face="bold", hjust = 0.9, size = 18),axis.title.x = element_blank(),axis.title.y = element_text(size = 16, margin = margin(t = 0, r = 20, b = 0, l = 0)),axis.text.x = element_text(size = 16),axis.text.y = element_text(size = 16),panel.grid.major = element_blank (), panel.grid.minor = element_blank(),panel.background = element_rect(fill='transparent'),plot.background = element_rect(fill='transparent', color=NA), axis.line = element_line(colour = "black"))





