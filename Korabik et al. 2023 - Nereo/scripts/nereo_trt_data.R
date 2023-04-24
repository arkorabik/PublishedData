#Nereo Treatment Data 
#Calculations of Ranges of Treatments
#Data by J. Hollarsmith, Code by A. Korabik

#cold treatment
library(readr)
COLD_tidbit <- read_csv("Desktop/Nereo/trt_data/COLD_tidbit.csv")
View(COLD_tidbit)

coldmain = COLD_tidbit[1:1924,] #main expt ended 10/24
coldmain$X3 = as.numeric(coldmain$X3)

library(tidyverse)
coldmain2 = coldmain %>% 
  filter(!is.na(X3))

c_mu = mean(coldmain2$X3)
c_sd = sd(coldmain2$X3)
c_mu #11.63412
c_sd #0.5380758

#heat treatments

HOT_tidbit <- read_csv("Desktop/Nereo/trt_data/HOT_tidbit.csv")
View(HOT_tidbit)

hotmain = HOT_tidbit[1:1924,] #main expt ended 10/24
hotmain$X3 = as.numeric(hotmain$X3)

hotmain2 = hotmain %>% 
  filter(!is.na(X3))

h_mu = mean(hotmain2$X3)
h_sd = sd(hotmain2$X3)
h_mu #15.56069
h_sd #0.8265108

#pH treatments

pH_CA <- read_csv("Desktop/Nereo/trt_data/Hollarsmith_pH_CA.csv")
View(pH_CA)

ph = pH_CA %>% 
  filter(!is.na(pH_calc))

#filter out low pH trts (XA)

lowph = ph %>% 
  filter(Treatment_pH == "Low")

low_mu = mean(lowph$pH_calc)
low_sd = sd(lowph$pH_calc)
low_mu #7.640171
low_sd #0.3222313

#filter out high pH trts (XB)

highph = ph %>% 
  filter(Treatment_pH == "High")

high_mu = mean(highph$pH_calc)
high_sd = sd(highph$pH_calc)
high_mu #7.931902
high_sd #0.2590007
