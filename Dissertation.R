#Chlorophyll fluorescence trial analysis 

#Loading data 
cftrial <- read.csv("~/Desktop/MSc EEB/WD/Dissertation/CFtrial.csv")

#Installing + loading packages
install.packages("lme4")
install.packages("lmerTest")

library(lme4)
library(lmerTest)

#Convert categorical variables to factors
cftrial$plot <- as.factor(cftrial$plot)
cftrial$time <- as.factor(cftrial$time)
cftrial$plant <- as.factor(cftrial$plant)
cftrial$leaf <- as.factor(cftrial$leaf)
