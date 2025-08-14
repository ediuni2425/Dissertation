
#Installing + loading packages

#install.packages("lme4")
#install.packages("lmerTest")
#install.packages("Matrix", dependencies = TRUE, type = "source")
#install.packages("emmeans")
#install.packages("ggplot2")
#install.packages("ggplot2")
#install.packages("car")


library(lme4)
library(lmerTest)
library(Matrix)
library(emmeans)
library(ggplot2)
library(patchwork)
library(car)



#Chlorophyll fluorescence trial analysis 

    #Loading data
    cftrial <- read.csv("~/Desktop/MSc EEB/WD/Dissertation/CFtrial.csv")
    
    #Convert categorical variables to factors
    cftrial$plot <- as.factor(cftrial$plot)
    cftrial$time <- as.factor(cftrial$time)
    cftrial$plant <- as.factor(cftrial$plant)
    cftrial$leaf <- as.factor(cftrial$leaf)
    
    #LMM for sources of variation 
    cftrialmodel <- lmer(FvFm ~ plot * time + 
                           (1|plot:plant) + 
                           (1|plot:plant:leaf),
                         data = cftrial)
    
    summary(cftrialmodel)
    
    

#Preliminary germination plots 

    #Loading data 
    germdata <- read.csv("~/Desktop/MSc EEB/WD/Dissertation/germsummary.csv")
    germdata$Block <- as.factor(germdata$Block)
    
    #Plots
    
    ggplot(germdata, aes(x = Date, y = Seedlings, color = Plot, group = Plot)) +
      geom_line() +
      geom_point() +
      theme_minimal() +
      labs(title = "Germination Over Time",
           x = "Date",
           y = "Number of Seedlings")
    
    ggplot(germdata, aes(x = Date, y = Total, group=Plot, colour = Treatment, shape = Block)) +
      geom_line() +
      geom_point() +
      theme_minimal() +
      labs(title = "Total Plants Over Time",
           x = "Date",
           y = "Number of Plants",
           color = "Admixture Combination",
           shape = "Block") + 
      scale_color_manual(values = c(
             "NoAdmixture" = "aquamarine3",  # blue
             "LocalAdmixture" = "royalblue2",  # green
             "CoreEdge" = "lightpink1",  # red
             "RefugiumEdge" = "gold1"   # orange
           )) +
      geom_point(size = 3)
    
    
    #Chlorophyll fluorescence plots and analysis 
    
    #Loading data
    CFdata <- read.csv("~/Desktop/MSc EEB/WD/Dissertation/CFdata.csv")
    CFdata2 <- read.csv("~/Desktop/MSc EEB/WD/Dissertation/CFdata2.csv")
    CFplot1 <- read.csv("~/Desktop/MSc EEB/WD/Dissertation/CFone.csv")
    CFplot2 <- read.csv("~/Desktop/MSc EEB/WD/Dissertation/CFtwo.csv")

    PlantID <- paste(CFdata$Plot, CFdata$Plant, CFdata$Day)
    
    p1 <- ggplot(CFplot1, aes(x = plot, y = FvFm, fill = treatment)) +
      geom_boxplot(outlier.size = 0.8) +
      scale_fill_manual(values = c(
          "NoAdmixture" = "aquamarine3",  # blue
          "LocalAdmixture" = "royalblue2",  # green
          "CoreEdge" = "lightpink1",  # red
          "RefugiumEdge" = "gold1"   # orange
        )) + theme_minimal() +
      geom_jitter(shape = 1, width = 0.2, alpha = 0.7, size = 0.6, colour = "black") + 
      xlab("Plot") + ylab("Chlorophyll Fluoresence (Fv/Fm)") + labs(fill="Admixture Combination") +
      ggtitle("Timepoint 1")
        
    p2 <- ggplot(CFplot2, aes(x = plot, y = FvFm, fill = treatment)) +
      geom_boxplot(outlier.size= 0.8) +
        scale_fill_manual(values = c(
          "NoAdmixture" = "aquamarine3",  # blue
          "LocalAdmixture" = "royalblue2",  # green
          "CoreEdge" = "lightpink1",  # red
          "RefugiumEdge" = "gold1"   # orange
        )) + theme_minimal() +
      geom_jitter(shape = 1, width = 0.2, alpha = 0.7, size = 0.6, colour ="black") +
      xlab("Plot") + ylab("Chlorophyll Fluoresence (Fv/Fm)") + labs(fill="Admixture Combination") +
      ggtitle("Timepoint 2")
    
    p1 / p2 + plot_annotation(title = "Chlorophyll Fluorescence Across Admixture Combinations")
    
    ggplot(CFdata2, aes(x = Plot, y = FvFm, fill = Treatment)) +
      geom_boxplot(outlier.size= 0.8) +
      scale_fill_manual(values = c(
        "NoAdmixture" = "aquamarine3",  # blue
        "LocalAdmixture" = "royalblue2",  # green
        "CoreEdge" = "lightpink1",  # red
        "RefugiumEdge" = "gold1"   # orange
      )) + theme_minimal() +
      geom_jitter(shape = 1, width = 0.2, alpha = 0.7, size = 0.6, colour ="black") +
      xlab("Plot") + ylab("Chlorophyll Fluoresence (Fv/Fm)") + labs(fill="Admixture Combination") + 
      ggtitle("Chlorophyll Fluorescence Across Admixture Combinations")
    
    #Analysis 
    
    #Convert categorical variables to factors
    CFdata$Plot <- as.factor(CFdata$Plot)
    CFdata$PlantID <- as.factor(CFdata$PlantID)
    CFdata$Day <- as.factor(CFdata$Day)
    CFdata$Treatment <- as.factor(CFdata$Treatment)
    
    CFdata$Treatment <- factor(CFdata$Treatment)
    CFdata$Treatment <- relevel(CFdata$Treatment, ref = "NoAdmixture")
    
    CFmodel <- lmer(
      FvFm ~ Treatment + Day +                     
        (1 | Plot) +                           
        (1 | PlantID),                            
      data = CFdata
    )
    
    summary(CFmodel)

    emmeans(CFmodel, pairwise ~ Treatment, adjust = "tukey")
    

#Height plots and analysis 
    
    #Loading data
    
    earlyheight <- read.csv("~/Desktop/MSc EEB/WD/Dissertation/earlyheight.csv")
    lateheight <- read.csv("~/Desktop/MSc EEB/WD/Dissertation/lateheight.csv")

    earlyheight$Treatment <- factor(earlyheight$Treatment)
    earlyheight$Treatment <- relevel(earlyheight$Treatment, ref = "NoAdmixture")
    
    lateheight$Treatment <- factor(lateheight$Treatment)
    lateheight$Treatment <- relevel(lateheight$Treatment, ref = "NoAdmixture")

    #Plots
  

    p4 <- ggplot(lateheight, aes(x = Plot, y = Height, fill = Treatment)) +
      geom_boxplot() +
      scale_fill_manual(values = c(
        "NoAdmixture" = "aquamarine3",
        "LocalAdmixture" = "royalblue2",
        "CoreEdge" = "lightpink1",
        "RefugiumEdge" = "gold1"
      )) +
      theme_minimal() +
      geom_jitter(shape = 1, width = 0.2, alpha = 0.7, size = 0.6, colour = "black") +
      xlab("Plot") +
      ylab("Plant Height (mm)") + ggtitle("Plant Height Across Admixture Combinations") + labs(fill="Admixture Combination")
    
    p4
    
    #Analysis
    
    #Linear mixed model + post-hoc test
    
   
  
    lateheightmodel <- lmer(Height ~ Treatment + (1|Plot) + (1 | Block), data = lateheight)
    summary(lateheightmodel)
    
  
    

    emmeans(lateheightmodel, pairwise ~ Treatment, adjust = "tukey")
    
    #Analysing density-dependent growth
    lateheightmodel2 <- lmer(Height ~ Treatment + Crowd + (1|Plot) + (1 | Block), data = lateheight)
    summary(lateheightmodel2)
    
   #Total plants in a plot analysis
    
    planttotal <- read.csv("~/Desktop/MSc EEB/WD/Dissertation/germtotal.csv")
    
   #Plot
    p7 <- ggplot(planttotal, aes(x = Treatment, y = Plants, fill= Treatment)) +
      geom_boxplot() +
      scale_fill_manual(values = c(
        "NoAdmixture" = "aquamarine3",
        "LocalAdmixture" = "royalblue2",
        "CoreEdge" = "lightpink1",
        "RefugiumEdge" = "gold1"
      )) +
      theme_minimal() +
      geom_jitter(shape = 1, width = 0.2, alpha = 0.7, size = 0.6, colour = "black") +
      xlab("Admixture Combination") +
      ylab("Number of Plants") + ggtitle("Final Population Census per Admixture Combination")
  
    p7
    
    #Analysis
    
    planttotal$Treatment <- factor(planttotal$Treatment)
    planttotal$Treatment <- relevel(planttotal$Treatment, ref = "NoAdmixture")
    
    totalplant <- lmer(Plants ~ Treatment + (1 | Block), data = planttotal)
    summary(totalplant)   

    
    emmeans(totalplant, pairwise ~ Treatment, adjust = "tukey")    
    