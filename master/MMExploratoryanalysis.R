library(haven)
library(plyr)
library(ggplot2)
library(ggpubr)
library(tidyr)
library(dplyr)

#Load data
HUNT12 <- read.csv("/home/magnwoln/Masteroppgave/INLAresultsHUNT12.csv")
HUNT23 <- read.csv("/home/magnwoln/Masteroppgave/INLAresultsHUNT23.csv")
HUNT34 <- read.csv("/home/magnwoln/Masteroppgave/INLAresultsHUNT34.csv")

data_full = read.csv("/home/magnwoln/Masteroppgave/INLAresultsdata_full.csv")
unknownmissing23 = read.csv("/home/magnwoln/Masteroppgave/INLAresultsunknownmissing23.csv")

#The blood pressure measurments are the mean of "all" the measurments done each HUNT 

# Histogram and density plot for age

plotage1 <- ggplot(data_full, aes(x = PartAg.NT1BLQ1)) + 
  geom_density(lwd = 1, colour = 2, fill = 2, alpha = 0.25) + xlim(15,105) + ylim(0,0.03) + labs(x = "", y = "")
plotage2 <- ggplot(data_full, aes(x = PartAg.NT2BLQ1)) + 
  geom_density(lwd = 1, colour = 2, fill = 2, alpha = 0.25)+ xlim(15,105) + ylim(0,0.03) + labs(x = "", y = "")
plotage3 <- ggplot(data_full, aes(x = PartAg.NT3BLQ1)) + 
  geom_density(lwd = 1, colour = 2, fill = 2, alpha = 0.25)+ xlim(15,105) + ylim(0,0.03) + labs(x = "", y = "")
plotage4 <- ggplot(data_full, aes(x = PartAg.NT4BLQ1)) + 
  geom_density(lwd = 1, colour = 2, fill = 2, alpha = 0.25)+ xlim(15,105) + ylim(0,0.03) + labs(x = "Age", y = "")

age <- ggarrange(plotage1, plotage2, plotage3, plotage4,
          labels = c("HUNT1", "HUNT2", "HUNT3", "HUNT4"),
          ncol = 2, nrow = 2)
#age

# Histogram and density plot for BP I NEED TO CHANGE THIS SO IT IS THE CORRECTED WITH MEDICINE NUMBERS

plotbp1 <- ggplot(data_full, aes(x = BPSystMn12.NT1BLM)) + 
  geom_density(lwd = 1, colour = 4, fill = 4, alpha = 0.25) + xlim(65,250) + ylim(0,0.025) + labs(x = "", y = "(%)")
plotbp2 <- ggplot(data_full, aes(x = BPSystMn23.NT2BLM)) + 
  geom_density(lwd = 1, colour = 4, fill = 4, alpha = 0.25)+ xlim(65,250) + ylim(0,0.025) + labs(x = "", y = "(%)")
plotbp3 <- ggplot(data_full, aes(x = BPSystMn23.NT3BLM)) + 
  geom_density(lwd = 1, colour = 4, fill = 4, alpha = 0.25)+ xlim(65,250) + ylim(0,0.025) + labs(x = "", y = "(%)")
plotbp4 <- ggplot(data_full, aes(x = BPSystMn23.NT4BLM)) + 
  geom_density(lwd = 1, colour = 4, fill = 4, alpha = 0.25)+ xlim(65,250) + ylim(0,0.025) + labs(x = "Blood Pressure", y = "(%)")

BP <- ggarrange(plotbp1, plotbp2, plotbp3, plotbp4,
                 labels = c("HUNT1", "HUNT2", "HUNT3", "HUNT4"),
                 ncol = 2, nrow = 2)
#BP

# density plot for BMI

plotbmi1 <- ggplot(data_full, aes(x = Bmi.NT1BLM)) + 
  geom_density(lwd = 1, colour = 3, fill = 3, alpha = 0.25) + xlim(15,45) + ylim(0,0.15) + labs(x = "", y = "")
plotbmi2 <- ggplot(data_full, aes(x = Bmi.NT2BLM)) + 
  geom_density(lwd = 1, colour = 3, fill = 3, alpha = 0.25)+ xlim(15,45) + ylim(0,0.15) + labs(x = "", y = "")
plotbmi3 <- ggplot(data_full, aes(x = Bmi.NT3BLM)) + 
  geom_density(lwd = 1, colour = 3, fill = 3, alpha = 0.25)+ xlim(15,45) + ylim(0,0.15) + labs(x = "", y = "")
plotbmi4 <- ggplot(data_full, aes(x = newdata3..Bmi.NT4BLM.)) + 
  geom_density(lwd = 1, colour = 3, fill = 3, alpha = 0.25)+ xlim(15,45) + ylim(0,0.15) + labs(x = "BMI", y = "")

BMI <- ggarrange(plotbmi1, plotbmi2, plotbmi3, plotbmi4,
                labels = c("HUNT1", "HUNT2", "HUNT3", "HUNT4"),
                ncol = 2, nrow = 2)

#BMI

#Histogram for sex 
plotsex1 <- ggplot(HUNT12, aes(x = as.factor(sex), fill=as.factor(sex))) + 
  geom_bar(alpha=0.5) + labs(x = "", y = "Number of participants") + scale_fill_manual(values = c(2, 4) ) +
  theme(legend.position="none") + ylim(0,40000)
plotsex2 <- ggplot(HUNT23, aes(x = as.factor(sex), fill=as.factor(sex))) + 
  geom_bar(alpha=0.5) + labs(x = "", y = "Number of participants") + scale_fill_manual(values = c(2, 4) ) +
  theme(legend.position="none")+ ylim(0,40000)
plotsex3 <- ggplot(HUNT34, aes(x = as.factor(sex), fill=as.factor(sex))) + 
  geom_bar(alpha=0.5) + labs(x = "", y = "Number of participants") + scale_fill_manual(values = c(2, 4) ) +
  theme(legend.position="none")+ ylim(0,40000)
plotsex4 <- ggplot(HUNT34[which(HUNT34$Rtotal == 0), ], aes(x = as.factor(sex), fill=as.factor(sex))) + 
  geom_bar(alpha=0.5) + labs(x = "", y = "") + scale_fill_manual(values = c(2, 4) ) +
  theme(legend.position="none") + labs(x = "Sex", y = "Number of participants") + ylim(0,40000)
#A collection of BP, age and BMI

sexplot <- ggarrange(plotsex1, plotsex2, plotsex3, plotsex4,
                 labels = c("HUNT1", "HUNT2", "HUNT3", "HUNT4"),
                 ncol = 2, nrow = 2)
sexplot

bpagebmi <- ggarrange(plotbp1, plotage1, plotbmi1, plotsex1, plotbp2, plotage2, plotbmi2, plotsex2, 
                      plotbp3, plotage3, plotbmi3, plotsex3, plotbp4, plotage4, plotbmi4, plotsex4,      
                      labels = c("HUNT1", "", "","", "HUNT2", "", "","", "HUNT3", "", "", "","HUNT4", "", "",""),
                  ncol = 4, nrow = 4)
bpagebmi

ggsave("bpagebmi.png", plot = bpagebmi, path = "/home/magnwoln/Masteroppgave/Figs/", width = 20, height = 28, units = "cm")

#Then for the cleaned and adjusted data
HUNT12 <- read.csv("/home/magnwoln/Masteroppgave/INLAresultsHUNT12.csv")
HUNT23 <- read.csv("/home/magnwoln/Masteroppgave/INLAresultsHUNT23.csv")
HUNT34 <- read.csv("/home/magnwoln/Masteroppgave/INLAresultsHUNT34.csv")

# Get Blood Pressure mean under different cases
summary(HUNT12)
summary(HUNT23)
summary(HUNT34)

mean(HUNT12[which(HUNT12$R1 == 1), ]$bp_1_corr)
mean(HUNT12[which(HUNT12$R2 == 1), ]$bp_1_corr)
mean(HUNT12[which(HUNT12$R3 == 1), ]$bp_1_corr)
mean(HUNT12[which(HUNT12$Rtotal == 1), ]$bp_1_corr)

mean(HUNT23[which(HUNT23$R1 == 1), ]$bp_1_corr)
mean(HUNT23[which(HUNT23$R2 == 1), ]$bp_1_corr)
mean(HUNT23[which(HUNT23$R3 == 1), ]$bp_1_corr)
mean(HUNT23[which(HUNT23$Rtotal == 1), ]$bp_1_corr)


mean(HUNT34[which(HUNT34$R1 == 1), ]$bp_1_corr)
mean(HUNT34[which(HUNT34$R2 == 1), ]$bp_1_corr)
mean(HUNT34[which(HUNT34$R3 == 1), ]$bp_1_corr)
mean(HUNT34[which(HUNT34$Rtotal == 1), ]$bp_1_corr)

# Plot density for all the Blood Pressure measurements
colors <- c("All Data" = "black", "Rtotal - Missing" = "darkorange","R1 - Deceased" = "bisque3", "R2 - Moved Out" = "azure4", "R3 - Invited, but no show" = "cadetblue")

HUNT12_bp <- ggplot() + 
  geom_density(data = HUNT12, aes(x = bp_1_corr, color = "All Data"), lwd = 1, alpha = 0) + 
  geom_density(data = HUNT12[which(HUNT12$R1 == 1), ], aes(x = bp_1_corr, color = "R1 - Deceased"), lwd = 1, alpha = 0) + #This one is zero!!
  geom_density(data = HUNT12[which(HUNT12$R2 == 1), ], aes(x = bp_1_corr, color = "R2 - Moved Out"), lwd = 1,  alpha = 0) + #This one is 4!!
  geom_density(data = HUNT12[which(HUNT12$R3 == 1), ], aes(x = bp_1_corr, color = "R3 - Invited, but no show"), lwd = 1, alpha = 0) +
  geom_density(data = HUNT12[which(HUNT12$Rtotal == 1), ], aes(x = bp_1_corr, color = "Rtotal - Missing"), lwd = 1, alpha = 0) + 
  labs(x = "", y = "(%)", color = "Colour Coding") +
  scale_color_manual(values = colors) +
  theme(legend.position = c(0.80, 0.75), legend.key.size = unit(0.3, 'cm')) +
  xlim(65,250) + ylim(0,0.03)

#HUNT12_bp

HUNT23_bp <- ggplot() + 
  geom_density(data = HUNT23, aes(x = bp_1_corr, color = "All Data"), lwd = 1, alpha = 0) + 
  geom_density(data = HUNT23[which(HUNT23$Rtotal == 1), ], aes(x = bp_1_corr, color = "Rtotal - Missing"), lwd = 1, alpha = 0) + 
  geom_density(data = HUNT23[which(HUNT23$R1 == 1), ], aes(x = bp_1_corr, color = "R1 - Deceased"), lwd = 1, alpha = 0) + 
  geom_density(data = HUNT23[which(HUNT23$R2 == 1), ], aes(x = bp_1_corr, color = "R2 - Moved Out"), lwd = 1,  alpha = 0) + 
  geom_density(data = HUNT23[which(HUNT23$R3 == 1), ], aes(x = bp_1_corr, color = "R3 - Invited, but no show"), lwd = 1, alpha = 0) +
  labs(x = "", y = "", color = "Colour Coding") +
  scale_color_manual(values = colors) +
  theme(legend.position = c(0.80, 0.75), legend.key.size = unit(0.3, 'cm')) +
  xlim(65,250) + ylim(0,0.03)

HUNT34_bp <- ggplot() + 
  geom_density(data = HUNT34, aes(x = bp_1_corr, color = "All Data"), lwd = 1, alpha = 0) + 
  geom_density(data = HUNT34[which(HUNT34$Rtotal == 1), ], aes(x = bp_1_corr, color = "Rtotal - Missing"), lwd = 1, alpha = 0) + 
  geom_density(data = HUNT34[which(HUNT34$R1 == 1), ], aes(x = bp_1_corr, color = "R1 - Deceased"), lwd = 1, alpha = 0) + 
  geom_density(data = HUNT34[which(HUNT34$R2 == 1), ], aes(x = bp_1_corr, color = "R2 - Moved Out"), lwd = 1,  alpha = 0) + 
  geom_density(data = HUNT34[which(HUNT34$R3 == 1), ], aes(x = bp_1_corr, color = "R3 - Invited, but no show"), lwd = 1, alpha = 0) +
  labs(x = "Blood Pressure", y = "(%)", color = "Colour Coding") +
  scale_color_manual(values = colors) +
  theme(legend.position = c(0.80, 0.75), legend.key.size = unit(0.3, 'cm')) +
  xlim(65,250) + ylim(0,0.03)

all_bp_missing <- ggarrange(HUNT12_bp, HUNT23_bp, HUNT34_bp,      
                      labels = c("HUNT12", "HUNT23", "HUNT34"),
                      ncol = 3, nrow = 1)
all_bp_missing

ggsave("all_bp_missing.png", plot = all_bp_missing, path = "/home/magnwoln/Masteroppgave/Figs/", width = 20, height = 7, units = "cm")

# Present the missing of HUNT23 with age etc 

HUNT23_R1 <- HUNT23[which(HUNT23$R1 == 1), ]
HUNT23_R2 <- HUNT23[which(HUNT23$R2 == 1), ]
HUNT23_R3 <- HUNT23[which(HUNT23$R3 == 1), ]
HUNT23_Rtotal <- HUNT23[which(HUNT23$Rtotal == 1), ]

# Histogram and density plot for age

plotageR1 <- ggplot() + 
  geom_density(data=HUNT23, aes(x = age_1, y = ..density..),lwd = 1, colour = "grey", fill="grey", alpha = 0.25) +
  geom_density(data=HUNT23_R1, aes(x = age_1, y = ..density..), lwd = 1, colour = 2, fill = 2, alpha = 0.25) + xlim(15,105) + ylim(0,0.055) + labs(x = "", y = "")
plotageR2 <- ggplot() + 
  geom_density(data=HUNT23, aes(x = age_1, y = ..density..),lwd = 1, colour = "grey", fill="grey", alpha = 0.25) +
  geom_density(data=HUNT23_R2, aes(x = age_1, y = ..density..),lwd = 1, colour = 2, fill = 2, alpha = 0.25)+ xlim(15,105) + ylim(0,0.055) + labs(x = "", y = "")
plotageR3 <- ggplot() + 
  geom_density(data=HUNT23, aes(x = age_1, y = ..density..),lwd = 1, colour = "grey", fill="grey", alpha = 0.25) +
  geom_density(data=HUNT23_R3, aes(x = age_1, y = ..density..),lwd = 1, colour = 2, fill = 2, alpha = 0.25)+ xlim(15,105) + ylim(0,0.055) + labs(x = "", y = "")
plotageR4 <- ggplot() + 
  geom_density(data=HUNT23, aes(x = age_1, y = ..density..),lwd = 1, colour = "grey", fill="grey", alpha = 0.25) +
  geom_density(data=HUNT23_Rtotal, aes(x = age_1, y = ..density..),lwd = 1, colour = 2, fill = 2, alpha = 0.25)+ xlim(15,105) + ylim(0,0.055) + labs(x = "Age", y = "")

ageR <- ggarrange(plotageR1, plotageR2, plotageR3, plotageR4,
                 labels = c("R1", "R2", "R3", "Rtotal"),
                 ncol = 2, nrow = 2)
# ageR
# Histogram and density plot for BP

plotbpR1 <- ggplot() + 
  geom_density(data=HUNT23, aes(x = bp_1_corr, y = ..density..),lwd = 1, colour = "grey", fill="grey", alpha = 0.25) +
  geom_density(data=HUNT23_R1, aes(x = bp_1_corr, y = ..density..),lwd = 1, colour = 4, fill = 4, alpha = 0.25) + xlim(65,250) + ylim(0,0.03) + labs(x = "", y = "(%)")
plotbpR2 <- ggplot() + 
  geom_density(data=HUNT23, aes(x = bp_1_corr, y = ..density..),lwd = 1, colour = "grey", fill="grey", alpha = 0.25) +
  geom_density(data=HUNT23_R2, aes(x = bp_1_corr, y = ..density..),lwd = 1, colour = 4, fill = 4, alpha = 0.25)+ xlim(65,250) + ylim(0,0.03) + labs(x = "", y = "(%)")
plotbpR3 <- ggplot() + 
  geom_density(data=HUNT23, aes(x = bp_1_corr, y = ..density..),lwd = 1, colour = "grey", fill="grey", alpha = 0.25) +
  geom_density(data=HUNT23_R3, aes(x = bp_1_corr, y = ..density..),lwd = 1, colour = 4, fill = 4, alpha = 0.25)+ xlim(65,250) + ylim(0,0.03) + labs(x = "", y = "(%)")
plotbpR4 <- ggplot() + 
  geom_density(data=HUNT23, aes(x = bp_1_corr, y = ..density..),lwd = 1, colour = "grey", fill="grey", alpha = 0.25) +
  geom_density(data=HUNT23_Rtotal, aes(x = bp_1_corr, y = ..density..),lwd = 1, colour = 4, fill = 4, alpha = 0.25)+ xlim(65,250) + ylim(0,0.03) + labs(x = "Blood Pressure", y = "(%)")

BPR <- ggarrange(plotbpR1, plotbpR2, plotbpR3, plotbpR4,
                labels = c("R1", "R2", "R3", "Rtotal"),
                ncol = 2, nrow = 2)
#BPR

# Histogram and density plot for BMI

plotbmiR1 <- ggplot() + 
  geom_density(data=HUNT23, aes(x = bmi_1, y = ..density..),lwd = 1, colour = "grey", fill="grey", alpha = 0.25) +
  geom_density(data=HUNT23_R1, aes(x = bmi_1), lwd = 1, colour = 3, fill = 3, alpha = 0.25) + xlim(15,45) + ylim(0,0.15) + labs(x = "", y = "")
plotbmiR2 <- ggplot() + 
  geom_density(data=HUNT23, aes(x = bmi_1, y = ..density..),lwd = 1, colour = "grey", fill="grey", alpha = 0.25) +
  geom_density(data=HUNT23_R2, aes(x = bmi_1),lwd = 1, colour = 3, fill = 3, alpha = 0.25)+ xlim(15,45) + ylim(0,0.15) + labs(x = "", y = "")
plotbmiR3 <- ggplot() + 
  geom_density(data=HUNT23, aes(x = bmi_1, y = ..density..),lwd = 1, colour = "grey", fill="grey", alpha = 0.25) +
  geom_density(data=HUNT23_R3, aes(x = bmi_1),lwd = 1, colour = 3, fill = 3, alpha = 0.25)+ xlim(15,45) + ylim(0,0.15) + labs(x = "", y = "")
plotbmiR4 <- ggplot() + 
  geom_density(data=HUNT23, aes(x = bmi_1, y = ..density..),lwd = 1, colour = "grey", fill="grey", alpha = 0.25) +
  geom_density(data=HUNT23_Rtotal, aes(x = bmi_1),lwd = 1, colour = 3, fill = 3, alpha = 0.25)+ xlim(15,45) + ylim(0,0.15) + labs(x = "BMI", y = "")

BMIR <- ggarrange(plotbmiR1, plotbmiR2, plotbmiR3, plotbmiR4,
                 labels = c("R1", "R2", "R3", "Rtotal"),
                 ncol = 2, nrow = 2)
#BMIR

#Histogram for sex 
plotsexR1 <- ggplot() + 
  geom_bar(data = HUNT23, aes(x = as.factor(sex), fill=as.factor(sex)), alpha=0.5, colour="gray") +
  geom_bar(data = HUNT23_R1, aes(x = as.factor(sex), fill=as.factor(sex)), alpha=1) + labs(x = "", y = "Number of participants") + scale_fill_manual(values = c(2, 4) ) +
  theme(legend.position="none") + ylim(0,40000)
plotsexR2 <- ggplot() + 
  geom_bar(data = HUNT23, aes(x = as.factor(sex), fill=as.factor(sex)), alpha=0.5, colour="gray") +
  geom_bar(data = HUNT23_R2, aes(x = as.factor(sex), fill=as.factor(sex)), alpha=1) + labs(x = "", y = "Number of participants") + scale_fill_manual(values = c(2, 4) ) +
  theme(legend.position="none")+ ylim(0,40000)
plotsexR3 <- ggplot() + 
  geom_bar(data = HUNT23, aes(x = as.factor(sex), fill=as.factor(sex)), alpha=0.5, colour="gray") +
  geom_bar(data = HUNT23_R1, aes(x = as.factor(sex), fill=as.factor(sex)), alpha=1) + labs(x = "", y = "Number of participants") + scale_fill_manual(values = c(2, 4) ) +
  theme(legend.position="none")+ ylim(0,40000)
plotsexRtotal <- ggplot() + 
  geom_bar(data = HUNT23, aes(x = as.factor(sex), fill=as.factor(sex)), alpha=0.5, colour="gray") +
  geom_bar(data = HUNT23_Rtotal, aes(x = as.factor(sex), fill=as.factor(sex)), alpha=1) + labs(x = "", y = "Number of participants") + scale_fill_manual(values = c(2, 4) ) +
  theme(legend.position="none") + labs(x = "Sex", y = "Number of participants") + ylim(0,40000)

plotsexR1
#A collection of BP, age and BMI

bpagebmiR <- ggarrange(plotbpR1, plotageR1, plotbmiR1, plotsexR1, plotbpR2, plotageR2, plotbmiR2, plotsexR2, plotbpR3, plotageR3, plotbmiR3, plotsexR3, plotbpR4, plotageR4, plotbmiR4, plotsexRtotal,     
                      labels = c("R1", "", "", "", "R2","", "", "", "R3", "", "", "", "Rtotal", "", "", ""),
                      ncol = 4, nrow = 4)
bpagebmiR

ggsave("bpagebmiR.png", plot = bpagebmiR, path = "/home/magnwoln/Masteroppgave/Figs/", width = 20, height = 28, units = "cm")

# Then for the unknown missing data

#Check how many unexplained missing values
unknownmissing12 <- HUNT12[which(HUNT12$"R1" == 0 & is.na(HUNT12$"bp_2_corr")),]
unknownmissing23 <- HUNT23[which(HUNT23$"R1" == 0 & is.na(HUNT23$"bp_2_corr")),]
unknownmissing34 <- HUNT34[which(HUNT34$"R1" == 0 & is.na(HUNT34$"bp_2_corr")),]

summary(unknownmissing23)

write.csv(unknownmissing23,"/home/magnwoln/Masteroppgave/INLAresultsunknownmissing23.csv", row.names = FALSE)

plotageUM23 <- ggplot() + 
  geom_density(data=HUNT23_Rtotal, aes(x = age_1, y = ..density..),lwd = 1, colour = "grey", fill="grey", alpha = 0.25) +
  geom_density(data = unknownmissing23, aes(x = age_1), lwd = 1, colour = 2, fill = 2, alpha = 0.25) + labs(x = "Age", y = "Density") + xlim(0,100)
plotBPUM23 <- ggplot() + 
  geom_density(data=HUNT23_Rtotal, aes(x = bp_1_corr, y = ..density..),lwd = 1, colour = "grey", fill="grey", alpha = 0.25) +
  geom_density(data = unknownmissing23, aes(x = bp_1_corr),lwd = 1, colour = 4, fill = 4, alpha = 0.25) + labs(x = "Blood Pressure", y = "Density")
plotBMIUM23 <- ggplot() + 
  geom_density(data=HUNT23_Rtotal, aes(x = bmi_1, y = ..density..),lwd = 1, colour = "grey", fill="grey", alpha = 0.25) +
  geom_density(data = unknownmissing23, aes(x = bmi_1),lwd = 1, colour = 3, fill = 3, alpha = 0.25) + labs(x = "BMI", y = "Density")
plotsexUM23 <- ggplot() +
  geom_bar(data = HUNT23_Rtotal, aes(x = as.factor(sex), fill=as.factor(sex)), alpha = 0.5, colour="gray") +
  geom_bar(data = unknownmissing23, aes(x = as.factor(sex), fill=as.factor(sex)), alpha=1) + labs(x = "", y = "Number of participants") + scale_fill_manual(values = c(2, 4) ) +
  theme(legend.position="none") + labs(x = "Sex", y = "Number of participants") + ylim(0,20000)

unknown_missing23 <- ggarrange(plotBPUM23, plotageUM23, plotBMIUM23, plotsexUM23,     
                            labels = c("", "", "", ""),
                            ncol = 2, nrow =2)
unknown_missing23

ggsave("unknown_missing23.png", plot = unknown_missing23, path = "/home/magnwoln/Masteroppgave/Figs/", width = 20, height = 10, units = "cm")

#Check the sex. 0 = female, 1 = male

per_male <- c(sum(HUNT12$sex)/nrow(HUNT12), sum(HUNT23$sex)/nrow(HUNT23), sum(HUNT34$sex)/nrow(HUNT34), sum(HUNT23_R1$sex)/nrow(HUNT23_R1),
             sum(HUNT23_R2$sex)/nrow(HUNT23_R2),
             sum(HUNT23_R3$sex)/nrow(HUNT23_R3),
             sum(HUNT23_Rtotal$sex)/nrow(HUNT23_Rtotal),
             sum(unknownmissing23$sex)/nrow(unknownmissing23))

per_male <- round(per_male, 3)
type_of_data <- c("HUNT12", "HUNT23", "HUNT34", "HUNT23 - R1", "HUNT23 - R2", "HUNT23 - R3", "HUNT23 - Rtotal", "HUNT23 - Unknown missing")

gender <- data.frame(type_of_data, per_male)

# Barplot for gender
plot_sex <-ggplot(data=gender, aes(x=type_of_data, y=per_male)) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=per_male), vjust=-0.3, size=3.5) + ylab("Percentage of male") + xlab("") + coord_flip()

ggsave("plot_sex.png", plot = plot_sex, path = "/home/magnwoln/Masteroppgave/Figs/", width = 20, height = 14, units = "cm")

#Test med å fjerne na i forklaringsvariablene

#HUNT23_INLA_onlycomplete <- HUNT23_INLA %>% drop_na(age_1, sex, bp_1_corr, bmi_1)
testing <- ggplot() +
  geom_density(data=HUNT23, aes(x = age_1, y = ..density..),lwd = 1, colour = "grey", fill="grey", alpha = 0.25) + ylim(0,0.055) +
  geom_density(data=HUNT23_R1, aes(x = age_1, y = ..density..),lwd = 1, colour = 2, fill = 2, alpha = 0.25) + xlim(15,105) +
  labs(x = "", y = "")
testing
