library('ggplot2')
library(ggpubr)
library(INLA)

SPM1_H23 <- readRDS(file = ("/home/magnwoln/Masteroppgave/INLAresults/SPM1_HUNT23.RData"))
SPM3_H23 <- readRDS(file = ("/home/magnwoln/Masteroppgave/INLAresults/SPM3_H23.RData"))

bp_naive_H23 <- readRDS(file = ("/home/magnwoln/Masteroppgave/INLAresults/bp_naive_HUNT23.RData"))
R1_naive_H23 <- readRDS(file = ("/home/magnwoln/Masteroppgave/INLAresults/R1_naive_H23.RData"))
R2_naive_H23 <- readRDS(file = ("/home/magnwoln/Masteroppgave/INLAresults/R2_naive_H23.RData"))
R3_naive_H23 <- readRDS(file = ("/home/magnwoln/Masteroppgave/INLAresults/R3_naive_H23.RData"))

# Nivae blood pressure with marginals.predictior = True
run_naive_bp_e <- function(data){
  n <- nrow(data)
  sigma2 = 0.001
  
  #Prepare the response variables:
  y_gaussian <- data$bp_2_corr
  
  linear_covariates <- list(alpha_0 = rep(1,n),
                            y_SEX = data$sex,
                            y_BP1 = data$bp_1_corr,
                            y_AGE = data$age_1,
                            y_BMI = data$bmi_1,
                            y_eps1 = 1:n)
  naive_data_bp <- linear_covariates
  
  naive_data_bp$Y <- y_gaussian
  
  formula_naive_bp = Y ~ -1 + alpha_0  +  y_BP1 + y_SEX + y_BMI + y_AGE + f(y_eps1,model="iid") 
  
  start_time <- Sys.time()
  joint_inla <- inla(formula_naive_bp, 
                     family = "gaussian",
                     data = naive_data_bp, 
                     verbose = T,
                     control.compute = list(config=TRUE, dic=TRUE, return.marginals.predictor=TRUE), 
                     control.family=list(list(initial=log(1/sigma2^2),fixed=T)))
  
  return(joint_inla)
}

HUNT23_INLA <- read.csv("/home/magnwoln/Masteroppgave/INLAresultsHUNT23_INLA.csv")
bp_naive_e <- run_naive_bp_e(HUNT23_INLA)

#Plot for predicted blood pressure for 3 "random" individuals 

individual1 <- ggplot() +
  scale_colour_manual("Future blood pressure, models:", values = c("Naive"="black", "SPM 1 with Rtotal" = "red", "SPM 3" = "blue")) +
  geom_line(data = data.frame(bp_naive_e$marginals.fitted.values[[8160]]), aes(x,y, colour = "Naive"))  +
  geom_line(data = data.frame(SPM1_H23$marginals.fitted.values[[8160]]), aes(x,y, colour = "SPM 1 with Rtotal")) +
  geom_line(data = data.frame(SPM3_H23$marginals.fitted.values[[8160]]), aes(x,y, colour = "SPM 3")) + 
  xlab("Old female, high BMI and previous blood pressure") + ylab("") + xlim(-5, 5)
  
individual2 <- ggplot() +
  scale_colour_manual("Future blood pressure, models:", values = c("Naive"="black", "SPM 1 with Rtotal" = "red", "SPM 3" = "blue")) +
  geom_line(data = data.frame(bp_naive_e$marginals.fitted.values[[61892]]), aes(x,y, colour = "Naive"))  +
  geom_line(data = data.frame(SPM1_H23$marginals.fitted.values[[61892]]), aes(x,y, colour = "SPM 1 with Rtotal")) +
  geom_line(data = data.frame(SPM3_H23$marginals.fitted.values[[61892]]), aes(x,y, colour = "SPM 3")) +
  xlab("Middle-aged female, average BMI and previous blood pressure") + ylab("") + xlim(-5, 5)

individual3 <- ggplot() +
  scale_colour_manual("Future blood pressure, models:", values = c("Naive"="black", "SPM 1 with Rtotal" = "red", "SPM 3" = "blue")) +
  geom_line(data = data.frame(bp_naive_e$marginals.fitted.values[[38462]]), aes(x,y, colour = "Naive"))  +
  geom_line(data = data.frame(SPM1_H23$marginals.fitted.values[[38462]]), aes(x,y, colour = "SPM 1 with Rtotal")) +
  geom_line(data = data.frame(SPM3_H23$marginals.fitted.values[[38462]]), aes(x,y, colour = "SPM 3")) +
  xlab("Young female, low BMI and previous blood pressure") + ylab("Density") + xlim(-5, 5)

individuals <- ggarrange(individual1, individual2, individual3,
                              labels = c("", "", ""), common.legend=TRUE, legend="bottom",
                              ncol = 1, nrow = 3)
individuals

ggsave("individuals.png", plot = individuals, path = "/home/magnwoln/Masteroppgave/Figs/",  width = 20, height = 14, units = "cm", bg = "white")

#Predicted missingness for 3 "random" individuals 

individual3missingSPM1 <- ggplot() +
  scale_colour_manual("Future blood pressure, models:", values = c("Naive"="black", "SPM 1 with Rtotal" = "red", "SPM 3" = "blue")) +
  geom_line(data = data.frame(SPM1_H23$marginals.fitted.values[[8160+64387]]), aes(x,y, colour = "Naive"))  +
  geom_line(data = data.frame(SPM1_H23$marginals.fitted.values[[38462+64387]]), aes(x,y, colour = "SPM 1 with Rtotal")) +
  geom_line(data = data.frame(SPM1_H23$marginals.fitted.values[[61892+64387]]), aes(x,y, colour = "SPM 3")) +
  xlab("Young female, low BMI and previous blood pressure") + ylab("Density") + ylim(0,7)

individual3missingSPM1

individual3missingSPM3 <- ggplot() +
  scale_colour_manual("Missing process:", values = c("R1: Deceased"="black", "R2: Moved out" = "red", "R3: Invited, but no show" = "blue")) +
  geom_line(data = data.frame(SPM3_H23$marginals.fitted.values[[38462+64387]]), aes(x,y, colour = "R1: Deceased"))  +
  geom_line(data = data.frame(SPM3_H23$marginals.fitted.values[[38462+64387+64387]]), aes(x,y, colour = "R2: Moved out")) +
  geom_line(data = data.frame(SPM3_H23$marginals.fitted.values[[38462+64387+64387+64387]]), aes(x,y, colour = "R3: Invited, but no show")) +
  xlab("Young female, low BMI and previous blood pressure") + ylab("Density") + ylim(0,8)

individual1missingSPM3 <- ggplot() +
  scale_colour_manual("Missing process:", values = c("R1: Deceased"="black", "R2: Moved out" = "red", "R3: Invited, but no show" = "blue")) +
  geom_line(data = data.frame(SPM3_H23$marginals.fitted.values[[8160+64387]]), aes(x,y, colour = "R1: Deceased"))  +
  geom_line(data = data.frame(SPM3_H23$marginals.fitted.values[[8160+64387+64387]]), aes(x,y, colour = "R2: Moved out")) +
  geom_line(data = data.frame(SPM3_H23$marginals.fitted.values[[8160+64387+64387+64387]]), aes(x,y, colour = "R3: Invited, but no show")) +
  xlab("Old female, high BMI and previous blood pressure") + ylab("Density") + ylim(0,8)

individual2missingSPM3 <- ggplot() +
  scale_colour_manual("Missing process:", values = c("R1: Deceased"="black", "R2: Moved out" = "red", "R3: Invited, but no show" = "blue")) +
  geom_line(data = data.frame(SPM3_H23$marginals.fitted.values[[61892+64387]]), aes(x,y, colour = "R1: Deceased"))  +
  geom_line(data = data.frame(SPM3_H23$marginals.fitted.values[[61892+64387+64387]]), aes(x,y, colour = "R2: Moved out")) +
  geom_line(data = data.frame(SPM3_H23$marginals.fitted.values[[61892+64387+64387+64387]]), aes(x,y, colour = "R3: Invited, but no show")) +
  xlab("Middle-aged female, average BMI and previous blood pressure") + ylab("Density") + ylim(0,8)


individualsR <- ggarrange(individual1missingSPM3, individual2missingSPM3, individual3missingSPM3,
                         labels = c("", "", ""), common.legend=TRUE, legend="bottom",
                         ncol = 1, nrow = 3)
individualsR

ggsave("individualsR.png", plot = individualsR, path = "/home/magnwoln/Masteroppgave/Figs/",  width = 20, height = 14, units = "cm", bg = "white")


