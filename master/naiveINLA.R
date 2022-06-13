library(INLA)
library(brinla)
library(ggplot2)
library(ggpubr)

#Create the naive model for Blood Pressure

run_naive_bp <- function(data){
  n <- nrow(data)
  
  #Prepare the response variables:
  y_gaussian <- data$bp_2_corr
  
  linear_covariates <- list(alpha_0 = rep(1,n),
                            y_SEX = data$sex,
                            y_BP1 = data$bp_1_corr,
                            y_AGE = data$age_1,
                            y_BMI = data$bmi_1)
  naive_data_bp <- linear_covariates
  
  naive_data_bp$Y <- y_gaussian
  
  formula_naive_bp = Y ~ -1 + alpha_0  +  y_BP1 + y_SEX + y_BMI + y_AGE
  
  start_time <- Sys.time()
  joint_inla <- inla(formula_naive_bp, 
                     family = "gaussian",
                     data = naive_data_bp, 
                     verbose = T,
                     control.compute = list(config=TRUE, dic=TRUE, return.marginals.predictor=TRUE))
  
  return(joint_inla)
}

#HUNT12_INLA <- read.csv("/home/magnwoln/Masteroppgave/INLAresultsHUNT12_INLA.csv")
#HUNT23_INLA <- read.csv("/home/magnwoln/Masteroppgave/INLAresultsHUNT23_INLA.csv")
#HUNT34_INLA <- read.csv("/home/magnwoln/Masteroppgave/INLAresultsHUNT34_INLA.csv")

# This is done and saved on HUNT cloud

#bp_INLAnaive_HUNT12 <- run_naive_bp(HUNT12_INLA) #DONE
#bp_naive_HUNT23 <- run_naive_bp(HUNT23_INLA) #DONE
#bp_INLAnaive_HUNT34 <- run_naive_bp(HUNT34_INLA) #DONE

#saveRDS(bp_INLAnaive_HUNT12, file = ("/home/magnwoln/Masteroppgave/INLAresults/bp_INLAnaive_HUNT12.RData"))
#saveRDS(bp_naive_HUNT23, file = ("/home/magnwoln/Masteroppgave/INLAresults/bp_naive_HUNT23.RData"))
#saveRDS(bp_INLAnaive_HUNT34, file = ("/home/magnwoln/Masteroppgave/INLAresults/bp_naive_HUNT34.RData"))

run_naive_R3 <- function(data_sim, missing, verbose = F){
  data<- data_sim
  n <- nrow(data)
  
  linear_covariates_R <- list(beta_0 = rep(1,n),
                                m_SEX = data$sex,
                                m_BP1 = data$bp_1_corr,
                                m_AGE = data$age_1,
                                m_BMI = data$bmi_1)
  
  naive_data_R <- linear_covariates_R
  y_naive_R <- missing
  
  naive_data_R$Y <- y_naive_R
  formula_naive_R = Y ~ -1 + beta_0  + m_SEX + m_BP1 + m_BMI + f(m_AGE,model="rw2",constr=T)
  ntrials <- 1
  inla_naive_R <- inla(formula = formula_naive_R, 
                         family = "binomial", 
                         Ntrials = ntrials, 
                         data = naive_data_R, 
                         verbose=verbose,
                         control.compute=list(dic=TRUE))
  return(inla_naive_R)
}

run_naive_R2 <- function(data_sim, missing, verbose = F){
  data<- data_sim
  n <- nrow(data)
  
  linear_covariates_R <- list(beta_0 = rep(1,n),
                              m_BP1 = data$bp_1_corr,
                              m_AGE = data$age_1)
  
  naive_data_R <- linear_covariates_R
  y_naive_R <- missing
  
  naive_data_R$Y <- y_naive_R
  formula_naive_R = Y ~ -1 + beta_0  + m_BP1 + m_AGE
  ntrials <- 1
  inla_naive_R <- inla(formula = formula_naive_R, 
                       family = "binomial", 
                       Ntrials = ntrials, 
                       data = naive_data_R, 
                       verbose=verbose,
                       control.compute=list(dic=TRUE))
  return(inla_naive_R)
}

run_naive_R1 <- function(data_sim, missing, verbose = F){
  data<- data_sim
  n <- nrow(data)
  
  linear_covariates_R <- list(beta_0 = rep(1,n),
                              m_SEX = data$sex,
                              m_BP1 = data$bp_1_corr,
                              m_AGE = data$age_1,
                              m_BMI = data$bmi_1)
  
  naive_data_R <- linear_covariates_R
  y_naive_R <- missing
  
  naive_data_R$Y <- y_naive_R
  formula_naive_R = Y ~ -1 + beta_0  + m_SEX + m_BP1 + m_BMI + m_AGE
  ntrials <- 1
  inla_naive_R <- inla(formula = formula_naive_R, 
                       family = "binomial", 
                       Ntrials = ntrials, 
                       data = naive_data_R, 
                       verbose=verbose,
                       control.compute=list(dic=TRUE))
  return(inla_naive_R)
}

#Run for several situations
#Rtotal_naive_H23 <- run_naive_R3(HUNT23_INLA, HUNT23_INLA$Rtotal, verbose = T)
#R1_naive_H23 <- run_naive_R1(HUNT23_INLA, HUNT34_INLA$R1, verbose = T)
#R2_naive_H23 <- run_naive_R2(HUNT23_INLA, HUNT23_INLA$R2, verbose = T)
#R3_naive_H23 <- run_naive_R3(HUNT23_INLA, HUNT23_INLA$R3, verbose = T)

R1_naive_H34 <- run_naive_R1(HUNT34_INLA, HUNT34_INLA$R1, verbose = T)
R2_naive_H34 <- run_naive_R2(HUNT34_INLA, HUNT34_INLA$R2, verbose = T)
R3_naive_H34 <- run_naive_R3(HUNT34_INLA, HUNT34_INLA$R3, verbose = T)

#saveRDS(Rtotal_naive_H23, file = ("/home/magnwoln/Masteroppgave/INLAresults/Rtotal_naive_H23.RData"))
#saveRDS(R1_naive_H23, file = ("/home/magnwoln/Masteroppgave/INLAresults/R1_naive_H23.RData"))
#saveRDS(R2_naive_H23, file = ("/home/magnwoln/Masteroppgave/INLAresults/R2_naive_H23.RData"))
#saveRDS(R3_naive_H23, file = ("/home/magnwoln/Masteroppgave/INLAresults/R3_naive_H23.RData"))

# Plot for the naive missing processes 
Rtotal_naive_H23 <- readRDS(file = ("/home/magnwoln/Masteroppgave/INLAresults/Rtotal_naive_H23.RData"))
R1_naive_H23 <- readRDS(file = ("/home/magnwoln/Masteroppgave/INLAresults/R1_naive_H23.RData"))
R2_naive_H23 <- readRDS(file = ("/home/magnwoln/Masteroppgave/INLAresults/R2_naive_H23.RData"))
R3_naive_H23 <- readRDS(file = ("/home/magnwoln/Masteroppgave/INLAresults/R3_naive_H23.RData"))

#Naive R
b_H23_Rtotal <- Rtotal_naive_H23$marginals.fixed$beta_0
b_H23_R1 <- R1_naive_H23$marginals.fixed$beta_0
b_H23_R2 <- R2_naive_H23$marginals.fixed$beta_0
b_H23_R3 <- R3_naive_H23$marginals.fixed$beta_0

bp_H23_Rtotal <- Rtotal_naive_H23$marginals.fixed$m_BP1
bp_H23_R1 <- R1_naive_H23$marginals.fixed$m_BP1
bp_H23_R2 <- R2_naive_H23$marginals.fixed$m_BP1
bp_H23_R3 <- R3_naive_H23$marginals.fixed$m_BP1

age_H23_R1 <- R1_naive_H23$marginals.fixed$m_AGE
age_H23_R2 <- R2_naive_H23$marginals.fixed$m_AGE

age_H23_Rtotal <- Rtotal_naive_H23$summary.random$m_AGE
age_H23_R3 <- R3_naive_H23$summary.random$m_AGE

sex_H23_Rtotal <- Rtotal_naive_H23$marginals.fixed$m_SEX
sex_H23_R1 <- R1_naive_H23$marginals.fixed$m_SEX
#sex_H23_R2 <- R2_naive_H23$marginals.fixed$m_SEX
sex_H23_R3 <- R3_naive_H23$marginals.fixed$m_SEX

bmi_H23_Rtotal <- Rtotal_naive_H23$marginals.fixed$m_BMI
bmi_H23_R1 <- R1_naive_H23$marginals.fixed$m_BMI
#bmi_H23_R2 <- R2_naive_H23$marginals.fixed$m_BMI
bmi_H23_R3 <- R3_naive_H23$marginals.fixed$m_BMI

beta0plot <- ggplot() + 
  scale_colour_manual("Missing Process:", values = c("Rtotal" = "black", "R1 - Deceased"="blue", 
                                                     "R2 - Moved Out"="green", "R3 - Invited, but no show"="red")) +
  geom_line(data = as.data.frame(b_H23_Rtotal), aes(x = x, y = y, colour = "Rtotal")) +
  geom_line(data = as.data.frame(b_H23_R1), aes(x = x, y = y, colour = "R1 - Deceased")) +
  geom_line(data = as.data.frame(b_H23_R2), aes(x = x, y = y, colour = "R2 - Moved Out")) +
  geom_line(data = as.data.frame(b_H23_R3), aes(x = x, y = y, colour = "R3 - Invited, but no show")) +
  xlab(expression(beta[0])) + ylab("Density")  + theme(legend.position = "none")

#beta0plot

betabpplot <- ggplot() + 
  scale_colour_manual("Missing Process:", values = c("Rtotal" = "black", "R1 - Deceased"="blue", 
                                                     "R2 - Moved Out"="green", "R3 - Invited, but no show"="red")) +
  geom_line(data = as.data.frame(bp_H23_Rtotal), aes(x = x, y = y, colour = "Rtotal")) +
  geom_line(data = as.data.frame(bp_H23_R1), aes(x = x, y = y, colour = "R1 - Deceased")) +
  geom_line(data = as.data.frame(bp_H23_R2), aes(x = x, y = y, colour = "R2 - Moved Out")) +
  geom_line(data = as.data.frame(bp_H23_R3), aes(x = x, y = y, colour = "R3 - Invited, but no show")) +
  xlab(expression(beta["BP"])) + ylab("Density")  + theme(legend.position = "none")

betasexplot <- ggplot() + 
  scale_colour_manual("Missing Process:", values = c("Rtotal" = "black", "R1 - Deceased"="blue", 
                                                     "R2 - Moved Out"="green", "R3 - Invited, but no show"="red")) +
  geom_line(data = as.data.frame(sex_H23_Rtotal), aes(x = x, y = y, colour = "Rtotal")) +
  geom_line(data = as.data.frame(sex_H23_R1), aes(x = x, y = y, colour = "R1 - Deceased")) +
  #geom_line(data = as.data.frame(sex_H23_R2), aes(x = x, y = y, colour = "R2 - Moved Out")) +
  geom_line(data = as.data.frame(sex_H23_R3), aes(x = x, y = y, colour = "R3 - Invited, but no show")) +
  xlab(expression(beta["sex"])) + ylab("Density")  + theme(legend.position = "none")

betabmiplot <- ggplot() + 
  scale_colour_manual("Missing Process:", values = c("Rtotal" = "black", "R1 - Deceased"="blue", 
                                                     "R2 - Moved Out"="green", "R3 - Invited, but no show"="red")) +
  geom_line(data = as.data.frame(bmi_H23_Rtotal), aes(x = x, y = y, colour = "Rtotal")) +
  geom_line(data = as.data.frame(bmi_H23_R1), aes(x = x, y = y, colour = "R1 - Deceased")) +
  #geom_line(data = as.data.frame(bmi_H23_R2), aes(x = x, y = y, colour = "R2 - Moved Out")) +
  geom_line(data = as.data.frame(bmi_H23_R3), aes(x = x, y = y, colour = "R3 - Invited, but no show")) +
  xlab(expression(beta["BMI"])) + ylab("Density")  + theme(legend.position = "none")


randomageplot <- ggplot() + 
  scale_colour_manual("Missing Process:", values = c("Rtotal" = "black", "R1 - Deceased"="blue", 
                                                     "R2 - Moved Out"="green", "R3 - Invited, but no show"="red")) +
  geom_line(data = as.data.frame(age_H23_Rtotal), aes(x = ID, y = `0.5quant`, colour = "Rtotal")) +
  geom_line(data = as.data.frame(age_H23_Rtotal), aes(x = ID, y = `0.025quant`, colour = "Rtotal"), linetype="dashed") +
  geom_line(data = as.data.frame(age_H23_Rtotal), aes(x = ID, y = `0.975quant`, colour = "Rtotal"), linetype="dashed") +
  geom_line(data = as.data.frame(age_H23_R3), aes(x = ID, y = `0.5quant`, colour = "R3 - Invited, but no show")) +
  geom_line(data = as.data.frame(age_H23_R3), aes(x = ID, y = `0.025quant`, colour = "R3 - Invited, but no show"), linetype="dashed") +
  geom_line(data = as.data.frame(age_H23_R3), aes(x = ID, y = `0.975quant`, colour = "R3 - Invited, but no show"), linetype="dashed") +
  xlab("Age") + ylab("Random effect")  + theme(legend.position = "bottom") 

betaageplot <- ggplot() + 
  scale_colour_manual("Missing Process:", values = c("Rtotal" = "black", "R1 - Deceased"="blue", 
                                                     "R2 - Moved Out"="green", "R3 - Invited, but no show"="red")) +
  geom_line(data = as.data.frame(age_H23_R1), aes(x = x, y = y, colour = "R1 - Deceased")) +
  geom_line(data = as.data.frame(age_H23_R2), aes(x = x, y = y, colour = "R2 - Moved Out")) +
  xlab(expression(beta["age"])) + ylab("Density")  + theme(legend.position = "bottom") 


rplotsH23 <- ggarrange(beta0plot, betabpplot, betasexplot, betabmiplot, betaageplot, randomageplot,
                       labels = c("", "", "", "", "", ""), common.legend=TRUE, legend="bottom",
                       ncol = 2, nrow = 3)
rplotsH23

ggsave("rplotsH23.png", plot = rplotsH23, path = "/home/magnwoln/Masteroppgave/Figs/", width = 20, height = 14, units = "cm",bg="white")
