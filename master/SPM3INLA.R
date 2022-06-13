library(INLA)
library(brinla)
library(tidyr)
library(dplyr)
library(haven)
library(plyr)

# Create the SPM3 model for Blood Pressure, with three missing process

run_spm3 <- function(data, c_1 = 0, c_2 = 0, c_3 = 0, c1_sigma = 1, c2_sigma = 1, c3_sigma = 1, verbose = F){
  
  sigma2 = 0.01
  #Prepare the response variables:
  n <- nrow(data)
  y_gauss <- c(data$bp_2_corr, rep(NA,n), rep(NA, n), rep(NA, n))
  R1_bin <- c(rep(NA,n), data$R1, rep(NA, n), rep(NA, n)) 
  R2_bin <- c(rep(NA,n), rep(NA, n), data$R2, rep(NA, n))
  R3_bin <- c(rep(NA,n), rep(NA, n), rep(NA, n), data$R3)
  joint_response <- list(y_gauss, R1_bin, R2_bin, R3_bin)

  #Must specify number of trials for binomial repsonse variable
  ntrials <- c(rep(NA,n), rep(1,n), rep(1,n), rep(1,n))
     
  linear_covariates <- data.frame(alpha_0 = c(rep(1,n),rep(NA,n),rep(NA,n),rep(NA,n)),
                                  r1_0 = c(rep(NA,n),rep(1,n),rep(NA,n),rep(NA,n)),
                                  r2_0 = c(rep(NA,n),rep(NA,n),rep(1,n),rep(NA,n)),
                                  r3_0 = c(rep(NA,n),rep(NA,n),rep(NA,n),rep(1,n)),
                                  y_SEX = c(data$sex,rep(NA,n),rep(NA,n),rep(NA,n)),
                                  y_BP1 = c(data$bp_1_corr,rep(NA,n),rep(NA,n),rep(NA,n)),
                                  y_AGE = c(data$age_1,rep(NA,n),rep(NA,n),rep(NA,n)),
                                  y_BMI = c(data$bmi_1,rep(NA,n),rep(NA,n),rep(NA,n)),
                                  r1_SEX = c(rep(NA,n),data$sex,rep(NA,n),rep(NA,n)),
                                  r1_BP1 = c(rep(NA,n),data$bp_1_corr,rep(NA,n),rep(NA,n)),
                                  r1_AGE = c(rep(NA,n),data$age_1,rep(NA,n),rep(NA,n)),
                                  r1_BMI = c(rep(NA,n),data$bmi_1,rep(NA,n),rep(NA,n)),
                                  r2_SEX = c(rep(NA,n),rep(NA,n),data$sex,rep(NA,n)),
                                  r2_BP1 = c(rep(NA,n),rep(NA,n),data$bp_1_corr,rep(NA,n)),
                                  r2_AGE = c(rep(NA,n),rep(NA,n),data$age_1,rep(NA,n)),
                                  r2_BMI = c(rep(NA,n),rep(NA,n),data$bmi_1,rep(NA,n)),
                                  r3_SEX = c(rep(NA,n),rep(NA,n),rep(NA,n),data$sex),
                                  r3_BP1 = c(rep(NA,n),rep(NA,n),rep(NA,n),data$bp_1_corr),
                                  r3_AGE = c(rep(NA,n),rep(NA,n),rep(NA,n),data$age_1),
                                  r3_BMI = c(rep(NA,n),rep(NA,n),rep(NA,n),data$bmi_1)
                                                 )
  random_covariates <- data.frame(y_eps1 = c(1:n,rep(NA,n),rep(NA,n),rep(NA,n)),
                                  r1_eps1 = c(rep(NA,n),1:n,rep(NA,n),rep(NA,n)),
                                  r2_eps1 = c(rep(NA,n),rep(NA,n),1:n,rep(NA,n)),
                                  r3_eps1 = c(rep(NA,n),rep(NA,n),rep(NA,n),1:n))

  joint_data <- c(linear_covariates,random_covariates)

  joint_data$Y <- joint_response
  formula_spm = Y ~ -1 + alpha_0 + y_BP1 + y_AGE + y_SEX + y_BMI + 
                  r1_0 + r1_SEX + r1_BP1 +  r1_BMI + r1_AGE +
                  r2_0 + r2_BP1 + r2_AGE +
                  r3_0 + r3_SEX + r3_BP1 +  r3_BMI +
                  f(r3_AGE,model="rw2",constr=T) +
                  f(y_eps1,model="iid") +
                  f(r1_eps1,copy="y_eps1",fixed=F,param=c(c_1,c1_sigma)) +
                  f(r2_eps1,copy="y_eps1",fixed=F,param=c(c_2,c2_sigma)) +
                  f(r3_eps1,copy="y_eps1",fixed=F,param=c(c_3,c3_sigma)) #Copy epsilon1 into binomial dropout process with association parameter
                   # Use the option param to set mean and precision on Gaussian prior for association parameter
  
  # Set the correct link function
  link = rep(NA, (n*4))
  
  link[which(is.na(y_gauss[1:(n)]))] = 1
  link[(n) + which(is.na(R1_bin[((n)+1):(2*(n))]))] = 2
  link[(2*n) + which(is.na(R2_bin[((2*n)+1):(3*(n))]))] = 3
  link[(3*n) + which(is.na(R3_bin[((3*n)+1):(4*(n))]))] = 4
  print(length(link))
  
  start_time <- Sys.time()
  joint_inla <- inla(formula_spm, family=c("gaussian","binomial","binomial","binomial"),data=joint_data,
                                       Ntrials=ntrials, verbose=verbose, 
                                       control.family=list(list(initial=log(1/sigma2^2),fixed=T),list(),list(),list()),
                       control.predictor = list(link = link),
                       control.compute = list(dic = TRUE, return.marginals.predictor=TRUE), control.inla = list())
 # Use control.family to fix sigma2. Specified through log-precision.
 end_time <- Sys.time()
 return(joint_inla)
}

#HUNT23_INLA <- read.csv("/home/magnwoln/Masteroppgave/INLAresultsHUNT23_INLA.csv")
#HUNT12_INLA <- read.csv("/home/magnwoln/Masteroppgave/INLAresultsHUNT12_INLA.csv")
#HUNT34_INLA <- read.csv("/home/magnwoln/Masteroppgave/INLAresultsHUNT34_INLA.csv")

#SPM3_H12 <- run_spm3(HUNT12_INLA, c_1 = 0, c_2 = 0, c_3 = 0, c1_sigma = 1, c2_sigma = 1, c3_sigma = 1, verbose = T) 
#SPM3_H23 <- run_spm3(HUNT23_INLA, c_1 = 0, c_2 = 0, c_3 = 0, c1_sigma = 1, c2_sigma = 1, c3_sigma = 1, verbose = T) 
#SPM3_H34 <- run_spm3(HUNT34_INLA, c_1 = 0, c_2 = 0, c_3 = 0, c1_sigma = 1, c2_sigma = 1, c3_sigma = 1, verbose = T) 
#saveRDS(SPM3_H34, file = ("/home/magnwoln/Masteroppgave/INLAresults/SPM3_H34.RData"))
