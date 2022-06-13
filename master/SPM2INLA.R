library(INLA)
library(brinla)

# Create the SPM2 model for Blood Pressure, with two missing processes

run_spm2_1lin <- function(data, missing1, missing2, c_1 = 0, c_2 = 0, c1_sigma = 1, c2_sigma = 1, verbose = F){
  
  sigma2 = 0.01
  #Prepare the response variables:
  n <- nrow(data)
  y_gauss <- c(data$bp_2_corr, rep(NA,n), rep(NA, n))
  R1_bin <- c(rep(NA,n), missing1, rep(NA, n)) 
  R2_bin <- c(rep(NA,n), rep(NA, n), missing2)
  joint_response <- list(y_gauss, R1_bin, R2_bin)
  
  #Must specify number of trials for binomial response variable
  ntrials <- c(rep(NA,n), rep(1,n), rep(1,n))
  
  linear_covariates <- data.frame(alpha_0 = c(rep(1,n),rep(NA,n),rep(NA,n)),
                                  r1_0 = c(rep(NA,n),rep(1,n),rep(NA,n)),
                                  r2_0 = c(rep(NA,n),rep(NA,n),rep(1,n)),
                                  y_SEX = c(data$sex,rep(NA,n),rep(NA,n)),
                                  y_BP1 = c(data$bp_1_corr,rep(NA,n),rep(NA,n)),
                                  y_AGE = c(data$age_1,rep(NA,n),rep(NA,n)),
                                  y_BMI = c(data$bmi_1,rep(NA,n),rep(NA,n)),
                                 
                                  r1_BP1 = c(rep(NA,n),data$bp_1_corr,rep(NA,n)),
                                  r1_AGE = c(rep(NA,n),data$age_1,rep(NA,n)),
                                  
                                  r2_SEX = c(rep(NA,n),rep(NA,n),data$sex),
                                  r2_BP1 = c(rep(NA,n),rep(NA,n),data$bp_1_corr),
                                  r2_AGE = c(rep(NA,n),rep(NA,n),data$age_1),
                                  r2_BMI = c(rep(NA,n),rep(NA,n),data$bmi_1)
  )
  random_covariates <- data.frame(y_eps1 = c(1:n,rep(NA,n),rep(NA,n)),
                                  r1_eps1 = c(rep(NA,n),1:n,rep(NA,n)),
                                  r2_eps1 = c(rep(NA,n),rep(NA,n),1:n))
  
  joint_data <- c(linear_covariates,random_covariates)
  
  joint_data$Y <- joint_response
  formula_spm = Y ~ -1 + alpha_0 + y_BP1 + y_AGE + y_SEX + y_BMI + 
    r1_0 + r1_BP1 + r1_AGE + 
    r2_0 + r2_SEX + r2_BP1 +  r2_BMI + 
    f(r2_AGE,model="rw2",constr=T) +
    f(y_eps1,model="iid") +
    f(r1_eps1,copy="y_eps1",fixed=F,param=c(c_1,c1_sigma)) +
    f(r2_eps1,copy="y_eps1",fixed=F,param=c(c_2,c2_sigma)) 
    #Copy epsilon1 into binomial dropout process with association parameter
  # Use the option param to set mean and precision on Gaussian prior for association parameter
  start_time <- Sys.time()
  joint_inla <- inla(formula_spm, family=c("gaussian","binomial","binomial"),data=joint_data,
                       Ntrials=ntrials, verbose=verbose,
                       control.family=list(list(initial=log(1/sigma2^2),fixed=T),list(),list()),
                       control.predictor = list(compute = TRUE),
                       control.compute = list(dic = TRUE), control.inla = list(cmin = 0))
  # Use control.family to fix sigma2. Specified through log-precision.
  end_time <- Sys.time()
  return(joint_inla)
}

run_spm2_2lin <- function(data, missing1, missing2, c_1 = 0, c_2 = 0, c1_sigma = 1, c2_sigma = 1, verbose = F){
  
  sigma2 = 0.01
  #Prepare the response variables:
  n <- nrow(data)
  y_gauss <- c(data$bp_2_corr, rep(NA,n), rep(NA, n))
  R1_bin <- c(rep(NA,n), missing1, rep(NA, n)) 
  R2_bin <- c(rep(NA,n), rep(NA, n), missing2)
  joint_response <- list(y_gauss, R1_bin, R2_bin)
  
  #Must specify number of trials for binomial response variable
  ntrials <- c(rep(NA,n), rep(1,n), rep(1,n))
  
  linear_covariates <- data.frame(alpha_0 = c(rep(1,n),rep(NA,n),rep(NA,n)),
                                  r1_0 = c(rep(NA,n),rep(1,n),rep(NA,n)),
                                  r2_0 = c(rep(NA,n),rep(NA,n),rep(1,n)),
                                  y_SEX = c(data$sex,rep(NA,n),rep(NA,n)),
                                  y_BP1 = c(data$bp_1_corr,rep(NA,n),rep(NA,n)),
                                  y_AGE = c(data$age_1,rep(NA,n),rep(NA,n)),
                                  y_BMI = c(data$bmi_1,rep(NA,n),rep(NA,n)),
                                  r1_SEX = c(rep(NA,n),data$sex,rep(NA,n)),
                                  r1_BP1 = c(rep(NA,n),data$bp_1_corr,rep(NA,n)),
                                  r1_AGE = c(rep(NA,n),data$age_1,rep(NA,n)),
                                  r1_BMI = c(rep(NA,n),data$bmi_1,rep(NA,n)),
                                  r2_BP1 = c(rep(NA,n),rep(NA,n),data$bp_1_corr),
                                  r2_AGE = c(rep(NA,n),rep(NA,n),data$age_1)
  )
  random_covariates <- data.frame(y_eps1 = c(1:n,rep(NA,n),rep(NA,n)),
                                  r1_eps1 = c(rep(NA,n),1:n,rep(NA,n)),
                                  r2_eps1 = c(rep(NA,n),rep(NA,n),1:n))
  
  joint_data <- c(linear_covariates,random_covariates)
  
  joint_data$Y <- joint_response
  formula_spm = Y ~ -1 + alpha_0 + y_BP1 + y_AGE + y_SEX + y_BMI + 
    r1_0 + r1_SEX + r1_BP1 +  r1_BMI + 
    r2_0 + r2_BP1 +
    r1_AGE +
    r2_AGE +
    f(y_eps1,model="iid") +
    f(r1_eps1,copy="y_eps1",fixed=F,param=c(c_1,c1_sigma)) +
    f(r2_eps1,copy="y_eps1",fixed=F,param=c(c_2,c2_sigma)) 
  #Copy epsilon1 into binomial dropout process with association parameter
  # Use the option param to set mean and precision on Gaussian prior for association parameter
  start_time <- Sys.time()
  joint_inla <- inla(formula_spm, family=c("gaussian","binomial","binomial"),data=joint_data,
                     Ntrials=ntrials, verbose=verbose,
                     control.family=list(list(initial=log(1/sigma2^2),fixed=T),list(),list()),
                     control.predictor = list(compute = TRUE),
                     control.compute = list(dic = TRUE), control.inla = list(cmin = 0))
  # Use control.family to fix sigma2. Specified through log-precision.
  end_time <- Sys.time()
  return(joint_inla)
}

HUNT23_INLA <- read.csv("/home/magnwoln/Masteroppgave/INLAresultsHUNT23_INLA.csv")
#HUNT12_INLA <- read.csv("/home/magnwoln/Masteroppgave/INLAresultsHUNT12_INLA.csv")

#INLA_SPM2_HUNT12_R12 <- run_spm2(HUNT12_INLA, HUNT12_INLA$R1, HUNT12_INLA$R2, c_1 = 0,c_2 = 0, c1_sigma = 10, c2_sigma = 10, verbose = T, compute_all = FALSE)

#SPM2_HUNT23_R12 <- run_spm2_2lin(HUNT23_INLA, HUNT23_INLA$R1, HUNT23_INLA$R2, c_1 = 0, c_2 = 0, c1_sigma = 1, c2_sigma = 1, verbose = T) #DONE
#summary(SPM2_HUNT23_R12)
#saveRDS(SPM2_HUNT23_R12, file = ("/home/magnwoln/Masteroppgave/INLAresults/SPM2_HUNT23_R12.RData")) #DONE


#SPM2_HUNT23_R13 <- run_spm2_1lin(HUNT23_INLA, HUNT23_INLA$R1, HUNT23_INLA$R3, c_1 = 0, c_2 = 0, c1_sigma = 1, c2_sigma = 1, verbose = T) #DONE
#summary(SPM2_HUNT23_R13)
#saveRDS(SPM2_HUNT23_R13, file = ("/home/magnwoln/Masteroppgave/INLAresults/SPM2_HUNT23_R13.RData")) #DONE

SPM2_HUNT23_R23 <- run_spm2_1lin(HUNT23_INLA, HUNT23_INLA$R2, HUNT23_INLA$R3, c_1 = 0, c_2 = 0, c1_sigma = 1, c2_sigma = 1, verbose = T) #DONE
#summary(SPM2_HUNT23_R23)
saveRDS(SPM2_HUNT23_R23, file = ("/home/magnwoln/Masteroppgave/INLAresults/SPM2_HUNT23_R23.RData")) #DONE
