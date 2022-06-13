library(INLA)
library(brinla)

# Create the SPM1 model for Blood Pressure, with one missing processes - Rtotal, R1, R2, R3

run_spm1 <- function(data, missing, c_mu = 0, c_sigma = 1, verbose = F){
  sigma2 = 0.01
  #Prepare the response variables:
  n <- nrow(data)
  y_gaussian <- c(data$bp_2_corr,rep(NA,n))
  m_binomial <- c(rep(NA,n), missing) #Change to the right missing process
  joint_response <- list(y_gaussian,m_binomial)
  
  #Must specify number of trials for binomial response variable
  ntrials <- c(rep(NA,n),rep(1,n))
  
  #Create linear variables
  linear_covariates <- data.frame(alpha_0 = c(rep(1,n),rep(NA,n)),
                                  beta_0 = c(rep(NA,n),rep(1,n)),
                                  y_SEX = c(data$sex,rep(NA,n)),
                                  y_BP1 = c(data$bp_1_corr,rep(NA,n)),
                                  y_AGE = c(data$age_1,rep(NA,n)),
                                  y_BMI = c(data$bmi_1,rep(NA,n)),
                                  m_SEX = c(rep(NA,n),data$sex),
                                  m_BP1 = c(rep(NA,n),data$bp_1_corr),
                                  m_AGE = c(rep(NA,n),data$age_1),
                                  m_BMI = c(rep(NA,n),data$bmi_1)
  )
  random_covariates <- data.frame(y_eps1 = c(1:n,rep(NA,n)),
                                  m_eps1 = c(rep(NA,n),1:n))
  
  joint_data <- c(linear_covariates,random_covariates)
  
  joint_data$Y <- joint_response
  
  formula_spm = Y ~ -1 + alpha_0 + y_BP1 + y_AGE + y_SEX + y_BMI +
    beta_0 + m_SEX + m_BP1 +  m_BMI +
    f(m_AGE,model="rw2",constr=T) + 
    f(y_eps1,model="iid") +
    f(m_eps1,copy="y_eps1",fixed=F,param=c(c_mu,c_sigma)) #Copy epsilon1 into binomial dropout process with association parameter
  # Use the option param to set mean and precision on Gaussian prior for association parameter

  start_time <- Sys.time()
  joint_inla <- inla(formula_spm, family=c("gaussian","binomial"),data=joint_data,
                       Ntrials=ntrials, verbose=verbose,
                       control.family=list(list(initial=log(1/sigma2^2),fixed=T),list()),
                       control.predictor = list(compute = TRUE),
                       control.compute = list(dic = TRUE, return.marginals.predictor=TRUE), control.inla = list(cmin = 0 ))
  # Use control.family to fix sigma2. Specified through log-precision.
  end_time <- Sys.time()
  return(joint_inla)
}

run_spm1_R2 <- function(data, missing, c_mu = 0, c_sigma = 1, verbose = F){
  sigma2 = 0.01
  #Prepare the response variables:
  n <- nrow(data)
  y_gaussian <- c(data$bp_2_corr,rep(NA,n))
  m_binomial <- c(rep(NA,n), missing) #Change to the right missing process
  joint_response <- list(y_gaussian,m_binomial)
  
  #Must specify number of trials for binomial response variable
  ntrials <- c(rep(NA,n),rep(1,n))
  
  #Create linear variables
  linear_covariates <- data.frame(alpha_0 = c(rep(1,n),rep(NA,n)),
                                  beta_0 = c(rep(NA,n),rep(1,n)),
                                  y_SEX = c(data$sex,rep(NA,n)),
                                  y_BP1 = c(data$bp_1_corr,rep(NA,n)),
                                  y_AGE = c(data$age_1,rep(NA,n)),
                                  y_BMI = c(data$bmi_1,rep(NA,n)),
                                  m_BP1 = c(rep(NA,n),data$bp_1_corr),
                                  m_AGE = c(rep(NA,n),data$age_1)
  )
  random_covariates <- data.frame(y_eps1 = c(1:n,rep(NA,n)),
                                  m_eps1 = c(rep(NA,n),1:n))
  
  joint_data <- c(linear_covariates,random_covariates)
  
  joint_data$Y <- joint_response
  
  formula_spm = Y ~ -1 + alpha_0 + y_BP1 + y_AGE + y_SEX + y_BMI +
    beta_0 + m_BP1 + m_AGE +
    f(y_eps1,model="iid") +
    f(m_eps1,copy="y_eps1",fixed=F,param=c(c_mu,c_sigma)) #Copy epsilon1 into binomial dropout process with association parameter
  # Use the option param to set mean and precision on Gaussian prior for association parameter
  start_time <- Sys.time()
  joint_inla <- inla(formula_spm, family=c("gaussian","binomial"),data=joint_data,
                     Ntrials=ntrials, verbose=verbose,
                     control.family=list(list(initial=log(1/sigma2^2),fixed=T),list()),
                     control.predictor = list(compute = TRUE),
                     control.compute = list(dic = TRUE), control.inla = list(cmin = 0 ))
  # Use control.family to fix sigma2. Specified through log-precision.
  
  end_time <- Sys.time()
  return(joint_inla)
}

run_spm1_lin <- function(data, missing, c_mu = 0, c_sigma = 1, verbose = F){
  sigma2 = 0.01
  #Prepare the response variables:
  n <- nrow(data)
  y_gaussian <- c(data$bp_2_corr,rep(NA,n))
  m_binomial <- c(rep(NA,n), missing) #Change to the right missing process
  joint_response <- list(y_gaussian,m_binomial)
  
  #Must specify number of trials for binomial response variable
  ntrials <- c(rep(NA,n),rep(1,n))
  
  #Create linear variables
  linear_covariates <- data.frame(alpha_0 = c(rep(1,n),rep(NA,n)),
                                  beta_0 = c(rep(NA,n),rep(1,n)),
                                  y_SEX = c(data$sex,rep(NA,n)),
                                  y_BP1 = c(data$bp_1_corr,rep(NA,n)),
                                  y_AGE = c(data$age_1,rep(NA,n)),
                                  y_BMI = c(data$bmi_1,rep(NA,n)),
                                  m_SEX = c(rep(NA,n),data$sex),
                                  m_BP1 = c(rep(NA,n),data$bp_1_corr),
                                  m_AGE = c(rep(NA,n),data$age_1),
                                  m_BMI = c(rep(NA,n),data$bmi_1)
  )
  random_covariates <- data.frame(y_eps1 = c(1:n,rep(NA,n)),
                                  m_eps1 = c(rep(NA,n),1:n))
  
  joint_data <- c(linear_covariates,random_covariates)
  
  joint_data$Y <- joint_response
  
  formula_spm = Y ~ -1 + alpha_0 + y_BP1 + y_AGE + y_SEX + y_BMI +
    beta_0 + m_SEX + m_BP1 +  m_BMI +
    m_AGE + 
    f(y_eps1,model="iid") +
    f(m_eps1,copy="y_eps1",fixed=F,param=c(c_mu,c_sigma)) #Copy epsilon1 into binomial dropout process with association parameter
  # Use the option param to set mean and precision on Gaussian prior for association parameter
  start_time <- Sys.time()
  joint_inla <- inla(formula_spm, family=c("gaussian","binomial"),data=joint_data,
                     Ntrials=ntrials, verbose=verbose,
                     control.family=list(list(initial=log(1/sigma2^2),fixed=T),list()),
                     control.predictor = list(compute = TRUE),
                     control.compute = list(dic = TRUE, return.marginals.predictor=TRUE), control.inla = list(cmin = 0 ))
  # Use control.family to fix sigma2. Specified through log-precision.
  
  end_time <- Sys.time()
  return(joint_inla)
}

HUNT23_INLA <- read.csv("/home/magnwoln/Masteroppgave/INNLAresultsHUNT23_INLA.csv")

#HUNT12_INLA <- read.csv("/home/magnwoln/Masteroppgave/INNLAresultsHUNT12_INLA.csv")
#INLA_SPM1_HUNT12 <- run_spm1(HUNT12_INLA, HUNT12_INLA$Rtotal, c_mu = 0, c_sigma = 1, verbose = T) #DONE
#saveRDS(INLA_SPM1_HUNT12, file = ("/home/magnwoln/Masteroppgave/INLAresults/INLA_SPM1_HUNT12.RData")) #DONE

#Comment out what you want to run, takes 5 min
SPM1_HUNT23 <- run_spm1(HUNT23_INLA, HUNT23_INLA$Rtotal, c_mu = 0, c_sigma = 1, verbose = T) #DONE
saveRDS(SPM1_HUNT23, file = ("/home/magnwoln/Masteroppgave/INLAresults/SPM1_HUNT23.RData")) #DONE

#SPM1_HUNT23_R1 <- run_spm1_lin(HUNT23_INLA, HUNT23_INLA$R1, c_mu = 0, c_sigma = 1, verbose = T)
#saveRDS(SPM1_HUNT23_R1, file = ("/home/magnwoln/Masteroppgave/INLAresults/SPM1_HUNT23_R1.RData"))

#SPM1_HUNT23_R2 <- run_spm1_R2(HUNT23_INLA, HUNT23_INLA$R2, c_mu = 0, c_sigma = 1, verbose = T) #DONE
#saveRDS(SPM1_HUNT23_R2, file = ("/home/magnwoln/Masteroppgave/INLAresults/SPM1_HUNT23_R2.RData"))

#SPM1_HUNT23_R3 <- run_spm1(HUNT23_INLA, HUNT23_INLA$R3, c_mu = 0, c_sigma = 1, verbose = T) 
#saveRDS(SPM1_HUNT23_R3, file = ("/home/magnwoln/Masteroppgave/INLAresults/SPM1_HUNT23_R3.RData"))

