# Model selection
library(INLA)
library(brinla)

linear_bp <- function(data){
  n <- nrow(data)
  sigma2 = 0.001 #Fixed to a neglectable value
  
  #Prepare the response variables:
  y_gaussian <- data$bp_2_corr
  
  linear_covariates <- list(alpha_0 = rep(1,n),
                            y_BP1 = data$bp_1_corr,
                            y_AGE = data$age_1,
                            y_eps1 = 1:n)
  
  naive_data_bp <- linear_covariates
  
  naive_data_bp$Y <- y_gaussian
  
  formula_naive_bp = Y ~ -1 + alpha_0 + y_BP1 + f(y_AGE,model="rw2",constr=T) + f(y_eps1, model="iid") 
  
  start_time <- Sys.time()
  joint_inla <- inla(formula_naive_bp, 
                     family = "gaussian",
                     data = naive_data_bp, 
                     verbose = T,
                     control.family = list(list(initial=log(1/sigma2^2),fixed=T)),
                     control.compute = list(config=TRUE, dic=TRUE))
  
  return(joint_inla)
}

lbp <- linear_bp(HUNT23_INLA)
lbp$dic[1]
#logbbp <- logage_bp(HUNT23_INLA)
#summary(logbbp)
logbbp$dic[1]

bp_INLAnaive_HUNT23$dic[1]

logage_bp <- function(data){
  n <- nrow(data)
  sigma2 = 0.001 #Fixed to a neglectable value
  
  #Prepare the response variables:
  y_gaussian <- data$bp_2_corr
  
  linear_covariates <- list(alpha_0 = rep(1,n),
                            y_SEX = data$sex,
                            y_BP1 = data$bp_1_corr,
                            y_AGE = log(data$age_1),
                            y_BMI = data$bmi_1,
                            y_eps1 = 1:n)
  naive_data_bp <- linear_covariates
  
  naive_data_bp$Y <- y_gaussian
  
  formula_naive_bp = Y ~ -1 + alpha_0 + y_BP1 + y_AGE + y_SEX + y_BMI + f(y_eps1,model="iid") 
  
  start_time <- Sys.time()
  joint_inla <- inla(formula_naive_bp, 
                     family = "gaussian",
                     data = naive_data_bp, 
                     verbose = T,
                     control.family = list(list(initial=log(1/sigma2^2),fixed=T)),
                     control.compute = list(config=TRUE, dic=TRUE))
  
  return(joint_inla)
}

linear_R <- function(data_sim, missing, verbose = F, compute_all = FALSE){
  data<- data_sim
  n <- nrow(data)
  
  linear_covariates_R <- list(beta_0 = rep(1,n),
                              m_SEX = data$sex,
                              m_AGE = data$age_1,
                              m_BMI = data$bmi_1,
                              m_eps1 =1:n)
  
  naive_data_R <- linear_covariates_R
  y_naive_R <- missing
  
  naive_data_R$Y <- y_naive_R
  formula_naive_R = Y ~ -1 + beta_0  + m_SEX + m_BMI + f(m_AGE,model="rw2",constr=T)
  ntrials <- 1
  
  if(compute_all == FALSE){
    inla_naive_R <- inla(formula = formula_naive_R, 
                         family = "binomial", 
                         Ntrials = ntrials, 
                         data = naive_data_R, 
                         verbose=verbose,
                         control.compute=list(dic=TRUE))
  }else{
    inla_naive_R <- inla(formula = formula_naive_R, 
                         family = "binomial", 
                         data = naive_data_R, 
                         Ntrials=ntrials, 
                         verbose=verbose,
                         control.compute=list(config=TRUE, dic=TRUE))
  }
  # Use control.family to fix sigma2. Specified through log-precision.
  
  return(inla_naive_R)
}

reduced1_R <- function(data_sim, missing, verbose = F, compute_all = FALSE){
  data<- data_sim
  n <- nrow(data)
  
  linear_covariates_R <- list(beta_0 = rep(1,n),
                              m_BP1 = data$bp_1_corr,
                              m_AGE = data$age_1,
                              m_eps1 =1:n)
  
  naive_data_R <- linear_covariates_R
  y_naive_R <- missing
  
  naive_data_R$Y <- y_naive_R
  formula_naive_R = Y ~ -1 + beta_0  + f(m_BP1,model="rw2",constr=T) + f(m_AGE,model="rw2",constr=T)
  ntrials <- 1
  
  if(compute_all == FALSE){
    inla_naive_R <- inla(formula = formula_naive_R, 
                         family = "binomial", 
                         Ntrials = ntrials, 
                         data = naive_data_R, 
                         verbose=verbose,
                         control.compute=list(dic=TRUE))
  }else{
    inla_naive_R <- inla(formula = formula_naive_R, 
                         family = "binomial", 
                         data = naive_data_R, 
                         Ntrials=ntrials, 
                         verbose=verbose,
                         control.compute=list(config=TRUE, dic=TRUE))
  }
  
  # Use control.family to fix sigma2. Specified through log-precision.
  
  return(inla_naive_R)
}

#l1 <- linear_R(HUNT23_INLA, HUNT23_INLA$R3, verbose = T, compute_all = F)
#f1 <- reduced1_R(HUNT23_INLA, HUNT23_INLA$Rtotal, verbose = T, compute_all = TRUE)
#f2 <- reduced2_R(HUNT23_INLA, HUNT23_INLA$R1, verbose = T, compute_all = TRUE)
#f1$dic[1]
#f2$dic[1]
#l1$dic[1]
#R2_INLAnaive_HUNT23$dic[1]

#ltotal <- linear_R(HUNT23_INLA, HUNT23_INLA$Rtotal, verbose = T, compute_all = TRUE)
#ltotal$dic[1]
#Rtotal_INLAnaive_HUNT23$dic[1]



