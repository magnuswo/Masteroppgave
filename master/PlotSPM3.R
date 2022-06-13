library('ggplot2')
library(ggpubr)

#NAIVE blood pressure, the missing processes is in the naiveINLA.R
bp_naive_H23 <- readRDS(file = ("/home/magnwoln/Masteroppgave/INLAresults/bp_naive_HUNT23.RData"))

R1_naive_H23 <- readRDS(file = ("/home/magnwoln/Masteroppgave/INLAresults/R1_naive_H23.RData"))
R2_naive_H23 <- readRDS(file = ("/home/magnwoln/Masteroppgave/INLAresults/R2_naive_H23.RData"))
R3_naive_H23 <- readRDS(file = ("/home/magnwoln/Masteroppgave/INLAresults/R3_naive_H23.RData"))

#Naive R
b_H23_R1 <- R1_naive_H23$marginals.fixed$beta_0
b_H23_R2 <- R2_naive_H23$marginals.fixed$beta_0
b_H23_R3 <- R3_naive_H23$marginals.fixed$beta_0

bp_H23_R1 <- R1_naive_H23$marginals.fixed$m_BP1
bp_H23_R2 <- R2_naive_H23$marginals.fixed$m_BP1
bp_H23_R3 <- R3_naive_H23$marginals.fixed$m_BP1

age_H23_R1 <- R1_naive_H23$marginals.fixed$m_AGE
age_H23_R2 <- R2_naive_H23$marginals.fixed$m_AGE

age_H23_R3 <- R3_naive_H23$summary.random$m_AGE

sex_H23_R1 <- R1_naive_H23$marginals.fixed$m_SEX
sex_H23_R3 <- R3_naive_H23$marginals.fixed$m_SEX

bmi_H23_R1 <- R1_naive_H23$marginals.fixed$m_BMI
bmi_H23_R3 <- R3_naive_H23$marginals.fixed$m_BMI

a_naive <- bp_naive_H23$marginals.fixed$alpha_0
bp_naive <- bp_naive_H23$marginals.fixed$y_BP1
age_naive <- bp_naive_H23$marginals.fixed$y_AGE
sex_naive <- bp_naive_H23$marginals.fixed$y_SEX
bmi_naive <- bp_naive_H23$marginals.fixed$y_BMI
e_naive <- bp_naive_H23$marginals.hyperpar$`Precision for the Gaussian observations`

# SPM 1 for comparison 

SPM1_Rtotal_H23 <- readRDS(file = ("/home/magnwoln/Masteroppgave/INLAresults/SPM1_HUNT23.RData"))

c_SPM1_Rtotal_H23 <- SPM1_Rtotal_H23$marginals.hyperpar$`Beta for m_eps1`
  
# SPM 3

SPM3_H23 <- readRDS(file = ("/home/magnwoln/Masteroppgave/INLAresults/SPM3_H23_linR1R2.RData"))

c1_SPM3_R1_H23 <- SPM3_H23$marginals.hyperpar$`Beta for r1_eps1`
c2_SPM3_R2_H23 <- SPM3_H23$marginals.hyperpar$`Beta for r2_eps1`
c3_SPM3_R3_H23 <- SPM3_H23$marginals.hyperpar$`Beta for r3_eps1`

cplotSPM3 <- ggplot() + 
  scale_colour_manual("Models:", values = c("SPM 1 with Rtotal" = "black", "SPM 3: Deceased" = "blue", 
                                            "SPM 3: Moved out"="green",
                                            "SPM 3: Invited, but did not show"="red" )) +
  geom_line(data = as.data.frame(c1_SPM3_R1_H23), aes(x = x, y = y, colour = "SPM 3: Deceased")) +
  geom_line(data = as.data.frame(c2_SPM3_R2_H23), aes(x = x, y = y, colour = "SPM 3: Moved out")) +
  geom_line(data = as.data.frame(c3_SPM3_R3_H23), aes(x = x, y = y, colour = "SPM 3: Invited, but did not show")) +
  geom_line(data = as.data.frame(c_SPM1_Rtotal_H23), aes(x = x, y = y, colour = "SPM 1 with Rtotal")) +
  xlab("c") + ylab("Density")  + theme(legend.position = "bottom")

cplotSPM3

ggsave("cplotSPM3.png", plot = cplotSPM3, path = "/home/magnwoln/Masteroppgave/Figs/", width = 20, height = 14, units = "cm", bg="white")

a_SPM3_H23 <- SPM3_H23$marginals.fixed$alpha_0
bp_SPM3_H23 <- SPM3_H23$marginals.fixed$y_BP1
age_SPM3_H23 <- SPM3_H23$marginals.fixed$y_AGE
bmi_SPM3_H23 <- SPM3_H23$marginals.fixed$y_BMI
sex_SPM3_H23 <- SPM3_H23$marginals.fixed$y_SEX
e_SPM3_H23 <- SPM3_H23$marginals.hyperpar$`Precision for y_eps1`

#Plot for latent fields
alpha0plotSPM3 <- ggplot() + 
  scale_colour_manual("Models:", values = c("Naive"="black", "SPM 3" = "red")) +
  geom_line(data = as.data.frame(a_naive), aes(x = x, y = y, colour = "Naive")) +
  geom_line(data = as.data.frame(a_SPM3_H23), aes(x = x, y = y, colour = "SPM 3" )) +
  xlab(expression(alpha[0])) + ylab("Density") + theme(legend.position = "none")

alphaBPplotSPM3 <- ggplot() + 
  scale_colour_manual("Models:", values = c("Naive"="black", "SPM 3" = "red")) +
  geom_line(data = as.data.frame(bp_naive), aes(x = x, y = y, colour = "Naive")) +
  geom_line(data = as.data.frame(bp_SPM3_H23), aes(x = x, y = y, colour = "SPM 3" )) +
  xlab(expression(alpha["BP1"])) + ylab("Density") + theme(legend.position = "none")

alphaageplotSPM3 <- ggplot() + 
  scale_colour_manual("Models:", values = c("Naive"="black", "SPM 3" = "red")) +
  geom_line(data = as.data.frame(age_naive), aes(x = x, y = y, colour = "Naive")) +
  geom_line(data = as.data.frame(age_SPM3_H23), aes(x = x, y = y, colour = "SPM 3")) +
  xlab(expression(alpha["age"])) + ylab("Density") + theme(legend.position = "none")

alphasexplotSPM3 <- ggplot() + 
  scale_colour_manual("Models:", values = c("Naive"="black", "SPM 3" = "red")) +
  geom_line(data = as.data.frame(sex_naive), aes(x = x, y = y, colour = "Naive")) +
  geom_line(data = as.data.frame(sex_SPM3_H23), aes(x = x, y = y, colour = "SPM 3")) +
  xlab(expression(alpha["sex"])) + ylab("Density") + theme(legend.position = "none")

alphaBMIplotSPM3 <- ggplot() + 
  scale_colour_manual("Models:", values = c("Naive"="black", "SPM 3" = "red")) +
  geom_line(data = as.data.frame(bmi_naive), aes(x = x, y = y, colour = "Naive")) +
  geom_line(data = as.data.frame(bmi_SPM3_H23), aes(x = x, y = y, colour = "SPM 3" )) +
  xlab(expression(alpha["BMI"])) + ylab("Density")   + theme(legend.position = "none")

eplotSPM3 <- ggplot() + 
  scale_colour_manual("Models:", values = c("Naive"="black", "SPM 3" = "red")) +
  geom_line(data = as.data.frame(e_naive), aes(x = x, y = y, colour = "Naive")) +
  geom_line(data = as.data.frame(e_SPM3_H23), aes(x = x, y = y, colour = "SPM 3" )) +
  xlab(expression(sigma[epsilon]^2)) + ylab("Density")   + theme(legend.position = "none")

SPM3H23ResultPlot <- ggarrange(alpha0plotSPM3, alphaBPplotSPM3, alphaageplotSPM3, alphasexplotSPM3, alphaBMIplotSPM3,eplotSPM3,
                               labels = c("", "", "", "","",""), common.legend=TRUE, legend="bottom",
                               ncol = 2, nrow = 3)
SPM3H23ResultPlot

ggsave("SPM3H23ResultPlot.png", plot = SPM3H23ResultPlot, path = "/home/magnwoln/Masteroppgave/Figs/", width = 20, height = 14, units = "cm",bg="white")

#Plot for missing processes in SPM 3 first beta
b_SPM3_R1_H23 <- SPM3_H23$marginals.fixed$r1_0
b_SPM3_R2_H23 <- SPM3_H23$marginals.fixed$r2_0
b_SPM3_R3_H23 <- SPM3_H23$marginals.fixed$r3_0

age_SPM3_R1_H23 <- SPM3_H23$marginals.fixed$r1_AGE
age_SPM3_R2_H23 <- SPM3_H23$marginals.fixed$r2_AGE
age_SPM3_R3_H23 <- SPM3_H23$summary.random$r3_AGE

bp_SPM3_R1_H23 <- SPM3_H23$marginals.fixed$r1_BP1
bp_SPM3_R2_H23 <- SPM3_H23$marginals.fixed$r2_BP1
bp_SPM3_R3_H23 <- SPM3_H23$marginals.fixed$r3_BP1

sex_SPM3_R1_H23 <- SPM3_H23$marginals.fixed$r1_SEX
sex_SPM3_R3_H23 <- SPM3_H23$marginals.fixed$r3_SEX

bmi_SPM3_R1_H23 <- SPM3_H23$marginals.fixed$r1_BMI
bmi_SPM3_R3_H23 <- SPM3_H23$marginals.fixed$r3_BMI

#Plot for latent fields
beta0plotSPM3 <- ggplot() + 
  scale_colour_manual("Models:", values = c("R1 - Deceased" = "blue", 
                                            "R2 - Moved out"="green",
                                            "R3 - Invited, but did not show"="red")) +
  geom_line(data = as.data.frame(b_SPM3_R1_H23), aes(x = x, y = y, colour = "R1 - Deceased" )) +
  geom_line(data = as.data.frame(b_H23_R1), aes(x = x, y = y, colour = "R1 - Deceased" ), linetype = "dashed") +
  geom_line(data = as.data.frame(b_SPM3_R2_H23), aes(x = x, y = y, colour = "R2 - Moved out" )) +
  geom_line(data = as.data.frame(b_H23_R2), aes(x = x, y = y, colour = "R2 - Moved out" ), linetype="dashed") +
  geom_line(data = as.data.frame(b_SPM3_R3_H23), aes(x = x, y = y, colour = "R3 - Invited, but did not show" )) +
  geom_line(data = as.data.frame(b_H23_R3), aes(x = x, y = y, colour = "R3 - Invited, but did not show" ), linetype="dashed") +
  xlab(expression(beta[0])) + ylab("Density") + theme(legend.position = "none") 

beta0plotSPM3

betaBPplotSPM3 <- ggplot() + 
  scale_colour_manual("Models:", values = c("R1 - Deceased" = "blue", 
                                            "R2 - Moved out"="green",
                                            "R3 - Invited, but did not show"="red")) +
  geom_line(data = as.data.frame(bp_SPM3_R1_H23), aes(x = x, y = y, colour = "R1 - Deceased" )) +
  geom_line(data = as.data.frame(bp_H23_R1), aes(x = x, y = y, colour = "R1 - Deceased" ), linetype="dashed") +
  geom_line(data = as.data.frame(bp_SPM3_R2_H23), aes(x = x, y = y, colour = "R2 - Moved out" )) +
  geom_line(data = as.data.frame(bp_H23_R2), aes(x = x, y = y, colour = "R2 - Moved out" ), linetype="dashed") +
  geom_line(data = as.data.frame(bp_SPM3_R3_H23), aes(x = x, y = y, colour = "R3 - Invited, but did not show" )) +
  geom_line(data = as.data.frame(bp_H23_R3), aes(x = x, y = y, colour = "R3 - Invited, but did not show" ), linetype = "dashed") +
  xlab(expression(beta["BP1"])) + ylab("Density") + theme(legend.position = "none")

betasexplotSPM3 <- ggplot() + 
  scale_colour_manual("Models:", values = c("R1 - Deceased" = "blue", 
                                            "R2 - Moved out"="green",
                                            "R3 - Invited, but did not show"="red")) +
  geom_line(data = as.data.frame(sex_SPM3_R1_H23), aes(x = x, y = y, colour = "R1 - Deceased" )) +
  geom_line(data = as.data.frame(sex_H23_R1), aes(x = x, y = y, colour = "R1 - Deceased" ), linetype="dashed") +
  geom_line(data = as.data.frame(sex_SPM3_R3_H23), aes(x = x, y = y, colour = "R3 - Invited, but did not show" )) +
  geom_line(data = as.data.frame(sex_H23_R3), aes(x = x, y = y, colour = "R3 - Invited, but did not show" ), linetype="dashed") +
  xlab(expression(beta["sex"])) + ylab("Density") + theme(legend.position = "none")

betaageplotSPM3 <- ggplot() + 
  scale_colour_manual("Models:", values = c("R1 - Deceased" = "blue", 
                                            "R2 - Moved out"="green",
                                            "R3 - Invited, but did not show"="red")) +
  geom_line(data = as.data.frame(age_SPM3_R1_H23), aes(x = x, y = y, colour = "R1 - Deceased" )) +
  geom_line(data = as.data.frame(age_H23_R1), aes(x = x, y = y, colour = "R1 - Deceased" ), linetype="dashed") +
  geom_line(data = as.data.frame(age_SPM3_R2_H23), aes(x = x, y = y, colour = "R2 - Moved out" )) +
  geom_line(data = as.data.frame(age_H23_R2), aes(x = x, y = y, colour = "R2 - Moved out" ), linetype="dashed") +
  xlab(expression(beta["age"])) + ylab("Density") + theme(legend.position = "none")

randomageplotSPM3 <- ggplot() + 
  scale_colour_manual("Models:", values = c("R1 - Deceased" = "blue", 
                                            "R2 - Moved out"="green",
                                            "R3 - Invited, but did not show"="red")) +
  geom_line(data = as.data.frame(age_SPM3_R3_H23), aes(x = ID, y = `0.5quant`, colour = "R3 - Invited, but did not show" )) +
  geom_line(data = as.data.frame(age_SPM3_R3_H23), aes(x = ID, y = `0.025quant`, colour = "R3 - Invited, but did not show" ), linetype="dashed") +
  geom_line(data = as.data.frame(age_SPM3_R3_H23), aes(x = ID, y = `0.975quant`, colour = "R3 - Invited, but did not show" ), linetype="dashed") +
  geom_line(data = as.data.frame(age_H23_R3), aes(x = ID, y = `0.5quant`), linetype="dashed", colour="gray") +
  geom_line(data = as.data.frame(age_H23_R3), aes(x = ID, y = `0.025quant`), linetype="dashed", colour="gray") +
  geom_line(data = as.data.frame(age_H23_R3), aes(x = ID, y = `0.975quant`), linetype="dashed", colour="gray") +
  xlab("Age") + ylab("Random effect") + theme(legend.position = "none")

betaBMIplotSPM3 <- ggplot() + 
  scale_colour_manual("Models:", values = c("R1 - Deceased" = "blue", 
                                            "R2 - Moved out"="green",
                                            "R3 - Invited, but did not show"="red")) +
  geom_line(data = as.data.frame(bmi_SPM3_R1_H23), aes(x = x, y = y, colour = "R1 - Deceased" )) +
  geom_line(data = as.data.frame(bmi_H23_R1), aes(x = x, y = y, colour = "R1 - Deceased" ), linetype="dashed") +
  geom_line(data = as.data.frame(bmi_SPM3_R3_H23), aes(x = x, y = y, colour = "R3 - Invited, but did not show" )) +
  geom_line(data = as.data.frame(bmi_H23_R3), aes(x = x, y = y, colour = "R3 - Invited, but did not show" ), linetype="dashed") +
  xlab(expression(beta["BMI"])) + ylab("Density") + theme(legend.position = "none")

betaSPM3H23Plot <- ggarrange(beta0plotSPM3, betaBPplotSPM3, betasexplotSPM3, betaBMIplotSPM3, betaageplotSPM3, randomageplotSPM3,
                               labels = c("", "", "", ""), common.legend=TRUE, legend="bottom",
                               ncol = 2, nrow = 3)
betaSPM3H23Plot

ggsave("betaSPM3H23Plot.png", plot = betaSPM3H23Plot, path = "/home/magnwoln/Masteroppgave/Figs/", width = 20, height = 14, units = "cm", bg="white")
