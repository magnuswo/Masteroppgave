library('ggplot2')
library(ggpubr)

#NAIVE blood pressure, the missing processes is in the naiveINLA.R
bp_naive_H23 <- readRDS(file = ("/home/magnwoln/Masteroppgave/INLAresults/bp_naive_HUNT23.RData"))
 
a_naive <- bp_naive_H23$marginals.fixed$alpha_0
bp_naive <- bp_naive_H23$marginals.fixed$y_BP1
age_naive <- bp_naive_H23$marginals.fixed$y_AGE
sex_naive <- bp_naive_H23$marginals.fixed$y_SEX
bmi_naive <- bp_naive_H23$marginals.fixed$y_BMI
e_naive <- bp_naive_H23$marginals.hyperpar$`Precision for the Gaussian observations`

#SPM 1
SPM1_H23 <- readRDS(file = ("/home/magnwoln/Masteroppgave/INLAresults/SPM1_HUNT23.RData"))
SPM1_H23_R1 <- readRDS(file = ("/home/magnwoln/Masteroppgave/INLAresults/SPM1_HUNT23_R1.RData"))
SPM1_H23_R2 <- readRDS(file = ("/home/magnwoln/Masteroppgave/INLAresults/SPM1_HUNT23_R2.RData"))
SPM1_H23_R3 <- readRDS(file = ("/home/magnwoln/Masteroppgave/INLAresults/SPM1_HUNT23_R3.RData"))

#c for SPM 1
c_SPM1_Rtotal_H23 <- SPM1_H23$marginals.hyperpar$`Beta for m_eps1`
c_SPM1_R1_H23 <- SPM1_H23_R1$marginals.hyperpar$`Beta for m_eps1`
c_SPM1_R2_H23 <- SPM1_H23_R2$marginals.hyperpar$`Beta for m_eps1`
c_SPM1_R3_H23 <- SPM1_H23_R3$marginals.hyperpar$`Beta for m_eps1`

e_SPM1_Rtotal_H23 <- SPM1_H23$marginals.hyperpar$`Precision for y_eps1`
e_SPM1_R1_H23 <- SPM1_H23_R1$marginals.hyperpar$`Precision for y_eps1`
e_SPM1_R2_H23 <- SPM1_H23_R2$marginals.hyperpar$`Precision for y_eps1`
e_SPM1_R3_H23 <- SPM1_H23_R3$marginals.hyperpar$`Precision for y_eps1`

#latent field for blood pressure model
a_SPM1_Rtotal_H23 <- SPM1_H23$marginals.fixed$alpha_0
a_SPM1_R1_H23 <- SPM1_H23_R1$marginals.fixed$alpha_0
a_SPM1_R2_H23 <- SPM1_H23_R2$marginals.fixed$alpha_0
a_SPM1_R3_H23 <- SPM1_H23_R3$marginals.fixed$alpha_0

bp_SPM1_Rtotal_H23 <- SPM1_H23$marginals.fixed$y_BP1
bp_SPM1_R1_H23 <- SPM1_H23_R1$marginals.fixed$y_BP1
bp_SPM1_R2_H23 <- SPM1_H23_R2$marginals.fixed$y_BP1
bp_SPM1_R3_H23 <- SPM1_H23_R3$marginals.fixed$y_BP1

sex_SPM1_Rtotal_H23 <- SPM1_H23$marginals.fixed$y_SEX
sex_SPM1_R1_H23 <- SPM1_H23_R1$marginals.fixed$y_SEX
sex_SPM1_R2_H23 <- SPM1_H23_R2$marginals.fixed$y_SEX
sex_SPM1_R3_H23 <- SPM1_H23_R3$marginals.fixed$y_SEX

age_SPM1_Rtotal_H23 <- SPM1_H23$marginals.fixed$y_AGE
age_SPM1_R1_H23 <- SPM1_H23_R1$marginals.fixed$y_AGE
age_SPM1_R2_H23 <- SPM1_H23_R2$marginals.fixed$y_AGE
age_SPM1_R3_H23 <- SPM1_H23_R3$marginals.fixed$y_AGE

bmi_SPM1_Rtotal_H23 <- SPM1_H23$marginals.fixed$y_BMI
bmi_SPM1_R1_H23 <- SPM1_H23_R1$marginals.fixed$y_BMI
bmi_SPM1_R2_H23 <- SPM1_H23_R2$marginals.fixed$y_BMI
bmi_SPM1_R3_H23 <- SPM1_H23_R3$marginals.fixed$y_BMI

alpha0SPM1plot <- ggplot() + 
  scale_colour_manual("Models:", values = c("Naive"="gray", "SPM 1 with Rtotal" = "black", 
                                            "SPM 1 with R1"="blue", "SPM 1 with R2"="green", "SPM 1 with R3"="red")) +
  geom_line(data = as.data.frame(a_naive), aes(x = x, y = y, colour = "Naive")) +
  geom_line(data = as.data.frame(a_SPM1_Rtotal_H23), aes(x = x, y = y, colour = "SPM 1 with Rtotal")) + 
  geom_line(data = as.data.frame(a_SPM1_R1_H23), aes(x = x, y = y, colour = "SPM 1 with R1" )) +
  geom_line(data = as.data.frame(a_SPM1_R2_H23), aes(x = x, y = y, colour = "SPM 1 with R2")) +
  geom_line(data = as.data.frame(a_SPM1_R3_H23), aes(x = x, y = y, colour = "SPM 1 with R3")) +
  xlab(expression(alpha[0])) + ylab("Density") + theme(legend.position = "none")

#alpha0SPM1plot

alphaBPSPM1plot <- ggplot() + 
  scale_colour_manual("Models:", values = c("Naive"="gray", "SPM 1 with Rtotal" = "black", 
                                                                  "SPM 1 with R1"="blue", "SPM 1 with R2"="green", "SPM 1 with R3"="red")) +
                        geom_line(data = as.data.frame(bp_naive), aes(x = x, y = y, colour = "Naive")) +
                        geom_line(data = as.data.frame(bp_SPM1_Rtotal_H23), aes(x = x, y = y, colour = "SPM 1 with Rtotal")) + 
                        geom_line(data = as.data.frame(bp_SPM1_R1_H23), aes(x = x, y = y, colour = "SPM 1 with R1" )) +
                        geom_line(data = as.data.frame(bp_SPM1_R2_H23), aes(x = x, y = y, colour = "SPM 1 with R2")) +
                        geom_line(data = as.data.frame(bp_SPM1_R3_H23), aes(x = x, y = y, colour = "SPM 1 with R3")) +
  xlab(expression(alpha["BP1"])) + ylab("Density") + theme(legend.position = "none")

#alphaBPSPM1plot

alphasexSPM1plot <- ggplot() + 
  scale_colour_manual("Models:", values = c("Naive"="gray", "SPM 1 with Rtotal" = "black", 
                                            "SPM 1 with R1"="blue", "SPM 1 with R2"="green", "SPM 1 with R3"="red")) +
  geom_line(data = as.data.frame(sex_naive), aes(x = x, y = y, colour = "Naive")) +
  geom_line(data = as.data.frame(sex_SPM1_Rtotal_H23), aes(x = x, y = y, colour = "SPM 1 with Rtotal")) + 
  geom_line(data = as.data.frame(sex_SPM1_R1_H23), aes(x = x, y = y, colour = "SPM 1 with R1" )) +
  geom_line(data = as.data.frame(sex_SPM1_R2_H23), aes(x = x, y = y, colour = "SPM 1 with R2")) +
  geom_line(data = as.data.frame(sex_SPM1_R3_H23), aes(x = x, y = y, colour = "SPM 1 with R3")) +
  xlab(expression(alpha["sex"])) + ylab("Density") + theme(legend.position = "right")

alphaageSPM1plot <- ggplot() + 
  scale_colour_manual("Models:", values = c("Naive"="gray", "SPM 1 with Rtotal" = "black", 
                                            "SPM 1 with R1"="blue", "SPM 1 with R2"="green", "SPM 1 with R3"="red")) +
  geom_line(data = as.data.frame(age_naive), aes(x = x, y = y, colour = "Naive")) +
  geom_line(data = as.data.frame(age_SPM1_Rtotal_H23), aes(x = x, y = y, colour = "SPM 1 with Rtotal")) + 
  geom_line(data = as.data.frame(age_SPM1_R1_H23), aes(x = x, y = y, colour = "SPM 1 with R1" )) +
  geom_line(data = as.data.frame(age_SPM1_R2_H23), aes(x = x, y = y, colour = "SPM 1 with R2")) +
  geom_line(data = as.data.frame(age_SPM1_R3_H23), aes(x = x, y = y, colour = "SPM 1 with R3")) +
  xlab(expression(alpha["age"])) + ylab("Density") + theme(legend.position = "none")

alphaBMISPM1plot <- ggplot() + 
  scale_colour_manual("Models:", values = c("Naive"="gray", "SPM 1 with Rtotal" = "black", 
                                            "SPM 1 with R1"="blue", "SPM 1 with R2"="green", "SPM 1 with R3"="red")) +
  geom_line(data = as.data.frame(bmi_naive), aes(x = x, y = y, colour = "Naive")) +
  geom_line(data = as.data.frame(bmi_SPM1_Rtotal_H23), aes(x = x, y = y, colour = "SPM 1 with Rtotal")) + 
  geom_line(data = as.data.frame(bmi_SPM1_R1_H23), aes(x = x, y = y, colour = "SPM 1 with R1" )) +
  geom_line(data = as.data.frame(bmi_SPM1_R2_H23), aes(x = x, y = y, colour = "SPM 1 with R2")) +
  geom_line(data = as.data.frame(bmi_SPM1_R3_H23), aes(x = x, y = y, colour = "SPM 1 with R3")) +
  xlab(expression(alpha["BMI"])) + ylab("Density")   + theme(legend.position = "none")

eSPM1plot <- ggplot() + 
  scale_colour_manual("Models:", values = c("Naive"="gray", "SPM 1 with Rtotal" = "black", 
                                            "SPM 1 with R1"="blue", "SPM 1 with R2"="green", "SPM 1 with R3"="red")) +
  geom_line(data = as.data.frame(e_naive), aes(x = x, y = y, colour = "Naive")) +
  geom_line(data = as.data.frame(e_SPM1_Rtotal_H23), aes(x = x, y = y, colour = "SPM 1 with Rtotal")) + 
  geom_line(data = as.data.frame(e_SPM1_R1_H23), aes(x = x, y = y, colour = "SPM 1 with R1" )) +
  geom_line(data = as.data.frame(e_SPM1_R2_H23), aes(x = x, y = y, colour = "SPM 1 with R2")) +
  geom_line(data = as.data.frame(e_SPM1_R3_H23), aes(x = x, y = y, colour = "SPM 1 with R3")) +
  xlab(expression(sigma[epsilon]^2)) + ylab("Density")   + theme(legend.position = "none")

SPM1H23alphaPlot <- ggarrange(alpha0SPM1plot, alphaBPSPM1plot, alphasexSPM1plot, alphaageSPM1plot, alphaBMISPM1plot, eSPM1plot,
                                  labels = c("", "", "", "", "", ""), common.legend=TRUE, legend="bottom",
                                  ncol = 2, nrow = 3)
SPM1H23alphaPlot
ggsave("SPM1H23alphaPlot.png", plot = SPM1H23alphaPlot, path = "/home/magnwoln/Masteroppgave/Figs/", width = 20, height = 14, units = "cm", bg = "white")

cSPM1plotHUNT23 <- ggplot() + 
  scale_colour_manual("Missing Process", values = c("Rtotal" = "black", 
                                           "R1 - Deceased"="blue", "R2 - Moved Out"="green", "R3 - Invited, but no show"="red")) +
  geom_line(data = as.data.frame(c_SPM1_Rtotal_H23), aes(x = x, y = y, colour = "Rtotal")) +
  geom_line(data = as.data.frame(c_SPM1_R1_H23), aes(x = x, y = y, colour = "R1 - Deceased")) +
  geom_line(data = as.data.frame(c_SPM1_R2_H23), aes(x = x, y = y, colour = "R2 - Moved Out")) +
  geom_line(data = as.data.frame(c_SPM1_R3_H23), aes(x = x, y = y, colour = "R3 - Invited, but no show")) +
  xlab("c") + ylab("Density")  + theme(legend.position = "bottom")

cSPM1plotHUNT23

ggsave("cSPM1plotHUNT23.png", plot = cSPM1plotHUNT23, path = "/home/magnwoln/Masteroppgave/Figs/", width = 20, height = 14, units = "cm", bg="white")

# NEED TO CREATE A PLOT FOR THE MISSING PPROCESSES AS WELL FOR SPM1

#latent field for blood pressure model
b_SPM1_Rtotal_H23 <- SPM1_H23$marginals.fixed$beta_0
b_SPM1_R1_H23 <- SPM1_H23_R1$marginals.fixed$beta_0
b_SPM1_R2_H23 <- SPM1_H23_R2$marginals.fixed$beta_0
b_SPM1_R3_H23 <- SPM1_H23_R3$marginals.fixed$beta_0

bbp_SPM1_Rtotal_H23 <- SPM1_H23$marginals.fixed$m_BP1
bbp_SPM1_R1_H23 <- SPM1_H23_R1$marginals.fixed$m_BP1
bbp_SPM1_R2_H23 <- SPM1_H23_R2$marginals.fixed$m_BP1
bbp_SPM1_R3_H23 <- SPM1_H23_R3$marginals.fixed$m_BP1

bsex_SPM1_Rtotal_H23 <- SPM1_H23$marginals.fixed$m_SEX
bsex_SPM1_R1_H23 <- SPM1_H23_R1$marginals.fixed$m_SEX
bsex_SPM1_R3_H23 <- SPM1_H23_R3$marginals.fixed$m_SEX

sage_SPM1_Rtotal_H23 <- SPM1_H23$summary.random$m_AGE
bage_SPM1_R1_H23 <- SPM1_H23_R1$marginals.fixed$m_AGE
bage_SPM1_R2_H23 <- SPM1_H23_R2$marginals.fixed$m_AGE
sage_SPM1_R3_H23 <- SPM1_H23_R3$summary.random$m_AGE

bbmi_SPM1_Rtotal_H23 <- SPM1_H23$marginals.fixed$m_BMI
bbmi_SPM1_R1_H23 <- SPM1_H23_R1$marginals.fixed$m_BMI
bbmi_SPM1_R3_H23 <- SPM1_H23_R3$marginals.fixed$m_BMI

beta0SPM1plot <- ggplot() + 
  scale_colour_manual("Models:", values = c( "SPM 1 with Rtotal" = "black", 
                                            "SPM 1 with R1"="blue", "SPM 1 with R2"="green", "SPM 1 with R3"="red")) +
  geom_line(data = as.data.frame(b_SPM1_Rtotal_H23), aes(x = x, y = y, colour = "SPM 1 with Rtotal")) + 
  geom_line(data = as.data.frame(b_SPM1_R1_H23), aes(x = x, y = y, colour = "SPM 1 with R1" )) +
  geom_line(data = as.data.frame(b_SPM1_R2_H23), aes(x = x, y = y, colour = "SPM 1 with R2")) +
  geom_line(data = as.data.frame(b_SPM1_R3_H23), aes(x = x, y = y, colour = "SPM 1 with R3")) +
  xlab(expression(beta[0])) + ylab("Density") + theme(legend.position = "none")

betaBPSPM1plot <- ggplot() + 
  scale_colour_manual("Models:", values = c("SPM 1 with Rtotal" = "black", 
                                            "SPM 1 with R1"="blue", "SPM 1 with R2"="green", "SPM 1 with R3"="red")) +
  geom_line(data = as.data.frame(bbp_SPM1_Rtotal_H23), aes(x = x, y = y, colour = "SPM 1 with Rtotal")) + 
  geom_line(data = as.data.frame(bbp_SPM1_R1_H23), aes(x = x, y = y, colour = "SPM 1 with R1" )) +
  geom_line(data = as.data.frame(bbp_SPM1_R2_H23), aes(x = x, y = y, colour = "SPM 1 with R2")) +
  geom_line(data = as.data.frame(bbp_SPM1_R3_H23), aes(x = x, y = y, colour = "SPM 1 with R3")) +
  xlab(expression(beta["BP1"])) + ylab("Density") + theme(legend.position = "none")

betasexSPM1plot <- ggplot() + 
  scale_colour_manual("Models:", values = c("SPM 1 with Rtotal" = "black", 
                                            "SPM 1 with R1"="blue", "SPM 1 with R2"="green", "SPM 1 with R3"="red")) +
  geom_line(data = as.data.frame(bsex_SPM1_Rtotal_H23), aes(x = x, y = y, colour = "SPM 1 with Rtotal")) + 
  geom_line(data = as.data.frame(bsex_SPM1_R1_H23), aes(x = x, y = y, colour = "SPM 1 with R1" )) +
  #geom_line(data = as.data.frame(bsex_SPM1_R2_H23), aes(x = x, y = y, colour = "SPM 1 with R2")) +
  geom_line(data = as.data.frame(bsex_SPM1_R3_H23), aes(x = x, y = y, colour = "SPM 1 with R3")) +
  xlab(expression(beta["sex"])) + ylab("Density") + theme(legend.position = "right")

betaageSPM1plot <- ggplot() + 
  scale_colour_manual("Models:", values = c("SPM 1 with Rtotal" = "black", 
                                            "SPM 1 with R1"="blue", "SPM 1 with R2"="green", "SPM 1 with R3"="red")) +
  geom_line(data = as.data.frame(bage_SPM1_R1_H23), aes(x = x, y = y, colour = "SPM 1 with R1" )) +
  geom_line(data = as.data.frame(bage_SPM1_R2_H23), aes(x = x, y = y, colour = "SPM 1 with R2")) +
  xlab(expression(beta["age"])) + ylab("Density") + theme(legend.position = "none")

betaBMISPM1plot <- ggplot() + 
  scale_colour_manual("Models:", values = c( "SPM 1 with Rtotal" = "black", 
                                            "SPM 1 with R1"="blue", "SPM 1 with R2"="green", "SPM 1 with R3"="red")) +
  geom_line(data = as.data.frame(bbmi_SPM1_Rtotal_H23), aes(x = x, y = y, colour = "SPM 1 with Rtotal")) + 
  geom_line(data = as.data.frame(bbmi_SPM1_R1_H23), aes(x = x, y = y, colour = "SPM 1 with R1" )) +
  #geom_line(data = as.data.frame(bbmi_SPM1_R2_H23), aes(x = x, y = y, colour = "SPM 1 with R2")) +
  geom_line(data = as.data.frame(bbmi_SPM1_R3_H23), aes(x = x, y = y, colour = "SPM 1 with R3")) +
  xlab(expression(beta["BMI"])) + ylab("Density")   + theme(legend.position = "none")

randomageSPM1plot <- ggplot() + 
  scale_colour_manual("Models:", values = c( "SPM 1 with Rtotal" = "black", 
                                            "SPM 1 with R1"="blue", "SPM 1 with R2"="green", "SPM 1 with R3"="red")) +
  geom_line(data = as.data.frame(sage_SPM1_Rtotal_H23), aes(x = ID, y = `0.5quant`, colour = "SPM 1 with Rtotal")) + 
  geom_line(data = as.data.frame(sage_SPM1_Rtotal_H23), aes(x = ID, y = `0.025quant`, colour = "SPM 1 with Rtotal"), linetype="dashed") +
  geom_line(data = as.data.frame(sage_SPM1_Rtotal_H23), aes(x = ID, y = `0.975quant`, colour = "SPM 1 with Rtotal"), linetype="dashed") +
  geom_line(data = as.data.frame(sage_SPM1_R3_H23), aes(x = ID, y = `0.5quant`, colour = "SPM 1 with R3")) +
  geom_line(data = as.data.frame(sage_SPM1_R3_H23), aes(x = ID, y = `0.025quant`, colour = "SPM 1 with R3"), linetype="dashed") +
  geom_line(data = as.data.frame(sage_SPM1_R3_H23), aes(x = ID, y = `0.975quant`, colour = "SPM 1 with R3"), linetype="dashed") +
  xlab("age") + ylab("Random effect") + theme(legend.position = "none")


SPM1H23betaPlot <- ggarrange(beta0SPM1plot, betaBPSPM1plot, betasexSPM1plot, betaBMISPM1plot, betaageSPM1plot, randomageSPM1plot,
                              labels = c("", "", "", "", "", ""), common.legend=TRUE, legend="bottom",
                              ncol = 2, nrow = 3)
SPM1H23betaPlot
ggsave("SPM1H23betaPlot.png", plot = SPM1H23betaPlot, path = "/home/magnwoln/Masteroppgave/Figs/", width = 20, height = 14, units = "cm", bg = "white")


