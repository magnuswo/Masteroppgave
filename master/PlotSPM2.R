library(ggplot2)
library(ggpubr)

#NAIVE blood pressure, the missing processes is in the naiveINLA.R
bp_naive_H23 <- readRDS(file = ("/home/magnwoln/Masteroppgave/INLAresults/bp_naive_HUNT23.RData"))

a_naive <- bp_naive_H23$marginals.fixed$alpha_0
bp_naive <- bp_naive_H23$marginals.fixed$y_BP1
age_naive <- bp_naive_H23$marginals.fixed$y_AGE
sex_naive <- bp_naive_H23$marginals.fixed$y_SEX
bmi_naive <- bp_naive_H23$marginals.fixed$y_BMI
e_naive <- bp_naive_H23$marginals.hyperpar$`Precision for the Gaussian observations`

#SPM2

SPM2_H23_R12 <- readRDS(file = ("/home/magnwoln/Masteroppgave/INLAresults/SPM2_HUNT23_R12.RData"))
SPM2_H23_R13 <- readRDS(file = ("/home/magnwoln/Masteroppgave/INLAresults/SPM2_HUNT23_R13.RData"))
SPM2_H23_R23 <- readRDS(file = ("/home/magnwoln/Masteroppgave/INLAresults/SPM2_HUNT23_R23.RData"))

a_SPM2_R12_H23 <- SPM2_H23_R12$marginals.fixed$alpha_0
a_SPM2_R13_H23 <- SPM2_H23_R13$marginals.fixed$alpha_0
a_SPM2_R23_H23 <- SPM2_H23_R23$marginals.fixed$alpha_0

bp_SPM2_R12_H23 <- SPM2_H23_R12$marginals.fixed$y_BP1
bp_SPM2_R13_H23 <- SPM2_H23_R13$marginals.fixed$y_BP1
bp_SPM2_R23_H23 <- SPM2_H23_R23$marginals.fixed$y_BP1

age_SPM2_R12_H23 <- SPM2_H23_R12$marginals.fixed$y_AGE
age_SPM2_R13_H23 <- SPM2_H23_R13$marginals.fixed$y_AGE
age_SPM2_R23_H23 <- SPM2_H23_R23$marginals.fixed$y_AGE

sex_SPM2_R12_H23 <- SPM2_H23_R12$marginals.fixed$y_SEX
sex_SPM2_R13_H23 <- SPM2_H23_R13$marginals.fixed$y_SEX
sex_SPM2_R23_H23 <- SPM2_H23_R23$marginals.fixed$y_SEX

bmi_SPM2_R12_H23 <- SPM2_H23_R12$marginals.fixed$y_BMI
bmi_SPM2_R13_H23 <- SPM2_H23_R13$marginals.fixed$y_BMI
bmi_SPM2_R23_H23 <- SPM2_H23_R23$marginals.fixed$y_BMI

c1_SPM2_R1_H23_R12 <- SPM2_H23_R12$marginals.hyperpar$`Beta for r1_eps1`
c2_SPM2_R2_H23_R12 <- SPM2_H23_R12$marginals.hyperpar$`Beta for r2_eps1`
c1_SPM2_R1_H23_R13 <- SPM2_H23_R13$marginals.hyperpar$`Beta for r1_eps1`
c2_SPM2_R3_H23_R13 <- SPM2_H23_R13$marginals.hyperpar$`Beta for r2_eps1`
c1_SPM2_R2_H23_R23 <- SPM2_H23_R23$marginals.hyperpar$`Beta for r1_eps1`
c2_SPM2_R3_H23_R23 <- SPM2_H23_R23$marginals.hyperpar$`Beta for r2_eps1`

e_SPM2_H23_R12 <- SPM2_H23_R12$marginals.hyperpar$`Precision for y_eps1`
e_SPM2_H23_R13 <- SPM2_H23_R13$marginals.hyperpar$`Precision for y_eps1`
e_SPM2_H23_R23 <- SPM2_H23_R23$marginals.hyperpar$`Precision for y_eps1`

alpha0SPM2plot <- ggplot() + 
  scale_colour_manual("Models:", values = c("Naive"="gray",  
                                            "SPM 2 with R1 and R2"="blue", "SPM 2 with R1 and R3"="green", "SPM 2 with R2 and R3"="red")) +
  geom_line(data = as.data.frame(a_naive), aes(x = x, y = y, colour = "Naive")) +
  geom_line(data = as.data.frame(a_SPM2_R12_H23), aes(x = x, y = y, colour = "SPM 2 with R1 and R2" )) +
  geom_line(data = as.data.frame(a_SPM2_R13_H23), aes(x = x, y = y, colour = "SPM 2 with R1 and R3")) +
  geom_line(data = as.data.frame(a_SPM2_R23_H23), aes(x = x, y = y, colour = "SPM 2 with R2 and R3")) +
  xlab(expression(alpha[0])) + ylab("Density") + theme(legend.position = "none")

alphaBPSPM2plot <- ggplot() + 
  scale_colour_manual("Models:", values = c("Naive"="gray",
                                            "SPM 2 with R1 and R2"="blue", "SPM 2 with R1 and R3"="green","SPM 2 with R2 and R3"="red")) +
  geom_line(data = as.data.frame(bp_naive), aes(x = x, y = y, colour = "Naive")) +
  geom_line(data = as.data.frame(bp_SPM2_R12_H23), aes(x = x, y = y, colour = "SPM 2 with R1 and R2" )) +
  geom_line(data = as.data.frame(bp_SPM2_R13_H23), aes(x = x, y = y, colour = "SPM 2 with R1 and R3")) +
  geom_line(data = as.data.frame(bp_SPM2_R23_H23), aes(x = x, y = y, colour = "SPM 2 with R2 and R3")) +
  xlab(expression(alpha["BP1"])) + ylab("Density") + theme(legend.position = "none")

alphasexSPM2plot <- ggplot() + 
  scale_colour_manual("Models:", values = c("Naive"="gray", 
                                            "SPM 2 with R1 and R2"="blue", "SPM 2 with R1 and R3"="green","SPM 2 with R2 and R3"="red")) +
  geom_line(data = as.data.frame(sex_naive), aes(x = x, y = y, colour = "Naive")) +
  geom_line(data = as.data.frame(sex_SPM2_R12_H23), aes(x = x, y = y, colour = "SPM 2 with R1 and R2" )) +
  geom_line(data = as.data.frame(sex_SPM2_R13_H23), aes(x = x, y = y, colour = "SPM 2 with R1 and R3")) +
  geom_line(data = as.data.frame(sex_SPM2_R23_H23), aes(x = x, y = y, colour = "SPM 2 with R2 and R3")) +
  xlab(expression(alpha["sex"])) + ylab("Density") + theme(legend.position = "right")

alphaageSPM2plot <- ggplot() + 
  scale_colour_manual("Models:", values = c("Naive"="gray", 
                                            "SPM 2 with R1 and R2"="blue", "SPM 2 with R1 and R3"="green","SPM 2 with R2 and R3"="red")) +
  geom_line(data = as.data.frame(age_naive), aes(x = x, y = y, colour = "Naive")) +
  geom_line(data = as.data.frame(age_SPM2_R12_H23), aes(x = x, y = y, colour = "SPM 2 with R1 and R2" )) +
  geom_line(data = as.data.frame(age_SPM2_R13_H23), aes(x = x, y = y, colour = "SPM 2 with R1 and R3")) +
  geom_line(data = as.data.frame(age_SPM2_R23_H23), aes(x = x, y = y, colour = "SPM 2 with R2 and R3")) +
  xlab(expression(alpha["age"])) + ylab("Density") + theme(legend.position = "none")

alphaBMISPM2plot <- ggplot() + 
  scale_colour_manual("Models:", values = c("Naive"="gray",
                                            "SPM 2 with R1 and R2"="blue", "SPM 2 with R1 and R3"="green","SPM 2 with R2 and R3"="red")) +
  geom_line(data = as.data.frame(bmi_naive), aes(x = x, y = y, colour = "Naive")) +
  geom_line(data = as.data.frame(bmi_SPM2_R12_H23), aes(x = x, y = y, colour = "SPM 2 with R1 and R2" )) +
  geom_line(data = as.data.frame(bmi_SPM2_R13_H23), aes(x = x, y = y, colour = "SPM 2 with R1 and R3")) +
  geom_line(data = as.data.frame(bmi_SPM2_R23_H23), aes(x = x, y = y, colour = "SPM 2 with R2 and R3")) +
  xlab(expression(alpha["BMI"])) + ylab("Density")   + theme(legend.position = "none")

eSPM2plot <- ggplot() + 
  scale_colour_manual("Models:", values = c("Naive"="gray",
                                            "SPM 2 with R1 and R2"="blue", "SPM 2 with R1 and R3"="green","SPM 2 with R2 and R3"="red")) +
  geom_line(data = as.data.frame(e_naive), aes(x = x, y = y, colour = "Naive")) +
  geom_line(data = as.data.frame(e_SPM2_H23_R12), aes(x = x, y = y, colour = "SPM 2 with R1 and R2" )) +
  geom_line(data = as.data.frame(e_SPM2_H23_R13), aes(x = x, y = y, colour = "SPM 2 with R1 and R3")) +
  geom_line(data = as.data.frame(e_SPM2_H23_R23), aes(x = x, y = y, colour = "SPM 2 with R2 and R3")) +
  xlab(expression(sigma[epsilon]^2)) + ylab("Density")   + theme(legend.position = "none")

SPM2HUNT23ResultPlot <- ggarrange(alpha0SPM2plot, alphaBPSPM2plot, alphasexSPM2plot, alphaageSPM2plot, alphaBMISPM2plot, eSPM2plot,
                                  labels = c("", "", "", "", "", ""), common.legend=TRUE, legend="bottom",
                                  ncol = 2, nrow = 3)
SPM2HUNT23ResultPlot

ggsave("SPM2resultHUNT23.png", plot = SPM2HUNT23ResultPlot, path = "/home/magnwoln/Masteroppgave/Figs/", width = 20, height = 14, units = "cm", bg="white")


cSPM2plot <- ggplot() + 
  scale_colour_manual("Models:", values = c(
    "SPM 2 with R1 and R2"="blue", "SPM 2 with R1 and R3"="green","SPM 2 with R2 and R3"="red")) +
  geom_line(data = as.data.frame(c1_SPM2_R1_H23_R12), aes(x = x, y = y, colour = "SPM 2 with R1 and R2")) + 
  geom_line(data = as.data.frame(c2_SPM2_R2_H23_R12), aes(x = x, y = y, colour = "SPM 2 with R1 and R2" ), linetype="dashed") +
  geom_line(data = as.data.frame(c1_SPM2_R1_H23_R13), aes(x = x, y = y, colour = "SPM 2 with R1 and R3")) +
  geom_line(data = as.data.frame(c2_SPM2_R3_H23_R13), aes(x = x, y = y, colour = "SPM 2 with R1 and R3"), linetype = "dashed") +
  geom_line(data = as.data.frame(c1_SPM2_R2_H23_R23), aes(x = x, y = y, colour = "SPM 2 with R2 and R3")) +
  geom_line(data = as.data.frame(c2_SPM2_R3_H23_R23), aes(x = x, y = y, colour = "SPM 2 with R2 and R3"), linetype = "dashed") +
  xlab("c") + ylab("Density")  + theme(legend.position = "bottom")

cSPM2plot

ggsave("cSPM2H23.png", plot = cSPM2plot, path = "/home/magnwoln/Masteroppgave/Figs/", width = 20, height = 14, units = "cm", bg = "white")

#For the missing processes

b1_SPM2_R12_H23 <- SPM2_H23_R12$marginals.fixed$r1_0
b1_SPM2_R13_H23 <- SPM2_H23_R13$marginals.fixed$r1_0
b1_SPM2_R23_H23 <- SPM2_H23_R23$marginals.fixed$r1_0
b2_SPM2_R12_H23 <- SPM2_H23_R12$marginals.fixed$r2_0
b2_SPM2_R13_H23 <- SPM2_H23_R13$marginals.fixed$r2_0
b2_SPM2_R23_H23 <- SPM2_H23_R23$marginals.fixed$r2_0

bp1_SPM2_R12_H23 <- SPM2_H23_R12$marginals.fixed$r1_BP1
bp1_SPM2_R13_H23 <- SPM2_H23_R13$marginals.fixed$r1_BP1
bp1_SPM2_R23_H23 <- SPM2_H23_R23$marginals.fixed$r1_BP1
bp2_SPM2_R12_H23 <- SPM2_H23_R12$marginals.fixed$r2_BP1
bp2_SPM2_R13_H23 <- SPM2_H23_R13$marginals.fixed$r2_BP1
bp2_SPM2_R23_H23 <- SPM2_H23_R23$marginals.fixed$r2_BP1

age1_SPM2_R12_H23 <- SPM2_H23_R12$marginals.fixed$r1_AGE
age1_SPM2_R13_H23 <- SPM2_H23_R13$marginals.fixed$r1_AGE
age1_SPM2_R23_H23 <- SPM2_H23_R23$marginals.fixed$r1_AGE
age2_SPM2_R12_H23 <- SPM2_H23_R12$marginals.fixed$r2_AGE
age2_SPM2_R13_H23 <- SPM2_H23_R13$summary.random$r2_AGE
age2_SPM2_R23_H23 <- SPM2_H23_R23$summary.random$r2_AGE

sex1_SPM2_R12_H23 <- SPM2_H23_R12$marginals.fixed$r1_SEX
sex1_SPM2_R13_H23 <- SPM2_H23_R13$marginals.fixed$r1_SEX
sex2_SPM2_R13_H23 <- SPM2_H23_R13$marginals.fixed$r2_SEX
sex2_SPM2_R23_H23 <- SPM2_H23_R23$marginals.fixed$r2_SEX

bmi1_SPM2_R12_H23 <- SPM2_H23_R12$marginals.fixed$r1_BMI
bmi1_SPM2_R13_H23 <- SPM2_H23_R13$marginals.fixed$r1_BMI
bmi2_SPM2_R13_H23 <- SPM2_H23_R13$marginals.fixed$r2_BMI
bmi2_SPM2_R23_H23 <- SPM2_H23_R23$marginals.fixed$r2_BMI

beta0SPM2plot <- ggplot() + 
  scale_colour_manual("Models:", values = c("SPM 2 with R1 and R2"="blue", "SPM 2 with R1 and R3"="green", "SPM 2 with R2 and R3"="red")) +
  geom_line(data = as.data.frame(b1_SPM2_R12_H23), aes(x = x, y = y, colour = "SPM 2 with R1 and R2" )) +
  geom_line(data = as.data.frame(b1_SPM2_R13_H23), aes(x = x, y = y, colour = "SPM 2 with R1 and R3")) +
  geom_line(data = as.data.frame(b1_SPM2_R23_H23), aes(x = x, y = y, colour = "SPM 2 with R2 and R3")) +
  geom_line(data = as.data.frame(b2_SPM2_R12_H23), aes(x = x, y = y, colour = "SPM 2 with R1 and R2"), linetype='dashed') +
  geom_line(data = as.data.frame(b2_SPM2_R13_H23), aes(x = x, y = y, colour = "SPM 2 with R1 and R3"), linetype='dashed') +
  geom_line(data = as.data.frame(b2_SPM2_R23_H23), aes(x = x, y = y, colour = "SPM 2 with R2 and R3"), linetype='dashed') +
  xlab(expression(beta[0])) + ylab("Density") + theme(legend.position = "none")

betaBPSPM2plot <- ggplot() + 
  scale_colour_manual("Models:", values = c("SPM 2 with R1 and R2"="blue", "SPM 2 with R1 and R3"="green", "SPM 2 with R2 and R3"="red")) +
  geom_line(data = as.data.frame(bp1_SPM2_R12_H23), aes(x = x, y = y, colour = "SPM 2 with R1 and R2" )) +
  geom_line(data = as.data.frame(bp1_SPM2_R13_H23), aes(x = x, y = y, colour = "SPM 2 with R1 and R3")) +
  geom_line(data = as.data.frame(bp1_SPM2_R23_H23), aes(x = x, y = y, colour = "SPM 2 with R2 and R3")) +
  geom_line(data = as.data.frame(bp2_SPM2_R12_H23), aes(x = x, y = y, colour = "SPM 2 with R1 and R2"), linetype='dashed') +
  geom_line(data = as.data.frame(bp2_SPM2_R13_H23), aes(x = x, y = y, colour = "SPM 2 with R1 and R3"), linetype='dashed') +
  geom_line(data = as.data.frame(bp2_SPM2_R23_H23), aes(x = x, y = y, colour = "SPM 2 with R2 and R3"), linetype='dashed') +
  xlab(expression(beta["BP1"])) + ylab("Density") + theme(legend.position = "none")

betasexSPM2plot <- ggplot() + 
  scale_colour_manual("Models:", values = c("SPM 2 with R1 and R2"="blue", "SPM 2 with R1 and R3"="green", "SPM 2 with R2 and R3"="red")) +
  geom_line(data = as.data.frame(sex1_SPM2_R12_H23), aes(x = x, y = y, colour = "SPM 2 with R1 and R2" )) +
  geom_line(data = as.data.frame(sex1_SPM2_R13_H23), aes(x = x, y = y, colour = "SPM 2 with R1 and R3")) +
  #geom_line(data = as.data.frame(sex1_SPM2_R23_H23), aes(x = x, y = y, colour = "SPM 2 with R2 and R3")) +
  #geom_line(data = as.data.frame(sex2_SPM2_R12_H23), aes(x = x, y = y, colour = "SPM 2 with R1 and R2"), linetype='dashed') +
  geom_line(data = as.data.frame(sex2_SPM2_R13_H23), aes(x = x, y = y, colour = "SPM 2 with R1 and R3"), linetype='dashed') +
  geom_line(data = as.data.frame(sex2_SPM2_R23_H23), aes(x = x, y = y, colour = "SPM 2 with R2 and R3"), linetype='dashed') +
  xlab(expression(beta["sex"])) + ylab("Density") + theme(legend.position = "none")

betaBMISPM2plot <- ggplot() + 
  scale_colour_manual("Models:", values = c("SPM 2 with R1 and R2"="blue", "SPM 2 with R1 and R3"="green", "SPM 2 with R2 and R3"="red")) +
  geom_line(data = as.data.frame(bmi1_SPM2_R12_H23), aes(x = x, y = y, colour = "SPM 2 with R1 and R2" )) +
  geom_line(data = as.data.frame(bmi1_SPM2_R13_H23), aes(x = x, y = y, colour = "SPM 2 with R1 and R3")) +
  #geom_line(data = as.data.frame(bmi1_SPM2_R23_H23), aes(x = x, y = y, colour = "SPM 2 with R2 and R3")) +
  #geom_line(data = as.data.frame(bmi2_SPM2_R12_H23), aes(x = x, y = y, colour = "SPM 2 with R1 and R2"), linetype='dashed') +
  geom_line(data = as.data.frame(bmi2_SPM2_R13_H23), aes(x = x, y = y, colour = "SPM 2 with R1 and R3"), linetype='dashed') +
  geom_line(data = as.data.frame(bmi2_SPM2_R23_H23), aes(x = x, y = y, colour = "SPM 2 with R2 and R3"), linetype='dashed') +
  xlab(expression(beta["BMI"])) + ylab("Density") + theme(legend.position = "none")

betaageSPM2plot <- ggplot() + 
  scale_colour_manual("Models:", values = c("SPM 2 with R1 and R2"="blue", "SPM 2 with R1 and R3"="green", "SPM 2 with R2 and R3"="red")) +
  geom_line(data = as.data.frame(age1_SPM2_R12_H23), aes(x = x, y = y, colour = "SPM 2 with R1 and R2" )) +
  geom_line(data = as.data.frame(age1_SPM2_R13_H23), aes(x = x, y = y, colour = "SPM 2 with R1 and R3")) +
  geom_line(data = as.data.frame(age1_SPM2_R23_H23), aes(x = x, y = y, colour = "SPM 2 with R2 and R3")) +
  geom_line(data = as.data.frame(age2_SPM2_R12_H23), aes(x = x, y = y, colour = "SPM 2 with R1 and R2"), linetype='dashed') +
  xlab(expression(beta["age"])) + ylab("Density") + theme(legend.position = "none")

randomageSPM2plot <- ggplot() + 
  scale_colour_manual("Models:", values = c("SPM 2 with R1 and R2"="blue", "SPM 2 with R1 and R3"="green","SPM 2 with R2 and R3"="red")) +
  geom_line(data = as.data.frame(age2_SPM2_R13_H23), aes(x = ID, y = `0.5quant`, colour = "SPM 2 with R1 and R3"), linetype='dashed') +
  geom_line(data = as.data.frame(age2_SPM2_R13_H23), aes(x = ID, y = `0.025quant`, colour = "SPM 2 with R1 and R3"), linetype='dotted') +
  geom_line(data = as.data.frame(age2_SPM2_R13_H23), aes(x = ID, y = `0.975quant`, colour = "SPM 2 with R1 and R3"), linetype='dotted') +
  geom_line(data = as.data.frame(age2_SPM2_R23_H23), aes(x = ID, y = `0.5quant`, colour = "SPM 2 with R2 and R3"), linetype='dashed') +
  geom_line(data = as.data.frame(age2_SPM2_R23_H23), aes(x = ID, y = `0.025quant`, colour = "SPM 2 with R2 and R3"), linetype='dotted') +
  geom_line(data = as.data.frame(age2_SPM2_R23_H23), aes(x = ID, y = `0.975quant`, colour = "SPM 2 with R2 and R3"), linetype='dotted') +
  xlab("Age") + ylab("Random effect")   + theme(legend.position = "none")

betaSPM2HUNT23ResultPlot <- ggarrange(beta0SPM2plot, betaBPSPM2plot, betasexSPM2plot, betaBMISPM2plot, betaageSPM2plot, randomageSPM2plot,
                                  labels = c("", "", "", "", "", ""), common.legend=TRUE, legend="bottom",
                                  ncol = 2, nrow = 3)
betaSPM2HUNT23ResultPlot

ggsave("betaSPM2resultHUNT23.png", plot = betaSPM2HUNT23ResultPlot, path = "/home/magnwoln/Masteroppgave/Figs/", width = 20, height = 14, units = "cm", bg = "white")


