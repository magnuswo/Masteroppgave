library(haven)
library(plyr)
library("data.table")
library(foreign)
library(dplyr)
library(tidyverse)
library("sjmisc")

# Here I want to create my own datasets where I have corrected for blood pressure medicine and removed unnecessary information. 
#data1 = read_sav("/home/magnwoln/Masteroppgave/INLAresults/2021-11-16_111751_Data.sav")
#data_extra = read_sav("/home/magnwoln/Masteroppgave/INLAresults/2021-11-17_111751_Tillegg.sav")

newdata1 <- read_sav("/home/magnwoln/Masteroppgave/INLAresults/2022-04-20_112123_Data.sav")
newdata2 <- read_sav("/home/magnwoln/Masteroppgave/INLAresults/2022-05-06_112123_Data.sav")
newdata3 <- read_sav("/home/magnwoln/Masteroppgave/INLAresults/2022-05-09_112123_Data.sav")
  
newdata_full <- cbind(newdata1, newdata2$ObsEndDat, newdata3$`Bmi@NT4BLM`)
write.csv(newdata_full,"/home/magnwoln/Masteroppgave/INLAresults/data_full.csv", row.names = FALSE)

# Merge data 
#data_full <- merge(data1, data_extra, on = "id")

# Choose certain rows
#data_chosen <- data_full[, -c(5, 6, 8, 10,11,13,22,30,36) ]
newdata_chosen <- newdata_full[, -c(6,7,9,11,12,14,20,30)]
# Change column names for clarity
setnames(newdata_chosen, c(colnames(newdata_chosen)),
                         c("id",
                           "sex",
                           "birth_year",
                           "reg_status",
                           "part_1",
                           "part_2",
                           "part_3",
                           "part_4",
                           "bp_med_ev_3",
                           "age_3",
                           "bp_3",
                           "bmi_3", 
                           "bp_4", 
                           "age_1",
                           "bmi_1",
                           "bp_1",
                           "bp_med_1",
                           "age_2",
                           "bp_med_2",
                           "bp_2",
                           "bmi_2",
                           "bp_med_sideff_3",
                           "bp_med_4",
                           "age_4",
                           "obs_end_date",
                           "bmi_4"
                         ))

setnames(data_chosen, c(colnames(data_chosen)),
          c("id",
           "sex",
           "birth_year",
           "part_1",
           "part_2",
           "part_3",
           "part_4",
           "reg_status",
           "reg_status_date",
           "obs_end_date",
           "bp_med_ev_3",
           "bp_3",
           "bmi_3", 
           "bp_4", 
           "bmi_4",
           "bmi_1",
           "bp_1",
           "bp_med_1",
           "bp_med_2",
           "bp_2",
           "bmi_2",
           "bp_med_sideff_3",
           "bp_med_4",
           "age_4",
           "age_1",
           "age_2",
           "age_3"
         ))

data_chosen <- newdata_chosen
# Adjust for blood pressure medicine by adding 10 if the participants is currently on medication

data_chosen <- as.data.table(data_chosen)
data_chosen[, bp_1_corr:= bp_1]
data_chosen[, bp_2_corr:= bp_2]
data_chosen[, bp_3_corr:= bp_3]
data_chosen[, bp_4_corr:= bp_4]
data_chosen[bp_med_1 == 1, bp_1_corr := bp_1 + 10]
data_chosen[bp_med_2 == 1, bp_2_corr := bp_2 + 10]
data_chosen[!is.na(bp_med_sideff_3), bp_3_corr := bp_3 + 10] #Workaround due to lack in questionnaire
data_chosen[bp_med_4 ==1, bp_4_corr := bp_4 + 10]

data_chosen[10,] #random check

# Sort the different missing processes, R1 - deceased, R2 - moved out of Trøndelag, R3 - invited, but didn't show and Rtotal - all of the previous

# Check for dead or moved out of Trøndelag
start_hunt2 <- as.Date("1995-08-18") #This is just a guess, ask Emma
end_hunt2 <- as.Date("1997-07-18") 
start_hunt3 <- as.Date("2006-03-10")
end_hunt3 <- as.Date("2008-06-25")
start_hunt4 <- as.Date("2017-01-09")
end_hunt4 <- as.Date("2019-08-09")
# Create R1-deceased
data_chosen[, R1_2 := 0]
data_chosen[, R1_3 := 0]
data_chosen[, R1_4 := 0]
data_chosen[reg_status == 5 & obs_end_date < end_hunt2 & is.na(bp_2_corr), R1_2 := 1] 
data_chosen[reg_status == 5 & obs_end_date < end_hunt3 & is.na(bp_3_corr), R1_3 := 1]
data_chosen[reg_status == 5 & obs_end_date < end_hunt4 & is.na(bp_4_corr), R1_4 := 1] # This also includes the ones who died before HUNT3

# Create R2-moved out
data_chosen[, R2_2 := 0]
data_chosen[, R2_3 := 0]
data_chosen[, R2_4 := 0]
data_chosen[reg_status %in% c(1,2,3) & !is.na(obs_end_date) & obs_end_date< end_hunt2 & is.na(bp_2_corr), R2_2 := 1] # There is only 4?? with the date I suggested
data_chosen[reg_status %in% c(1,2,3) & !is.na(obs_end_date) & obs_end_date< end_hunt3 & is.na(bp_3_corr), R2_3 := 1]
data_chosen[reg_status %in% c(1,2,3) & !is.na(obs_end_date) & obs_end_date< end_hunt4 & is.na(bp_4_corr), R2_4 := 1]

# Create R3-invited, but no show
data_chosen[, R3_2 := 0]
data_chosen[, R3_3 := 0]
data_chosen[, R3_4 := 0]
data_chosen[part_2 == 0 &  R1_2 == 0 & R2_2 == 0, R3_2 := 1] # Think this works:)
data_chosen[part_3 == 0 &  R1_3 == 0 & R2_3 == 0, R3_3 := 1]
data_chosen[part_4 == 0 &  R1_4 == 0 & R2_4 == 0, R3_4 := 1]


# Create Rtotal
data_chosen[, Rtotal_2 := 0]
data_chosen[, Rtotal_3 := 0]
data_chosen[, Rtotal_4 := 0]
data_chosen[is.na(data_chosen$bp_2_corr), Rtotal_2 := 1] # Think this works:)
data_chosen[is.na(data_chosen$bp_3_corr), Rtotal_3 := 1]
data_chosen[is.na(data_chosen$bp_4_corr), Rtotal_4 := 1]

HUNT12 <- subset(data_chosen, select = c("age_1", "age_2", "sex", "bmi_1", "bp_1_corr", "bp_2_corr", "Rtotal_2", "R1_2", "R2_2", "R3_2"))     

HUNT23 <- subset(data_chosen, select = c("age_2", "age_3", "sex", "bmi_2", "bp_2_corr", "bp_3_corr", "Rtotal_3", "R1_3", "R2_3", "R3_3")) 

HUNT34 <- subset(data_chosen, select = c("age_3", "age_4", "sex", "bmi_3", "bp_3_corr", "bp_4_corr", "Rtotal_4", "R1_4", "R2_4", "R3_4")) 

# Prepare the data set for INLA by renaming to general names and removing NA in the first Blood Pressure measurement
rename_data <- function(data){
  data_renamed <- setnames(data,
                           c(colnames(data)
                           ),
                           c("age_1", 
                             "age_2",
                             "sex", 
                             "bmi_1", 
                             "bp_1_corr", 
                             "bp_2_corr", 
                             "Rtotal", 
                             "R1", 
                             "R2",
                             "R3"
                           ))
  return(data_renamed)
}

# Scale the data
scale_data <- function(data){
  mu = mean(HUNT23$bp_1_corr) #mean of the second blood pressure measurement in HUNT in total
  sigma = 19.8847
  data$bp_1_corr <- (data$bp_1_corr - mu)/sigma
  data$bp_2_corr <- (data$bp_2_corr - mu)/sigma
  data_scaled <- data %>% mutate_at(c("age_1", "age_2",
                                      "bmi_1" 
  ), ~(scale(.) %>% as.vector))
  return(data_scaled)
}

set_na_in_missing <- function(data){
  data$R3[data$R1 == 1 | data$R2 == 1] <- NA # Think this works:)
  data$R2[data$R1 == 1 | data$R3 == 1] <- NA
  data$R1[data$R2 == 1 | data$R3 == 1] <- NA
  return(data)
}

# Removing the rows with NA in first blood pressure measurement
HUNT12 <- HUNT12 %>% drop_na(age_1, bmi_1, sex, bp_1_corr) 
HUNT23 <- HUNT23 %>% drop_na(age_2, bmi_2, sex, bp_2_corr) 
HUNT34 <- HUNT34 %>% drop_na(age_3, bmi_3, sex, bp_3_corr) 

# Rename
HUNT12 <- rename_data(HUNT12) 
HUNT23 <- rename_data(HUNT23) 
HUNT34 <- rename_data(HUNT34) 

# Set NA for the other missing processes. 
HUNT12 <- set_na_in_missing(HUNT12)
HUNT23 <- set_na_in_missing(HUNT23)
HUNT34 <- set_na_in_missing(HUNT34)

#write.csv(HUNT12,"/home/magnwoln/Masteroppgave/INLAresults/HUNT12.csv", row.names = FALSE)
#write.csv(HUNT23,"/home/magnwoln/Masteroppgave/INLAresults/HUNT23.csv", row.names = FALSE)
#write.csv(HUNT34,"/home/magnwoln/Masteroppgave/INLAresults/HUNT34.csv", row.names = FALSE)

# Scale
HUNT12_INLA <- scale_data(HUNT12) 
HUNT23_INLA <- scale_data(HUNT23) 
HUNT34_INLA <- scale_data(HUNT34) 

#write.csv(HUNT12_INLA,"/home/magnwoln/Masteroppgave/INLAresults/HUNT12_INLA.csv", row.names = FALSE)
#write.csv(HUNT23_INLA,"/home/magnwoln/Masteroppgave/INLAresults/HUNT23_INLA.csv", row.names = FALSE)
#write.csv(HUNT34_INLA,"/home/magnwoln/Masteroppgave/INLAresultsHUNT34_INLA.csv", row.names = FALSE)
