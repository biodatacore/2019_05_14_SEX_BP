library(haven)
library(MASS)
library(reshape2)
library(reshape)
library(dplyr)
library(magrittr)

fhsclin4568 <- readRDS("data/fhsclin_ex4568.rds")

fhsclin4568 <- fhsclin4568 %>% 
  mutate(hardcvd1 = ifelse(hardchd1==1|chf1==1|cva1==1,1,0))

FHS_clin <- transform(fhsclin4568, hardcvd_age = pmin(hardchdtime1+AGE1, chftime1+AGE1, cvatime1+AGE1, na.rm = T)) %>%
  dplyr::select(id = ID, AGE = AGE1, SEX = SEX, 
                SBP = SBP1, DBP = DBP1, HRX = HRX1, 
                BMI = BMI1, DM = curr_diab1, SMK = CURRSMK1, 
                HDL = HDL1, TC = TC1,
                hardcvd = hardcvd1, hardcvd_age) %>%
  mutate(race=1)

saveRDS(FHS_clin, "data/FHS_clin.rds")


dat1 <- fhsclin4568 %>%
  dplyr::select(AGE = AGE1, SEX = SEX, DM = curr_diab1, SBP = SBP1, DBP = DBP1, HRX = HRX1, 
                BMI = BMI1, ID, SMK = CURRSMK1, FG = fasting_bg1, TC = TC1) %>%
  mutate(TP = 1, race = 1)
dat2 <- fhsclin4568 %>%
  dplyr::select(AGE = AGE2, SEX = SEX, DM = curr_diab2, SBP = SBP2, DBP = DBP2, HRX = HRX2, 
                BMI = BMI2, ID, SMK = CURRSMK2, FG = fasting_bg2, TC = TC2) %>%
  mutate(TP = 2, race = 1)
dat3 <- fhsclin4568 %>%
  dplyr::select(AGE = AGE3, SEX = SEX, DM = curr_diab3, SBP = SBP3, DBP = DBP3, HRX = HRX3, 
                BMI = BMI3, ID, SMK = CURRSMK3, FG = FASTING_BG3, TC = TC3) %>%
  mutate(TP = 3, race = 1)
dat4 <- fhsclin4568 %>%
  dplyr::select(AGE = AGE4, SEX = SEX, DM = curr_diab4, SBP = SBP4, DBP = DBP4, HRX = HRX4, 
                BMI = BMI4, ID, SMK = CURRSMK4, FG = FASTING_BG4, TC = TC4) %>%
  mutate(TP = 4, race = 1)
dat5 <- fhsclin4568 %>%
  dplyr::select(AGE = AGE5, SEX = SEX, DM = curr_diab5, SBP = SBP5, DBP = DBP5, HRX = HRX5, 
                BMI = BMI5, ID, SMK = CURRSMK5, FG = FASTING_BG5, TC = TC5) %>%
  mutate(TP = 5, race = 1)
dat6 <- fhsclin4568 %>%
  dplyr::select(AGE = AGE6, SEX = SEX, DM = curr_diab6, SBP = SBP6, DBP = DBP6, HRX = HRX6, 
                BMI = BMI6, ID, SMK = CURRSMK6, FG = FASTING_BG6, TC = TC6) %>%
  mutate(TP = 6, race = 1)
dat7 <- fhsclin4568 %>%
  dplyr::select(AGE = AGE7, SEX = SEX, DM = curr_diab7, SBP = SBP7, DBP = DBP7, HRX = HRX7, 
                BMI = BMI7, ID, SMK = CURRSMK7, FG = FASTING_BG7, TC = TC7) %>%
  mutate(TP = 7, race = 1)
dat8 <- fhsclin4568 %>%
  dplyr::select(AGE = AGE8, SEX = SEX, DM = curr_diab8, SBP = SBP8, DBP = DBP8, HRX = HRX8, 
                BMI = BMI8, ID, SMK = CURRSMK8, FG = FASTING_BG8, TC = TC8) %>%
  mutate(TP = 8, race = 1)
dat9 <- fhsclin4568 %>%
  dplyr::select(AGE = AGE9, SEX = SEX, DM = curr_diab9, SBP = SBP9, DBP = DBP9, HRX = HRX9, 
                BMI = BMI9, ID, SMK = CURRSMK9, FG = FASTING_BG9, TC = TC9) %>%
  mutate(TP = 9, race = 1)

dat <- rbind(dat1, dat2, dat3, dat4, dat5, dat6, dat7, dat8, dat9) 

rm(dat1, dat2, dat3, dat4, dat5, dat6, dat7, dat8, dat9)

saveRDS(dat, "data/fhs_cleaned.rds")

dat$ID %>% unique() %>% length()


