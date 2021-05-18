library(haven)
library(MASS)
library(reshape2)
library(reshape)
library(dplyr)
library(magrittr)

fhsclin4568 <- readRDS("data/fhsclin_ex4568.rds")

fhsclin4568 <- fhsclin4568 %>% 
  mutate(hardcvd1 = ifelse(hardchd1==1|chf1==1|cva1==1,1,0),
         chd1 = ifelse(!is.na(CHDdateSOE),1,0),
         chdtime1 = chd_age - AGE1)

FHS_clin <- fhsclin4568 %>%
  transform(hardcvd_age = pmin(hardchdtime1+AGE1, chftime1+AGE1, cvatime1+AGE1, na.rm = T),
            harcchd_age = hardchdtime1+AGE1) %>%
  dplyr::select(id = ID, AGE = AGE1, SEX = SEX, HR = VENT_RT1,
                SBP = SBP1, DBP = DBP1, HRX = HRX1, CHOLMED =LIPRX1,
                BMI = BMI1, DM = curr_diab1, SMK = CURRSMK1, 
                HDL = HDL1, TC = TC1, TG = TRIG1, LDL = CALC_LDL1, LVEFz = lvef, chfdateSOE,
                hardcvd = hardcvd1, hardcvd_age, hardchd = hardchd1, hardchd_age,
                dth = dth1, dthtime = dthtime1,
                mi = hardchd1, mitime = hardchdtime1,
                chd = chd1, chdtime = chdtime1,
                chf = chf1, chftime = chftime1,
                strk = cva1, strktime = cvatime1, FG = fasting_bg1) %>%
  mutate(race = 1, 
         dth_age = AGE + dthtime,
         strk_age = AGE + strktime,
         chf_age = AGE + chftime,
         chd_age = AGE + chdtime,
         mi_age = AGE + mitime)

saveRDS(FHS_clin, "data/FHS_clin_HR.rds")
FHS_clin %>% filter(dth==1 & dthtime<mitime)

dat1 <- fhsclin4568 %>%
  dplyr::select(AGE = AGE1, SEX = SEX, DM = curr_diab1, SBP = SBP1, DBP = DBP1, HRX = HRX1, HDL = HDL1, HR = VENT_RT1, LVH = DLVH1,
                BMI = BMI1, ID, SMK = CURRSMK1, FG = fasting_bg1, TC = TC1, TG = TRIG1, LDL = CALC_LDL1, CHOLMED = LIPRX1) %>% 
  mutate(WAISTCIRC = NA, HIPCIRC = NA) %>%
  mutate(TP = 1, race = 1)
dat2 <- fhsclin4568 %>%
  dplyr::select(AGE = AGE2, SEX = SEX, DM = curr_diab2, SBP = SBP2, DBP = DBP2, HRX = HRX2, HDL = HDL2, HR = VENT_RT2, LVH = DLVH2,
                BMI = BMI2, ID, SMK = CURRSMK2, FG = fasting_bg2, TC = TC2, TG = TRIG2, LDL = CALC_LDL2, CHOLMED = LIPRX2) %>% 
  mutate(WAISTCIRC = NA, HIPCIRC = NA) %>%
  mutate(TP = 2, race = 1)
dat3 <- fhsclin4568 %>%
  dplyr::select(AGE = AGE3, SEX = SEX, DM = curr_diab3, SBP = SBP3, DBP = DBP3, HRX = HRX3, HDL = HDL3, HR = VENT_RT3, LVH = DLVH3,
                BMI = BMI3, ID, SMK = CURRSMK3, FG = FASTING_BG3, TC = TC3, TG = TRIG3, LDL = CALC_LDL3, CHOLMED = LIPRX3) %>% 
  mutate(WAISTCIRC = NA, HIPCIRC = NA) %>%
  mutate(TP = 3, race = 1)
dat4 <- fhsclin4568 %>%
  dplyr::select(AGE = AGE4, SEX = SEX, DM = curr_diab4, SBP = SBP4, DBP = DBP4, HRX = HRX4, HDL = HDL4, HR = VENT_RT4, LVH = DLVH4,WAISTCIRC = WAIST4, HIPCIRC = HIP4,
                BMI = BMI4, ID, SMK = CURRSMK4, FG = FASTING_BG4, TC = TC4, TG = TRIG4, LDL = CALC_LDL4, CHOLMED = LIPRX4) %>%
  mutate(TP = 4, race = 1)
dat5 <- fhsclin4568 %>%
  dplyr::select(AGE = AGE5, SEX = SEX, DM = curr_diab5, SBP = SBP5, DBP = DBP5, HRX = HRX5, HDL = HDL5, HR = VENT_RT5, LVH = DLVH5,WAISTCIRC = WAIST5, HIPCIRC = HIP5,
                BMI = BMI5, ID, SMK = CURRSMK5, FG = FASTING_BG5, TC = TC5, TG = TRIG5, LDL = CALC_LDL5, CHOLMED = LIPRX5) %>%
  mutate(TP = 5, race = 1)
dat6 <- fhsclin4568 %>%
  dplyr::select(AGE = AGE6, SEX = SEX, DM = curr_diab6, SBP = SBP6, DBP = DBP6, HRX = HRX6, HDL = HDL6, HR = VENT_RT6, LVH = DLVH6,WAISTCIRC = WAIST6, HIPCIRC = HIP6,
                BMI = BMI6, ID, SMK = CURRSMK6, FG = FASTING_BG6, TC = TC6, TG = TRIG6, LDL = CALC_LDL6, CHOLMED = LIPRX6) %>%
  mutate(TP = 6, race = 1)
dat7 <- fhsclin4568 %>%
  dplyr::select(AGE = AGE7, SEX = SEX, DM = curr_diab7, SBP = SBP7, DBP = DBP7, HRX = HRX7, HDL = HDL7, HR = VENT_RT7, LVH = DLVH7,WAISTCIRC = WAIST7, HIPCIRC = HIP7,
                BMI = BMI7, ID, SMK = CURRSMK7, FG = FASTING_BG7, TC = TC7, TG = TRIG7, LDL = CALC_LDL7, CHOLMED = LIPRX7) %>%
  mutate(TP = 7, race = 1)
dat8 <- fhsclin4568 %>%
  dplyr::select(AGE = AGE8, SEX = SEX, DM = curr_diab8, SBP = SBP8, DBP = DBP8, HRX = HRX8, HDL = HDL8, HR = VENT_RT8, LVH = DLVH8, WAISTCIRC = WAIST8,
                BMI = BMI8, ID, SMK = CURRSMK8, FG = FASTING_BG8, TC = TC8, TG = TRIG8, LDL = CALC_LDL8, CHOLMED = LIPRX8) %>% 
  mutate(HIPCIRC = NA) %>%
  mutate(TP = 8, race = 1)
dat9 <- fhsclin4568 %>%
  dplyr::select(AGE = AGE9, SEX = SEX, DM = curr_diab9, SBP = SBP9, DBP = DBP9, HRX = HRX9, HDL = HDL9, HR = VENT_RT9, LVH = DLVH9,WAISTCIRC = WAIST9, HIPCIRC = HIP9,
                BMI = BMI9, ID, SMK = CURRSMK9, FG = FASTING_BG9, TC = TC9, TG = TRIG9, LDL = CALC_LDL9, CHOLMED = LIPRX9) %>%
  mutate(TP = 9, race = 1)

dat <- rbind(dat1, dat2, dat3, dat4, dat5, dat6, dat7, dat8, dat9)

rm(dat1, dat2, dat3, dat4, dat5, dat6, dat7, dat8, dat9)

saveRDS(dat, "data/fhs_cleaned_HR.rds")

dat$ID %>% unique() %>% length()

readRDS("data/fhs_cleaned_HR.rds") %>% filter(TP == 4) %>% dplyr::select(HRX) %>% pull() %>% sum(na.rm = T)

