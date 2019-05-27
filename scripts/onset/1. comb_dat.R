library(dplyr)
library(magrittr)
library(ggplot2)

# fhs

fhsclin4568 <- readRDS("data/fhsclin_ex4568.rds")

dat1 <- fhsclin4568 %>%
  dplyr::select(AGE = AGE1, SEX = SEX, DM = curr_diab1, SBP = SBP1, DBP = DBP1, HRX = HRX1, BMI = BMI1, ID, SMK = CURRSMK1, FG = fasting_bg1) %>%
  mutate(TP = 1)
dat2 <- fhsclin4568 %>%
  dplyr::select(AGE = AGE2, SEX = SEX, DM = curr_diab2, SBP = SBP2, DBP = DBP2, HRX = HRX2, BMI = BMI2, ID, SMK = CURRSMK2, FG = fasting_bg2) %>%
  mutate(TP = 2)
dat3 <- fhsclin4568 %>%
  dplyr::select(AGE = AGE3, SEX = SEX, DM = curr_diab3, SBP = SBP3, DBP = DBP3, HRX = HRX3, BMI = BMI3, ID, SMK = CURRSMK3, FG = FASTING_BG3) %>%
  mutate(TP = 3)
dat4 <- fhsclin4568 %>%
  dplyr::select(AGE = AGE4, SEX = SEX, DM = curr_diab4, SBP = SBP4, DBP = DBP4, HRX = HRX4, BMI = BMI4, ID, SMK = CURRSMK4, FG = FASTING_BG4) %>%
  mutate(TP = 4)
dat5 <- fhsclin4568 %>%
  dplyr::select(AGE = AGE5, SEX = SEX, DM = curr_diab5, SBP = SBP5, DBP = DBP5, HRX = HRX5, BMI = BMI5, ID, SMK = CURRSMK5, FG = FASTING_BG5) %>%
  mutate(TP = 5)
dat6 <- fhsclin4568 %>%
  dplyr::select(AGE = AGE6, SEX = SEX, DM = curr_diab6, SBP = SBP6, DBP = DBP6, HRX = HRX6, BMI = BMI6, ID, SMK = CURRSMK6, FG = FASTING_BG6) %>%
  mutate(TP = 6)
dat7 <- fhsclin4568 %>%
  dplyr::select(AGE = AGE7, SEX = SEX, DM = curr_diab7, SBP = SBP7, DBP = DBP7, HRX = HRX7, BMI = BMI7, ID, SMK = CURRSMK7, FG = FASTING_BG7) %>%
  mutate(TP = 7)
dat8 <- fhsclin4568 %>%
  dplyr::select(AGE = AGE8, SEX = SEX, DM = curr_diab8, SBP = SBP8, DBP = DBP8, HRX = HRX8, BMI = BMI8, ID, SMK = CURRSMK8, FG = FASTING_BG8) %>%
  mutate(TP = 8)
dat9 <- fhsclin4568 %>%
  dplyr::select(AGE = AGE9, SEX = SEX, DM = curr_diab9, SBP = SBP9, DBP = DBP9, HRX = HRX9, BMI = BMI9, ID, SMK = CURRSMK9, FG = FASTING_BG9) %>%
  mutate(TP = 9)

dat <- rbind(dat1, dat2, dat3, dat4, dat5, dat6, dat7, dat8, dat9) 

rm(dat1, dat2, dat3, dat4, dat5, dat6, dat7, dat8, dat9)

fhs <- dat[,c("SBP","DBP","SEX","AGE","HRX","ID","TP")] %>% 
  mutate(id=ID, 
         cohort = "fhs",
         visit = TP,
         TP = paste("fhs",TP,sep = "")) %>% 
  dplyr::select(-ID) %>% na.omit()

rm(dat)

# MESA

library(haven)

MESA_ALT <- read.csv("data/mesa_cleaned.csv")
MESA_ALT <- MESA_ALT[,c("SBP","DBP","SEX","AGE","HRX","id","TP")] %>% 
  mutate(SEX = ifelse(SEX=="F",2,1), 
         cohort = "MESA",
         SBP = SBP + 0.5, 
         DBP = DBP + 2.9,
         visit = TP,
         TP = paste("mesa",TP,sep = "")) %>%
  na.omit() 

# CARDIA

cardia <- read.csv("data/cardia_cleaned.csv")
cardia <- cardia[,c("SBP","DBP","SEX","AGE","HRX","id","TP")] %>% 
  mutate(SEX = ifelse(SEX=="F",2,1), 
         cohort = "cardia",
         SBP = ifelse(TP == 20 | TP == 25, 3.74 + 0.96*SBP, SBP),
         DBP = ifelse(TP == 20 | TP == 25, 1.30 + 0.97*DBP, DBP),
         SBP = SBP + 2.6,
         DBP = DBP + 6.2,
         HRX = HRX -1,
         visit = TP + 1,
         TP = paste("cardia",TP,sep = "")) %>% 
  na.omit()


# ARIC 

ARIC <- read_dta("data/ARIC_main-for-PAR.dta")
ARIC_dat1 <- ARIC %>% dplyr::select(AGE = v1age, SEX = gender, SBP = v1sbp, DBP = v1dbp, HRX = v1htnmed, id = id) %>% 
  mutate(TP = 1)
ARIC_dat2 <- ARIC %>% dplyr::select(AGE = v2age, SEX = gender, SBP = v2sbp, DBP = v2dbp, HRX = v2htnmed, id = id) %>% 
  mutate(TP = 2)
ARIC_dat3 <- ARIC %>% dplyr::select(AGE = v3age, SEX = gender, SBP = v3sbp, DBP = v3dbp, HRX = v3htnmed, id = id) %>% 
  mutate(TP = 3)
ARIC_dat4 <- ARIC %>% dplyr::select(AGE = v4age, SEX = gender, SBP = v4sbp, DBP = v4dbp, HRX = v4htnmed, id = id) %>% 
  mutate(TP = 4) 


rm(ARIC)
ARIC_comb <- rbind(ARIC_dat1, ARIC_dat2, ARIC_dat3, ARIC_dat4) %>% mutate(SEX = ifelse(SEX=="F",2,1), 
                                                                          SBP = SBP + 2.6,
                                                                          DBP = DBP + 6.2,
                                                                          cohort = "ARIC",
                                                                          visit = TP,
                                                                          TP = paste("aric",TP,sep = "")) %>% na.omit()

comb_dat <- rbind(cardia, fhs, MESA_ALT, ARIC_comb) %>% 
  mutate(SBP = ifelse(HRX==1, SBP + 10 , SBP),
         DBP = ifelse(HRX==1, DBP + 5 , DBP),
         PP = SBP - DBP,
         MAP = DBP + 1/3*PP,
         AGE = round(AGE),
         TP = as.factor(TP),
         id = as.factor(id))

saveRDS(comb_dat, "data/comb_dat.rds")
