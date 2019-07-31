library(haven)
library(MASS)
library(reshape2)
library(reshape)
library(dplyr)
library(magrittr)

ARIC1 <- read_dta("data/ARICmaster_050916.dta")
ARIC2 <- read_dta("data/ARIC_main-for-PAR.dta")
ARICevent <- read_dta("data/ARICmaster_032919_w_cvd.dta")

ARIC_clinic <- ARICevent %>% 
  mutate(hardcvd = incvd17,
         cvd_day_dif = ft17cvd) %>%
  dplyr::select(id, AGE = v1age01, SEX = v1_gender, SBP = v1_sbpa21, DBP = v1_sbpa22, DM = v1_diabts03,
                HDL = HDL_V1, TC = TOTCHOL_V1, BMI = v1_bmi01, SMK = v1_cigt01, 
                hardcvd, cvd_day_dif) %>%
  mutate(SEX = ifelse(SEX=="M",1,2),
         hardcvd_age = AGE + cvd_day_dif/365.25,
         DM = ifelse(DM == 1, 1, 0)) %>%
  left_join(ARIC1 %>% dplyr::select(id, race = RACEGRP51), by = "id") %>%
  mutate(race = ifelse(race == "W", 1, ifelse(race == "B", 3, ifelse(race == "A", 2, NA))))

ARIC_htntrt <- ARIC2 %>% dplyr::select(id = id, HRX = v1htnmed) 

ARIC_clin <- ARIC_clinic %>% left_join(ARIC_htntrt, by = "id")

saveRDS(ARIC_clin, "data/ARIC_clin.rds")



ARIC <- read_dta("data/ARIC_main-for-PAR.dta")
ARIC_dat1 <- ARIC %>% 
  dplyr::select(AGE = v1age, SEX = gender, SBP = v1sbp, DBP = v1dbp, HRX = v1htnmed, 
                id = id, SMK = v1cig, DM = v1diabetes, BMI = v1bmi, race = racegrp, TC = v1totchol) %>% 
  mutate(TP = 1)
ARIC_dat2 <- ARIC %>% 
  dplyr::select(AGE = v2age, SEX = gender, SBP = v2sbp, DBP = v2dbp, HRX = v2htnmed, 
                id = id, SMK = v2cig, DM = v2diabetes, BMI = v2bmi, race = racegrp, TC = v2totchol) %>% 
  mutate(TP = 2)
ARIC_dat3 <- ARIC %>% 
  dplyr::select(AGE = v3age, SEX = gender, SBP = v3sbp, DBP = v3dbp, HRX = v3htnmed, 
                id = id, SMK = v3cig, DM = v3diabetes, BMI = v3bmi, race = racegrp, TC = v3totchol) %>% 
  mutate(TP = 3)
ARIC_dat4 <- ARIC %>% 
  dplyr::select(AGE = v4age, SEX = gender, SBP = v4sbp, DBP = v4dbp, HRX = v4htnmed, 
                id = id, SMK = v4cig, DM = v4diabetes, BMI = v4bmi, race = racegrp, TC = v4totchol) %>% 
  mutate(TP = 4) 

ARIC_cleaned <- rbind(ARIC_dat1, ARIC_dat2, ARIC_dat3, ARIC_dat4) %>%
  mutate(SEX = ifelse(SEX=="F",2,1), 
         SMK = ifelse(SMK == 1, 1, 0),
         DM = ifelse(DM == 1, 1, 0),
         race = ifelse(race == "W", 1, ifelse(race == "B", 3, ifelse(race == "A", 2, NA))))

saveRDS(ARIC_cleaned, "data/ARIC_cleaned.rds")
ARIC_cleaned$id %>% unique() %>% length()
