library(haven)
library(MASS)
library(reshape2)
library(reshape)
library(dplyr)
library(magrittr)

MESA1 <- read_dta("data/MESA/MESAe1FinalLabel02092016.dta")
MESA2 <- read_dta("data/MESA/MESAe2FinalLabel05202016.dta")
MESA3 <- read_dta("data/MESA/MESAe3FinalLabel05202016.dta")
MESA4 <- read_dta("data/MESA/MESAe4FinalLabel05202016.dta")
# MESA5 <- read_dta("data/MESA/MESAe5_FinalLabel_20160520.dta")
MESAev <- read_dta("data/MESA/MESAEvThru2013_20160308.dta")

# "SBP","DBP","SEX","AGE","HRX","MAP","PP","id"

MESA_clin <- MESA1 %>% 
  dplyr::select(id = idno, SEX = gender1, AGE = age1c, 
                SBP = sbp1c, DBP = dbp1c, HRX = htnmed1c, 
                DM = dm031c, TC = chol1, HDL = hdl1, BMI = bmi1c, SMK = cig1c, race = race1c) %>%
  mutate(SEX = ifelse(SEX==1,1,2)) %>%
  mutate_all(as.numeric) %>%
  na.omit()

MESAev <- MESAev %>% 
  mutate(hardcvd = ifelse(strk==1|chf==1|mi==1,1,0))

MESAev <- transform(MESAev, cvd_day_dif = pmin(strktt, chftt, mitt, na.rm = T))%>%
  dplyr::select(id = idno, hardcvd, cvd_day_dif) %>%
  mutate_all(as.numeric)

MESA_clin <- MESA_clin %>% 
  left_join(MESAev, by = "id") %>%
  mutate(hardcvd_age = AGE + cvd_day_dif/365.25,
         DM = ifelse(DM > 1, 1, 0))

saveRDS(MESA_clin, "data/MESA_clin.rds")


MESA1 <- MESA1 %>% dplyr::select(id = idno, SEX = gender1, AGE = age1c, SBP = sbp1c, DBP = dbp1c, HRX = htnmed1c, 
                                 race = race1c, DM = dm031c, SMK = cig1c, BMI = bmi1c, TC = chol1) %>% mutate(TP = 1)
MESA2 <- MESA2 %>% dplyr::select(id = idno, SEX = gender1, AGE = age2c, SBP = sbp2c, DBP = dbp2c, HRX = htnmed2c, 
                                 race = race1c, DM = dm032c, SMK = cig2c, BMI = bmi2c, TC = chol2) %>% mutate(TP = 2)
MESA3 <- MESA3 %>% dplyr::select(id = idno, SEX = gender1, AGE = age3c, SBP = sbp3c, DBP = dbp3c, HRX = htnmed3c, 
                                 race = race1c, DM = dm033c, SMK = cig3c, BMI = bmi3c, TC = chol3) %>% mutate(TP = 3)
MESA4 <- MESA4 %>% dplyr::select(id = idno, SEX = gender1, AGE = age4c, SBP = sbp4c, DBP = dbp4c, HRX = htnmed4c, 
                                 race = race1c, DM = dm034c, SMK = cig4c, BMI = bmi4c, TC = chol4) %>% mutate(TP = 4)
# MESA5 <- MESA5 %>% dplyr::select(id = idno, SEX = gender1, AGE = age5c, SBP = sbp5c, DBP = dbp5c, HRX = htnmed5c) %>% mutate(TP = 5)

mesa_cleaned <- rbind(MESA1, MESA2, MESA3, MESA4) %>%
  mutate(SEX = ifelse(SEX==1,1,2),
         SMK = ifelse(SMK == 2, 1, 0),
         DM = ifelse(DM > 1, 1, 0)) %>% 
  mutate_all(as.numeric) %>% 
  filter(!is.na(AGE) & !is.na(SEX) & !is.na(SBP) & !is.na(DBP) & !is.na(HRX))

saveRDS(mesa_cleaned, "data/mesa_cleaned.rds")

mesa_cleaned$id %>% unique() %>% length()

mesa_cleaned$race %>% as.factor() %>% summary()








