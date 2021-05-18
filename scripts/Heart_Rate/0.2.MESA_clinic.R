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
MESA5 <- read_dta("data/MESA/MESAe5_FinalLabel_20160520.dta")
MESAev <- read_dta("data/MESA/MESAEvThru2013_20160308.dta")


MESA_clin <- MESA1 %>% 
  dplyr::select(id = idno, SEX = gender1, AGE = age1c, HR = hrtrate1, 
                SBP = sbp1c, DBP = dbp1c, HRX = htnmed1c, FG = glucos1c,
                DM = dm031c, TC = chol1, TG = trig1, LDL =ldl1, HDL = hdl1, BMI = bmi1c, SMK = cig1c, race = race1c, CHOLMED = cholmed1) %>%
  mutate(SEX = ifelse(SEX==1,1,2)) %>%
  mutate_all(as.numeric) %>%
  na.omit()

MESAev <- MESAev %>% 
  mutate(hardcvd = ifelse(strk==1|chf==1|mi==1,1,0),
         chd = ifelse(mi==1|ang==1,1,0),
         chddeath = if_else(dth==1 & dthtype == 1, 1, 0, missing = 0),
         chddeathtime = dthtt) %>%
  transform(cvd_day_dif = pmin(strktt, chftt, mitt, na.rm = T),
            chdtime = pmin(angtt, mitt, na.rm = T))%>%
  dplyr::select(id = idno, hardcvd, cvd_day_dif, dth, dthtime = dthtt, strktime = strktt, chftime = chftt, mitime = mitt, strk, chf, mi, chd, chdtime, chddeath, chddeathtime) %>%
  mutate_all(as.numeric)

MESA_clin <- MESA_clin %>% 
  left_join(MESAev, by = "id") %>%
  mutate(hardcvd_age = AGE + cvd_day_dif/365.25,
         dth_age = AGE + dthtime/365.25,
         strk_age = AGE + strktime/365.25,
         chf_age = AGE + chftime/365.25,
         mi_age = AGE + mitime/365.25,
         chd_age = AGE + chdtime/365.25,
         chddeath_age = AGE + chddeathtime/365.25,
         DM = ifelse(DM > 1, 1, 0),
         SMK = ifelse(SMK == 2, 1, 0))

saveRDS(MESA_clin, "data/MESA_clin_HR.rds")
MESA_clin %>% filter(dth == 1 & dthtime<mitime)

MESA1 <- MESA1 %>% dplyr::select(id = idno, SEX = gender1, AGE = age1c, SBP = sbp1c, DBP = dbp1c, HRX = htnmed1c, HR = hrtrate1, CHOLMED = cholmed1,
                                 race = race1c, DM = dm031c, SMK = cig1c, BMI = bmi1c, TC = chol1, HDL = hdl1, LDL = ldl1, TG = trig1, FG= glucos1c) %>% mutate(TP = 1)
MESA2 <- MESA2 %>% dplyr::select(id = idno, SEX = gender1, AGE = age2c, SBP = sbp2c, DBP = dbp2c, HRX = htnmed2c, HR = hrdina2c,
                                 race = race1c, DM = dm032c, SMK = cig2c, BMI = bmi2c, TC = chol2, HDL = hdl2, LDL = ldl2, TG = trig2, FG= glucos2c) %>% mutate(TP = 2, CHOLMED = NA)
MESA3 <- MESA3 %>% dplyr::select(id = idno, SEX = gender1, AGE = age3c, SBP = sbp3c, DBP = dbp3c, HRX = htnmed3c, HR = hrdina3c, 
                                 race = race1c, DM = dm033c, SMK = cig3c, BMI = bmi3c, TC = chol3, HDL = hdl3, LDL = ldl3, TG = trig3, FG= glucos3c) %>% mutate(TP = 3, CHOLMED = NA)
MESA4 <- MESA4 %>% dplyr::select(id = idno, SEX = gender1, AGE = age4c, SBP = sbp4c, DBP = dbp4c, HRX = htnmed4c, HR = hrdina4c,
                                 race = race1c, DM = dm034c, SMK = cig4c, BMI = bmi4c, TC = chol4, HDL = hdl4, LDL = ldl4, TG = trig4, FG= glucos4c) %>% mutate(TP = 4, CHOLMED = NA)

mesa_cleaned <- rbind(MESA1, MESA2, MESA3, MESA4) %>%
  mutate(SEX = ifelse(SEX==1,1,2),
         SMK = ifelse(SMK == 2, 1, 0),
         DM = ifelse(DM > 1, 1, 0),
         HR = ifelse(HR>200|HR<20,NA,HR)) %>% 
  mutate_all(as.numeric) %>% 
  filter(!is.na(AGE) & !is.na(SEX) & !is.na(SBP) & !is.na(DBP) & !is.na(HRX))
mesa_cleaned$HR %>% summary()
saveRDS(mesa_cleaned, "data/mesa_cleaned_HR.rds")

mesa_cleaned$id %>% unique() %>% length()
mesa_cleaned$SMK %>% as.factor() %>% summary()
mesa_cleaned$race %>% as.factor() %>% summary()






