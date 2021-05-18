library(haven)
library(MASS)
library(reshape2)
library(reshape)
library(dplyr)
library(magrittr)
ARIC1$v5date51 %>% max(na.rm = T)
ARIC1 <- read_dta("data/ARICmaster_050916.dta")
ARIC2 <- read_dta("data/ARIC_main-for-PAR.dta")
ARICevent <- read_dta("data/ARICmaster_032919_w_cvd.dta")
ariclabels <- label(ARIC1) %>% as.data.frame()
ariceventlabels <- label(ARICevent) %>% as.data.frame()

ARIC_clinic <- ARICevent %>%
  transform(strkdate = pmin(C7_ED17ISC, C7_ED17HEM, na.rm = T),
            strkdate = pmin(C7_ED17ISC, C7_ED17HEM, na.rm = T)) %>% 
  mutate(hardcvd = incvd17,
         cvd_day_dif = ft17cvd,
         hardchd = C7_IN_17S,
         chd = ifelse(C7_MI17==1, 1,0),
         strk = ifelse(C7_IN17HEM==1 | C7_IN17ISC==1, 1,0),
         chddeath = C7_FATCHD17)%>%
  dplyr::select(id, AGE = v1age01, SEX = v1_gender, SBP = v1_sbpa21, DBP = v1_sbpa22, DM = v1_diabts03, HR = v1_ecgma31, FG= GLUC_V1, CHOLMED = v1_cholmdcode01,
                HDL = HDL_V1, TC = TOTCHOL_V1, LDL = LDL_V1, TG = TGS_V1, BMI = v1_bmi01, SMK = v1_cigt01, LVEFz = lvef_cur, chfdiag, mi=C7_IN_17S, chf = C7_INCHF17,strkdate,strk,
                hardcvd, cvd_day_dif, dth = DEAD17, dthdate = C7_ENDDATED_BWH, chd, v1date01,C7_DATEISP, C7_DATE_INCHF17, C7_DATEMI, chddeath) %>%
  mutate(SEX = ifelse(SEX=="M",1,2),
         hardcvd_age = AGE + cvd_day_dif/365.25,
         dthtime = dthdate - as.numeric(v1date01),
         strktime = as.numeric(strkdate) - as.numeric(v1date01),
         chdtime = as.numeric(C7_DATEISP) - as.numeric(v1date01),
         chftime = as.numeric(C7_DATE_INCHF17) - as.numeric(v1date01),
         mitime = as.numeric(C7_DATEMI) - as.numeric(v1date01),
         dth_age = AGE + dthtime/365.25,
         strk_age = AGE + strktime/365.25,
         chf_age = AGE + chftime/365.25,
         chd_age = AGE + chdtime/365.25,
         mi_age = AGE + mitime/365.25,
         SMK = ifelse(SMK == 1, 1, 0),
         DM = ifelse(DM == 1, 1, 0)) %>%
  left_join(ARIC1 %>% dplyr::select(id, race = RACEGRP51), by = "id") %>%
  mutate(race = ifelse(race == "W", 1, ifelse(race == "B", 3, ifelse(race == "A", 2, NA))))

ARIC_htntrt <- ARIC2 %>% dplyr::select(id = id, HRX = v1htnmed) 

ARIC_clin <- ARIC_clinic %>% left_join(ARIC_htntrt, by = "id")

saveRDS(ARIC_clin, "data/ARIC_clin_HR.rds")
ARIC_clinic %>% filter(dth == 1 & dthtime<mitime)


ARIC <- read_dta("data/ARIC_main-for-PAR.dta") 

ARIC <- ARIC %>% left_join(ARICevent %>% dplyr::select(id, GLUC_V1,GLUC_V2,GLUC_V3,GLUC_V4), by = "id")
ARIC_dat1 <- ARIC %>% 
  dplyr::select(AGE = v1age, SEX = gender, SBP = v1sbp, DBP = v1dbp, HRX = v1htnmed, TG = v1trig, LDL=v1ldl, FG = GLUC_V1,HR = v1ecghr,
                id = id, SMK = v1cig, DM = v1diabetes, BMI = v1bmi, race = racegrp, TC = v1totchol, HDL = v1hdl, CHOLMED = v1cholmed) %>% 
  mutate(TP = 1)
ARIC_dat2 <- ARIC %>% 
  dplyr::select(AGE = v2age, SEX = gender, SBP = v2sbp, DBP = v2dbp, HRX = v2htnmed, TG = v2trig, LDL=v2ldl, FG = GLUC_V2,HR = v2ecghr,
                id = id, SMK = v2cig, DM = v2diabetes, BMI = v2bmi, race = racegrp, TC = v2totchol, HDL = v2hdl, CHOLMED = v2cholmed) %>% 
  mutate(TP = 2)
ARIC_dat3 <- ARIC %>% 
  dplyr::select(AGE = v3age, SEX = gender, SBP = v3sbp, DBP = v3dbp, HRX = v3htnmed, TG = v3trig, LDL=v3ldl, FG = GLUC_V3,HR = v3ecghr,
                id = id, SMK = v3cig, DM = v3diabetes, BMI = v3bmi, race = racegrp, TC = v3totchol, HDL = v3hdl, CHOLMED = v3cholmed) %>% 
  mutate(TP = 3)
ARIC_dat4 <- ARIC %>% 
  dplyr::select(AGE = v4age, SEX = gender, SBP = v4sbp, DBP = v4dbp, HRX = v4htnmed, TG = v4trig, LDL=v4ldl, FG = GLUC_V4,HR = v4ecghr,
                id = id, SMK = v4cig, DM = v4diabetes, BMI = v4bmi, race = racegrp, TC = v4totchol, HDL = v4hdl, CHOLMED = v4cholmed) %>% 
  mutate(TP = 4) 

ARIC_cleaned <- rbind(ARIC_dat1, ARIC_dat2, ARIC_dat3, ARIC_dat4) %>%
  mutate(SEX = ifelse(SEX=="F",2,1), 
         SMK = ifelse(SMK == 1, 1, 0),
         DM = ifelse(DM == 1, 1, 0),
         race = ifelse(race == "W", 1, ifelse(race == "B", 3, ifelse(race == "A", 2, NA))),
         HR = ifelse(HR>200|HR<20,NA,HR))

saveRDS(ARIC_cleaned, "data/ARIC_cleaned_HR.rds")
ARIC_cleaned$id %>% unique() %>% length()
ARIC_cleaned$SMK %>% as.factor() %>% summary()
ARIC_cleaned$HR %>% summary()

