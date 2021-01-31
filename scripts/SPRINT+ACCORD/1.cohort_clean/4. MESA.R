library(haven)
library(MASS)
library(reshape2)
library(reshape)
library(dplyr)
library(magrittr)
library(data.table)

MESA1 <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/MESA_2020a/Primary/Exam1/Data/mesae1dres06192012.csv")
MESA2 <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/MESA_2020a/Primary/Exam2/Data/mesae2dres06222012.csv")
MESA3 <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/MESA_2020a/Primary/Exam3/Data/mesae3dres06222012.csv")
MESA4 <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/MESA_2020a/Primary/Exam4/Data/mesae4dres06222012.csv")
MESA5 <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/MESA_2020a/Primary/Exam5/Data/mesae5_drepos_20151101.csv")
MESAev <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/MESA_2020a/Primary/Events/CVD/Data/mesaevthr2015_drepos_20200330.csv")

MESA_event <- MESAev %>% 
  left_join(MESA1 %>% dplyr::select(MESAID, bsl_AGE = age1c), by = "MESAID") %>%
  mutate(hardcvd = ifelse(STRK==1|CHF==1|MI==1,1,0),
         dthtime = DTHTT,
         chd = ifelse(MI==1|ANG==1,1,0),
         chddeath = if_else(DTH==1 & DTHTYPE == 1, 1, 0, missing = 0),
         chddeathtime = DTHTT) %>%
  transform(cvd_day_dif = pmin(STRKTT, CHFTT, MITT, na.rm = T),
            chdtime = pmin(ANGTT, MITT,RCATT, CBGTT, na.rm = T))%>%
  mutate(nfchd = ifelse((chd==1&chddeath==0)|(chd==1&chddeath==1&dthtime-chdtime>7),1,0),
         nfchdtime = chdtime) %>%
  dplyr::select(id = MESAID, bsl_AGE, hardcvd, cvd_day_dif, dth = DTH, dthtime = DTHTT, strktime = STRKTT, chftime = CHFTT, 
                mitime = MITT, strk = STRK, chf = CHF, mi = MI, chd, chdtime, chddeath, chddeathtime, nfchd, nfchdtime) %>%
  mutate_all(as.numeric) %>% 
  mutate(hardcvd_age = bsl_AGE + cvd_day_dif/365.25,
         dth_age = bsl_AGE + dthtime/365.25,
         strk_age = bsl_AGE + strktime/365.25,
         chf_age = bsl_AGE + chftime/365.25,
         mi_age = bsl_AGE + mitime/365.25,
         chd_age = bsl_AGE + chdtime/365.25,
         chddeath_age = bsl_AGE + chddeathtime/365.25,
         nfchd_age = bsl_AGE + nfchdtime/365.25,
         PID = paste("MESA",id, sep = ""))

MESA_event$chd %>% sum(na.rm = T)
saveRDS(MESA_event, "data/MESA_event.rds")


MESA_visit1 <- MESA1 %>% dplyr::select(id = MESAID, SEX = gender1, AGE = age1c, SBP = sbp1c, DBP = dbp1c, HRX = htnmed1c, HR = hrtrate1, CHOLMED = lipid1c,
                                       race = race1c, DM = dm031c, SMK = cig1c, BMI = bmi1c, TC = chol1, HDL = hdl1, LDL = ldl1, TG = trig1, FG= glucos1c,
                                       PD_STOP_CURR = mnpause1, PD_STOP_AGE = menoage1) %>% mutate(TP = 1)
MESA_visit2 <- MESA2 %>% dplyr::select(id = mesaid, SEX = gender1, AGE = age2c, SBP = sbp2c, DBP = dbp2c, HRX = htnmed2c, HR = hrdina2c, CHOLMED = lipid2c,
                                       race = race1c, DM = dm032c, SMK = cig2c, BMI = bmi2c, TC = chol2, HDL = hdl2, LDL = ldl2, TG = trig2, FG= glucos2c,
                                       PD_STOP_CURR = menper2) %>% mutate(TP = 2, PD_STOP_CURR = ifelse(PD_STOP_CURR==0,1,0), PD_STOP_AGE = NA)
MESA_visit3 <- MESA3 %>% dplyr::select(id = mesaid, SEX = gender1, AGE = age3c, SBP = sbp3c, DBP = dbp3c, HRX = htnmed3c, HR = hrdina3c, CHOLMED = lipid3c,
                                       race = race1c, DM = dm033c, SMK = cig3c, BMI = bmi3c, TC = chol3, HDL = hdl3, LDL = ldl3, TG = trig3, FG= glucos3c,
                                       PD_STOP_CURR = menper3) %>% mutate(TP = 3, PD_STOP_CURR = ifelse(PD_STOP_CURR==0,1,0), PD_STOP_AGE = NA)
MESA_visit4 <- MESA4 %>% dplyr::select(id = mesaid, SEX = gender1, AGE = age4c, SBP = sbp4c, DBP = dbp4c, HRX = htnmed4c, HR = hrdina4c, CHOLMED = lipid4c,
                                       race = race1c, DM = dm034c, SMK = cig4c, BMI = bmi4c, TC = chol4, HDL = hdl4, LDL = ldl4, TG = trig4, FG= glucos4c,
                                       PD_STOP_CURR = menper4) %>% mutate(TP = 4, PD_STOP_CURR = ifelse(PD_STOP_CURR==0,1,0), PD_STOP_AGE = NA)
MESA_visit5 <- MESA5 %>% dplyr::select(id = mesaid, SEX = gender1, AGE = age5c, SBP = sbp5c, DBP = dbp5c, HRX = htnmed5c, HR = hrdina5c, CHOLMED = lipid5c,
                                       race = race1c, DM = dm035c, SMK = cig5c, BMI = bmi5c, TC = chol5, HDL = hdl5, LDL = ldl5, TG = trig5, FG= glucose5,
                                       PD_STOP_CURR = menper5) %>% mutate(TP = 5, PD_STOP_CURR = ifelse(PD_STOP_CURR==0,1,0), PD_STOP_AGE = NA)


MESA_select <- rbind(MESA_visit1, MESA_visit2, MESA_visit3, MESA_visit4, MESA_visit5) %>%
  mutate(SEX = ifelse(SEX==1,1,2),
         SMK = ifelse(SMK == 2, 1, 0),
         DM = ifelse(DM > 1, 1, 0)) %>% 
  mutate_all(as.numeric) 

PD_stop <- MESA_select %>%
  filter(PD_STOP_CURR != 9) %>%
  mutate(PD_STOP_AGE = ifelse(PD_STOP_CURR==1 & is.na(PD_STOP_AGE), AGE, NA)) %>%
  setDT() %>%
  data.table::dcast(id ~ TP, value.var = c("PD_STOP_AGE"))
PD_stop$PD_STOP_AGE <- apply(dplyr::select(PD_stop, -id), 1, FUN = min, na.rm = T)
PD_stop %<>% 
  mutate(PD_STOP_AGE = ifelse(PD_STOP_AGE == Inf, NA, PD_STOP_AGE)) %>%
  dplyr::select(id, PD_STOP_AGE_visit = PD_STOP_AGE)

MESA_select <- MESA_select %>% 
  left_join(PD_stop, by = "id") %>%
  mutate(PD_STOP_AGE = ifelse(is.na(PD_STOP_AGE), PD_STOP_AGE_visit, PD_STOP_AGE),
         PID = paste("MESA",id,sep = ""),
         num = as.numeric(as.factor(id)))
MESA_select$num %>% summary()
saveRDS(MESA_select, "data/MESA_select.rds")






