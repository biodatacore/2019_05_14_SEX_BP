library(haven)
library(MASS)
library(reshape2)
library(reshape)
library(dplyr)
library(magrittr)

cardia_outcome <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/CARDIA_2019a/OUTCOMES/DATA/csv/outcomes2016.csv")
cardia_AGE <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/CARDIA_2019a/Y00/DATA/csv/aaf01.csv") %>% dplyr::select(PID, bsl_AGE = A01AGE1)

CARDIA_event <- cardia_outcome %>%
  left_join(cardia_AGE, by = "PID") %>%
  mutate(PID = paste("CARDIA",PID, sep = "")) %>%
  mutate(hardcvd = ifelse(strokeafnf==1|chffnf==1|CHDhfnf==1, 1, 0),
         hardchd = CHDhfnf,
         chddeath = ifelse(CHDafnf == 1 & mi == 0 & acs == 0, 1, 0),
         dth = ifelse(DEAD == 1,1,0),
         dthtime = DEATHATT,
         strk = strokeafnf,
         strktime = STROKEAFNFATT,
         chf = chffnf,
         chftime = CHFFNFATT,
         mi = CHDhfnf,
         mitime = CHDHFNFATT,
         chd = ifelse(mi==1|acs==1,1,0)) %>%
  transform(chdtime = pmin(MIATT, ACSATT, na.rm = T)) %>%
  transform(cvd_day_dif = pmin(strktime, chftime, mitime, na.rm = T)) %>%
  mutate(nfchd = ifelse((chd==1&chddeath==0)|(chd==1&chddeath==1&dthtime-chdtime>7),1,0),
         nfchdtime = chdtime) %>%
  dplyr::select(PID, bsl_AGE, cvd_day_dif, hardcvd, hardchd, dth, dthtime, strk,strktime,chf,chftime,mi,mitime, chd, chdtime, chddeath, nfchdtime, nfchd) %>%
  mutate(hardcvd_age = bsl_AGE + cvd_day_dif/365.25,
         dth_age = bsl_AGE + dthtime/365.25,
         strk_age = bsl_AGE + strktime/365.25,
         chf_age = bsl_AGE + chftime/365.25,
         mi_age = bsl_AGE + mitime/365.25,
         chd_age = bsl_AGE + chdtime/365.25,
         chddeath_age = bsl_AGE + dthtime/365.25,
         nfchd_age = bsl_AGE + chdtime/365.25,
         id = PID)


saveRDS(CARDIA_event, "data/CARDIA_event.rds")



cardia_HR0 <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/CARDIA_2019a/Y00/DATA/csv/aaf22.csv") %>% dplyr::select(id = PID, HR = A22HRPRE)
# cardia_HR1 <- read.csv("data/cardia/y02/DATA/csv/baf22.csv")
cardia_HR2 <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/CARDIA_2019a/Y05/DATA/csv/caf40.csv") %>% dplyr::select(id = PID, HR = C40HR)
cardia_HR3 <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/CARDIA_2019a/Y07/DATA/csv/daf22.csv") %>% dplyr::select(id = PID, HR = D22SUPIN)
cardia_HR4 <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/CARDIA_2019a/Y10/DATA/csv/eaf40.csv") %>% dplyr::select(id = PID, HR = E40HR)
# cardia_HR5 <- read.csv("data/cardia/y15/DATA/csv/faf02.csv")
cardia_HR6 <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/CARDIA_2019a/Y20/DATA/csv/gaf22.csv") %>% dplyr::select(id = PID, HR = G22SUPIN)
cardia_HR7 <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/CARDIA_2019a/Y25/DATA/csv/haf76.csv") %>% dplyr::select(id = PID, HR = H76CHR)
cardia_HR8 <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/CARDIA_2019a/Y30/DATA/csv/iaecho.csv") %>% dplyr::select(id = PID, HR = IHR)

cardia_med0 <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/CARDIA_2019a/Y00/DATA/csv/aaf08v2.csv")
cardia_med1 <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/CARDIA_2019a/Y02/DATA/csv/baf08v2.csv")
cardia_med2 <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/CARDIA_2019a/Y05/DATA/csv/caf08.csv")
cardia_med3 <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/CARDIA_2019a/Y07/DATA/csv/daf08.csv")
cardia_med4 <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/CARDIA_2019a/Y10/DATA/csv/eaf08.csv")
cardia_med5 <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/CARDIA_2019a/Y15/DATA/csv/faf08.csv")
cardia_med6 <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/CARDIA_2019a/Y20/DATA/csv/gaf08.csv")
cardia_med7 <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/CARDIA_2019a/Y25/DATA/csv/haf08.csv")
cardia_med8 <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/CARDIA_2019a/Y30/DATA/csv/iaf08.csv")

cardia_med0 %<>% mutate(CHOLMED = NA, HRX = A08BPMED, HORMONE = A08HORMN, CONTCPT = A08BRTHC) %>%
  dplyr::select(id = PID, CHOLMED, HRX, HORMONE, CONTCPT)
cardia_med1 %<>% mutate(CHOLMED = NA, HRX = B08BPMED, HORMONE = B08HORMN, CONTCPT = B08BRTHC) %>%
  dplyr::select(id = PID, CHOLMED, HRX, HORMONE, CONTCPT)
cardia_med2 %<>% mutate(CHOLMED = ifelse(C08CHNOW == 2,1,0), HRX = C08HBNOW, HORMONE = C08HMNOW, CONTCPT = C08BCNOW) %>% 
  dplyr::select(id = PID, CHOLMED, HRX, HORMONE, CONTCPT)
cardia_med3 %<>% mutate(CHOLMED = ifelse(D08CHNOW == 2,1,0), HRX = D08HBNOW, HORMONE = D08HMNOW, CONTCPT = D08BCNOW) %>% 
  dplyr::select(id = PID, CHOLMED, HRX, HORMONE, CONTCPT)
cardia_med4 %<>% mutate(CHOLMED = ifelse(E08CHNOW == 2,1,0), HRX = E08HBNOW, HORMONE = E08HMNOW, CONTCPT = E08BRTHC) %>% 
  dplyr::select(id = PID, CHOLMED, HRX, HORMONE, CONTCPT)
cardia_med5 %<>% mutate(CHOLMED = ifelse(F08CHNOW == 2,1,0), HRX = F08HBNOW, HORMONE = F08HMNOW, CONTCPT = F08BRTHC) %>% 
  dplyr::select(id = PID, CHOLMED, HRX, HORMONE, CONTCPT)
cardia_med6 %<>% mutate(CHOLMED = ifelse(G08CHNOW == 2,1,0), HRX = G08HBNOW, HORMONE = G08HMNOW, CONTCPT = G08BRTHC) %>% 
  dplyr::select(id = PID, CHOLMED, HRX, HORMONE, CONTCPT)
cardia_med7 %<>% mutate(CHOLMED = ifelse(H08CHNOW == 2,1,0), HRX = H08HBNOW, HORMONE = H08HMNOW, CONTCPT = H08BCNOW) %>% 
  dplyr::select(id = PID, CHOLMED, HRX, HORMONE, CONTCPT) 
cardia_med8 %<>% mutate(CHOLMED = ifelse(I08CHNOW == 2,1,0), HRX = I08HBNOW, HORMONE = I08HMNOW, CONTCPT = I08BCNOW) %>% 
  dplyr::select(id = PID, CHOLMED, HRX, HORMONE, CONTCPT) 

cardia_BP0 <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/CARDIA_2019a/Y00/DATA/csv/aaf02.csv")
cardia_BP1 <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/CARDIA_2019a/Y02/DATA/csv/baf02.csv")
cardia_BP2 <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/CARDIA_2019a/Y05/DATA/csv/caf02.csv")
cardia_BP3 <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/CARDIA_2019a/Y07/DATA/csv/daf02.csv")
cardia_BP4 <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/CARDIA_2019a/Y10/DATA/csv/eaf02.csv")
cardia_BP5 <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/CARDIA_2019a/Y15/DATA/csv/faf02.csv")
cardia_BP6 <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/CARDIA_2019a/Y20/DATA/csv/gaf02.csv")
cardia_BP7 <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/CARDIA_2019a/Y25/DATA/csv/haf02.csv")
cardia_BP8 <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/CARDIA_2019a/Y30/DATA/csv/iaf02.csv")

cardia_BP0 %<>% dplyr::select(id = PID, SBP = A02SBP, DBP = A02DBP) %>% mutate(TP = 0)
cardia_BP1 %<>% 
  rowwise() %>%
  mutate(SBP = mean(c(B02SBP2, B02SBP3),na.rm = T), 
         DBP = mean(c(B02DBP2, B02DBP3),na.rm = T)) %>% 
  dplyr::select(id = PID, SBP, DBP) %>% mutate(TP = 1)
cardia_BP2 %<>%   
  rowwise() %>%
  mutate(SBP = mean(c(C02SBP2, C02SBP3),na.rm = T), 
         DBP = mean(c(C02DBP2, C02DBP3),na.rm = T)) %>% 
  dplyr::select(id = PID, SBP, DBP) %>% mutate(TP = 2)
cardia_BP3 %<>%   
  rowwise() %>%
  mutate(SBP = mean(c(D02SBP2, D02SBP3),na.rm = T), 
         DBP = mean(c(D02DBP2, D02DBP3),na.rm = T)) %>% 
  dplyr::select(id = PID, SBP, DBP) %>% mutate(TP = 3)
cardia_BP4 %<>%   
  rowwise() %>%
  mutate(SBP = mean(c(E02SBP2, E02SBP3),na.rm = T), 
         DBP = mean(c(E02DBP2, E02DBP3),na.rm = T)) %>% 
  dplyr::select(id = PID, SBP, DBP) %>% mutate(TP = 4)
cardia_BP5 %<>%   
  rowwise() %>%
  mutate(SBP = mean(c(F02SBP2, F02SBP3),na.rm = T), 
         DBP = mean(c(F02DBP2, F02DBP3),na.rm = T)) %>% 
  dplyr::select(id = PID, SBP, DBP) %>% mutate(TP = 5)
cardia_BP6 %<>%   
  rowwise() %>%
  mutate(SBP = mean(c(G02R2S, G02R3S),na.rm = T), 
         DBP = mean(c(G02R2D, G02R3D),na.rm = T)) %>% 
  dplyr::select(id = PID, SBP, DBP) %>% mutate(TP = 6)
cardia_BP7 %<>%   
  rowwise() %>%
  mutate(SBP = mean(c(H02R2S, H02R3S),na.rm = T), 
         DBP = mean(c(H02R2D, H02R3D),na.rm = T)) %>% 
  dplyr::select(id = PID, SBP, DBP) %>% mutate(TP = 7)
cardia_BP8 %<>%   
  rowwise() %>%
  mutate(SBP = mean(c(I02R2S, I02R3S),na.rm = T), 
         DBP = mean(c(I02R2D, I02R3D),na.rm = T)) %>% 
  dplyr::select(id = PID, SBP, DBP) %>% mutate(TP = 8)


cardia_AGE0 <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/CARDIA_2019a/Y00/DATA/csv/aaf01.csv") %>% dplyr::select(id = PID, AGE = A01AGE1, SEX = A01SEX, race = A01RACE1)
cardia_AGE1 <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/CARDIA_2019a/Y02/DATA/csv/baref.csv") %>% dplyr::select(id = PID, AGE = EX2_AGE, SEX, race = RACE )
cardia_AGE2 <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/CARDIA_2019a/Y05/DATA/csv/caref.csv") %>% dplyr::select(id = PID, AGE = EX3_AGE, SEX, race = RACE )
cardia_AGE3 <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/CARDIA_2019a/Y07/DATA/csv/daref.csv") %>% dplyr::select(id = PID, AGE = EX4_AGE, SEX, race = RACE )
cardia_AGE4 <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/CARDIA_2019a/Y10/DATA/csv/earef.csv") %>% dplyr::select(id = PID, AGE = EX5_AGE, SEX, race = RACE )
cardia_AGE5 <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/CARDIA_2019a/Y15/DATA/csv/faref.csv") %>% dplyr::select(id = PID, AGE = EX6_AGE, SEX, race = RACE )
cardia_AGE6 <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/CARDIA_2019a/Y20/DATA/csv/garef.csv") %>% dplyr::select(id = PID, AGE = EX7_AGE, SEX, race = RACE )
cardia_AGE7 <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/CARDIA_2019a/Y25/DATA/csv/haref.csv") %>% dplyr::select(id = PID, AGE = EX8_AGE, SEX, race = RACE )
cardia_AGE8 <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/CARDIA_2019a/Y30/DATA/csv/iaref.csv") %>% dplyr::select(id = PID, AGE = EX9_AGE, SEX, race = RACE )

cardia_PD5 <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/CARDIA_2019a/Y15/DATA/csv/faf68.csv") %>% dplyr::select(id = PID, PD_STOP_AGE = F68STPAG)
cardia_PD6 <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/CARDIA_2019a/Y20/DATA/csv/gaf68.csv") %>% dplyr::select(id = PID, PD_STOP_AGE = G68STPAG)
cardia_PD7 <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/CARDIA_2019a/Y25/DATA/csv/haf68.csv") %>% dplyr::select(id = PID, PD_STOP_AGE = H68STPAG)
cardia_PD8 <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/CARDIA_2019a/Y30/DATA/csv/iaf68.csv") %>% dplyr::select(id = PID, PD_STOP_AGE = I68STPAG)

cardia_bmi0 <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/CARDIA_2019a/Y00/DATA/csv/aaf20.csv") %>% dplyr::select(id = PID, BMI = A20BMI)
cardia_bmi1 <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/CARDIA_2019a/Y02/DATA/csv/baf20.csv") %>% dplyr::select(id = PID, BMI = B20BMI)
cardia_bmi2 <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/CARDIA_2019a/Y05/DATA/csv/caf20.csv") %>% dplyr::select(id = PID, BMI = C20BMI)
cardia_bmi3 <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/CARDIA_2019a/Y07/DATA/csv/daf20.csv") %>% dplyr::select(id = PID, BMI = D20BMI)
cardia_bmi4 <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/CARDIA_2019a/Y10/DATA/csv/eaf20.csv") %>% dplyr::select(id = PID, BMI = E20BMI)
cardia_bmi5 <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/CARDIA_2019a/Y15/DATA/csv/faf20.csv") %>% dplyr::select(id = PID, BMI = F20BMI)
cardia_bmi6 <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/CARDIA_2019a/Y20/DATA/csv/gaf20.csv") %>% dplyr::select(id = PID, BMI = G20BMI)
cardia_bmi7 <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/CARDIA_2019a/Y25/DATA/csv/haf20.csv") %>% dplyr::select(id = PID, BMI = H20BMI)
cardia_bmi8 <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/CARDIA_2019a/Y30/DATA/csv/iaf20.csv") %>% dplyr::select(id = PID, BMI = I20BMI)

cardia_dm0 <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/CARDIA_2019a/Y00/DATA/csv/aaf08v2.csv") %>% dplyr::select(id = PID, DM = A08DIAB)
cardia_dm1 <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/CARDIA_2019a/Y02/DATA/csv/baf08v2.csv") %>% dplyr::select(id = PID, DM = B08DIAB)
cardia_dm2 <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/CARDIA_2019a/Y05/DATA/csv/caf08.csv") %>% dplyr::select(id = PID, DM = C08DIAB)
cardia_dm3 <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/CARDIA_2019a/Y07/DATA/csv/daf08.csv") %>% dplyr::select(id = PID, DM = D08DIAB)
cardia_dm4 <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/CARDIA_2019a/Y10/DATA/csv/eaf08.csv") %>% dplyr::select(id = PID, DM = E08DIAB)
cardia_dm5 <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/CARDIA_2019a/Y15/DATA/csv/faf08.csv") %>% dplyr::select(id = PID, DM = F08DIAB)
cardia_dm6 <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/CARDIA_2019a/Y20/DATA/csv/gaf08.csv") %>% dplyr::select(id = PID, DM = G08DIAB)
cardia_dm7 <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/CARDIA_2019a/Y25/DATA/csv/haf08.csv") %>% dplyr::select(id = PID, DM = H08DIAB)
cardia_dm8 <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/CARDIA_2019a/Y30/DATA/csv/iaf08.csv") %>% dplyr::select(id = PID, DM = I08DIAB)

cardia_gluc0 <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/CARDIA_2019a/Y00/DATA/csv/aachem.csv") %>% dplyr::select(id = PID, FG = AL3_GLU)
# cardia_gluc1 <- read.csv("data/cardia/y02/DATA/csv/baf08v2.csv") %>% dplyr::select(id = PID, FG = )
# cardia_gluc2 <- read.csv("data/cardia/y05/DATA/csv/caf08.csv") %>% dplyr::select(id = PID, FG = )
cardia_gluc3 <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/CARDIA_2019a/Y07/DATA/csv/daglu.csv") %>% dplyr::select(id = PID, FG = DL7GLU)
cardia_gluc4 <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/CARDIA_2019a/Y10/DATA/csv/eaglu.csv") %>% dplyr::select(id = PID, FG = EL7GLU)
cardia_gluc5 <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/CARDIA_2019a/Y15/DATA/csv/faglu.csv") %>% dplyr::select(id = PID, FG = FL7GLU)
cardia_gluc6 <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/CARDIA_2019a/Y20/DATA/csv/gaglu.csv") %>% dplyr::select(id = PID, FG = GL7GLU)
cardia_gluc7 <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/CARDIA_2019a/Y25/DATA/csv/haglu.csv") %>% dplyr::select(id = PID, FG = HL7GLU)
cardia_gluc8 <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/CARDIA_2019a/Y30/DATA/csv/iaglu.csv") %>% dplyr::select(id = PID, FG = IL7GLU)


cardia_smk0 <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/CARDIA_2019a/Y00/DATA/csv/aaf09tob.csv") %>% dplyr::select(id = PID, SMK = A09SMKNW)
cardia_smk1 <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/CARDIA_2019a/Y02/DATA/csv/baf09tob.csv") %>% dplyr::select(id = PID, SMK = B09SMKNW)
cardia_smk2 <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/CARDIA_2019a/Y05/DATA/csv/caf09tob.csv") %>% dplyr::select(id = PID, SMK = C09SMKNW)
cardia_smk3 <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/CARDIA_2019a/Y07/DATA/csv/daf09tob.csv") %>% dplyr::select(id = PID, SMK = D09SMKNW)
cardia_smk4 <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/CARDIA_2019a/Y10/DATA/csv/eaf09tob.csv") %>% dplyr::select(id = PID, SMK = E09SMKNW)
cardia_smk5 <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/CARDIA_2019a/Y15/DATA/csv/faf09tob.csv") %>% dplyr::select(id = PID, SMK = F09SMKNW)
cardia_smk6 <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/CARDIA_2019a/Y20/DATA/csv/gaf09tob.csv") %>% dplyr::select(id = PID, SMK = G09SMKNW)
cardia_smk7 <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/CARDIA_2019a/Y25/DATA/csv/haf09tob.csv") %>% dplyr::select(id = PID, SMK = H09SMKNW)
cardia_smk8 <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/CARDIA_2019a/Y30/DATA/csv/iaf09tob.csv") %>% dplyr::select(id = PID, SMK = I09SMKNW)


cardia_tc0 <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/CARDIA_2019a/Y00/DATA/csv/aalip.csv") %>% dplyr::select(id = PID, TC = AL1CHOL, HDL = AL1HDL, TG = AL1NTRIG, LDL = AL1LDL)
# cardia_tc1 <- read.csv("data/cardia/y02/DATA/csv/balip.csv") %>% dplyr::select(id = PID, TC = BL1CHOL) # visit02 absent
cardia_tc2 <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/CARDIA_2019a/Y05/DATA/csv/calip.csv") %>% dplyr::select(id = PID, TC = CL1CHOL, HDL = CL1HDL, TG = CL1NTRIG, LDL = CL1LDL)
cardia_tc3 <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/CARDIA_2019a/Y07/DATA/csv/dalip.csv") %>% dplyr::select(id = PID, TC = DL1CHOL, HDL = DL1HDL, TG = DL1NTRIG, LDL = DL1LDL)
cardia_tc4 <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/CARDIA_2019a/Y10/DATA/csv/ealip.csv") %>% dplyr::select(id = PID, TC = EL1CHOL, HDL = EL1HDL, TG = EL1NTRIG, LDL = EL1LDL)
cardia_tc5 <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/CARDIA_2019a/Y15/DATA/csv/falip.csv") %>% dplyr::select(id = PID, TC = FL1CHOL, HDL = FL1HDL, TG = FL1NTRIG, LDL = FL1LDL)
cardia_tc6 <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/CARDIA_2019a/Y20/DATA/csv/galip.csv") %>% dplyr::select(id = PID, TC = GL1CHOL, HDL = GL1HDL, TG = GL1NTRIG, LDL = GL1LDL)
cardia_tc7 <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/CARDIA_2019a/Y25/DATA/csv/halip.csv") %>% dplyr::select(id = PID, TC = HL1CHOL, HDL = HL1HDL, TG = HL1NTRIG, LDL = HL1LDL)
cardia_tc8 <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/CARDIA_2019a/Y30/DATA/csv/ialip.csv") %>% dplyr::select(id = PID, TC = IL1CHOL, HDL = IL1HDL, TG = IL1NTRIG, LDL = IL1LDL)



cardia0 <- cardia_BP0 %>% 
  left_join(cardia_HR0, by = "id") %>% 
  left_join(cardia_AGE0, by = "id") %>% 
  left_join(cardia_bmi0, by = "id") %>% 
  left_join(cardia_gluc0, by = "id") %>% 
  left_join(cardia_dm0, by = "id") %>% 
  left_join(cardia_med0, by = "id") %>% 
  left_join(cardia_smk0, by = "id") %>% 
  left_join(cardia_tc0, by = "id") %>% mutate(PD_STOP_AGE = NA, VISIT = 1)

cardia1 <- cardia_BP1 %>%
  left_join(cardia_AGE1, by = "id") %>%
  left_join(cardia_bmi1, by = "id") %>%
  left_join(cardia_med1, by = "id") %>% 
  left_join(cardia_dm1, by = "id") %>%
  left_join(cardia_smk1, by = "id") %>% mutate(TC = NA, HDL = NA, TG = NA, LDL = NA, HR = NA, FG = NA, PD_STOP_AGE = NA, VISIT = 2)

cardia2 <- cardia_BP2 %>% 
  left_join(cardia_HR2, by = "id") %>% 
  left_join(cardia_AGE2, by = "id") %>% 
  left_join(cardia_bmi2, by = "id") %>% 
  left_join(cardia_dm2, by = "id") %>% 
  left_join(cardia_smk2, by = "id") %>% 
  left_join(cardia_med2, by = "id") %>% 
  left_join(cardia_tc2, by = "id") %>% mutate(FG = NA, PD_STOP_AGE = NA, VISIT = 3)

cardia3 <- cardia_BP3 %>% 
  left_join(cardia_HR3, by = "id") %>% 
  left_join(cardia_AGE3, by = "id") %>% 
  left_join(cardia_gluc3, by = "id") %>% 
  left_join(cardia_bmi3, by = "id") %>% 
  left_join(cardia_dm3, by = "id") %>% 
  left_join(cardia_smk3, by = "id") %>% 
  left_join(cardia_med3, by = "id") %>% 
  left_join(cardia_tc3, by = "id") %>% mutate(PD_STOP_AGE = NA, VISIT = 4)

cardia4 <- cardia_BP4 %>% 
  left_join(cardia_HR4, by = "id") %>% 
  left_join(cardia_AGE4, by = "id") %>% 
  left_join(cardia_gluc4, by = "id") %>% 
  left_join(cardia_bmi4, by = "id") %>% 
  left_join(cardia_dm4, by = "id") %>% 
  left_join(cardia_smk4, by = "id") %>% 
  left_join(cardia_med4, by = "id") %>% 
  left_join(cardia_tc4, by = "id") %>% mutate(PD_STOP_AGE = NA, VISIT = 5)

cardia5 <- cardia_BP5 %>%
  left_join(cardia_AGE5, by = "id") %>%
  left_join(cardia_gluc5, by = "id") %>% 
  left_join(cardia_bmi5, by = "id") %>%
  left_join(cardia_dm5, by = "id") %>%
  left_join(cardia_PD5, by = "id") %>%
  left_join(cardia_smk5, by = "id") %>%
  left_join(cardia_med5, by = "id") %>% 
  left_join(cardia_tc5, by = "id") %>% mutate(HR = NA, VISIT = 6)

cardia6 <- cardia_BP6 %>% 
  left_join(cardia_HR6, by = "id") %>% 
  left_join(cardia_AGE6, by = "id") %>% 
  left_join(cardia_gluc6, by = "id") %>% 
  left_join(cardia_bmi6, by = "id") %>% 
  left_join(cardia_dm6, by = "id") %>% 
  left_join(cardia_PD6, by = "id") %>%
  left_join(cardia_smk6, by = "id") %>% 
  left_join(cardia_med6, by = "id") %>% 
  left_join(cardia_tc6, by = "id") %>% mutate(HR = NA, VISIT = 7)

cardia7 <- cardia_BP7 %>% 
  left_join(cardia_HR7, by = "id") %>% 
  left_join(cardia_AGE7, by = "id") %>% 
  left_join(cardia_gluc7, by = "id") %>% 
  left_join(cardia_bmi7, by = "id") %>% 
  left_join(cardia_dm7, by = "id") %>% 
  left_join(cardia_PD7, by = "id") %>%
  left_join(cardia_smk7, by = "id") %>% 
  left_join(cardia_med7, by = "id") %>% 
  left_join(cardia_tc7, by = "id") %>% mutate(VISIT = 8)

cardia8 <- cardia_BP8 %>% 
  left_join(cardia_HR8, by = "id") %>% 
  left_join(cardia_AGE8, by = "id") %>% 
  left_join(cardia_gluc8, by = "id") %>% 
  left_join(cardia_bmi8, by = "id") %>% 
  left_join(cardia_dm8, by = "id") %>% 
  left_join(cardia_PD8, by = "id") %>%
  left_join(cardia_smk8, by = "id") %>% 
  left_join(cardia_med8, by = "id") %>% 
  left_join(cardia_tc8, by = "id") %>% mutate(VISIT = 9)

CARDIA_select <- rbind(cardia0, cardia1, cardia2, cardia3, cardia4, cardia5, cardia6, cardia7, cardia8) %>% 
  mutate(race = ifelse(race==4, 3, 1),
         SMK = if_else(SMK== 1, 1, 0, missing = 0),
         DM = ifelse(DM== 2, 1, 0),
         HRX = ifelse(HRX==1,0,1),
         HR = ifelse(HR>200|HR<20,NA,HR),
         PID = paste("CARDIA",id, sep = ""),
         num = as.numeric(as.factor(id)))
CARDIA_select$num %>% summary()
plot(CARDIA_select$TP, CARDIA_select$PD_STOP_AGE)

CARDIA_select$id %>% unique() %>% length()
CARDIA_select$HR %>% summary()
CARDIA_select$SBP %>% summary()
CARDIA_select$SMK %>% as.factor() %>% summary()
CARDIA_select$HRX %>% as.factor() %>% summary()
CARDIA_select$CHOLMED %>% as.factor() %>% summary()
saveRDS(CARDIA_select, "data/CARDIA_select.rds")

table(CARDIA_select$VISIT,CARDIA_select$HRX)




