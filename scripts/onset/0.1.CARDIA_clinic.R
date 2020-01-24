library(haven)
library(MASS)
library(reshape2)
library(reshape)
library(dplyr)
library(magrittr)

cardia_AGE <- read.csv("data/cardia/y00/data/csv/aaf01.csv")
cardia_BP <- read.csv("data/cardia/y00/data/csv/aaf02.csv")
cardia_HDL <- read.csv("data/cardia/y00/data/csv/aalip.csv")
cardia_BMI <- read.csv("data/cardia/y00/data/csv/aaf20.csv")
cardia_smk <- read.csv("data/cardia/y00/data/csv/aaf10.csv")
cardia_dm <- read.csv("data/cardia/y00/data/csv/aaf08v2.csv")
cardia_htntrt <- read.csv("data/cardia/y00/data/csv/aaf09med.csv")
cardia_outcome <- read.csv("data/cardia/OUTCOMES/DATA/csv/outcomes2012.csv")

cardia_AGE %<>% dplyr::select(id = PID, AGE = A01AGE1, SEX = A01SEX, race = A01RACE2)
cardia_BP %<>% dplyr::select(id = PID, SBP = A02SBP, DBP = A02DBP)
cardia_HDL %<>% dplyr::select(id = PID, TC = AL1CHOL, HDL = AL1HDL)
cardia_BMI %<>% dplyr::select(id = PID, BMI = A20BMI)
cardia_smk %<>% dplyr::select(id = PID, SMK = A10SMOKE) %>% mutate(SMK = ifelse(SMK == 1, 1, 0))
cardia_dm %<>% dplyr::select(id = PID, DM = A08DIAB)
cardia_htntrt %<>% dplyr::select(id = PID, HRX = A09MDTYP) %>% dcast(id ~ HRX) %>% dplyr::select(id, HRX = HBP)
cardia_outcome %<>%
  mutate(hardcvd = ifelse(strokeafnf==1|chffnf==1|CHDhfnf==1, 1, 0),
         hardchd = CHDhfnf,
         dth = ifelse(dead == 1,1,0),
         dthtime = deathatt,
         strk = strokeafnf,
         strktime = strokeafnfatt,
         chf = chffnf,
         chftime = chffnfatt,
         mi = CHDhfnf,
         mitime = chdhfnfatt)

cardia_outcome <- transform(cardia_outcome, cvd_day_dif = pmin(strokeafnfatt, chffnfatt, chdhfnfatt, na.rm = T)) %>%
  dplyr::select(id = PID, hardcvd, cvd_day_dif, hf_day_dif = chffnfatt, hardchd, dth, dthtime, strk,strktime,chf,chftime,mi,mitime)

cardia_clin <- cardia_AGE %>% 
  left_join(cardia_BP, by = "id") %>% 
  left_join(cardia_HDL, by = "id") %>% 
  left_join(cardia_BMI, by = "id") %>% 
  left_join(cardia_smk, by = "id") %>% 
  left_join(cardia_dm, by ="id") %>%
  left_join(cardia_htntrt, by = "id") %>%
  left_join(cardia_outcome, by = "id") %>%
  mutate(race = ifelse(race == 4, 3, 1),
         HRX = ifelse(is.na(HRX), 0, HRX),
         DM = ifelse(DM == 2, 1, 0)) %>% 
  mutate(hardcvd_age = AGE + cvd_day_dif/365.25,
         dth_age = AGE + dthtime/365.25,
         strk_age = AGE + strktime/365.25,
         chf_age = AGE + chftime/365.25,
         mi_age = AGE + mitime/365.25)

saveRDS(cardia_clin, "data/cardia_clin.rds")

cardia_BP0 <- read.csv("data/cardia/y00/data/csv/aaf02.csv")
cardia_BP1 <- read.csv("data/cardia/y02/DATA/csv/baf02.csv")
cardia_BP2 <- read.csv("data/cardia/y05/DATA/csv/caf02.csv")
cardia_BP3 <- read.csv("data/cardia/y07/DATA/csv/daf02.csv")
cardia_BP4 <- read.csv("data/cardia/y10/data/csv/eaf02.csv")
cardia_BP5 <- read.csv("data/cardia/y15/DATA/csv/faf02.csv")
cardia_BP6 <- read.csv("data/cardia/y20/data/csv/gaf02.csv")
cardia_BP7 <- read.csv("data/cardia/y25/data/csv/haf02.csv")

cardia_BP0 %<>% dplyr::select(id = PID, SBP = A02SBP, DBP = A02DBP) %>% mutate(TP = 0)
cardia_BP1 %<>% mutate(SBP = (B02SBP1 + B02SBP2 + B02SBP3)/3, DBP = (B02DBP1 + B02DBP2 + B02DBP3)/3) %>% 
  dplyr::select(id = PID, SBP, DBP) %>% mutate(TP = 1)

cardia_BP2 %<>% mutate(SBP = (C02SBP1 + C02SBP2 + C02SBP3)/3, DBP = (C02DBP1 + C02DBP2 + C02DBP3)/3) %>% 
  dplyr::select(id = PID, SBP, DBP) %>% mutate(TP = 2)

cardia_BP3 %<>% mutate(SBP = (D02SBP1 + D02SBP2 + D02SBP3)/3, DBP = (D02DBP1 + D02DBP2 + D02DBP3)/3) %>% 
  dplyr::select(id = PID, SBP, DBP) %>% mutate(TP = 3)

cardia_BP4 %<>% mutate(SBP = (E02SBP1 + E02SBP2 + E02SBP3)/3, DBP = (E02DBP1 + E02DBP2 + E02DBP3)/3) %>% 
  dplyr::select(id = PID, SBP, DBP) %>% mutate(TP = 4)

cardia_BP5 %<>% mutate(SBP = (F02SBP1 + F02SBP2 + F02SBP3)/3, DBP = (F02DBP1 + F02DBP2 + F02DBP3)/3) %>% 
  dplyr::select(id = PID, SBP, DBP) %>% mutate(TP = 5)

cardia_BP6 %<>% mutate(SBP = (G02R1S + G02R2S + G02R3S)/3, DBP = (G02R1D + G02R2D + G02R3D)/3) %>% 
  dplyr::select(id = PID, SBP, DBP) %>% mutate(TP = 6)

cardia_BP7 %<>% mutate(SBP = (H02R1S + H02R2S + H02R3S)/3, DBP = (H02R1D + H02R2D + H02R3D)/3) %>% 
  dplyr::select(id = PID, SBP, DBP) %>% mutate(TP = 7)


cardia_htntrt0 <- read.csv("data/cardia/y00/data/csv/aaf09med.csv") %>% filter(A09MDTYP == "HBP") %>% dplyr::select(id = PID) %>% unique() %>% mutate(HRX = 1)
cardia_htntrt1 <- read.csv("data/cardia/y02/DATA/csv/baf09mhb.csv")%>% dplyr::select(id = PID) %>% unique() %>% mutate(HRX = 1)
cardia_htntrt2 <- read.csv("data/cardia/y05/DATA/csv/caf09mhb.csv")%>% dplyr::select(id = PID) %>% unique() %>% mutate(HRX = 1)
cardia_htntrt3 <- read.csv("data/cardia/y07/DATA/csv/daf09mhb.csv")%>% dplyr::select(id = PID) %>% unique() %>% mutate(HRX = 1)
cardia_htntrt4 <- read.csv("data/cardia/y10/data/csv/eaf09mhb.csv")%>% dplyr::select(id = PID) %>% unique() %>% mutate(HRX = 1)
cardia_htntrt5 <- read.csv("data/cardia/y15/DATA/csv/faf08.csv") %>% filter(F08HBNOW == 2)%>% dplyr::select(id = PID) %>% unique() %>% mutate(HRX = 1)
cardia_htntrt6 <- read.csv("data/cardia/y20/data/csv/gaf08.csv") %>% filter(G08HBNOW == 2)%>% dplyr::select(id = PID) %>% unique() %>% mutate(HRX = 1)
cardia_htntrt7 <- read.csv("data/cardia/y25/data/csv/haf08.csv") %>% filter(H08HBNOW == 2)%>% dplyr::select(id = PID) %>% unique() %>% mutate(HRX = 1)


cardia_AGE0 <- read.csv("data/cardia/y00/data/csv/aaf01.csv") %>% dplyr::select(id = PID, AGE = A01AGE1, SEX = A01SEX, race = A01RACE1)
cardia_AGE1 <- read.csv("data/cardia/y02/DATA/csv/baref.csv") %>% dplyr::select(id = PID, AGE = EX2_AGE, SEX, race = RACE )
cardia_AGE2 <- read.csv("data/cardia/y05/DATA/csv/caref.csv") %>% dplyr::select(id = PID, AGE = EX3_AGE, SEX, race = RACE )
cardia_AGE3 <- read.csv("data/cardia/y07/DATA/csv/daref.csv") %>% dplyr::select(id = PID, AGE = EX4_AGE, SEX, race = RACE )
cardia_AGE4 <- read.csv("data/cardia/y10/data/csv/earef.csv") %>% dplyr::select(id = PID, AGE = EX5_AGE, SEX, race = RACE )
cardia_AGE5 <- read.csv("data/cardia/y15/DATA/csv/faref.csv") %>% dplyr::select(id = PID, AGE = EX6_AGE, SEX, race = RACE )
cardia_AGE6 <- read.csv("data/cardia/y20/data/csv/garef.csv") %>% dplyr::select(id = PID, AGE = EX7_AGE, SEX, race = RACE )
cardia_AGE7 <- read.csv("data/cardia/y25/data/csv/haref.csv") %>% dplyr::select(id = PID, AGE = EX8_AGE, SEX, race = RACE )

cardia_bmi0 <- read.csv("data/cardia/y00/data/csv/aaf20.csv") %>% dplyr::select(id = PID, BMI = A20BMI)
cardia_bmi1 <- read.csv("data/cardia/y02/DATA/csv/baf20.csv") %>% dplyr::select(id = PID, BMI = B20BMI)
cardia_bmi2 <- read.csv("data/cardia/y05/DATA/csv/caf20.csv") %>% dplyr::select(id = PID, BMI = C20BMI)
cardia_bmi3 <- read.csv("data/cardia/y07/DATA/csv/daf20.csv") %>% dplyr::select(id = PID, BMI = D20BMI)
cardia_bmi4 <- read.csv("data/cardia/y10/data/csv/eaf20.csv") %>% dplyr::select(id = PID, BMI = E20BMI)
cardia_bmi5 <- read.csv("data/cardia/y15/DATA/csv/faf20.csv") %>% dplyr::select(id = PID, BMI = F20BMI)
cardia_bmi6 <- read.csv("data/cardia/y20/data/csv/gaf20.csv") %>% dplyr::select(id = PID, BMI = G20BMI)
cardia_bmi7 <- read.csv("data/cardia/y25/data/csv/haf20.csv") %>% dplyr::select(id = PID, BMI = H20BMI)

cardia_dm0 <- read.csv("data/cardia/y00/data/csv/aaf08v2.csv") %>% dplyr::select(id = PID, DM = A08DIAB)
cardia_dm1 <- read.csv("data/cardia/y02/DATA/csv/baf08v2.csv") %>% dplyr::select(id = PID, DM = B08DIAB)
cardia_dm2 <- read.csv("data/cardia/y05/DATA/csv/caf08.csv") %>% dplyr::select(id = PID, DM = C08DIAB)
cardia_dm3 <- read.csv("data/cardia/y07/DATA/csv/daf08.csv") %>% dplyr::select(id = PID, DM = D08DIAB)
cardia_dm4 <- read.csv("data/cardia/y10/data/csv/eaf08.csv") %>% dplyr::select(id = PID, DM = E08DIAB)
cardia_dm5 <- read.csv("data/cardia/y15/DATA/csv/faf08.csv") %>% dplyr::select(id = PID, DM = F08DIAB)
cardia_dm6 <- read.csv("data/cardia/y20/data/csv/gaf08.csv") %>% dplyr::select(id = PID, DM = G08DIAB)
cardia_dm7 <- read.csv("data/cardia/y25/data/csv/haf08.csv") %>% dplyr::select(id = PID, DM = H08DIAB)


cardia_smk0 <- read.csv("data/cardia/y00/data/csv/aaf09tob.csv") %>% dplyr::select(id = PID, SMK = A09SMKNW)
cardia_smk1 <- read.csv("data/cardia/y02/DATA/csv/baf09tob.csv") %>% dplyr::select(id = PID, SMK = B09SMKNW)
cardia_smk2 <- read.csv("data/cardia/y05/DATA/csv/caf09tob.csv") %>% dplyr::select(id = PID, SMK = C09SMKNW)
cardia_smk3 <- read.csv("data/cardia/y07/DATA/csv/daf09tob.csv") %>% dplyr::select(id = PID, SMK = D09SMKNW)
cardia_smk4 <- read.csv("data/cardia/y10/data/csv/eaf09tob.csv") %>% dplyr::select(id = PID, SMK = E09SMKNW)
cardia_smk5 <- read.csv("data/cardia/y15/DATA/csv/faf09tob.csv") %>% dplyr::select(id = PID, SMK = F09SMKNW)
cardia_smk6 <- read.csv("data/cardia/y20/data/csv/gaf09tob.csv") %>% dplyr::select(id = PID, SMK = G09SMKNW)
cardia_smk7 <- read.csv("data/cardia/y25/data/csv/haf09tob.csv") %>% dplyr::select(id = PID, SMK = H09SMKNW)


cardia_tc0 <- read.csv("data/cardia/y00/data/csv/aalip.csv") %>% dplyr::select(id = PID, TC = AL1CHOL, HDL = AL1HDL, TG = AL1NTRIG, LDL = AL1LDL)
# cardia_tc1 <- read.csv("data/cardia/y02/DATA/csv/balip.csv") %>% dplyr::select(id = PID, TC = BL1CHOL) # visit02 absent
cardia_tc2 <- read.csv("data/cardia/y05/DATA/csv/calip.csv") %>% dplyr::select(id = PID, TC = CL1CHOL, HDL = CL1HDL, TG = CL1NTRIG, LDL = CL1LDL)
cardia_tc3 <- read.csv("data/cardia/y07/DATA/csv/dalip.csv") %>% dplyr::select(id = PID, TC = DL1CHOL, HDL = DL1HDL, TG = DL1NTRIG, LDL = DL1LDL)
cardia_tc4 <- read.csv("data/cardia/y10/data/csv/ealip.csv") %>% dplyr::select(id = PID, TC = EL1CHOL, HDL = EL1HDL, TG = EL1NTRIG, LDL = EL1LDL)
cardia_tc5 <- read.csv("data/cardia/y15/DATA/csv/falip.csv") %>% dplyr::select(id = PID, TC = FL1CHOL, HDL = FL1HDL, TG = FL1NTRIG, LDL = FL1LDL)
cardia_tc6 <- read.csv("data/cardia/y20/data/csv/galip.csv") %>% dplyr::select(id = PID, TC = GL1CHOL, HDL = GL1HDL, TG = GL1NTRIG, LDL = GL1LDL)
cardia_tc7 <- read.csv("data/cardia/y25/data/csv/halip.csv") %>% dplyr::select(id = PID, TC = HL1CHOL, HDL = HL1HDL, TG = HL1NTRIG, LDL = HL1LDL)



cardia0 <- cardia_BP0 %>% 
  left_join(cardia_htntrt0, by = "id") %>% 
  mutate(HRX = ifelse(is.na(HRX), 0, HRX)) %>% 
  left_join(cardia_AGE0, by = "id") %>% 
  left_join(cardia_bmi0, by = "id") %>% 
  left_join(cardia_dm0, by = "id") %>% 
  left_join(cardia_smk0, by = "id") %>% 
  left_join(cardia_tc0, by = "id")

cardia1 <- cardia_BP1 %>% 
  left_join(cardia_htntrt1, by = "id") %>% 
  mutate(HRX = ifelse(is.na(HRX), 0, HRX)) %>% 
  left_join(cardia_AGE1, by = "id") %>% 
  left_join(cardia_bmi1, by = "id") %>% 
  left_join(cardia_dm1, by = "id") %>% 
  left_join(cardia_smk1, by = "id") %>% mutate(TC = NA, HDL = NA, TG = NA, LDL = NA)

cardia2 <- cardia_BP2 %>% 
  left_join(cardia_htntrt2, by = "id") %>% 
  mutate(HRX = ifelse(is.na(HRX), 0, HRX)) %>% 
  left_join(cardia_AGE2, by = "id") %>% 
  left_join(cardia_bmi2, by = "id") %>% 
  left_join(cardia_dm2, by = "id") %>% 
  left_join(cardia_smk2, by = "id") %>% 
  left_join(cardia_tc2, by = "id")

cardia3 <- cardia_BP3 %>% 
  left_join(cardia_htntrt3, by = "id") %>% 
  mutate(HRX = ifelse(is.na(HRX), 0, HRX)) %>% 
  left_join(cardia_AGE3, by = "id") %>% 
  left_join(cardia_bmi3, by = "id") %>% 
  left_join(cardia_dm3, by = "id") %>% 
  left_join(cardia_smk3, by = "id") %>% 
  left_join(cardia_tc3, by = "id")

cardia4 <- cardia_BP4 %>% 
  left_join(cardia_htntrt4, by = "id") %>% 
  mutate(HRX = ifelse(is.na(HRX), 0, HRX)) %>% 
  left_join(cardia_AGE4, by = "id") %>% 
  left_join(cardia_bmi4, by = "id") %>% 
  left_join(cardia_dm4, by = "id") %>% 
  left_join(cardia_smk4, by = "id") %>% 
  left_join(cardia_tc4, by = "id")

cardia5 <- cardia_BP5 %>% 
  left_join(cardia_htntrt5, by = "id") %>% 
  mutate(HRX = ifelse(is.na(HRX), 0, HRX)) %>% 
  left_join(cardia_AGE5, by = "id") %>% 
  left_join(cardia_bmi5, by = "id") %>% 
  left_join(cardia_dm5, by = "id") %>% 
  left_join(cardia_smk5, by = "id") %>% 
  left_join(cardia_tc5, by = "id")

cardia6 <- cardia_BP6 %>% 
  left_join(cardia_htntrt6, by = "id") %>% 
  mutate(HRX = ifelse(is.na(HRX), 0, HRX)) %>% 
  left_join(cardia_AGE6, by = "id") %>% 
  left_join(cardia_bmi6, by = "id") %>% 
  left_join(cardia_dm6, by = "id") %>% 
  left_join(cardia_smk6, by = "id") %>% 
  left_join(cardia_tc6, by = "id")

cardia7 <- cardia_BP7 %>% 
  left_join(cardia_htntrt7, by = "id") %>% 
  mutate(HRX = ifelse(is.na(HRX), 0, HRX)) %>% 
  left_join(cardia_AGE7, by = "id") %>% 
  left_join(cardia_bmi7, by = "id") %>% 
  left_join(cardia_dm7, by = "id") %>% 
  left_join(cardia_smk7, by = "id") %>% 
  left_join(cardia_tc7, by = "id")

cardia_clean <- rbind(cardia0, cardia1, cardia2, cardia3, cardia4, cardia5, cardia6, cardia7) %>% 
  mutate(race = ifelse(race==4, 3, 1),
         SMK = if_else(SMK== 1, 1, 0, missing = 0),
         DM = ifelse(DM== 2, 1, 0))
cardia_clean$id %>% unique() %>% length()
cardia_clean$SMK %>% as.factor() %>% summary()
saveRDS(cardia_clean, "data/cardia_cleaned.rds")
