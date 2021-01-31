
library(haven)
library(MASS)
library(reshape2)
library(reshape)
library(dplyr)
library(magrittr)

read_chs_bsl1 <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/CHS_2019a/BASELINE/base1final.csv")
read_chs_bsl2 <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/CHS_2019a/BASELINE/base2final.csv")
read_chs_event <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/CHS_2019a/EVENTS/events.csv")

read_chs_yr3 <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/CHS_2019a/YEAR3/yr3final.csv")
read_chs_yr4 <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/CHS_2019a/YEAR4/yr4final.csv")
read_chs_yr5.1 <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/CHS_2019a/YEAR5/yr5newfinal.csv")
read_chs_yr5.2 <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/CHS_2019a/YEAR5/yr5oldfinal.csv")
read_chs_yr6 <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/CHS_2019a/YEAR6/yr6final.csv")
read_chs_yr7 <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/CHS_2019a/YEAR7/yr7final.csv")
read_chs_yr8 <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/CHS_2019a/YEAR8/yr8final.csv")
read_chs_yr9 <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/CHS_2019a/YEAR9/yr9final.csv")
read_chs_yr10 <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/CHS_2019a/YEAR10/yr10final.csv")

CHS_DATA1 <- read_chs_bsl1 %>%
  dplyr::select(IDNO, AGE = AGE2, race = RACE01, SEX = GEND01, SBP1 = SYSC114, DBP1 = DIAC114, SBP2 = SYSC214, DBP2 = DIAC214, DMRX = KBLKR06, CHOLMED = LIPID06, HRX = HTNMED06, HR = BEAT14, 
                BW = WEIGHT, BH = STHT, BMI, SMK = SMK3008, DM = DIABADA, ESTROGEN = ESTRGN06, PD_STOP_AGE = MENOPS08, LIVE_BIRTH = LIVBTH, WAISTCIRC = WAIST, HIPCIRC = HIP) %>% 
  left_join(read_chs_bsl2 %>% dplyr::select(IDNO = idno, LVM = NEWLVM43, FG = GLU44, TC = CHOL44, TG = TRIG44, HDL = HDL44, LDL = LDL44)) %>%
  mutate(VISIT = 1, AGE = AGE*2-1+65, DM = ifelse(DM==1,0,1)) %>%
  rowwise() %>%
  mutate(SBP = mean(c(SBP1,SBP2),na.rm = T),
         DBP = mean(c(DBP1,DBP2),na.rm = T),
         PID = paste("CHS",IDNO,sep = ""),
         SEX = 2-SEX,
         race = ifelse(race==1,1,3))

CHS_DATA3 <- read_chs_yr3 %>%
  dplyr::select(IDNO, AGE = AGE2, SBP1 = SYSC114, DBP1 = DIAC114, SBP2 = SYSC214, DBP2 = DIAC214, DMRX = KBLKR06, CHOLMED = LIPID06, HRX = HTNMED06, HR = BEAT14, 
                BW = WEIGHT, SMK = SMK3038, DM = DIABET37, ESTROGEN = ESTRGN06, LVH = ECGLVH) %>% 
  mutate(BH = NA, PD_STOP_AGE = NA, LIVE_BIRTH = NA, WAISTCIRC = NA, HIPCIRC = NA) %>%
  mutate(VISIT = 3, AGE = AGE*2-1+65)

CHS_DATA4 <- read_chs_yr4 %>%
  dplyr::select(IDNO, AGE = AGE2, SBP1 = STSYS114, DBP1 = STDIA114, SBP2 = STSYS214, DBP2 = STDIA214, DMRX = KBLKR06, CHOLMED = LIPID06, HRX = HTNMED06, HR = BEAT14, 
                BW = WEIGHT, BH = HEIGHT27, SMK = SMK3039, DM = DIABET39, ESTROGEN = ESTRGN06) %>% 
  mutate(PD_STOP_AGE = NA, LIVE_BIRTH = NA, WAISTCIRC = NA, HIPCIRC = NA) %>%
  mutate(VISIT = 4, AGE = AGE*2+65)

CHS_DATA5 <- read_chs_yr5.2 %>% dplyr::select(intersect(colnames(read_chs_yr5.2), colnames(read_chs_yr5.1)), SMK = SMK3029) %>% mutate(AGE = AGE2*2+1+65) %>% 
  rbind(read_chs_yr5.1 %>% dplyr::select(intersect(colnames(read_chs_yr5.2), colnames(read_chs_yr5.1)), SMK = SMK3058) %>% mutate(AGE = AGE2*2-1+65)) %>%
  dplyr::select(IDNO, AGE, SBP1 = STSYS114, DBP1 = STDIA114, SBP2 = STSYS214, DBP2 = STDIA214, DMRX = KBLKR06, CHOLMED = LIPID06, HRX = HTNMED06, HR = BEAT14, 
                BW = WEIGHT, BH = HEIGHT27, SMK = SMK, DM = DIABADA, ESTROGEN = ESTRGN06, WAISTCIRC = WAIST, HIPCIRC = HIP,
                TC = CHOL44, LDL = LDL44, HDL = HDL44, TG = TRIG44, LVH = ECGLVH, FG = GLU44) %>% 
  mutate(PD_STOP_AGE = NA, LIVE_BIRTH = NA) %>%
  mutate(VISIT = 5)

CHS_DATA6 <- read_chs_yr6 %>%
  dplyr::select(IDNO, AGE = AGE2, SBP1 = STSYS114, DBP1 = STDIA114, SBP2 = STSYS214, DBP2 = STDIA214, DMRX = KBLKR06, CHOLMED = LIPID06, HRX = HTNMED06, HR = BEAT14, 
                BW = WEIGHT, BH = HEIGHT27, SMK = SMK3059, DM = DIABET59, ESTROGEN = ESTRGN06,
                TC = CHOL44, LVH = ECGLVH) %>% 
  mutate(PD_STOP_AGE = NA, LIVE_BIRTH = NA, WAISTCIRC = NA, HIPCIRC = NA, LDL = NA, HDL = NA, TG = NA, FG = NA) %>%
  mutate(VISIT = 6, AGE = AGE*2-1+65)

CHS_DATA7 <- read_chs_yr7 %>%
  dplyr::select(IDNO, AGE = AGE2, SBP1 = STSYS114, DBP1 = STDIA114, SBP2 = STSYS214, DBP2 = STDIA214, DMRX = KBLKR06, CHOLMED = LIPID06, HRX = HTNMED06, HR = BEAT14, 
                SMK = SMK3059, DM = DIABET59, ESTROGEN = ESTRGN06,
                TC = CHOL44, LVH = ECGLVH) %>% 
  mutate(PD_STOP_AGE = NA, LIVE_BIRTH = NA, WAISTCIRC = NA, HIPCIRC = NA, BW = NA, BH = NA, LDL = NA, HDL = NA, TG = NA, FG = NA) %>%
  mutate(VISIT = 7, AGE = AGE*2+65)

CHS_DATA9 <- read_chs_yr9 %>%
  dplyr::select(IDNO, AGE = AGE2, SBP1 = STSYS114, DBP1 = STDIA114, SBP2 = STSYS214, DBP2 = STDIA214, DMRX = KBLKR06, CHOLMED = LIPID06, HRX = HTNMED06, HR = BEAT14, 
                SMK = SMK3059, DM = DIABET59, ESTROGEN = ESTRGN06,
                TC = CHOLADJ, LVH = ECGLVH, FG = GLUADJ) %>% 
  mutate(PD_STOP_AGE = NA, LIVE_BIRTH = NA, WAISTCIRC = NA, HIPCIRC = NA, BW = NA, BH = NA, LDL = NA, HDL = NA, TG = NA) %>%
  mutate(VISIT = 9, AGE = AGE*2+65+3)

CHS_DATA10 <- read_chs_yr10 %>%
  dplyr::select(IDNO, AGE = AGE2, SBP1 = STSYS114, DBP1 = STDIA114, SBP2 = STSYS214, DBP2 = STDIA214, DMRX = KBLKR06, CHOLMED = LIPID06, HRX = HTNMED06, HR = BEAT14, 
                BW = WEIGHT, BH = HEIGHT27, SMK = SMK3059, DM = DIABET59, ESTROGEN = ESTRGN06,
                TC = CHOL44, LVH = ECGLVH) %>% 
  mutate(PD_STOP_AGE = NA, LIVE_BIRTH = NA, WAISTCIRC = NA, HIPCIRC = NA, LDL = NA, HDL = NA, TG = NA, FG = NA) %>%
  mutate(VISIT = 10, AGE = AGE*2+65+4)

saveRDS(CHS_DATA1,"data/CHS_select.rds")

event <- read_chs_event %>%
  filter(EVTYPE!=0) %>%
  arrange(TTOEVENT) %>%
  group_by(IDNO, EVTYPE, FATAL) %>%
  dplyr::summarise(TTOEVENT = ifelse(is.na(TTOEVENT), CENSTIME, first(TTOEVENT))) %>%
  unique()

censortime <- read_chs_event %>%
  dplyr::select(CENSTIME, IDNO) %>%
  unique()

chd_event <- event %>%
  filter(EVTYPE %in% c(1,10,11)) %>%
  arrange(TTOEVENT) %>%
  group_by(IDNO) %>%
  dplyr::summarise(chdtime = first(TTOEVENT))

nfchd_event <- event %>%
  filter(EVTYPE %in% c(1,10) & FATAL==0) %>%
  arrange(TTOEVENT) %>%
  group_by(IDNO) %>%
  dplyr::summarise(nfchdtime = first(TTOEVENT))

chddeath_event <- event %>%
  filter(EVTYPE %in% c(1,10,11) & FATAL==1) %>%
  arrange(TTOEVENT) %>%
  group_by(IDNO) %>%
  dplyr::summarise(chddeathtime = first(TTOEVENT))

chs_event <- CHS_DATA1 %>%
  left_join(censortime, by = "IDNO") %>%
  left_join(chd_event, by = "IDNO") %>% 
  left_join(nfchd_event, by = "IDNO") %>% 
  left_join(chddeath_event, by = "IDNO") %>% 
  mutate(chd = ifelse(is.na(chdtime),0,1),
         chdtime = ifelse(is.na(chdtime),CENSTIME,chdtime),
         nfchd = ifelse(is.na(nfchdtime),0,1),
         nfchdtime = ifelse(is.na(nfchdtime),CENSTIME,nfchdtime),
         chddeath = ifelse(is.na(chddeathtime),0,1),
         chddeathtime = ifelse(is.na(chddeathtime),CENSTIME,chddeathtime),
         chd_age = AGE + chdtime/365.25,
         nfchd_age = AGE + nfchdtime/365.25,
         chddeath_age = AGE + chddeathtime/365.25)

saveRDS(chs_event,"data/CHS_event.rds")








