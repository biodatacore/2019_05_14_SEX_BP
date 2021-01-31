
library(haven)
library(MASS)
library(reshape2)
library(reshape)
library(dplyr)
library(magrittr)
library(rms)


aric_v1 <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/ARIC_2020b_TIZJ9uH/Main_Study/v1/csv/derive13.csv")
aric_v2 <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/ARIC_2020b_TIZJ9uH/Main_Study/v2/csv/derive2_10.csv")
aric_v3 <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/ARIC_2020b_TIZJ9uH/Main_Study/v3/csv/derive37.csv")
aric_v4 <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/ARIC_2020b_TIZJ9uH/Main_Study/v4/csv/derive47.csv")
aric_v5 <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/ARIC_2020b_TIZJ9uH/Main_Study/v5/csv/derive51.csv")

aric_v1_sbp <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/ARIC_2020b_TIZJ9uH/Main_Study/v1/csv/sbpa02.csv")
aric_v1_hr <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/ARIC_2020b_TIZJ9uH/Main_Study/v1/csv/erha.csv")
aric_v1_pd <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/ARIC_2020b_TIZJ9uH/Main_Study/v1/csv/rhxa.csv")

aric_v2_sbp <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/ARIC_2020b_TIZJ9uH/Main_Study/v2/csv/sbpb02.csv")
aric_v2_hr <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/ARIC_2020b_TIZJ9uH/Main_Study/v2/csv/ecgc.csv")
# aric_v2_pd <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/ARIC_2020b_TIZJ9uH/Main_Study/v1/csv/rhxa.csv") # V2 没有生殖系统问卷

aric_v3_sbp <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/ARIC_2020b_TIZJ9uH/Main_Study/v3/csv/sbpc04_02.csv")
aric_v3_hr <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/ARIC_2020b_TIZJ9uH/Main_Study/v3/csv/ecgd04.csv")
aric_v3_pd <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/ARIC_2020b_TIZJ9uH/Main_Study/v3/csv/rhxb04.csv")

aric_v4_sbp <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/ARIC_2020b_TIZJ9uH/Main_Study/v4/csv/sbpd04_02.csv")
aric_v4_hr <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/ARIC_2020b_TIZJ9uH/Main_Study/v4/csv/ecge04.csv")
aric_v4_pd <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/ARIC_2020b_TIZJ9uH/Main_Study/v4/csv/rhxc04.csv")

aric_v5_sbp <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/ARIC_2020b_TIZJ9uH/Main_Study/v5/csv/sbp.csv")
aric_v5_hr <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/ARIC_2020b_TIZJ9uH/Main_Study/v5/csv/ecg.csv")
# aric_v5_pd <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/ARIC_2020b_TIZJ9uH/Main_Study/v5/csv/rhxc04.csv") # V5 没有生殖系统问卷

surv <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/ARIC_2020b_TIZJ9uH/Main_Study/cohort_incident/csv/incps16.csv")

aric_v1$GENDER %>% as.factor() %>% summary()

aric_select_v1 <- aric_v1 %>%
  mutate(ID_C = as.character(ID_C)) %>%
  left_join(aric_v1_sbp %>% dplyr::select(-forprofit) %>% mutate(ID_C = as.character(ID_C)), by = "ID_C") %>%
  left_join(aric_v1_hr %>% dplyr::select(-forprofit) %>% mutate(ID_C = as.character(ID_C)), by = "ID_C") %>%
  left_join(aric_v1_pd %>% dplyr::select(-forprofit) %>% mutate(ID_C = as.character(ID_C)), by = "ID_C") %>%
  dplyr::select(ID_C, AGE = V1AGE01, SEX = GENDER, RACE = RACEGRP, SBP = SBPA21, DBP = SBPA22,
                DM = DIABTS03, HRX = HYPTMDCODE01, HR = ERHA21, LVH = CLVH01, CHOLMED = CHOLMDCODE01, ESTROGEN = RHXA16, WHR = WSTHPR01,
                BMI = BMI01, SMK = CURSMK01, FG = GLUCOS01, TC = TCHSIU01, HDL = HDL01, LDL = LDL02, TG = TRGSIU01, INSULIN= INSSIU01,
                PD_START_AGE = RHXA01, PD_STOP_CURR = RHXA04, PD_STOP_CAUSE = RHXA09, LIVE_BIRTH = RHXA03, CONTCPT = RHXA11) %>% 
  mutate(PD_STOP_CURR = ifelse(PD_STOP_CURR == "N", 1, 0),
         PD_STOP_AGE = NA,
         CHOLMED = ifelse(CHOLMED==1,1,0),
         LVH = ifelse(LVH == 1,1,0),
         VISIT = 1)


aric_select_v2 <- aric_v2 %>%
  mutate(ID_C = as.character(ID_C)) %>%
  left_join(aric_v2_sbp %>% dplyr::select(-forprofit) %>% mutate(ID_C = as.character(ID_C)), by = "ID_C") %>%
  left_join(aric_v2_hr %>% dplyr::select(-forprofit) %>% mutate(ID_C = as.character(ID_C)), by = "ID_C") %>%
  dplyr::select(ID_C, AGE = V2AGE22, SEX = GENDER, RACE = RACEGRP, SBP = SBPB21, DBP = SBPB22,
                DM = DIABTS23, HRX = HYPTMDCODE21, HR = ECGC31, LVH = CLVH21, CHOLMED = CHOLMDCODE21, WHR = WSTHPR21, ESTROGEN = HORMON22,
                BMI = BMI21, SMK = CURSMK21, FG = GLUSIU21, TC = TCHSIU21, HDL = HDL221, LDL = LDL22, TG = TRGSIU21) %>% 
  mutate(PD_START_AGE = NA, PD_STOP_AGE = NA, PD_STOP_CURR = NA, PD_STOP_CAUSE = NA, LIVE_BIRTH = NA, CONTCPT = NA, INSULIN= NA,
         CHOLMED = ifelse(CHOLMED==1,1,0),
         LVH = ifelse(LVH == 1,1,0),
         VISIT = 2)


aric_select_v3 <- aric_v3 %>%
  mutate(ID_C = as.character(ID_C)) %>%
  left_join(aric_v1 %>% dplyr::select(GENDER, ID_C, RACEGRP) %>% mutate(ID_C = as.character(ID_C)), by = "ID_C") %>%
  left_join(aric_v3_sbp %>% dplyr::select(-forprofit) %>% mutate(ID_C = as.character(ID_C)), by = "ID_C") %>%
  left_join(aric_v3_hr %>% dplyr::select(-forprofit) %>% mutate(ID_C = as.character(ID_C)), by = "ID_C") %>%
  left_join(aric_v3_pd %>% dplyr::select(-forprofit) %>% mutate(ID_C = as.character(ID_C)), by = "ID_C") %>%
  dplyr::select(ID_C, AGE = V3AGE31, SEX = GENDER, RACE = RACEGRP, SBP = SBPC22, DBP = SBPC23,
                DM = DIABTS34, HRX = HYPTMDCODE31, HR = ECGD31, LVH = CLVH31, CHOLMED = CHOLMDCODE31, WHR = WSTHPR31, ESTROGEN = HORMON31, 
                BMI = BMI32, SMK = CURSMK31, TC = TCHSIU31, HDL = HDLSIU31, LDL = LDL32, TG = TRGSIU31, 
                PD_STOP_AGE = RHXB7, PD_STOP_CURR = RHXB6, PD_STOP_CAUSE = RHXB4, CONTCPT = RHXB10) %>% 
  mutate(PD_START_AGE = NA, LIVE_BIRTH = NA, INSULIN= NA,FG = NA, 
         PD_STOP_CURR = ifelse(PD_STOP_CURR == "Y", 1, 0),
         CHOLMED = ifelse(CHOLMED==1,1,0),
         LVH = ifelse(LVH == 1,1,0),
         VISIT = 3)


aric_select_v4 <- aric_v4 %>%
  mutate(ID_C = as.character(ID_C)) %>%
  left_join(aric_v4_sbp %>% dplyr::select(-forprofit) %>% mutate(ID_C = as.character(ID_C)), by = "ID_C") %>%
  left_join(aric_v4_hr %>% dplyr::select(-forprofit) %>% mutate(ID_C = as.character(ID_C)), by = "ID_C") %>%
  left_join(aric_v4_pd %>% dplyr::select(-forprofit) %>% mutate(ID_C = as.character(ID_C)), by = "ID_C") %>%
  dplyr::select(ID_C, AGE = V4AGE41, SEX = GENDER, RACE = RACEGRP, SBP = SBPD19, DBP = SBPD20,
                DM = DIABTS42, HRX = HYPTMDCODE41, HR = ECGE31, LVH = CLVH41, CHOLMED = CHOLMDCODE41, WHR = WSTHPR41, ESTROGEN = HORMON41, 
                BMI = BMI41, SMK = CURSMK41, FG = GLUSIU41, TC = TCHSIU41, HDL = HDLSIU41, LDL = LDL41, TG = TRGSIU41, 
                PD_STOP_AGE = RHXC7, PD_STOP_CURR = RHXC6, PD_STOP_CAUSE = RHXC8, CONTCPT = RHXC10) %>% 
  mutate(PD_START_AGE = NA, LIVE_BIRTH = NA, INSULIN= NA,
         PD_STOP_CURR = ifelse(PD_STOP_CURR == "Y", 1, 0),
         CHOLMED = ifelse(CHOLMED==1,1,0),
         LVH = ifelse(LVH == 1,1,0),
         VISIT = 4)


aric_select_v5 <- aric_v5 %>%
  mutate(ID_C = as.character(ID_C)) %>%
  left_join(aric_v1 %>% dplyr::select(GENDER, ID_C, RACEGRP) %>% mutate(ID_C = as.character(ID_C)), by = "ID_C") %>%
  left_join(aric_v5_sbp %>% dplyr::select(-forprofit) %>% mutate(ID_C = as.character(ID_C)), by = "ID_C") %>%
  left_join(aric_v5_hr %>% dplyr::select(-forprofit) %>% mutate(ID_C = as.character(ID_C)), by = "ID_C") %>%
  dplyr::select(ID_C, AGE = V5AGE52, SEX = GENDER, RACE = RACEGRP, SBP = SBP14, DBP = SBP15,
                DM = DIABTS54, HRX = HYPTMDCODE51, HR = ECG6, LVH = CLVH51, CHOLMED = CHOLMDCODE51, WHR = WSTHPR51, 
                BMI = BMI51, SMK = CURSMK52, FG = GLUSIU51, TC = TCHSIU51, HDL = HDLSIU51, LDL = LDL51, TG = TRGSIU51) %>% 
  mutate(PD_START_AGE = NA, LIVE_BIRTH = NA, INSULIN= NA, ESTROGEN = NA, 
         PD_STOP_AGE = NA, PD_STOP_CURR = NA, PD_STOP_CAUSE = NA, CONTCPT = NA,
         PD_STOP_CURR = ifelse(PD_STOP_CURR == "Y", 1, 0),
         CHOLMED = ifelse(CHOLMED==1,1,0),
         LVH = ifelse(LVH == 1,1,0),
         VISIT = 5)


rm(aric_v1, aric_v1_hr, aric_v1_pd, aric_v1_sbp, 
   aric_v2, aric_v2_hr, aric_v2_sbp, 
   aric_v3, aric_v3_hr, aric_v3_pd, aric_v3_sbp, 
   aric_v4, aric_v4_hr, aric_v4_pd, aric_v4_sbp, 
   aric_v5, aric_v5_hr, aric_v5_sbp)

ARIC_select <- 
  rbind(aric_select_v1, aric_select_v2, aric_select_v3, aric_select_v4, aric_select_v5) %>%
  as.data.frame() %>%
  mutate(SEX = ifelse(SEX=="F",2,1),
         HR = as.numeric(as.character(HR)),
         HRX = ifelse(HRX==1,1,0),
         race = ifelse(RACE=="W",1,3),
         SMK = ifelse(SMK == 1, 1, 0),
         DM = ifelse(DM == 1, 1, 0),
         TC = TC*38.67,
         LDL = LDL,
         HDL = HDL,
         SBP = as.numeric(as.character(SBP)),
         PID = paste("ARIC",ID_C, sep = ""),
         num = as.numeric(as.factor(ID_C)))
ARIC_select$num %>% summary()
saveRDS(ARIC_select, "data/ARIC_select.rds")

ARIC_event <- surv %>%
  left_join(aric_select_v1 %>% dplyr::select(bsl_AGE = AGE, ID_C), by = "ID_C") %>%
  mutate(chd = ISP16,
         chd_age = AGIS16,
         chdtime = (AGIS16 - bsl_AGE)*365.25,
         chddeath = FATCHD16,
         chddeathtime = FUDTH16,
         nfchd = ifelse((chd==1&chddeath==0)|(chd==1&chddeath==1&chddeathtime-chdtime>7),1,0),
         nfchdtime = chdtime) %>%
  mutate(chddeath_age = bsl_AGE + chddeathtime/365.25,
         nfchd_age = bsl_AGE + chdtime/365.25,
         PID = paste("ARIC",ID_C, sep = ""))

ARIC_event$chd %>% sum()
ARIC_event$nfchd %>% sum()

saveRDS(ARIC_event, "data/ARIC_event.rds")

