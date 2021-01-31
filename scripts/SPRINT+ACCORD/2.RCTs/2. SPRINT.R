library(haven)
library(MASS)
library(reshape2)
library(reshape)
library(dplyr)
library(magrittr)
library(survival)
library(splines)
library(ggplot2)
library(rms)
library(tableone)

sprint_key <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/SPRINT_2019a/SPRINT/data/CSV/keyvar.csv")
bsl <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/SPRINT_2019a/SPRINT/data/CSV/incl_excl.csv")
labs <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/SPRINT_2019a/SPRINT/data/CSV/labs.csv")
bl_history <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/SPRINT_2019a/SPRINT/data/CSV/bl_history.csv")
bl_meds_physexam <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/SPRINT_2019a/SPRINT/data/CSV/bl_meds_physexam.csv") %>% mutate(WEIGHT = WEIGHT/2.20462262, HEIGHT = HEIGHT/39.3700787)
bl_meds_physexam4m <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/SPRINT_2019a/SPRINT/data/CSV/bl_medsphys4m.csv") %>% mutate(WEIGHT = WEIGHT/2.20462262, HEIGHT = HEIGHT/39.3700787)
bl_meds_physexam <- rbind(bl_meds_physexam, bl_meds_physexam4m %>% dplyr::select(-colnames(bl_meds_physexam4m)[which(!colnames(bl_meds_physexam4m) %in% colnames(bl_meds_physexam))]))

mioutcome <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/SPRINT_2019a/SPRINT/data/CSV/misurv.csv")
strokeoutcome <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/SPRINT_2019a/SPRINT/data/CSV/strokesurv.csv")
hfoutcome <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/SPRINT_2019a/SPRINT/data/CSV/heartfailsurv.csv")
primaryoutcomes <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/SPRINT_2019a/SPRINT/data/CSV/primarysurv.csv")
nonmiacs <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/SPRINT_2019a/SPRINT/data/CSV/nonmiacssurv.csv")
alldeath <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/SPRINT_2019a/SPRINT/data/CSV/alldeath.csv")
cvddeath <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/SPRINT_2019a/SPRINT/data/CSV/cvddeath.csv")

st_bps1 <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/SPRINT_2019a/SPRINT/data/CSV/stbp_manage_nanfu.csv") %>% dplyr::select(MASKID, SEATSYS, SEATDIAST, VISITCODE,FORMDAYS)  
st_bps2 <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/SPRINT_2019a/SPRINT/data/CSV/stbp_manage_anfu.csv") %>% dplyr::select(MASKID, SEATSYS, SEATDIAST, VISITCODE,FORMDAYS)

int_bps18 <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/SPRINT_2019a/SPRINT/data/CSV/intbp_manage_18m.csv") %>% dplyr::select(MASKID, SEATSYS, SEATDIAST, VISITCODE,FORMDAYS)  
int_bps6 <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/SPRINT_2019a/SPRINT/data/CSV/intbp_manage_6m.csv") %>% dplyr::select(MASKID, SEATSYS, SEATDIAST, VISITCODE,FORMDAYS) 
int_bps2 <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/SPRINT_2019a/SPRINT/data/CSV/intbp_manage_2m.csv") %>% dplyr::select(MASKID, SEATSYS, SEATDIAST, VISITCODE,FORMDAYS)  
int_bps1 <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/SPRINT_2019a/SPRINT/data/CSV/intbp_manage_1m.csv") %>% dplyr::select(MASKID, SEATSYS, SEATDIAST, VISITCODE,FORMDAYS) 

bsl_bps <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/SPRINT_2019a/SPRINT/data/CSV/bp_manage_base.csv") %>% dplyr::select(MASKID, SEATSYS, FORMDAYS) %>% mutate(VISITCODE = "0M")

safety_events <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/SPRINT_2019a/SPRINT/data/CSV/safety_events_v2.csv") %>% dplyr::select(MASKID, MC_HYPOTEN, MC_BRADY)

bp_med_log <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/SPRINT_2019a/SPRINT/data/CSV/bp_med_log_v2.csv") %>% dplyr::select(MASKID, THISMEDNBR, VISITCODE)

events <- mioutcome %>%
  dplyr::select(MASKID, mitime = EVENTDAYS, mi = EVENT) %>%
  left_join(alldeath %>% 
              mutate(chddeath = ifelse(MAINSPRINTOUTCOME==1 & EVENT==1,1,0)) %>%  
              dplyr::select(MASKID, chddeathtime = EVENTDAYS, chddeath, death = EVENT, deathtime = EVENTDAYS), 
            by = "MASKID") %>%
  left_join(cvddeath %>% dplyr::select(MASKID, cvddeathtime = EVENTDAYS, cvddeath = EVENT), by = "MASKID") %>%
  left_join(nonmiacs %>% dplyr::select(MASKID, nonmiacstime = EVENTDAYS, nonmiacs = EVENT), by = "MASKID") %>%
  left_join(primaryoutcomes %>% dplyr::select(MASKID, potime = EVENTDAYS, po = EVENT), by = "MASKID") %>%
  left_join(strokeoutcome %>% dplyr::select(MASKID, stroketime = EVENTDAYS, stroke = EVENT), by = "MASKID") %>%
  left_join(hfoutcome %>% dplyr::select(MASKID, chftime = EVENTDAYS, chf = EVENT), by = "MASKID") %>%
  transform(cvdtime = pmin(chftime, stroketime, mitime, na.rm = T),
            cvd = ifelse(mi==1|stroke==1|chf==1,1,0),
            chd = ifelse(mi==1|nonmiacs==1|chddeath==1,1,0),
            chdtime = pmin(nonmiacstime, mitime, chddeathtime, na.rm = T),
            nfchd = ifelse(mi==1|nonmiacs==1,1,0),
            nfchdtime = pmin(nonmiacstime, mitime, na.rm = T))%>%
  mutate(mitime = mitime/365.25,
         chdtime = chdtime/365.25,
         nfchdtime = nfchdtime/365.25,
         chddeathtime = chddeathtime/365.25,
         deathtime = deathtime/365.25,
         cvdtime = cvdtime/365.25,
         chftime = chftime/365.25,
         stroketime = stroketime/365.25) 

safety <- safety_events %>%
  mutate_at(vars(-MASKID),function(x){ifelse(is.na(x),0,x)}) %>%
  group_by(MASKID) %>%
  dplyr::summarise(MC_HYPOTEN = max(MC_HYPOTEN, na.rm = T),
                   MC_BRADY = max(MC_BRADY, na.rm = T)) %>%
  as.data.frame()

bps_sprint <- rbind(int_bps1, int_bps2, int_bps6, int_bps18, st_bps1, st_bps2) %>%
  mutate(visit = VISITCODE,
         visit = gsub("M","",visit)) %>%
  filter(visit != "PRN" & visit != "ASK") %>%
  mutate(visit = as.numeric(visit),
         start = FORMDAYS) %>%
  arrange(visit) %>%
  arrange(MASKID) %>%
  group_by(MASKID) %>%
  mutate(end = lead(start)) %>%
  as.data.frame()

saveRDS(bps_sprint, "data/bps_sprint.rds")
# Advicor® (niacin extended-release/lovastatin)
# Altoprev® (lovastatin extended-release)
# Caduet® (amlodipine and atorvastatin)
# Crestor® (rosuvastatin)
# Juvisync® (sitagliptin/simvastatin) 
# Lescol® (fluvastatin)
# Lescol XL (fluvastatin extended-release)
# Lipitor® (atorvastatin)
# Liptruzet™ (ezetimibe/atorvastatin)
# Livalo® (pitavastatin) 
# Mevacor® (lovastatin)
# Pravachol® (pravastatin)
# Simcor® (niacin extended-release/simvastatin)
# Vytorin® (ezetimibe/simvastatin)

statinmed <- 
  bl_meds_physexam$RX1 %>% unique() %>% as.character() %>% as.data.frame()

bl_meds_physexam$RX1[which(grepl('aycol',bl_meds_physexam$RX1))]

statin <- bl_meds_physexam %>% 
  mutate_at(vars(starts_with("RX")), function(x){ifelse(grepl('statin',x) | 
                                                          grepl('ipitor',x) | 
                                                          grepl('truzet',x) | 
                                                          grepl('dvicor',x) | 
                                                          grepl('ivalo',x) | 
                                                          grepl('mcor',x) | 
                                                          grepl('vacor',x) | 
                                                          grepl('vachol',x) | 
                                                          grepl('oprev',x) | 
                                                          grepl('aduet',x) |
                                                          grepl('visync',x) | 
                                                          grepl('restor',x) | 
                                                          grepl('zocor',x) | 
                                                          grepl('Zocor',x) | 
                                                          grepl('escol',x) | 
                                                          grepl('ytorin',x) |
                                                          
                                                          grepl('STATIN',x) | 
                                                          grepl('IPITOR',x) | 
                                                          grepl('TRUZET',x) | 
                                                          grepl('DVICOR',x) | 
                                                          grepl('IVALO',x) | 
                                                          grepl('MCOR',x) | 
                                                          grepl('VACOR',x) | 
                                                          grepl('VACHOL',x) | 
                                                          grepl('OPREV',x) | 
                                                          grepl('ADUET',x) |
                                                          grepl('VISYNC',x) | 
                                                          grepl('RESTOR',x) | 
                                                          grepl('ZOCOR',x) | 
                                                          grepl('ESCOL',x) | 
                                                          grepl('YTORIN',x) , 1,0)}) %>%
  mutate(statin = dplyr::select(., RX1:RX20) %>% apply(1,sum, na.rm = T),
         statin = ifelse(statin>0,1,0)) 


sprint_data <- sprint_key %>%
  mutate(MASKID = as.character(MASKID)) %>%
  left_join(labs %>% filter(VISITCODE == "RZ1") %>% mutate(MASKID = as.character(MASKID)), by = "MASKID") %>%
  left_join(bl_history %>% mutate(MASKID = as.character(MASKID)) %>% dplyr::select(MASKID, ASPIRIN, HEARTATT, ANGINA, CONHEARTFAIL, STROKE), by = "MASKID") %>%
  left_join(statin %>% mutate(MASKID = as.character(MASKID)) %>% dplyr::select(MASKID, statin) %>% unique(), by = "MASKID") %>%
  left_join(bl_meds_physexam %>% mutate(MASKID = as.character(MASKID)) %>% dplyr::select(MASKID, WEIGHT,HEIGHT) %>% unique(), by = "MASKID") %>%
  left_join(events %>% mutate(MASKID = as.character(MASKID)), by = "MASKID") %>%
  left_join(bsl %>% mutate(MASKID = as.character(MASKID)), by = "MASKID") %>%
  left_join(rbind(bps_sprint %>% dplyr::select(MASKID, SEATSYS, FORMDAYS, VISITCODE), bsl_bps) %>%
              mutate(VISITCODE = paste("SEATSYS_", VISITCODE, sep = ""),
                     MASKID = as.character(MASKID)) %>%
              dcast(MASKID ~ VISITCODE, value.var="SEATSYS"), 
            by = "MASKID") %>%
  left_join(safety %>% mutate(MASKID = as.character(MASKID)), by = "MASKID") %>%
  mutate(BMI = WEIGHT/HEIGHT^2,
         OB = ifelse(BMI>=30,1,0),
         SMK = CIGSMOKER,
         TC = TOTALCHOLEST,
         HDL = RESULT_HDL,
         LDL = RESULT_LDLR,
         RACE_WHITE = if_else(RACE_WHITE==1,1,0, missing = 0),
         cvd_hx_baseline = ifelse(HEARTATT == 1| CONHEARTFAIL == 1 | STROKE == 1,1,0)) %>%
  mutate(SEX = ifelse(GENDER==2,1,2),
         AGE = RZ_AGE,
         AGE65 = ifelse(AGE>=70,1,0)) %>%
  mutate(hd = ifelse(chf==1|chd==1,1,0)) %>%
  transform(hdtime = pmin(chdtime, chftime, na.rm = T))

# table 1 ----

saveRDS(sprint_data, "data/sprint_data.rds")

# traj ----

