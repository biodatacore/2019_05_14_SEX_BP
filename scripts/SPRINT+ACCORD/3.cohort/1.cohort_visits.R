library(haven)

# fhs
FHS <- readRDS("data/FHS_select.rds")
FHS <- FHS %>%
  dplyr::select(SBP, DBP, SEX, AGE, HRX, id = PID, visit = VISIT, BMI, SMK, DM, TC, HDL, LDL, HRX, CHOLMED, FG) %>%
  mutate(cohort = "FHS",
         race = 1,
         # SBP = SBP - 2.6,
         # DBP = DBP - 6.2,
         TP = paste("FHS",visit,sep = ""))

# MESA
MESA <- readRDS("data/MESA_select.rds")
MESA <- MESA %>%
  dplyr::select(SBP, DBP, SEX, AGE, HRX, id = PID, TP, BMI, SMK, DM, TC, HDL, LDL, HRX, CHOLMED, FG, race) %>%
  mutate(cohort = "MESA",
         visit = TP,
         # SBP = SBP + 0.5, 
         # DBP = DBP + 2.9,
         # SBP = SBP - 2.6,
         # DBP = DBP - 6.2,
         TP = paste("MESA",TP,sep = "")) 

# CARDIA
CARDIA <- readRDS("data/CARDIA_select.rds")
CARDIA <- CARDIA %>%
  dplyr::select(SBP, DBP, SEX, AGE, HRX, id = PID, TP, BMI, SMK, DM, TC, HDL, LDL, HRX, CHOLMED, FG, race) %>%
  mutate(cohort = "CARDIA",
         visit = TP + 1,
         # SBP = ifelse(visit == 8 | visit == 9, 3.74 + 0.96*SBP, SBP),
         # DBP = ifelse(visit == 8 | visit == 9, 1.30 + 0.97*DBP, DBP),
         # SBP = SBP + 2.6,
         # DBP = DBP + 6.2,
         TP = paste("CARDIA",TP,sep = ""))  

# ARIC 
ARIC <- readRDS("data/ARIC_select.rds")
ARIC <- ARIC %>%
  dplyr::select(SBP, DBP, SEX, AGE, HRX, id = ID_C, visit = VISIT, BMI, SMK, DM, TC, HDL, LDL, HRX, CHOLMED, FG, race) %>% 
  mutate(cohort = "ARIC",
         TP = paste("ARIC",visit,sep = ""),
         DBP = as.numeric(as.character(DBP)),
         SBP = as.numeric(as.character(SBP))) %>%
  as.data.frame() #%>%
  # mutate(SBP = SBP + 2.6,
  #        DBP = DBP + 6.2)

# CHS 
CHS <- readRDS("data/CHS_select.rds")
CHS <- CHS %>%
  dplyr::select(SBP, DBP, SEX, AGE, HRX, id = PID, visit = VISIT, BMI, SMK, DM, TC, HDL, LDL, HRX, CHOLMED, FG, race) %>% 
  mutate(cohort = "CHS",
         # SBP = SBP + 2.6,
         # DBP = DBP + 6.2,
         TP = paste("CHS",visit,sep = "")) %>%
  as.data.frame()


# JHS 

JHS <- readRDS("data/JHS_select.rds")
JHS <- JHS %>%
  dplyr::select(SBP, DBP, SEX, AGE, HRX, id = PID, visit, BMI, SMK, DM, TC, HDL, LDL, HRX, CHOLMED, FG, race) %>% 
  mutate(cohort = "JHS",
         # SBP = SBP + 2.6,
         # DBP = DBP + 6.2,
         TP = paste("JHS",visit,sep = "")) %>%
  as.data.frame()



comb_dat <- rbind(FHS, CARDIA, ARIC, MESA, CHS, JHS) %>% 
  as.data.frame() %>%
  mutate(PP = SBP - DBP,
         MAP = DBP + 1/3*PP)

CARDIA$id %>% unique() %>% length()
FHS$id %>% unique() %>% length()
MESA$id %>% unique() %>% length()
ARIC$id %>% unique() %>% length()
JHS$id %>% unique() %>% length()
CHS$id %>% unique() %>% length()

saveRDS(comb_dat, "data/comb_visits.rds")





