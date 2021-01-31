library(haven)
library(MASS)
library(reshape2)
library(reshape)
library(dplyr)
library(magrittr)

fhs_v1 <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/Framingham_Offspring_2020b/Datasets/CSV/EX1_1D_V3.csv")
fhs_v2 <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/Framingham_Offspring_2020b/Datasets/CSV/EX1_2D_V3.csv")
fhs_v3 <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/Framingham_Offspring_2020b/Datasets/CSV/EX1_3D_V1.csv")
fhs_v4 <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/Framingham_Offspring_2020b/Datasets/CSV/EX1_4D_V1.csv")
fhs_v5 <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/Framingham_Offspring_2020b/Datasets/CSV/EX1_5D_V1.csv")
fhs_v6 <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/Framingham_Offspring_2020b/Datasets/CSV/EX1_6D_V1.csv")
fhs_v7 <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/Framingham_Offspring_2020b/Datasets/CSV/EX1_7D_V2.csv")
fhs_v8 <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/Framingham_Offspring_2020b/Datasets/CSV/E_EXAM_EX08_1_0005D.csv")
fhs_v9 <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/Framingham_Offspring_2020b/Datasets/CSV/E_EXAM_EX09_1b_0844D.csv")
fhs_age <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/Framingham_Offspring_2020b/Datasets/CSV/VR_DATES_2014_A_0912D.csv")

fhs_v8_lab <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/Framingham_Offspring_2020b/Datasets/CSV/l_fhslab_ex08_1_0257d.csv")
fhs_v8_medication <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/Framingham_Offspring_2020b/Datasets/CSV/vr_meds_ex08_1_0280d.csv")

fhs_v9_lab <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/Framingham_Offspring_2020b/Datasets/CSV/l_fhslab_ex09_1b_0658d.csv")
fhs_v9_medication <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/Framingham_Offspring_2020b/Datasets/CSV/vr_meds_ex09_1b_0879d.csv")

cvdsurv <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/Framingham_Offspring_2020b/Datasets/CSV/VR_SURVCVD_2017_A_1194D.csv")
dthsurv <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/Framingham_Offspring_2020b/Datasets/CSV/VR_SURVDTH_2017_A_1192D.csv")


fhs_select_v1 <- fhs_v1 %>%
  filter(IDTYPE == 1) %>%
  left_join(fhs_age %>% filter(IDTYPE==1) %>% dplyr::select(PID, SEX, AGE = AGE1), by = "PID") %>%
  dplyr::select(PID, AGE, SEX, SBP1 = A53, SBP2 = A55, SBP3 = A57, DBP1 = A54, DBP2 = A56, DBP3 = A58, DMRX = A84, HRX = A79, DIURET = A78,HR = A145, LVH = A164,
                BW = A50, BH = A51, SMK = A101, FG = A31, TC = A9, HDL = A10, VLDL = A11, LDL = A12, TG = A13, CHOLMED = A80, 
                PD_STOP_AGE = A89, PD_STOP_CURR = A88, PD_STOP_CAUSE = A90, CONTCPT = A93) %>% 
  mutate(WAISTCIRC = NA, HIPCIRC = NA, INSULIN = NA, LIVE_BIRTH = NA, PD_START_AGE = NA,ESTROGEN=NA,
         SMK = if_else(SMK == 0, 1, 0, missing = 0),
         CHOLMED = ifelse(CHOLMED==1,1,0),
         HRX = ifelse(HRX == 7 | DIURET == 4,1,0),
         DM = if_else(FG>=126|DMRX==1|INSULIN==1,1,0, missing = 0),
         DMRX = ifelse(DMRX == 4,1,0),
         LVH = ifelse(LVH == 1,1,0),
         BW = BW/2.2,
         BH = BH*2.54,
         VISIT = 1)

fhs_select_v2 <- fhs_v2 %>%
  filter(IDTYPE == 1) %>%
  left_join(fhs_age %>% filter(IDTYPE==1) %>% dplyr::select(PID, SEX, AGE = AGE2), by = "PID") %>%
  dplyr::select(PID, AGE, SEX, SBP1 = B22, SBP2 = B24, SBP3 = B26, DBP1 = B23, DBP2 = B25, DBP3 = B27, DMRX = B65, HRX = B56, DIURET = B59, HR = B255, LVH = B275,
                BW = B13, BH = B14, SMK = B86, FG = B737, TC = B352, HDL = B355, VLDL = B356, LDL = B357, TG = B358, CHOLMED = B61, INSULIN = B68,
                PD_START_AGE = B75, PD_STOP_AGE = B71, PD_STOP_CURR = B70, PD_STOP_CAUSE = B78, CONTCPT = B78, LIVE_BIRTH = B76) %>% 
  mutate(WAISTCIRC = NA, HIPCIRC = NA,ESTROGEN=NA,
         SMK = ifelse(SMK == 1, 1, 0),
         CHOLMED = ifelse(CHOLMED==1,1,0),
         HRX = ifelse(HRX == 1 | DIURET == 4,1,0),
         DM = ifelse(FG>=126|DMRX==1|INSULIN==1,1,0),
         DMRX = ifelse(DMRX == 1,1,0),
         LVH = ifelse(LVH == 1,1,0),
         VISIT = 2)

fhs_select_v3 <- fhs_v3 %>%
  filter(IDTYPE == 1) %>%
  left_join(fhs_age %>% filter(IDTYPE==1) %>% dplyr::select(PID, SEX, AGE = AGE3), by = "PID") %>%
  dplyr::select(PID, AGE, SEX, SBP2 = C184, SBP3 = C286, DBP2 = C185, DBP3 = C287, DMRX = C30, HRX = C450, DIURET = C17, HR = C289, LVH = C330,
                BW = C416, BH = C417, SMK = C68, FG = C434, TC = C429, HDL = C431, VLDL = C441, LDL = C442, TG = C433, CHOLMED = C26, INSULIN = C29, ESTROGEN = C31,
                PD_STOP_AGE = C48, PD_STOP_CURR = C47, PD_STOP_CAUSE = C49, CONTCPT = C31, LIVE_BIRTH = C52) %>% 
  mutate(WAISTCIRC = NA, HIPCIRC = NA,PD_START_AGE = NA, SBP1 = NA, DBP1 = NA, 
         SMK = ifelse(SMK >0, 1, 0),
         CHOLMED = ifelse(CHOLMED==1,1,0),
         HRX = ifelse(HRX == 1 | DIURET == 1,1,0),
         DM = ifelse(FG>=126|DMRX==1|INSULIN==1,1,0),
         DMRX = ifelse(DMRX == 1,1,0),
         LVH = ifelse(LVH == 1,1,0),
         BW = BW/2.2,
         BH = BH*2.54,
         VISIT = 3)


fhs_select_v4 <- fhs_v4 %>%
  filter(IDTYPE == 1) %>%
  left_join(fhs_age %>% filter(IDTYPE==1) %>% dplyr::select(PID, SEX, AGE = AGE4), by = "PID") %>%
  dplyr::select(PID, AGE, SEX, SBP1 = D413, DBP1 = D414, SBP2 = D192, DBP2 = D193, SBP3 = D288, DBP3 = D289, DMRX = D037, HRX = D445, HR = D292, LVH = D334,
                BW = D401, BH = D402, SMK = D102, FG = D452, TC = D448, HDL = D449, TG = D451, CHOLMED = D030, INSULIN = D035, ESTROGEN = D038,
                WAISTCIRC = D410, HIPCIRC = D411, PD_STOP_AGE = D054, PD_STOP_CURR = D053, PD_STOP_CAUSE = D055, CONTCPT = D060, LIVE_BIRTH = D058) %>% 
  mutate(PD_START_AGE = NA, DIURET = NA, VLDL = NA, 
         LDL = TC - (HDL+TG*2),
         SMK = ifelse(SMK >0, 1, 0),
         CHOLMED = ifelse(CHOLMED==1,1,0),
         DM = ifelse(FG>=126|DMRX==1|INSULIN==1,1,0),
         DMRX = ifelse(DMRX == 1,1,0),
         LVH = ifelse(LVH == 1,1,0),
         BW = BW/2.2,
         BH = BH*2.54,
         VISIT = 4)

fhs_select_v5 <- fhs_v5 %>%
  filter(IDTYPE == 1) %>%
  left_join(fhs_age %>% filter(IDTYPE==1) %>% dplyr::select(PID, SEX, AGE = AGE5), by = "PID") %>%
  mutate(CHOLMED = ifelse(E245==1|E246==1|E247==1|E248==1|E249==1,1,0)) %>%
  dplyr::select(PID, AGE, SEX, SBP1 = E035, DBP1 = E036, SBP2 = E485, DBP2 = E486, SBP3 = E581, DBP3 = E582, DMRX = E256, ESTROGEN = E257, HRX = E221, 
                HR = E584, LVH = E618, BW = E024, BH = E025, SMK = E320, FG = E671, TC = E667, HDL = E668, TG = E670, CHOLMED = CHOLMED, INSULIN = E254, 
                WAISTCIRC = E032, HIPCIRC = E033, PD_STOP_AGE = E274, PD_STOP_CURR = E273, PD_STOP_CAUSE = E275, CONTCPT = E280, LIVE_BIRTH = E278) %>% 
  mutate(PD_START_AGE = NA, DIURET = NA, VLDL = NA, 
         LDL = TC - (HDL+TG*2),
         SMK = ifelse(SMK >0, 1, 0),
         CHOLMED = ifelse(CHOLMED==1,1,0),
         DM = ifelse(FG>=126|DMRX==1|INSULIN==1,1,0),
         DMRX = ifelse(DMRX == 1,1,0),
         LVH = ifelse(LVH == 1,1,0),
         BW = BW/2.2,
         BH = BH*2.54,
         VISIT = 5)


fhs_select_v6 <- fhs_v6 %>%
  filter(IDTYPE == 1) %>%
  left_join(fhs_age %>% filter(IDTYPE==1) %>% dplyr::select(PID, SEX, AGE = AGE6), by = "PID") %>%
  mutate(CHOLMED = ifelse(F209==1|F210==1|F211==1|F212==1|F213==1,1,0)) %>%
  dplyr::select(PID, AGE, SEX, SBP1 = F026, DBP1 = F027, SBP2 = F476, DBP2 = F477, SBP3 = F576, DBP3 = F577, DMRX = F220, ESTROGEN = F221, HRX = F184, 
                HR = F579, LVH = F597, BW = F007, BH = F008, SMK = F289, FG = F724, TC = F726, HDL = F725, TG = F727, CHOLMED = CHOLMED, INSULIN = F218, 
                WAISTCIRC = F016, HIPCIRC = F017, PD_STOP_CURR = F237, PD_STOP_AGE = F238, PD_STOP_CAUSE = F239, LIVE_BIRTH = F244, CONTCPT = F246) %>% 
  mutate(PD_START_AGE = NA, DIURET = NA, VLDL = NA, 
         LDL = TC - (HDL+TG*2),
         SMK = ifelse(SMK >0, 1, 0),
         CHOLMED = ifelse(CHOLMED==1,1,0),
         DM = ifelse(FG>=126|DMRX==1|INSULIN==1,1,0),
         DMRX = ifelse(DMRX == 1,1,0),
         LVH = ifelse(LVH == 1,1,0),
         BW = BW/2.2,
         BH = BH*2.54,
         VISIT = 6)


fhs_select_v7 <- fhs_v7 %>%
  filter(IDTYPE == 1) %>%
  left_join(fhs_age %>% filter(IDTYPE==1) %>% dplyr::select(PID, SEX, AGE = AGE7), by = "PID") %>%
  mutate(CHOLMED = ifelse(G041==1|G042==1|G043==1|G044==1|G045==1,1,0)) %>%
  dplyr::select(PID, AGE, SEX, SBP1 = G450, DBP1 = G451, SBP2 = G271, DBP2 = G272, SBP3 = G354, DBP3 = G355, DMRX = G052, ESTROGEN = G060, HRX = G008, 
                HR = G357, LVH = G375, BW = G440, BH = G441, SMK = G117, FG = G705, TC = G704, HDL = G703, TG = G706, CHOLMED = CHOLMED, INSULIN = G050, 
                PD_STOP_CURR = G077, PD_STOP_AGE = G078, PD_STOP_CAUSE = G079, LIVE_BIRTH = G085, CONTCPT = G087,
                WAISTCIRC = G445, HIPCIRC = G446) %>% 
  mutate(PD_START_AGE = NA, DIURET = NA, VLDL = NA, 
         LDL = TC - (HDL+TG*2),
         SMK = ifelse(SMK >0, 1, 0),
         HRX = ifelse(HRX==1,1,0),
         CHOLMED = ifelse(CHOLMED==1,1,0),
         DM = ifelse(FG>=126|DMRX==1|INSULIN==1,1,0),
         DMRX = ifelse(DMRX == 1,1,0),
         LVH = ifelse(LVH == 1,1,0),
         BW = BW/2.2,
         BH = BH*2.54,
         VISIT = 7)

fhs_select_v8 <- fhs_v8 %>% 
  filter(IDTYPE == 1) %>%
  left_join(fhs_age %>% filter(IDTYPE==1) %>% dplyr::select(PID, SEX, AGE = AGE8), by = "PID") %>%
  left_join(fhs_v8_lab, by = "PID") %>%
  left_join(fhs_v8_medication %>%
              mutate_at(vars(ATC_COD1,ATC_COD2,ATC_COD3,ATC_COD4), function(x){ifelse(startsWith(as.character(x),"A10"),1,0)}) %>%
              mutate(DMRX = ifelse(ATC_COD1==1|ATC_COD2==1|ATC_COD3==1|ATC_COD4==1,1,0)) %>%
              dplyr::select(PID, DMRX) %>%
              unique() %>%
              group_by(PID) %>%
              dplyr::summarise(DMRX = max(DMRX, na.rm = T)), by = "PID") %>%
  left_join(fhs_v8_medication %>%
              mutate_at(vars(ATC_COD1,ATC_COD2,ATC_COD3,ATC_COD4), function(x){ifelse(startsWith(as.character(x),"A10A"),1,0)}) %>%
              mutate(INSULIN = ifelse(ATC_COD1==1|ATC_COD2==1|ATC_COD3==1|ATC_COD4==1,1,0)) %>%
              dplyr::select(PID, INSULIN) %>%
              unique() %>%
              group_by(PID) %>%
              dplyr::summarise(INSULIN = max(INSULIN, na.rm = T)), by = "PID") %>%
  mutate(HRX = ifelse(H014==1,1,0),
         CHOLMED = ifelse(H015==1,1,0)) %>%
  dplyr::select(PID, AGE, SEX, SBP2 = H111, DBP2 = H112, SBP3 = H233, DBP3 = H234, DMRX = DMRX, ESTROGEN = H049, HRX = HRX, 
                HR = H304, LVH = H322, BW = H393, BH = H399, SMK = H062, FG = GLUCOSE, TC = TOT_CHOL, HDL = HDL_CHOL, TG = TRIG, CHOLMED = CHOLMED, INSULIN = INSULIN, 
                CONTCPT = H024, WAISTCIRC = H403) %>% 
  mutate(PD_START_AGE = NA, DIURET = NA, HIPCIRC = NA, VLDL = NA, SBP1 = NA, DBP1 = NA, LIVE_BIRTH = NA, PD_STOP_CURR = NA, PD_STOP_AGE = NA, PD_STOP_CAUSE = NA, 
         PD_STOP_CURR = ifelse(PD_STOP_CURR==2,1,0),
         BW = BW*5+100,
         LDL = TC - (HDL+TG*2),
         SMK = ifelse(SMK >0, 1, 0),
         CHOLMED = ifelse(CHOLMED==1,1,0),
         DM = ifelse(FG>=126|DMRX==1|INSULIN==1,1,0),
         DMRX = ifelse(DMRX == 1,1,0),
         LVH = ifelse(LVH == 1,1,0),
         BW = BW/2.2,
         BH = BH*2.54,
         VISIT = 8)

fhs_select_v9 <- fhs_v9 %>% 
  filter(IDTYPE == 1) %>%
  left_join(fhs_age %>% filter(IDTYPE==1) %>% dplyr::select(PID, SEX, AGE = AGE9), by = "PID") %>%
  left_join(fhs_v9_lab, by = "PID") %>%
  left_join(fhs_v9_medication %>%
              mutate_at(vars(ATC_COD1,ATC_COD2,ATC_COD3,ATC_COD4), function(x){ifelse(startsWith(as.character(x),"A10"),1,0)}) %>%
              mutate(DMRX = ifelse(ATC_COD1==1|ATC_COD2==1|ATC_COD3==1|ATC_COD4==1,1,0)) %>%
              dplyr::select(PID, DMRX) %>%
              unique() %>%
              group_by(PID) %>%
              dplyr::summarise(DMRX = max(DMRX, na.rm = T)), by = "PID") %>%
  left_join(fhs_v9_medication %>%
              mutate_at(vars(ATC_COD1,ATC_COD2,ATC_COD3,ATC_COD4), function(x){ifelse(startsWith(as.character(x),"A10A"),1,0)}) %>%
              mutate(INSULIN = ifelse(ATC_COD1==1|ATC_COD2==1|ATC_COD3==1|ATC_COD4==1,1,0)) %>%
              dplyr::select(PID, INSULIN) %>%
              unique() %>%
              group_by(PID) %>%
              dplyr::summarise(INSULIN = max(INSULIN, na.rm = T)), by = "PID") %>%
  dplyr::select(PID, AGE, SEX, SBP2 = J116, DBP2 = J118, SBP3 = J255, DBP3 = J257, DMRX = J019, HRX = J015, 
                HR = J359, LVH = J377, BW = J474, BH = J472, SMK = J062, FG = GLUC, TC = CHOL, HDL = HDL, TG = TRIG, CHOLMED = J017, INSULIN = INSULIN, 
                WAISTCIRC = J480, HIPCIRC = J482) %>% 
  mutate(CONTCPT = NA, PD_START_AGE = NA, DIURET = NA,  VLDL = NA, SBP1 = NA, DBP1 = NA, 
         LIVE_BIRTH = NA, PD_STOP_CURR = NA, PD_STOP_AGE = NA, PD_STOP_CAUSE = NA, ESTROGEN = NA,
         PD_STOP_CURR = ifelse(PD_STOP_CURR==2,1,0),
         LDL = TC - (HDL+TG*2),
         SMK = ifelse(SMK >0, 1, 0),
         CHOLMED = ifelse(CHOLMED==1,1,0),
         DM = ifelse(FG>=126|DMRX==1|INSULIN==1,1,0),
         DMRX = ifelse(DMRX == 1,1,0),
         LVH = ifelse(LVH == 1,1,0),
         BW = BW/2.2,
         BH = BH*2.54,
         VISIT = 9)

rm(fhs_v1, fhs_v2, fhs_v3, fhs_v4, fhs_v5, fhs_v6, fhs_v7, fhs_v8, fhs_v9, fhs_v8_lab, fhs_v8_medication, fhs_v9_lab, fhs_v9_medication)

FHS_select <- 
  rbind(fhs_select_v1, fhs_select_v2, fhs_select_v3, 
        fhs_select_v4, fhs_select_v5, fhs_select_v6, 
        fhs_select_v7, fhs_select_v8, fhs_select_v9) %>%
  as.data.frame() %>%  
  rowwise() %>%
  mutate(SBP = mean(c(SBP2, SBP3), na.rm = T),
         DBP = mean(c(DBP2, DBP3), na.rm = T),
         BMI = BW/((BH/100)^2),
         PID = paste("FHS", PID, sep = ""))

FHS_select$num <- FHS_select$PID %>% as.factor() %>% as.numeric()

saveRDS(FHS_select, "data/FHS_select.rds")

FHS_event <- cvdsurv %>%
  left_join(fhs_select_v1 %>% dplyr::select(bsl_AGE = AGE, PID), by = "PID") %>%
  left_join(dthsurv %>% dplyr::select(-IDTYPE), by = "PID") %>%
  filter(IDTYPE == 1) %>%
  mutate(chd = CHD,
         chdtime = CHDDATE,
         chddeath = if_else(CHDDEATH==1,1,0,missing = 0),
         chddeathtime = ifelse(is.na(DATEDTH),CHDDATE,DATEDTH),
         nfchd = ifelse((chd==1&chddeath==0)|(chd==1&chddeath==1&chddeathtime-chdtime>7),1,0),
         nfchdtime = chdtime) %>%
  mutate(chd_age = bsl_AGE + chdtime/365.25,
         chddeath_age = bsl_AGE + chddeathtime/365.25,
         nfchd_age = bsl_AGE + chdtime/365.25,
         PID = paste("FHS", PID, sep = ""))

saveRDS(FHS_event, "data/FHS_event.rds")

FHS_event$chd %>% sum()
