library(dplyr)
library(magrittr)
library(ggplot2)
library(haven)
library(data.table)
library(purrr)

FHS_clin <- readRDS("data/FHS_clin_HR.rds")
ARIC_clin <- readRDS("data/ARIC_clin_HR.rds")
MESA_clin <- readRDS("data/MESA_clin_HR.rds")
cardia_clin <- readRDS("data/CARDIA_clin_HR.rds")

cardia_clin <- cardia_clin %>% dplyr::select(id, AGE, SEX, race, SBP, DBP, TC, LDL, HDL, BMI, SMK, DM, HRX, CHOLMED, hardcvd, hardcvd_age, dth, dth_age, strk, strk_age, chf, chf_age, mi, mi_age, chd, chd_age, chddeath) %>% 
  mutate(cohort = "CARDIA",
         SBP = SBP + 2.6,
         DBP = DBP + 6.2)
MESA_clin <- MESA_clin %>% dplyr::select(id, AGE, SEX, race, SBP, DBP, TC, LDL, HDL, BMI, SMK, DM, HRX, CHOLMED, hardcvd, hardcvd_age, dth, dth_age, strk, strk_age, chf,chf_age, mi, mi_age, chd, chd_age, chddeath) %>% 
  mutate(cohort = "MESA", 
         SBP = SBP + 0.5, 
         DBP = DBP + 2.9)
ARIC_clin <- ARIC_clin %>% dplyr::select(id, AGE, SEX, race, SBP, DBP, TC, LDL, HDL, BMI, SMK, DM, HRX, CHOLMED, hardcvd, hardcvd_age, dth, dth_age, strk, strk_age, chf,chf_age, mi, mi_age, chd, chd_age, chddeath) %>% 
  mutate(cohort = "ARIC",
         SBP = SBP + 2.6,
         DBP = DBP + 6.2)
FHS_clin <- FHS_clin %>% dplyr::select(id, AGE, SEX, race, SBP, DBP, TC, LDL, HDL, BMI, SMK, DM, HRX, CHOLMED, hardcvd, hardcvd_age, dth, dth_age, strk, strk_age, chf,chf_age, mi, mi_age, chd, chd_age) %>% 
  mutate(cohort = "FHS") %>% mutate(chddeath = NA)

FHS_clin$mi %>% sum()
ARIC_clin$mi %>% sum()
cardia_clin$mi %>% sum(na.rm = T)
MESA_clin$mi %>% sum(na.rm = T)

comb_clin <- rbind(cardia_clin, FHS_clin, MESA_clin, ARIC_clin) %>%
  mutate(strktime = strk_age - AGE,
         chftime = chf_age - AGE,
         chdtime = chd_age - AGE,
         mitime = mi_age - AGE,
         dthtime = dth_age-AGE)
comb_clin$SMK %>% summary()
comb_clin$HRX %>% summary()
comb_clin$hardcvd %>% sum(na.rm = T)
comb_clin$dth %>% sum(na.rm = T)
saveRDS(comb_clin, "data/comb_clin_HR.rds")


# fhs
fhs <- readRDS("data/fhs_cleaned_HR.rds")

fhs <- fhs[,c("SBP","DBP","SEX","AGE","HRX","ID","TP","race","BMI","SMK","DM","TC","HDL","LDL","HR","CHOLMED","FG")] %>% 
  mutate(id = ID,
         cohort = "fhs",
         visit = TP,
         TP = paste("fhs",TP,sep = "")) %>% 
  dplyr::select(-ID) 


# MESA

library(haven)

MESA_ALT <- readRDS("data/mesa_cleaned_HR.rds")
MESA_ALT <- MESA_ALT[,c("SBP","DBP","SEX","AGE","HRX","id","TP","race","BMI","SMK","DM","TC","HDL","LDL","HR","CHOLMED","FG")] %>% 
  mutate(cohort = "MESA",
    visit = TP,
    TP = paste("mesa",TP,sep = "")) 

# CARDIA

cardia <- readRDS("data/cardia_cleaned_HR.rds")
cardia <- cardia[,c("SBP","DBP","SEX","AGE","HRX","id","TP","race","BMI","SMK","DM","TC","HDL","LDL","HR","CHOLMED","FG")] %>% 
  mutate(cohort = "cardia",
    visit = TP + 1,
    TP = paste("cardia",TP,sep = ""))  


# ARIC 


ARIC <- readRDS("data/ARIC_cleaned_HR.rds")
ARIC_comb <- ARIC[,c("SBP","DBP","SEX","AGE","HRX","id","TP","race","BMI","SMK","DM","TC","HDL","LDL","HR","CHOLMED","FG")] %>% 
  mutate(cohort = "ARIC",
         visit = TP,
         TP = paste("aric",TP,sep = "")) 


comb_dat <- rbind(cardia, fhs, MESA_ALT, ARIC_comb) %>% 
  mutate(PP = SBP - DBP,
         MAP = DBP + 1/3*PP,
         AGE = round(AGE),
         TP = as.factor(TP),
         id = as.factor(id))

comb_dat_id <- rbind(cardia, fhs, MESA_ALT, ARIC_comb) %>% 
  mutate(PP = SBP - DBP,
         MAP = DBP + 1/3*PP,
         AGE = round(AGE),
         TP = as.factor(TP))

cardia$id %>% unique() %>% length()
fhs$id %>% unique() %>% length()
MESA_ALT$id %>% unique() %>% length()
ARIC_comb$id %>% unique() %>% length()


saveRDS(comb_dat, "data/comb_dat_HR.rds")
saveRDS(comb_dat_id, "data/comb_dat_id_HR.rds")

# ------------------------------------------


# comb_dat_id$SBP %>% plot()
# comb_dat_id$BMI %>% plot()
# comb_dat_id$TC %>% plot()
# comb_dat_id$DM %>% as.factor() %>% summary()
# comb_dat_id$race %>% as.factor() %>% summary()
# comb_dat_id$SMK %>% as.factor() %>% summary()

# test<- comb_dat_id[which(comb_dat_id$AGE > 65 & comb_dat_id$SEX==1),]

