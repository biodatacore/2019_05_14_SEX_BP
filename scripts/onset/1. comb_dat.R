library(dplyr)
library(magrittr)
library(ggplot2)
library(haven)

varlist <- c("SBP","DBP","SEX","AGE","HRX","ID","TP","race","TC","HDL","BMI","SMK","DM")

# fhs

dat <- readRDS("data/fhs_cleaned.rds")

fhs <- dat[,varlist] %>% 
  mutate(id=ID, 
         cohort = "fhs",
         visit = TP,
         TP = paste("fhs",TP,sep = "")) %>% 
  dplyr::select(-id) %>% 
  filter(!is.na(AGE) & !is.na(SEX) & !is.na(SBP) & !is.na(DBP) & !is.na(HRX))

rm(dat)

# MESA

library(haven)

MESA_ALT <- readRDS("data/mesa_cleaned.rds") %>% mutate(ID = id)
MESA_ALT <- MESA_ALT[,varlist] %>% 
  mutate(cohort = "MESA",
         SBP = SBP + 0.5, 
         DBP = DBP + 2.9,
         visit = TP,
         TP = paste("mesa",TP,sep = ""))  %>% 
  filter(!is.na(AGE) & !is.na(SEX) & !is.na(SBP) & !is.na(DBP) & !is.na(HRX))


# CARDIA

cardia <- readRDS("data/cardia_cleaned.rds") %>% mutate(ID = id)
cardia <- cardia[,varlist] %>% 
  mutate(cohort = "cardia",
         SBP = ifelse(TP == max(cardia$TP) | TP == max(cardia$TP)-1, 3.74 + 0.96*SBP, SBP),
         DBP = ifelse(TP == max(cardia$TP) | TP == max(cardia$TP)-1, 1.30 + 0.97*DBP, DBP),
         SBP = SBP + 2.6,
         DBP = DBP + 6.2,
         visit = TP + 1,
         TP = paste("cardia",TP,sep = ""))  %>% 
  filter(!is.na(AGE) & !is.na(SEX) & !is.na(SBP) & !is.na(DBP) & !is.na(HRX))



# ARIC 

ARIC_cleaned <- readRDS("data/ARIC_cleaned.rds") %>% mutate(ID = id)

ARIC_comb <- ARIC_cleaned[,varlist] %>% 
  mutate(SBP = SBP + 2.6,
         DBP = DBP + 6.2,
         cohort = "ARIC",
         visit = TP,
         TP = paste("aric",TP,sep = ""))  %>% 
  filter(!is.na(AGE) & !is.na(SEX) & !is.na(SBP) & !is.na(DBP) & !is.na(HRX))



comb_dat <- rbind(cardia, fhs, MESA_ALT, ARIC_comb) %>% 
  mutate(SBP = ifelse(HRX==1, SBP + 10 , SBP),
         DBP = ifelse(HRX==1, DBP + 5 , DBP),
         PP = SBP - DBP,
         MAP = DBP + 1/3*PP,
         AGE = round(AGE),
         TP = as.factor(TP),
         id = as.factor(ID))

comb_dat_id <- rbind(cardia, fhs, MESA_ALT, ARIC_comb) %>% 
  mutate(SBP = ifelse(HRX==1, SBP + 10 , SBP),
         DBP = ifelse(HRX==1, DBP + 5 , DBP),
         PP = SBP - DBP,
         MAP = DBP + 1/3*PP,
         AGE = round(AGE),
         TP = as.factor(TP))

saveRDS(comb_dat, "data/comb_dat.rds")
saveRDS(comb_dat_id, "data/comb_dat_id.rds")

# ------------------------------------------




# comb_progressor <- rbind(cardia, fhs, MESA_ALT, ARIC_comb) %>% 
#   mutate(SBP = ifelse(HRX==1, SBP + 10 , SBP),
#          DBP = ifelse(HRX==1, DBP + 5 , DBP),
#          PP = SBP - DBP,
#          MAP = DBP + 1/3*PP,
#          AGE = round(AGE),
#          TP = as.factor(TP),
#          id = as.factor(id))

