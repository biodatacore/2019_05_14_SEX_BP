library(dplyr)
library(magrittr)
library(ggplot2)
library(haven)
# fhs

dat <- readRDS("data/fhs_cleaned.rds")

fhs <- dat[,c("SBP","DBP","SEX","AGE","HRX","ID","TP")] %>% 
  mutate(id=ID, 
         cohort = "fhs",
         visit = TP,
         TP = paste("fhs",TP,sep = "")) %>% 
  dplyr::select(-ID) %>% 
  filter(!is.na(AGE) & !is.na(SEX) & !is.na(SBP) & !is.na(DBP) & !is.na(HRX))

rm(dat)

# MESA

library(haven)

MESA_ALT <- readRDS("data/mesa_cleaned.rds")
MESA_ALT <- MESA_ALT[,c("SBP","DBP","SEX","AGE","HRX","id","TP")] %>% 
  mutate(cohort = "MESA",
         SBP = SBP + 0.5, 
         DBP = DBP + 2.9,
         visit = TP,
         TP = paste("mesa",TP,sep = ""))  %>% 
  filter(!is.na(AGE) & !is.na(SEX) & !is.na(SBP) & !is.na(DBP) & !is.na(HRX))


# CARDIA

cardia <- readRDS("data/cardia_cleaned.rds")
cardia <- cardia[,c("SBP","DBP","SEX","AGE","HRX","id","TP")] %>% 
  mutate(cohort = "cardia",
         SBP = ifelse(TP == 20 | TP == 25, 3.74 + 0.96*SBP, SBP),
         DBP = ifelse(TP == 20 | TP == 25, 1.30 + 0.97*DBP, DBP),
         SBP = SBP + 2.6,
         DBP = DBP + 6.2,
         visit = TP + 1,
         TP = paste("cardia",TP,sep = ""))  %>% 
  filter(!is.na(AGE) & !is.na(SEX) & !is.na(SBP) & !is.na(DBP) & !is.na(HRX))



# ARIC 

ARIC_cleaned <- readRDS("data/ARIC_cleaned.rds")

ARIC_comb <- ARIC_cleaned[,c("SBP","DBP","SEX","AGE","HRX","id","TP")] %>% 
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
         id = as.factor(id))

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

