library(haven)
library(MASS)
library(reshape2)
library(reshape)
library(dplyr)
library(magrittr)
library(survival)

accord_key <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/ACCORD_2017b_2/Main_Study/3-Data_Sets-Analysis/3a-Analysis_Data_Sets/csv/accord_key.csv")
cvdoutcomes <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/ACCORD_2017b_2/Main_Study/3-Data_Sets-Analysis/3a-Analysis_Data_Sets/csv/cvdoutcomes.csv")
bps_accord <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/ACCORD_2017b_2/Main_Study/3-Data_Sets-Analysis/3a-Analysis_Data_Sets/csv/bloodpressure.csv")
lipid_accord <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/ACCORD_2017b_2/Main_Study/3-Data_Sets-Analysis/3a-Analysis_Data_Sets/csv/lipids.csv")
cholmed_accord <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/ACCORD_2017b_2/Main_Study/3-Data_Sets-Analysis/3a-Analysis_Data_Sets/csv/concomitantmeds.csv")
labs_accord <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/ACCORD_2017b_2/Main_Study/3-Data_Sets-Analysis/3a-Analysis_Data_Sets/csv/otherlabs.csv")

accord_data <- accord_key %>% 
  mutate(bp_trt = ifelse(grepl('Intensive BP', treatment), 1, ifelse(grepl('Standard BP', treatment),0,NA))) %>%
  left_join(labs_accord %>% filter(Visit == "BLR") %>% dplyr::select(-Visit), by = "MaskID") %>%
  left_join(lipid_accord %>% filter(Visit == "BLR") %>% dplyr::select(-Visit), by = "MaskID") %>%
  left_join(cholmed_accord %>% filter(Visit == "BLR") %>% dplyr::select(-Visit), by = "MaskID") %>%
  left_join(cvdoutcomes, by = "MaskID") %>%
  left_join(bps_accord %>% filter(Visit == "BLR"), by = "MaskID") %>%
  mutate(SEX = 1 + female,
         po = ifelse(censor_po == 0, 1, 0),
         cvddeath = ifelse(censor_cm == 0, 1, 0),
         cvddeathtime = fuyrs_cm,
         chd = ifelse(censor_maj == 0, 1, 0),
         nfchd = ifelse(censor_maj == 0 & (type_maj == "Angina"|type_maj == "MI"), 1, 0),
         chddeath = ifelse(censor_maj == 0 & type_maj == "CVD Death", 1, 0),
         chdtime = fuyrs_maj,
         nfchdtime = fuyrs_maj,
         chddeathtime = fuyrs_maj,
         chf = ifelse(censor_chf == 0, 1, 0),
         chftime = fuyrs_chf,
         potime = fuyrs_po,
         hd = ifelse(chf==1|chd==1,1,0),
         death = ifelse(censor_tm==0,1,0),
         deathtime = fuyrs_tm) %>% 
  transform(hdtime = pmin(chdtime, chftime, na.rm = T)) %>%
  filter(!is.na(bp_trt))


test<-accord_data %>% filter(chddeath==1)
accord_data$chddeathtime %>% hist()


saveRDS(accord_data, "data/accord_data.rds")
