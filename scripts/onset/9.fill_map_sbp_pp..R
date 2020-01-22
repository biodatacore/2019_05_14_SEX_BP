library(reshape2)
library(data.table) # v1.9.5+
library(matrixStats)
library(magrittr)
library(testthat) # install.packages("testthat")
library(readr)
library(ggplot2)
library(tidyr)
library(purrr)
library(dplyr)
library(segmented) # install.packages("segmented")
library(nlme)
library(haven)
library(boot)

# baseline and followup data

comb_clin <- readRDS("data/comb_clin.rds")

# multiple BP data

comb_dat_id <- readRDS("data/comb_dat_id.rds") %>% 
  filter(!is.na(AGE)) %>% 
  mutate(SBP = ifelse(HRX==1, SBP-10, SBP), # in survival analysis, no imputation on BP
         DBP = ifelse(HRX==1, DBP-5, DBP), 
         AGE_3 = ifelse(AGE <=45, 1, ifelse(AGE <55,2, ifelse(AGE< 65,3,4))),
         outcome = SBP,
         id = ID) %>%
  filter(!is.na(outcome)) 

comb_dat <- comb_dat_id %>% mutate(pid = id) %>% na.omit()

# exclude participants with only one visit 

num_visit <-
  comb_dat %>%
  group_by(id) %>%
  dplyr::summarise(n = n()) %>%
  filter(n < 2)

comb_dat <- comb_dat %>% anti_join(num_visit, by = "id")

# linear interpolation over 3 months between measures

newseq <- NULL

for (i in unique(comb_dat$pid)) {
  
  print(paste(i, sep = ""))
  pt <- comb_dat[which(comb_dat$pid == i),]
  pt <- pt[order(pt$AGE),]
  
  fill_seq <- approx(pt$AGE, pt$outcome, xout = seq(min(pt$AGE),max(pt$AGE), 3/12), method="linear") %>% 
    as.data.frame() %>%
    mutate(AGE = x, SBP = y, id = pt$pid[1])
  
  newseq <- rbind(newseq, fill_seq) %>% distinct()}

saveRDS(newseq,"data/newseq.rds")


# calculate SBP burden (zeroed values and mean values)

newseq <- readRDS("data/newseq.rds")
newseq$id %>% unique() %>% length()
seq_sbp <- newseq %>%
  left_join(comb_clin %>% dplyr::select(id, hardcvd_age, dth_age, SEX) %>% transform(minage = pmin(hardcvd_age, dth_age, na.rm = T)), by = "id") %>% 
  mutate(SBP_t90 = ifelse(SBP < 90, 0, SBP-90), 
         SBP_t95 = ifelse(SBP < 95, 0, SBP-95), # SBP values were zeroed at certain thresholds, with values at or below the threshold set equal to 0 and values above the threshold expressed in millimeters of mercury (mm Hg) above the threshold
         SBP_t100 = ifelse(SBP < 100, 0, SBP-100),
         SBP_t105 = ifelse(SBP < 105, 0, SBP-105),
         SBP_t110 = ifelse(SBP < 110, 0, SBP-110),
         SBP_t115 = ifelse(SBP < 115, 0, SBP-115),
         SBP_t120 = ifelse(SBP < 120, 0, SBP-120),
         SBP_t125 = ifelse(SBP < 125, 0, SBP-125),
         SBP_t130 = ifelse(SBP < 130, 0, SBP-130),
         SBP_t135 = ifelse(SBP < 135, 0, SBP-135),
         SBP_t140 = ifelse(SBP < 140, 0, SBP-140),
         SBP_t145 = ifelse(SBP < 145, 0, SBP-145),
         SBP_t150 = ifelse(SBP < 150, 0, SBP-150)
         ) %>%
  filter(AGE < minage) %>%
  group_by(id) %>%
  dplyr::summarise(SBP_burden90 = mean(SBP_t90, na.rm = T), # mean of zeroed values (i.e. SBP > xx mmHg burden)
                   SBP_burden95 = mean(SBP_t95, na.rm = T),
                   SBP_burden100 = mean(SBP_t100, na.rm = T),
                   SBP_burden105 = mean(SBP_t105, na.rm = T),
                   SBP_burden110 = mean(SBP_t110, na.rm = T),
                   SBP_burden115 = mean(SBP_t115, na.rm = T),
                   SBP_burden120 = mean(SBP_t120, na.rm = T),
                   SBP_burden125 = mean(SBP_t125, na.rm = T),
                   SBP_burden130 = mean(SBP_t130, na.rm = T),
                   SBP_burden135 = mean(SBP_t135, na.rm = T),
                   SBP_burden140 = mean(SBP_t140, na.rm = T),
                   SBP_burden145 = mean(SBP_t145, na.rm = T),
                   SBP_burden150 = mean(SBP_t150, na.rm = T),
                   
                   SBP_average = mean(SBP, na.rm = T), # time-weighted average SBP
                   
                   n = n()) %>%
  filter(n >= 2) %>%
  left_join(comb_clin, by = "id") %>%
  mutate(hardcvdtime = hardcvd_age - AGE,
         dthtime = dth_age - AGE) %>% 
  na.omit()

head(seq_sbp)
saveRDS(seq_sbp,"data/seq_sbp.rds")
