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

comb_dat_f <- readRDS("data/comb_dat_f.rds")
comb_dat_m <- readRDS("data/comb_dat_m.rds")

comb_dat <- rbind(comb_dat_f, comb_dat_m)

dat <- matrix(nrow = length(unique(comb_dat[which(comb_dat$visit==1),]$pid)), ncol = 2) %>% as.data.frame()
colnames(dat) <- c("id","rate")
dat$id <- unique(comb_dat[which(comb_dat$visit==1),]$pid) %>% as.character()

for (i in unique(comb_dat$pid)) {
  pt <- comb_dat[which(comb_dat$pid == i),]
  where1 <- pt$visit==max(pt$visit, na.rm = T)
  where2 <- pt$visit==min(pt$visit, na.rm = T)
  rate <- (pt$SBP[where1] - pt$SBP[where2])/(pt$AGE[where1] - pt$AGE[where2])
  dat[which(dat$id==i),2] <- rate
}

bp_progr <- dat

comb_clin <- readRDS("data/comb_clin") %>% filter(!is.na(AGE))

meandata <- comb_dat %>%
  group_by(pid) %>% 
  dplyr::summarise(meanAGE = mean(AGE, na.rm = T),
                   meanSBP = mean(SBP, na.rm = T),
                   everHRX = max(HRX, na.rm = T),
                   meanTC = mean(TC, na.rm = T),
                   meanHDL = mean(HDL, na.rm = T),
                   meanBMI = mean(BMI, na.rm = T),
                   everSMK = max(SMK, na.rm = T),
                   everDM = max(DM, na.rm = T))

baselinedat <- comb_dat %>% 
  mutate(onsetage = ifelse(is.na(onsetage), 0, onsetage)) %>%
  dplyr::select(pid, onsetage, gender=SEX) %>% unique() %>% 
  left_join(bp_progr, by = c("pid"="id")) 

baselineage <- matrix(nrow = nrow(baselinedat), ncol = 7) %>% as.data.frame()
colnames(baselineage) <- c("pid","AGE","SBP","DBP","PP","MAP","HRX")
baselineage$pid <- baselinedat$pid

for (i in unique(baselineage$pid)) {
  pt <- comb_dat[which(comb_dat$pid == i),]
  where1 <- pt$visit==min(pt$visit, na.rm = T)
  AGE <- pt$AGE[where1]
  SBP <- pt$SBP[where1]
  DBP <- pt$DBP[where1]
  PP <- pt$PP[where1]
  MAP <- pt$MAP[where1]
  HRX <- pt$HRX[where1]
  
  baselineage[which(baselineage$pid==i),2] <- AGE
  baselineage[which(baselineage$pid==i),3] <- SBP
  baselineage[which(baselineage$pid==i),4] <- DBP
  baselineage[which(baselineage$pid==i),5] <- PP
  baselineage[which(baselineage$pid==i),6] <- MAP
  baselineage[which(baselineage$pid==i),7] <- HRX
  
}

baselinedat <- baselinedat %>% left_join(baselineage, by = "pid")

progr <- baselinedat %>%
  dplyr::select(pid, rate) %>%
  left_join(comb_clin, by =c("pid"="id")) %>%
  left_join(meandata, by = "pid") %>%
  mutate(onsetgroup = as.character(onsetgroup)) %>%
  mutate_all(function(x){ifelse(x == "-Inf"|x == "Inf",NA,x)})

rapidthreshold <- mean(filter(progr, onsetgroup == "NO_HTN")$rate, na.rm=T) + 2*sd(filter(progr, onsetgroup == "NO_HTN")$rate, na.rm = T)
rapidthreshold1 <- mean(filter(progr, onsetgroup == "NO_HTN" & SEX==1)$rate, na.rm=T) + 2*sd(filter(progr, onsetgroup == "NO_HTN"  & SEX==1)$rate, na.rm = T)
rapidthreshold2 <- mean(filter(progr, onsetgroup == "NO_HTN" & SEX==2)$rate, na.rm=T) + 2*sd(filter(progr, onsetgroup == "NO_HTN"  & SEX==2)$rate, na.rm = T)

# progr <- progr %>% mutate(progr = ifelse((SEX==1&rate >= rapidthreshold1)|(SEX==2&rate >= rapidthreshold2),1,0))
progr <- progr %>% mutate(progr = ifelse(rate >= rapidthreshold ,1,0))

saveRDS(progr, "data/progr.rds")
saveRDS(baselinedat, "data/baselinedat.rds")

baselinedat <- readRDS("data/baselinedat.rds") %>% 
  left_join(comb_dat %>% dplyr::select(pid,cohort) %>% mutate(pid = as.character(pid)) %>% unique(), by = c("pid"="pid")) %>%
  mutate(onsetage = ifelse(onsetage == 0 , NA, onsetage)) 
baselinedat$cohort %>% as.factor()%>% summary()

baselinedat$pid %>% duplicated() %>% sum()
library(tableone)
library(officer)
library(flextable)

listVars <- c("AGE","SBP","DBP","MAP","PP","HRX","onsetage","cohort")

tabledata <- baselinedat

tabledata$SEX <- as.factor(tabledata$gender)
tabledata$HRX <- as.factor(tabledata$HRX)
tabledata$cohort <- as.factor(tabledata$cohort)

table1 <- CreateTableOne(vars = listVars, data = tabledata , strata = "SEX", testNonNormal = kruskal.test)
table1 <- print(table1)

write.csv(table1, "output/table1.csv")

tabledata$onsetage %>% summary()

tabledata$onsetage %>% length()

tabledata[which(!is.na(tabledata$onsetage)),]$SEX %>% as.factor() %>% summary()

sum(comb_dat$category_3!="NO_HTN" & comb_dat$SEX==2)

baselinedat$gender %>% as.factor() %>% summary()

