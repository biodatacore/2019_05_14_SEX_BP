library(reshape2)
library(data.table) # v1.9.5+
library(magrittr)
library(readr)
library(ggplot2)
library(tidyr)
library(purrr)
library(dplyr)

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

bp_progr <- dat %>% mutate(progr = ntile(rate, 2) -1)

comb_clin <- readRDS("data/comb_clin")


baselinedat <- comb_dat %>% 
  mutate(onsetage = ifelse(is.na(onsetage),0,onsetage)) %>%
  dplyr::select(pid,onsetage,gender=SEX) %>% unique() %>% 
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
  dplyr::select(pid, progr) %>%
  left_join(comb_clin, by =c("pid"="id")) 

saveRDS(progr, "data/progr.rds")
saveRDS(baselinedat, "data/baselinedat.rds")


progdat <- progr %>% filter(onsetgroup != "NO_HTN")

# coronary heart disease
chdall <- glm(hardchd ~ progr + SEX + AGE + SBP + TC + HDL + BMI + SMK + DM, family = binomial("logit"), data = progdat) %>% summary()
chdm <- glm(hardchd ~ progr + AGE + SBP + TC + HDL + BMI + SMK + DM, family = binomial("logit"), data = progdat %>% filter(SEX==1)) %>% summary()
chdf <- glm(hardchd ~ progr + AGE + SBP + TC + HDL + BMI + SMK + DM, family = binomial("logit"), data = progdat %>% filter(SEX==2)) %>% summary()
chdall <- chdall$coefficients["progr",]
chdm <- chdm$coefficients["progr",]
chdf <- chdf$coefficients["progr",]

chd <- rbind(chdall, chdf, chdm) %>% 
  as.data.frame() %>% 
  mutate(coef=paste(substr(exp(Estimate),1,5),"(",substr(exp(Estimate-1.96*`Std. Error`),1,5),",",substr(exp(Estimate+1.96*`Std. Error`),1,5),")",sep = ""))

# cardiovascular disease
cvdall <- glm(hardcvd ~ progr + SEX + AGE + SBP + TC + HDL + BMI + SMK + DM, family = binomial("logit"), data = progdat) %>% summary()
cvdm <- glm(hardcvd ~ progr + AGE + SBP + TC + HDL + BMI + SMK + DM, family = binomial("logit"), data = progdat %>% filter(SEX==1)) %>% summary()
cvdf <- glm(hardcvd ~ progr + AGE + SBP + TC + HDL + BMI + SMK + DM, family = binomial("logit"), data = progdat %>% filter(SEX==2)) %>% summary()
cvdall <- cvdall$coefficients["progr",]
cvdm <- cvdm$coefficients["progr",]
cvdf <- cvdf$coefficients["progr",]

cvd <- rbind(cvdall, cvdf, cvdm) %>% 
  as.data.frame() %>% 
  mutate(coef=paste(substr(exp(Estimate),1,5),"(",substr(exp(Estimate-1.96*`Std. Error`),1,5),",",substr(exp(Estimate+1.96*`Std. Error`),1,5),")",sep = ""))

write.csv(chd, "output/chd_progr.csv")
write.csv(cvd, "output/cvd_progr.csv")

inter1 <- glm(hardchd ~ progr*SEX + AGE + SBP + TC + HDL + BMI + SMK + DM, family = binomial("logit"), data = progr) %>% summary()
inter2 <- glm(hardcvd ~ progr*SEX + AGE + SBP + TC + HDL + BMI + SMK + DM, family = binomial("logit"), data = progr) %>% summary()
inter1
inter2



listVars <- c("progr")

tabledata <- baselinedat

tabledata$SEX <- as.factor(tabledata$gender)
tabledata$progr <- as.factor(tabledata$progr)

table1 <- CreateTableOne(vars = listVars, data = tabledata , strata = "SEX", testNonNormal = kruskal.test)
table1 <- print(table1)

write.csv(table1,"output/rapid_progr.csv")

sum(baselinedat$onsetage!=0, na.rm = T)
sum(baselinedat$onsetage!=0 & baselinedat$gender==2, na.rm = T)
