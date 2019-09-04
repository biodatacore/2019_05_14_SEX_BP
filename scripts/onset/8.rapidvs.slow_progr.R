library(reshape2)
library(data.table) # v1.9.5+
library(magrittr)
library(readr)
library(ggplot2)
library(tidyr)
library(purrr)
library(dplyr)

progr <- readRDS("data/progr.rds")

progdat <- progr # %>% filter(onsetgroup != "NO_HTN")
progr$onsetgroup
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

write.csv(cvd, "output/cvd_progr.csv")

inter2 <- glm(hardcvd ~ progr*SEX + AGE + SBP + TC + HDL + BMI + SMK + DM, family = binomial("logit"), data = progr) %>% summary()
inter2


baselinedat <- readRDS("data/baselinedat.rds")

listVars <- c("progr")

tabledata <- baselinedat

tabledata$SEX <- as.factor(tabledata$gender)
tabledata$progr <- as.factor(tabledata$progr)

table1 <- CreateTableOne(vars = listVars, data = tabledata , strata = "SEX", testNonNormal = kruskal.test)
table1 <- print(table1)

write.csv(table1,"output/rapid_progr.csv")

sum(baselinedat$onsetage!=0, na.rm = T)
sum(baselinedat$onsetage!=0 & baselinedat$gender==2, na.rm = T)



