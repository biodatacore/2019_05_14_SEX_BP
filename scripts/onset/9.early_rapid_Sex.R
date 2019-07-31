comb_clin <- readRDS("data/comb_clin")
progr <- readRDS("data/progr.rds")

dat <- comb_clin %>% 
  left_join(progr[,c("pid","progr")], by =c("id"="pid")) %>%
  mutate(rapid = ifelse(progr ==1, 1, 0))

early_rapid <- glm(rapid ~ SEX + AGE + SBP, family = binomial("logit"), data = dat) %>% summary()

early_rapid <- early_rapid$coefficients %>%
  as.data.frame() %>% 
  mutate(term = rownames(.)) %>%
  mutate(coef=paste(substr(exp(Estimate),1,5),"(",substr(exp(Estimate-1.96*`Std. Error`),1,5),",",substr(exp(Estimate+1.96*`Std. Error`),1,5),")",sep = "")) %>%
  dplyr::select(term, everything(.))

write.csv(early_rapid,"output/early_rapid.csv")


# cardiovascular disease
cvdall <- glm(hardcvd ~ rapid + SEX + AGE + SBP + TC + HDL + BMI + SMK + DM, family = binomial("logit"), data = dat) %>% summary()
cvdm <- glm(hardcvd ~ rapid + AGE + SBP + TC + HDL + BMI + SMK + DM, family = binomial("logit"), data = dat %>% filter(SEX==1)) %>% summary()
cvdf <- glm(hardcvd ~ rapid + AGE + SBP + TC + HDL + BMI + SMK + DM, family = binomial("logit"), data = dat %>% filter(SEX==2)) %>% summary()
cvdall <- cvdall$coefficients["rapid",]
cvdm <- cvdm$coefficients["rapid",]
cvdf <- cvdf$coefficients["rapid",]

cvd <- rbind(cvdall, cvdf, cvdm) %>% 
  as.data.frame() %>% 
  mutate(coef=paste(substr(exp(Estimate),1,5),"(",substr(exp(Estimate-1.96*`Std. Error`),1,5),",",substr(exp(Estimate+1.96*`Std. Error`),1,5),")",sep = ""))





