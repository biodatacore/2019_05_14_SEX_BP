library(tidyr)
library(magrittr)

dat <- readRDS("data/comb_clin")

# cardiovascular disease
cvdall <- glm(hardcvd ~ onsetgroup + SEX + AGE + SBP + TC + HDL + BMI + SMK + DM, family = binomial("logit"), data = dat) %>% summary()
cvdm <- glm(hardcvd ~ onsetgroup + AGE + SBP + TC + HDL + BMI + SMK + DM, family = binomial("logit"), data = dat %>% filter(SEX==1)) %>% summary()
cvdf <- glm(hardcvd ~ onsetgroup + AGE + SBP + TC + HDL + BMI + SMK + DM, family = binomial("logit"), data = dat %>% filter(SEX==2)) %>% summary()
cvdall <- cvdall$coefficients["onsetgroup",]
cvdm <- cvdm$coefficients["onsetgroup",]
cvdf <- cvdf$coefficients["onsetgroup",]

cvd <- rbind(cvdall, cvdf, cvdm) %>% 
  as.data.frame() %>% 
  mutate(coef=paste(substr(exp(Estimate),1,5),"(",substr(exp(Estimate-1.96*`Std. Error`),1,5),",",substr(exp(Estimate+1.96*`Std. Error`),1,5),")",sep = ""))

write.csv(chd, "output/chd.csv")
write.csv(cvd, "output/cvd.csv")


inter1 <- glm(hardchd ~ onsetgroup*SEX + AGE + SBP + TC + HDL + BMI + SMK + DM, family = binomial("logit"), data = dat) %>% summary()
inter2 <- glm(hardcvd ~ onsetgroup*SEX + AGE + SBP + TC + HDL + BMI + SMK + DM, family = binomial("logit"), data = dat) %>% summary()
inter1
inter2
