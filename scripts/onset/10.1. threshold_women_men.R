
# presence vs. absence of SBP burden

sdseq <- paste("sd",seq(100,150,5),sep = "") # predictor names
signum <- function(x){x = ifelse(x < 0.001,"<0.001", ifelse(x < 0.01, sprintf("%.03f", x), ifelse(x < 0.06 ,sprintf("%.03f", x), sprintf("%.02f", x))))}

all_coefs <- function(model_list){
  map(model_list, function(model){
    new_coef_tib <- as.tibble(summary(model)$coefficients) %>% 
      mutate(term = rownames(summary(model)$coefficients)) %>% 
      dplyr::select(term, everything())
  }) %>%  bind_rows()}

# logistic model estimating odds ratio of late vs. early HF/MI/STROKE, with standardized SBP burden as predictor
# HF
sd_chf_m <- map(sdseq, function(burden){
  glm(late_chf ~ get(burden) + DBP + 
          AGE + BMI + DM + HRX + SMK + HDL + TC + race + cohort, 
      family = binomial("logit"),
      data = coxdat %>% filter(SEX==1 & chf==1))
})

sd_chf_f <- map(sdseq, function(burden){
  glm(late_chf ~ get(burden) + DBP + 
          AGE + BMI + DM + HRX + SMK + HDL + TC + race + cohort, 
      family = binomial("logit"),
      data = coxdat %>% filter(SEX==2 & chf==1))
})

#stroke

sd_strk_m <- map(sdseq, function(burden){
  glm(late_strk ~ get(burden) + DBP + 
        AGE + BMI + DM + HRX + SMK + HDL + TC + race + cohort, 
      family = binomial("logit"),
      data = coxdat %>% filter(SEX==1 & strk==1))
})

sd_strk_f <- map(sdseq, function(burden){
  glm(late_strk ~ get(burden) + DBP + 
        AGE + BMI + DM + HRX + SMK + HDL + TC + race + cohort, 
      family = binomial("logit"),
      data = coxdat %>% filter(SEX==2 & strk==1))
})

# MI

sd_mi_m <- map(sdseq, function(burden){
  glm(late_mi ~ get(burden) + DBP + 
        AGE + BMI + DM + HRX + SMK + HDL + TC + race + cohort, 
      family = binomial("logit"),
      data = coxdat %>% filter(SEX==1 & mi==1))
})

sd_mi_f <- map(sdseq, function(burden){
  glm(late_mi ~ get(burden) + DBP + 
        AGE + BMI + DM + HRX + SMK + HDL + TC + race + cohort, 
      family = binomial("logit"),
      data = coxdat %>% filter(SEX==2 & mi==1))
})


tab_chf_m <- all_coefs(sd_chf_m) %>% 
  filter(term == "get(burden)") %>%
  dplyr::select(Estimate, `Std. Error`, `Pr(>|z|)`) %>%
  mutate(SEX=1, cut = seq(100,150,5),
         hr = exp(Estimate),
         low = exp(Estimate - 1.96*`Std. Error`),
         up = exp(Estimate + 1.96*`Std. Error`),
         conf = paste(sprintf("%.02f",hr),"(",sprintf("%.02f",low),",",sprintf("%.02f",up),")",sep = ""),
         p = signum(`Pr(>|z|)`))

tab_chf_f <- all_coefs(sd_chf_f) %>% 
  filter(term == "get(burden)") %>%
  dplyr::select(Estimate, `Std. Error`, `Pr(>|z|)`) %>%
  mutate(SEX=2, cut = seq(100,150,5),
         hr = exp(Estimate),
         low = exp(Estimate - 1.96*`Std. Error`),
         up = exp(Estimate + 1.96*`Std. Error`),
         conf = paste(sprintf("%.02f",hr),"(",sprintf("%.02f",low),",",sprintf("%.02f",up),")",sep = ""),
         p = signum(`Pr(>|z|)`))


tab_strk_m <- all_coefs(sd_strk_m) %>% 
  filter(term == "get(burden)") %>%
  dplyr::select(Estimate, `Std. Error`, `Pr(>|z|)`) %>%
  mutate(SEX=1, cut = seq(100,150,5),
         hr = exp(Estimate),
         low = exp(Estimate - 1.96*`Std. Error`),
         up = exp(Estimate + 1.96*`Std. Error`),
         conf = paste(sprintf("%.02f",hr),"(",sprintf("%.02f",low),",",sprintf("%.02f",up),")",sep = ""),
         p = signum(`Pr(>|z|)`))


tab_strk_f <- all_coefs(sd_strk_f) %>% 
  filter(term == "get(burden)") %>%
  dplyr::select(Estimate, `Std. Error`, `Pr(>|z|)`) %>%
  mutate(SEX=2, cut = seq(100,150,5),
         hr = exp(Estimate),
         low = exp(Estimate - 1.96*`Std. Error`),
         up = exp(Estimate + 1.96*`Std. Error`),
         conf = paste(sprintf("%.02f",hr),"(",sprintf("%.02f",low),",",sprintf("%.02f",up),")",sep = ""),
         p = signum(`Pr(>|z|)`))


tab_mi_m <- all_coefs(sd_mi_m) %>% 
  filter(term == "get(burden)") %>%
  dplyr::select(Estimate, `Std. Error`, `Pr(>|z|)`) %>%
  mutate(SEX=1, cut = seq(100,150,5),
         hr = exp(Estimate),
         low = exp(Estimate - 1.96*`Std. Error`),
         up = exp(Estimate + 1.96*`Std. Error`),
         conf = paste(sprintf("%.02f",hr),"(",sprintf("%.02f",low),",",sprintf("%.02f",up),")",sep = ""),
         p = signum(`Pr(>|z|)`))


tab_mi_f <- all_coefs(sd_mi_f) %>% 
  filter(term == "get(burden)") %>%
  dplyr::select(Estimate, `Std. Error`, `Pr(>|z|)`) %>%
  mutate(SEX=2, cut = seq(100,150,5),
         hr = exp(Estimate),
         low = exp(Estimate - 1.96*`Std. Error`),
         up = exp(Estimate + 1.96*`Std. Error`),
         conf = paste(sprintf("%.02f",hr),"(",sprintf("%.02f",low),",",sprintf("%.02f",up),")",sep = ""),
         p = signum(`Pr(>|z|)`))

savetab_chf <- tab_chf_f %>% 
  dplyr::select(conf_f = conf, p_f = p, coef_f = Estimate, se_f = `Std. Error`) %>% 
  cbind(tab_chf_m %>% 
          dplyr::select(conf_m = conf, p_m = p, coef_m = Estimate, se_m = `Std. Error`))

savetab_strk <- tab_strk_f %>% 
  dplyr::select(conf_f = conf, p_f = p, coef_f = Estimate, se_f = `Std. Error`) %>% 
  cbind(tab_strk_m %>% 
          dplyr::select(conf_m = conf, p_m = p, coef_m = Estimate, se_m = `Std. Error`))

savetab_mi <- tab_mi_f %>% 
  dplyr::select(conf_f = conf, p_f = p, coef_f = Estimate, se_f = `Std. Error`) %>% 
  cbind(tab_mi_m %>% 
          dplyr::select(conf_m = conf, p_m = p, coef_m = Estimate, se_m = `Std. Error`))

write.csv(savetab_chf,"output/savetab_chf.csv")
write.csv(savetab_mi,"output/savetab_mi.csv")
write.csv(savetab_strk,"output/savetab_strk.csv")


