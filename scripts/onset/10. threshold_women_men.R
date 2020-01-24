library(survival)
library(tibble)

# dataset for survival analysis

coxdat <- 
  readRDS("data/seq_sbp.rds") %>% 
  mutate(hardcvdtime = hardcvd_age - AGE,
         dthtime = dth_age - AGE,
         chf_age = chftime + AGE,
         strk_age = strktime + AGE,
         mi_age = mitime + AGE,
         HRX = ifelse(HRX==0,0,1),
         sexhtn = ifelse((SEX == 1 & SBP >= 140)|(SEX == 2 & SBP >= 115),1,0),
         htn110 = ifelse(SBP>=110,1,0),
         htn120 = ifelse(SBP>=120,1,0),
         htn130 = ifelse(SBP>=130,1,0),
         htn140 = ifelse(SBP>=140,1,0),
         htngroup = ifelse(SBP<120,"0",ifelse(SBP<130,"1",ifelse(SBP<140,"2",ifelse(SBP < 160,"3","4")))),
         race = as.character(race),
         SBP_q = ntile(SBP,100),
         burden130_0 = ifelse(SBP_burden130>0,1,0),
         burden130_1 = ifelse(SBP_burden130>=1,1,0),
         burden130_5 = ifelse(SBP_burden130>=5,1,0),
         burden130_10 = ifelse(SBP_burden130>=10,1,0),
         burden130_15 = ifelse(SBP_burden130>=15,1,0),
         burden130_20 = ifelse(SBP_burden130>=20,1,0),
         burden90 = ifelse(SBP_burden90>0,1,0),
         burden95 = ifelse(SBP_burden95>0,1,0),
         burden100 = ifelse(SBP_burden100>0,1,0),
         burden105 = ifelse(SBP_burden105>0,1,0),
         burden110 = ifelse(SBP_burden110>0,1,0),
         burden115 = ifelse(SBP_burden115>0,1,0),
         burden120 = ifelse(SBP_burden120>0,1,0),
         burden125 = ifelse(SBP_burden125>0,1,0),
         burden130 = ifelse(SBP_burden130>0,1,0),
         burden135 = ifelse(SBP_burden135>0,1,0),
         burden140 = ifelse(SBP_burden140>0,1,0),
         burden145 = ifelse(SBP_burden145>0,1,0),
         burden150 = ifelse(SBP_burden150>0,1,0),
         sd90 = SBP_burden90/sd(SBP_burden90, na.rm = T),
         sd95 = SBP_burden95/sd(SBP_burden95, na.rm = T),
         sd100 = SBP_burden100/sd(SBP_burden100, na.rm = T),
         sd105 = SBP_burden105/sd(SBP_burden105, na.rm = T),
         sd110 = SBP_burden110/sd(SBP_burden110, na.rm = T),
         sd115 = SBP_burden115/sd(SBP_burden115, na.rm = T),
         sd120 = SBP_burden120/sd(SBP_burden120, na.rm = T),
         sd125 = SBP_burden125/sd(SBP_burden125, na.rm = T),
         sd130 = SBP_burden130/sd(SBP_burden130, na.rm = T),
         sd135 = SBP_burden135/sd(SBP_burden135, na.rm = T),
         sd140 = SBP_burden140/sd(SBP_burden140, na.rm = T),
         sd145 = SBP_burden145/sd(SBP_burden145, na.rm = T),
         sd150 = SBP_burden150/sd(SBP_burden150, na.rm = T)
  ) %>% 
  na.omit() %>% 
  mutate(early_chf = ifelse(chf_age < 65 & chf==1, 1, 0),
         late_chf = ifelse(chf_age >= 65 & chf==1, 1, 0),
         early_strk = ifelse(strk_age < 65 & strk==1, 1, 0),
         late_strk = ifelse(strk_age >= 65 & strk==1, 1, 0),
         early_mi = ifelse(mi_age < 65 & mi==1, 1, 0),
         late_mi = ifelse(mi_age >= 65 & mi==1, 1, 0),
         early_cvd = ifelse(hardcvd_age < 65 & hardcvd==1, 1, 0),
         late_cvd = ifelse(hardcvd_age >= 65 & hardcvd==1, 1, 0)) %>% 
  filter(AGE >= 18)

# coxdat$AGE %>% summary()
# coxdat$AGE %>% quantile(c(0.333,0.667))
# coxdat$SBP %>% quantile(c(0.025,0.975))
# coxdat$agegroup %>% as.factor() %>% summary()
# coxdat$cohort %>% class()
# coxdat$SBP_burden110 %>% summary()
# coxdat[which(coxdat$SBP_burden3!=0),]$SBP_burden3 %>% summary()
# coxdat$cohort %>% as.factor() %>% summary()


saveRDS(coxdat,"data/coxdat.rds")

sbprange = seq(110,140,10)
itvlrange = 5:20

sbp_thresh <- paste("SBP_",sbprange,sep = "")
sbp_itvl <- paste("perSBP_",itvlrange,sep = "")

all_coefs <- function(model_list){
  map(model_list, function(model){
    new_coef_tib <- as.tibble(summary(model)$coefficients) %>% 
      mutate(term = rownames(summary(model)$coefficients)) %>% 
      dplyr::select(term, everything())
  }) %>% 
    bind_rows()}
all_inter <- function(model_list){
  map(model_list, function(model){
    new_coef_tib <- as.tibble((model)$`P(>|Chi|)`[2])
  }) %>% 
    bind_rows()}

signum <- function(x){x = ifelse(x < 0.001,"<0.001", ifelse(x < 0.01, sprintf("%.03f", x), ifelse(x < 0.06 ,sprintf("%.03f", x), sprintf("%.02f", x))))}

# SBP (restricted cubic spline) and CVD risk

library(scales)
library(rms)

md <- coxdat %>% mutate(SBP = SBP)
nohxchd <- md
nohxchd1 <- md %>% filter(SEX==1)
nohxchd2 <- md %>% filter(SEX==2)

nohxchd1$pred <- nohxchd1$SBP_average
nohxchd2$pred <- nohxchd2$SBP_average
nohxchd$pred <- nohxchd$SBP_average

nohxchd1$outcome <- nohxchd1$chf
nohxchd2$outcome <- nohxchd2$chf
nohxchd$outcome <- nohxchd$chf

nohxchd1$outcometime <- nohxchd1$chftime
nohxchd2$outcometime <- nohxchd2$chftime
nohxchd$outcometime <- nohxchd$chftime

low <- quantile(nohxchd$pred, 0.025)
up <- quantile(nohxchd$pred, 0.975)

ddist <- datadist(nohxchd1)
options(datadist="ddist")
ddist$limits$pred[2] <- 115

surv_sbp1 <- cph(Surv(outcometime,outcome) ~ rcs(pred,4) +DBP + 
                   AGE + BMI + DM + HRX + race + SMK + HDL + TC + cohort
                 , data = nohxchd1, x = TRUE, y = TRUE)
pred1 <- Predict(surv_sbp1, pred, ref.zero = TRUE, fun = exp, conf.int = 0.95)


ddist <- datadist(nohxchd2)
options(datadist="ddist")
ddist$limits$pred[2] <- 115


surv_sbp2 <- cph(Surv(outcometime,outcome) ~ rcs(pred,4) +DBP + 
                   AGE + BMI + DM + HRX + race + SMK + HDL + TC + cohort
                 , data = nohxchd2, x = TRUE, y = TRUE)
pred2 <- Predict(surv_sbp2, pred, ref.zero = TRUE, fun = exp, conf.int = 0.95)


inter_p <- anova(
  coxph(Surv(outcometime,outcome) ~ pred*SEX +DBP + 
          AGE + BMI + DM + HRX + race + SMK + HDL + TC + cohort
        , data = nohxchd),
  coxph(Surv(outcometime,outcome) ~ pred + SEX +DBP + 
          AGE + BMI + DM + HRX + race + SMK + HDL + TC + cohort
        , data = nohxchd),
  test = "LRT")$`P(>|Chi|)`[2]


ggplot() +
  geom_ribbon(data = pred1, aes(x = pred, ymin = lower, ymax = upper, fill = "blue2")) + 
  geom_ribbon(data = pred2, aes(x = pred, ymin = lower, ymax = upper, fill = "red2")) + 
  geom_line(data = pred1, aes(x = pred, y = yhat, color = "blue4")) + 
  geom_line(data = pred2, aes(x = pred, y = yhat, color = "red4")) + 
  scale_color_manual(name = "Sex",
                     values = c("blue4" = "#2166ac", "red4" = "#b2182b", "blue2" = "#d1e5f0", "red2"="#fddbc7"),
                     labels = c("blue4" = "Men", "red4" = "Women")) +
  scale_fill_manual(name = "Sex",
                    values = c("blue4" = "#2166ac", "red4" = "#b2182b", "blue2" = "#d1e5f0", "red2"="#fddbc7"),
                    labels = c("blue2" = "Men", "red2" = "Women")) +
  theme_bw() +
  theme(axis.title.x = element_text(color = "#434443",size =20,face="bold"),
        axis.title.y = element_text(color = "#434443",size =20,face="bold"),
        panel.border = element_rect(colour = "#434443", size=1, linetype=1),
        axis.text.y = element_text(colour = "#434443", size = 15),
        axis.text.x=element_text(colour="#434443",size= 15),
        plot.title = element_text(hjust = 0.5, size= 20,colour="black",face="bold"),
        legend.text = element_text(colour = "black", size = 17,face="bold"),
        legend.title = element_text(colour = "black", size = 17,face="bold"),
        legend.position = c(0.25,0.75),
        legend.background = element_rect(fill="white",
                                         size=0.2, 
                                         linetype="solid", 
                                         colour ="#434443")) +
  scale_x_continuous(name=c("Time-Weighted Average SBP"), limits = c(low,up)) +
  scale_y_continuous(name="Hazard Ratio (95% CI)") + 
  coord_cartesian(ylim = c(0.5, 2.5)) +
  annotate("text", x = 110, y = 1.5, 
           label = paste("P=",sprintf("%.03f",inter_p),sep = ""),
           # label = "P<0.001",
           size=5,color="#434443",fontface="bold")


# baseline characteristics for survival data

baselinedat <- coxdat
baselinedat$hardcvdtime %>% summary()
baselinedat$hardcvd %>% sum()

baselinedat$pid %>% duplicated() %>% sum()
library(tableone)
library(officer)
library(flextable)

listVars <- c("AGE","BMI","SBP","HRX","SMK","DM","TC","HDL","race","cohort","hardcvd","mi","chf","strk",
              "early_cvd","early_mi","early_chf","early_strk",
              "late_cvd","late_mi","late_chf","late_strk")

tabledata <- baselinedat

tabledata$SEX <- as.factor(tabledata$SEX)
tabledata$HRX <- as.factor(tabledata$HRX)
tabledata$SMK <- as.factor(tabledata$SMK)
tabledata$DM <- as.factor(tabledata$DM)
tabledata$race <- as.factor(tabledata$race)
tabledata$cohort <- as.factor(tabledata$cohort)
tabledata$hardcvd <- as.factor(tabledata$hardcvd)
tabledata$mi <- as.factor(tabledata$mi)
tabledata$chf <- as.factor(tabledata$chf)
tabledata$strk <- as.factor(tabledata$strk)
tabledata$early_cvd <- as.factor(tabledata$early_cvd)
tabledata$early_mi <- as.factor(tabledata$early_mi)
tabledata$early_chf <- as.factor(tabledata$early_chf)
tabledata$early_strk <- as.factor(tabledata$early_strk)
tabledata$late_cvd <- as.factor(tabledata$late_cvd)
tabledata$late_mi <- as.factor(tabledata$late_mi)
tabledata$late_chf <- as.factor(tabledata$late_chf)
tabledata$late_strk <- as.factor(tabledata$late_strk)

table1 <- CreateTableOne(vars = listVars, data = tabledata, strata = "SEX", testNonNormal = kruskal.test)
table1 <- print(table1)

write.csv(table1, "output/table1_bsl.csv")




burdenseq <- paste("burden",seq(100,150,5),sep = "")

all_coefs <- function(model_list){
  map(model_list, function(model){
    new_coef_tib <- as.tibble(summary(model)$coefficients) %>% 
      mutate(term = rownames(summary(model)$coefficients)) %>% 
      dplyr::select(term, everything())
  }) %>%  bind_rows()}


burden_m <- map(burdenseq, function(burden){
  coxph(Surv(hardcvdtime, hardcvd) ~ get(burden) + DBP + 
          AGE + BMI + DM + HRX + SMK + HDL + TC + race + cohort, data = coxdat %>% filter(SEX==1))
})

burden_f <- map(burdenseq, function(burden){
  coxph(Surv(hardcvdtime, hardcvd) ~ get(burden) + DBP + 
          AGE + BMI + DM + HRX + SMK + HDL + TC + race + cohort, data = coxdat %>% filter(SEX==2))
})

tab1 <- all_coefs(burden_m) %>% 
  filter(term == "get(burden)") %>%
  dplyr::select(coef, `se(coef)`, `Pr(>|z|)`) %>%
  mutate(SEX=1, cut = seq(100,150,5),
         hr = exp(coef),
         low = exp(coef - 1.96*`se(coef)`),
         up = exp(coef + 1.96*`se(coef)`),
         conf = paste(sprintf("%.02f",hr),"(",sprintf("%.02f",low),",",sprintf("%.02f",up),")",sep = ""),
         p = signum(`Pr(>|z|)`))

tab2 <- all_coefs(burden_f) %>% 
  filter(term == "get(burden)") %>%
  dplyr::select(coef, `se(coef)`, `Pr(>|z|)`) %>%
  mutate(SEX=2, cut = seq(100,150,5),
         hr = exp(coef),
         low = exp(coef - 1.96*`se(coef)`),
         up = exp(coef + 1.96*`se(coef)`),
         conf = paste(sprintf("%.02f",hr),"(",sprintf("%.02f",low),",",sprintf("%.02f",up),")",sep = ""),
         p = signum(`Pr(>|z|)`))

inter_p <- map(burdenseq, function(burden){
  anova(coxph(Surv(hardcvdtime, hardcvd) ~ get(burden) + SEX + DBP + 
                AGE + BMI + DM + HRX + SMK + HDL + TC + race + cohort, data = coxdat),
        coxph(Surv(hardcvdtime, hardcvd) ~ get(burden)*SEX + DBP + 
                AGE + BMI + DM + HRX + SMK + HDL + TC + race + cohort, data = coxdat),
        test = "LRT")$`P(>|Chi|)`[2]
})

savetab <- tab2 %>% 
  dplyr::select(conf_f = conf, p_f = p, coef_f = coef, se_f = `se(coef)`) %>% 
  cbind(tab1 %>% 
          dplyr::select(conf_m = conf, p_m = p, coef_m = coef, se_m = `se(coef)`)) %>%
  mutate(p_inter = signum(inter_p %>% unlist()))
savetab
write.csv(savetab, "output/savetab_burden.csv")


