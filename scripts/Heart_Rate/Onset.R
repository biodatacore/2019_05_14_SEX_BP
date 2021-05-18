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
library(nlme)
library(haven)
library(boot)
library(splines)

## read combined dataset ----

comb_dat <- readRDS("data/comb_dat_id_HR.rds") %>% 
  mutate(race = as.character(race)) %>% 
  dplyr::select(c("AGE","SEX","HR","BMI","SBP","HRX","SMK","DM","TC","HDL","race","cohort","id","visit")) %>%
  na.omit() %>%
  filter(cohort!="cardia")

comb_dat %<>% mutate(AGE = round(AGE),
                     nid = as.numeric(as.factor(id)))
head(comb_dat)

# comb_dat$nid %>% unique() %>% length()
# comb_dat$id %>% unique() %>% length()
# comb_dat$cohort %>% as.factor() %>% summary()

comb_clin <- readRDS("data/comb_clin.rds")
comb_clin$cohort %>% as.factor() %>% summary()

healthy_hr <- 70

## Filter out those who had RHR<70 at baseline --------------------------

baseline_hr <-
  comb_dat %>%
  filter(visit == 1) %>% 
  distinct(HR, AGE, id) %>%
  filter(HR < healthy_hr)
baseline_hr$id %>% unique() %>% length()

comb_dat %<>%
  anti_join(baseline_hr, by = 'id') 
comb_dat$id %>% unique() %>% length()

comb_dat %<>%
  left_join(comb_clin %>% dplyr::select(id, hardcvd_age, hardcvd), by = "id") %>%
  filter(hardcvd_age > AGE)
comb_dat$id %>% unique() %>% length()

rm(baseline_hr)

## Filter out unmeasured exams and exams those never diagnosed with RHR<70  --------

onset <- comb_dat %>% 
  mutate(onsetage = ifelse(HR < healthy_hr, AGE, NA)) %>%
  setDT() %>%
  dcast(id ~ visit, value.var = c("onsetage")) 
onset$id %>% unique() %>% length()
onset$onsetage <- apply(dplyr::select(onset, -id), 1, FUN = min, na.rm = T)

onset %<>% 
  mutate(onsetage = ifelse(onsetage == Inf, NA, onsetage))

## read covariates at last visit

covdat <- comb_dat %>% 
  group_by(id) %>%
  dplyr::summarise(minAGE = dplyr::first(AGE),
                   AGE = dplyr::last(AGE),
                   SEX = dplyr::last(SEX),
                   firstHR = dplyr::first(HR),
                   HR = dplyr::last(HR),
                   BMI = dplyr::last(BMI),
                   SBP = dplyr::last(SBP),
                   HRX = dplyr::last(HRX),
                   DM = dplyr::last(DM),
                   TC = dplyr::last(TC),
                   HDL = dplyr::last(HDL),
                   SMK = dplyr::last(SMK),
                   race = dplyr::last(race),
                   cohort = dplyr::last(cohort),
                   hardcvd = dplyr::last(hardcvd),
                   hardcvd_age = dplyr::last(hardcvd_age),
                   n = n()) %>%
  mutate(duration = hardcvd_age - minAGE) %>% na.omit()
# onset$id %>% unique() %>% length()
# covdat$duration %>% summary()

hr_dat <- onset %>% 
  dplyr::select(id, onsetage) %>%
  mutate(onsetage = ifelse(is.na(onsetage),0,onsetage)) %>%
  left_join(covdat, by = "id") %>%
  mutate(onsetgroup = ifelse(onsetage==0,"NO_ONSET",ifelse(onsetage < 50,"1",ifelse(onsetage < 60,"2",ifelse(onsetage < 70,"3","4"))))) %>%
  mutate(hardcvdtime = hardcvd_age - AGE,
         cate_duration = hardcvd_age - onsetage) %>%
  na.omit()

# hr_dat$id %>% unique() %>% length()

hr_dat %<>% filter(onsetgroup!="NO_ONSET")
# hr_dat$id %>% unique() %>% length()
# hr_dat$cate_duration %>% summary()

hr_dat$onsetgroup <- relevel(factor(hr_dat$onsetgroup), ref = "4")

# hr_dat$onsetgroup %>% summary()
# hr_dat$onsetage %>% summary()
# hr_dat$onsetage %>%  sd()
# hr_dat$hardcvdtime %>% summary()
# hr_dat$hardcvdtime %>% sd()
# hr_dat$hardcvd %>% as.factor() %>% summary()
# hr_dat$minAGE %>% summary()
# hr_dat$minAGE %>% sd()
# hr_dat$SEX %>% as.factor() %>% summary()

heart_rate <- glm(hardcvd ~ onsetgroup + 
                  AGE + SEX + BMI + SBP + SMK + HRX + DM + TC + race + cohort, 
                  family = binomial("logit"),
                  data = hr_dat) %>% summary()
heart_rate

# odds ratio of early vs late attaining RHR < 70
glm(hardcvd ~ onsetgroup + 
      AGE + SEX + BMI + SBP + HRX + SMK + DM + TC + race + cohort,
    family = binomial("logit"),
    data = hr_dat %>% filter(cohort =="ARIC")) %>% summary()


#1
glm(hardcvd ~ onsetgroup + 
       firstHR + AGE + SEX + BMI + SBP + HRX + SMK + DM + TC + race + cohort, 
    family = binomial("logit"),
    data = hr_dat) %>% summary()

#2
glm(hardcvd ~ onsetgroup + 
      hardcvd_age + SEX + BMI + SBP + HRX + SMK + DM + TC + race + cohort, 
    family = binomial("logit"),
    data = hr_dat) %>% summary()

#3
glm(hardcvd ~ onsetgroup + 
      duration + minAGE + SEX + BMI + SBP + HRX + SMK + DM + TC + race + cohort, 
    family = binomial("logit"),
    data = hr_dat) %>% summary()

#4
anova(
  glm(hardcvd ~ onsetgroup + 
        AGE + SEX + BMI + SBP + HRX + DM + TC + race + cohort, 
      family = binomial("logit"),
      data = hr_dat),
  glm(hardcvd ~ onsetgroup*race + 
        AGE + SEX + BMI + SBP + HRX + DM + TC + race + cohort, 
      family = binomial("logit"),
      data = hr_dat),
  test = "LRT")

## ---- trajectory figure --------

comb_plot_dat <- comb_dat %>%
  left_join(hr_dat %>% dplyr::select(id, onsetage, onsetgroup), by = "id") %>%
  filter(!is.na(onsetgroup)) %>%
  mutate(cohort = ifelse(cohort == "fhs","fhs","other"),
         race = ifelse(race == "1","1","other"))

filter(comb_plot_dat, HR<50) %>% nrow()
comb_plot_dat%>% nrow()
comb_plot_dat$AGE %>% quantile(c(0.005,0.995))

limit1 <- filter(comb_plot_dat, onsetgroup == "1")$AGE %>% quantile(c(0.025,0.975))
limit2 <- filter(comb_plot_dat, onsetgroup == "2")$AGE %>% quantile(c(0.025,0.975))
limit3 <- filter(comb_plot_dat, onsetgroup == "3")$AGE %>% quantile(c(0.025,0.975))
limit4 <- filter(comb_plot_dat, onsetgroup == "4")$AGE %>% quantile(c(0.025,0.975))

library(lme4)

md1 <- lmer(HR ~ AGE + I(AGE^2) + I(AGE^3) + (1|id), data = comb_plot_dat %>% filter(onsetgroup == "1"))
md2 <- lmer(HR ~ AGE + I(AGE^2) + I(AGE^3) + (1|id), data = comb_plot_dat %>% filter(onsetgroup == "2"))
md3 <- lmer(HR ~ AGE + I(AGE^2) + I(AGE^3) + (1|id), data = comb_plot_dat %>% filter(onsetgroup == "3"))
md4 <- lmer(HR ~ AGE + I(AGE^2) + I(AGE^3) + (1|id), data = comb_plot_dat %>% filter(onsetgroup == "4"))

pdata <- expand.grid(AGE = 32:85,
                     BMI = mean(comb_plot_dat$BMI),
                     SMK = 0,
                     DM = 0,
                     TC = mean(comb_plot_dat$TC),
                     HDL = mean(comb_plot_dat$HDL),
                     HRX = 0,
                     cohort = c("fhs","other"),
                     race = c("1","other"))

pdata$ypred1 <- predict(md1, newdata = pdata, allow.new.levels = TRUE, re.form=NA)
pdata$ypred2 <- predict(md2, newdata = pdata, allow.new.levels = TRUE, re.form=NA)
pdata$ypred3 <- predict(md3, newdata = pdata, allow.new.levels = TRUE, re.form=NA)
pdata$ypred4 <- predict(md4, newdata = pdata, allow.new.levels = TRUE, re.form=NA)


pdata %<>% filter(cohort == "fhs", race =="1")


brady_traj <- 
  ggplot() + 
  geom_line(data = pdata %>% filter(AGE %in% seq(limit1[1],limit1[2],1)), aes(x = AGE, y = ypred1, color = "1"), size = 1) +
  geom_line(data = pdata %>% filter(AGE %in% seq(limit1[1],limit2[2],1)), aes(x = AGE, y = ypred2, color = "2"), size = 1) + 
  geom_line(data = pdata %>% filter(AGE %in% seq(limit1[1],limit3[2],1)), aes(x = AGE, y = ypred3, color = "3"), size = 1) + 
  geom_line(data = pdata %>% filter(AGE %in% seq(limit1[1],limit4[2],1)), aes(x = AGE, y = ypred4, color = "4"), size = 1) +
  # geom_line(data = pdata %>% filter(AGE %in% seq(limit1[1],limit5[2],1)), aes(x = AGE, y = ypred5, color = "5"), size = 1) +
  scale_color_manual(name = "Age at Attaining Lower RHR (i.e. <70 bpm)", 
                     label = c("1" = "~ 49", "2" = "50-59", "3"="60-69","4"="70 ~","5"="70 ~"), 
                     # values = c("1" = "#a42f3e","2" = "#e1743c", "3"="#edb847","4"="#1c7265","5"="#3f1d39")) +
  values = c("4" = "#a42f3e","3" = "#e1743c", "2"="#edb847","1"="#1c7265","5"="#3f1d39")) +
  scale_fill_manual(name = "Age at Attaining RHR <70 bpm", 
                    label = c("1" = "~ 49", "2" = "50-59", "3"="60-69","4"="70 ~","5"="70 ~"), 
                    # values = c("1" = "#ffb8c1","2" = "#ffc9ad", "3"="#ffe9ba","4"="#8fb3ad","5"="#3f1d39")) +
  values = c("4" = "#a42f3e","3" = "#e1743c", "2"="#edb847","1"="#1c7265","5"="#3f1d39")) +
  scale_y_continuous(name = "Resting Heart Rate, bpm") + 
  scale_x_continuous(name = "\nAge, years", breaks = seq(20,80,10)) + 
  ggtitle("A.") +
  theme_classic() + # use a white background
  theme(
    axis.line = element_line(colour = "black", size = 0.5),
    axis.title = element_text(colour = "black", size = 15,hjust = 0.5,face="bold"),
    axis.text = element_text(colour="black",size= 12,angle = 0,vjust=0.5,face="bold"),
    plot.title = element_text(colour = "black", size = 15,face="bold"),
    legend.text = element_text(colour = "black", size = 15,face="bold"),
    legend.title = element_text(colour = "black", size = 15,face="bold"),
    legend.position = "bottom",
    legend.background = element_blank()
  ) + guides(fill = guide_legend(order = 1),
             color = guide_legend(order = 1),
             linetype = guide_legend(order = 2))

brady_traj

## ---- forest plot  --------

signum <- function(x){x = ifelse(x < 0.001,"<0.001", ifelse(x < 0.01, sprintf("%.03f", x), ifelse(x < 0.06 ,sprintf("%.03f", x), sprintf("%.02f", x))))}

hr_dat_plot <- heart_rate$coefficients %>%
  as.data.frame() %>%
  mutate(term = rownames(.),
         or = exp(Estimate),
         low = exp(Estimate - 1.96*`Std. Error`),
         up = exp(Estimate + 1.96*`Std. Error`),
         conf = paste(sprintf("%.02f",or),"(",sprintf("%.02f",low),",",sprintf("%.02f",up),")",sep = "")) %>%
  filter(grepl('onsetgroup', term))
  
dat_plot <- hr_dat_plot %>%
  dplyr::select(or, low, up) %>% 
  rbind(data.frame(or = 1, low = NA, up = NA)) %>%
  mutate(n = (nrow(hr_dat_plot)+1):1,
         color = as.character(1:(nrow(hr_dat_plot)+1)))

cvd_sum <- hr_dat %>% 
  group_by(onsetgroup) %>%
  dplyr::summarise(hardcvd = sum(hardcvd),
                   num = n()) %>%
  mutate(noncvd = num - hardcvd,
         onsetgroup = as.numeric(as.character(onsetgroup)))
cvd_sum <- cvd_sum[order(cvd_sum$onsetgroup),]

text_plot <- hr_dat_plot %>%
  mutate(p = signum(`Pr(>|z|)`)) %>%
  dplyr::select(conf,p) %>%
  rbind(data.frame(conf = "Reference", p = "-")) %>%
  mutate(term = c("~ 49", "50-59", "60-69","70 ~"),
         n = (nrow(hr_dat_plot)+1):1) %>%
  cbind(cvd_sum) %>%
  mutate(event = paste(hardcvd,"/",noncvd, sep = "")) %>%
  rbind(data.frame(conf = "Odds Ratio\n(95%CI)",
                   p = "P value",
                   term = "",
                   n = (nrow(hr_dat_plot)+2),
                   onsetgroup = "onsetgroup",
                   hardcvd = "hardcvd",
                   num = "num",
                   noncvd = "noncvd",
                   event = "Hard CVD/\nNon-CVD"))


brady_plot <- ggplot() +
  coord_cartesian(xlim = c(0.0015,120)) +
  geom_errorbarh(data = dat_plot, aes(xmin = low, xmax = up, y = n, color = color), height = 0, size = 1.5, show.legend = FALSE) +
  geom_point(data = dat_plot, aes(x = or, y = n, color = color), size = 3, shape = 15) +
  geom_text(data = text_plot, aes(label = conf, y = n, x = 8, fontface = "bold"), size = 5, colour = 'black') +
  geom_text(data = text_plot, aes(label = term, y = n, x = 0.0025, fontface = "bold"), size = 5, colour = 'black') +
  geom_text(data = text_plot, aes(label = event, y = n, x = 0.02, fontface = "bold"), size = 5, colour = 'black') +
  geom_text(data = text_plot, aes(label = p, y = n, x = 80, fontface = "bold"), size = 5, colour = 'black') +

  geom_vline(xintercept = 1, size = 0.3, linetype = 1, color = "#434433") +
  geom_hline(yintercept = 0, size = 1.5, linetype = 1, color = "#434433") +
  ggtitle("B.")+
  scale_y_continuous(expand=expand_scale(mult = c(0, 0.1))) +
  scale_x_continuous(name = paste("\nOdds Ratio (95% CI)",sep = ""),
                     trans = "log2",
                     # limits = c(-0.5, 8.5),
                     breaks = c(0.125,0.25,0.5,1,2),
                     labels = c(0.125,0.25,0.5,1,2)
  ) + #c(10^(-log10(xrange))
  scale_color_manual(name = "Age at Attaining RHR <70 bpm", 
                     label = c("1" = "~ 49", "2" = "50-59", "3"="60-69","4"="70 ~"), 
                     # values = c("1" = "#a42f3e","2" = "#e1743c", "3"="#edb847","4"="#1c7265")) +
                     values = c("4" = "#a42f3e","3" = "#e1743c", "2"="#edb847","1"="#1c7265")) +
  scale_fill_manual(name = "Age at Attaining RHR <70 bpm", 
                     label = c("1" = "~ 49", "2" = "50-59", "3"="60-69","4"="70 ~"), 
                     # values = c("1" = "#a42f3e","2" = "#e1743c", "3"="#edb847","4"="#1c7265")) +
                    values = c("4" = "#a42f3e","3" = "#e1743c", "2"="#edb847","1"="#1c7265")) +
  theme_bw() + # use a white background
  theme(
    axis.line.x = element_line(colour = "black", size = 0.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(colour = "black", size = 15,hjust = 0.65,face="bold"),
    axis.title.y =  element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(colour="black",size= 10,angle = 0,vjust=0.5,face="bold"),
    plot.title = element_text(colour = "black", size = 15,face="bold"),
    axis.ticks.y = element_blank(),
    legend.text = element_text(colour = "black", size = 15,face="bold"),
    legend.title = element_text(colour = "black", size = 15,face="bold"),
    legend.position = "bottom"
  ) + guides(fill = F, 
             # color = F, 
             shape = FALSE) 

brady_plot

## KM curves and hazar ratio of 3 earlier onset subgroup versus last onset subgroup ----

library(survival)
library(rms)
library(survminer)
# library(ggkm)
library(ggquickeda) #install.packages("ggquickeda")
library(nph) # install.packages("nph")

comb_dat_id <- readRDS("data/comb_dat_id_HR.rds") %>% 
  mutate(race = as.character(race)) %>% 
  dplyr::select(c("AGE","SEX","HR","BMI","SBP","HRX","SMK","DM","TC","HDL","race","cohort","id","visit")) %>%
  na.omit()

coxdat <- hr_dat %>%
  dplyr::select(id, onsetage, cate_duration) %>%
  left_join(comb_dat_id, by = c("id","onsetage" = "AGE")) %>%
  group_by(id) %>%
  dplyr::summarise(onsetage = dplyr::first(onsetage),
                   visit = dplyr::first(visit),
                   cate_duration = dplyr::first(cate_duration)) %>%
  left_join(comb_dat_id, by = c("id","onsetage" = "AGE", "visit")) %>%
  left_join(comb_clin %>% dplyr::select(hardcvd_age, hardcvd, id), by = "id") %>%
  mutate(onsetgroup = ifelse(onsetage==0,"NO_ONSET",ifelse(onsetage < 50,"1",ifelse(onsetage < 60,"2",ifelse(onsetage < 70,"3","4")))),
         hardcvdtime = hardcvd_age - onsetage) 

coxdat$onsetgroup <- relevel(factor(coxdat$onsetgroup), ref = "4")

## set age 70 as time zero

coxdat_aft60 <- coxdat %>% 
  filter(hardcvd_age > 70) %>%
  mutate(hardcvdtime = hardcvd_age - 70)

# coxdat_aft60$onsetgroup %>% summary()

kmdata <- coxdat_aft60 %>% 
  filter(onsetgroup =="1"|onsetgroup=="2"|onsetgroup=="3") %>%
  mutate(#onsetgroup = ifelse(onsetgroup=="1"|onsetgroup=="2","1","2"),
         onsetgroup = relevel(factor(onsetgroup, levels = c("3","2","1")), ref = "3"),
         cate_duration = 70-onsetage,
         cate_duration10=cate_duration/10)

surv_1 <- coxph(Surv(hardcvdtime,hardcvd) ~ onsetgroup + SEX + race + cohort
              , data = kmdata) %>% summary()
surv_1

coef1 <- surv_1$coefficients["onsetgroup1","coef"]
low1 <- coef1-1.96*surv_1$coefficients["onsetgroup1","se(coef)"]
up1 <- coef1+1.96*surv_1$coefficients["onsetgroup1","se(coef)"]
conf1 <- paste(sprintf("%.02f",exp(coef1)),"(",sprintf("%.02f",exp(low1)),",",sprintf("%.02f",exp(up1)),")",sep = "")

coef2 <- surv_1$coefficients["onsetgroup2","coef"]
low2 <- coef1-1.96*surv_1$coefficients["onsetgroup2","se(coef)"]
up2 <- coef1+1.96*surv_1$coefficients["onsetgroup2","se(coef)"]
conf2 <- paste(sprintf("%.02f",exp(coef2)),"(",sprintf("%.02f",exp(low2)),",",sprintf("%.02f",exp(up2)),")",sep = "")

KWtest <- survdiff(Surv(hardcvdtime, hardcvd) ~ onsetgroup, data = kmdata)

p.val <- 1 - pchisq(KWtest$chisq, length(KWtest$n) - 1)

surv_pvalue(survfit(Surv(hardcvdtime, hardcvd) ~ onsetgroup, data = kmdata))

surv_curv1 <- ggplot() + 
  coord_cartesian(xlim = c(0,13)) +
  # geom_kmband(aes(time = hardcvdtime, status = hardcvd, fill = factor(onsetgroup)), alpha = 0.2, trans = "event", data = kmdata, show.legend = F) +
  geom_km(aes(time = hardcvdtime, status = hardcvd, color = factor(onsetgroup)), trans = "cumhaz", size = 1,data = kmdata, show.legend = F) +
  scale_color_manual(name = "Age at Attaining RHR <70 bpm", 
                     label = c("1" = "~ 39", "2" = "40-49", "3"="50-59","4"="60 ~"), 
                     # values = c("1" = "#a42f3e","2" = "#e1743c", "3"="#edb847","4"="#1c7265")) +
                     values = c("4" = "#a42f3e","3" = "#e1743c", "2"="#edb847","1"="#1c7265","5"="#3f1d39")) +
  scale_fill_manual(name = "Age at Attaining RHR <70 bpm", 
                    label = c("1" = "~ 39", "2" = "40-49", "3"="50-59","4"="60 ~"), 
                    # values = c("1" = "#a42f3e","2" = "#e1743c", "3"="#edb847","4"="#1c7265")) +
                    values = c("4" = "#a42f3e","3" = "#e1743c", "2"="#edb847","1"="#1c7265","5"="#3f1d39")) +
  scale_x_continuous(breaks = seq(0,15,3), name = "\nTime After Age 70, years\n") +
  scale_y_continuous(limits = c(0,0.4), name = "Cumulative Hazard") +
  ggtitle("C.") +
  theme_classic() +
  theme(axis.title.y.left = element_text(color = "black",size =15,face="bold"),
        axis.text.y.left = element_text(color = "black",size =12,face="bold"),
        axis.text.x = element_text(color = "black",size =15,face="bold"),
        title = element_text(color = "black",size =15,face="bold"),
        legend.text = element_text(color = "black",size =12,face="bold"),
        legend.position = c(0.2,0.8),
        legend.background = element_rect(fill="white",
                                         size=0.2, 
                                         linetype="solid", 
                                         colour ="black")) +
  annotate("text",5, 0.25, label=ifelse(p.val<0.001,paste("Log-rank test, P<0.001",sep = ""),
                                        paste("Log-rank test, P=",signum(p.val),sep="")), 
           fontface = 2, col = c('black'), size = 5)


surv_curv1

table1 <- ggsurvplot(survfit(Surv(hardcvdtime, hardcvd) ~ onsetgroup, data = kmdata),
                     data = kmdata,
                     break.x.by = 3,
                     fun = "event",
                     risk.table = TRUE)$data.survtable %>%
  mutate(strata = factor(strata, levels = c("onsetgroup=3","onsetgroup=2","onsetgroup=1")))


risktable1 <- ggplot() +
  geom_text(aes(x = time, y = strata, label = n.risk), size = 5, data = table1) +
  # coord_cartesian(xlim = c(18,85)) +
  scale_x_continuous(limits = c(0,13), breaks = seq(0,15,3), name = "") +
  scale_y_discrete(name = "", labels = c("onsetgroup=1" = "~ 49  ", "onsetgroup=2" = "50-59  ","onsetgroup=3"="60-69  ")) +
  ggtitle("No. at risk") +
  theme_classic() +
  theme(axis.title.y.left = element_text(color = "black",size =12,face="bold"),
        axis.text.y.left = element_text(color = "black",size =12,face="bold"),
        axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        title = element_text(color = "black",size =15,face="bold"),
        legend.text = element_text(color = "black",size =15,face="bold"))

comb_km <- cowplot::plot_grid(surv_curv1, risktable1, rel_heights=c(3, 1), ncol = 1, align = "v", axis = "r")


##  3 figures combined -----

library(gridExtra)
library(grid)
library(lemon) # install.packages("lemon")


setEPS()
postscript("output/forestplot_heart_rate.eps", width = 18, height = 6)
lemon::grid_arrange_shared_legend(brady_traj, brady_plot, comb_km, ncol = 3, nrow = 1, position = "top")

dev.off()

## per 10-year earlier attaining RHR<70

covdata70 <-
  comb_dat_id %>% 
  filter(id %in% kmdata$id) %>%
  group_by(id) %>% 
  dplyr::summarise(HR_coef = coef(lm(HR~AGE))[2],
                   BMI_coef = coef(lm(BMI~AGE))[2],
                   SMK_coef = coef(lm(SMK~AGE))[2],
                   SBP_coef = coef(lm(SBP~AGE))[2],
                   HRX_coef = coef(lm(HRX~AGE))[2],
                   DM_coef = coef(lm(DM~AGE))[2],
                   TC_coef = coef(lm(TC~AGE))[2],
                   HDL_coef = coef(lm(HDL~AGE))[2])


coxdata <- kmdata %>% 
  left_join(covdata70, by = "id") %>%
  mutate(HR70 = HR + HR_coef*(70-onsetage),
         BMIR70 = BMI + BMI_coef*(70-onsetage),
         SMK70 = SMK + SMK_coef*(70-onsetage),
         SBP70 = SBP + SBP_coef*(70-onsetage),
         HRX70 = HRX + HRX_coef*(70-onsetage),
         DM70 = DM + DM_coef*(70-onsetage),
         TC70 = TC + TC_coef*(70-onsetage),
         HDL70 = HDL + HDL_coef*(70-onsetage))


coxph(Surv(hardcvdtime, hardcvd) ~ cate_duration10 + SEX + HR70 + BMIR70 + SBP70 + TC70 + 
        SMK + HRX + DM + race + cohort, data = coxdata) %>% summary()


anova(coxph(Surv(hardcvdtime, hardcvd) ~ onsetage + SEX + HR70 + BMIR70 + SBP70 + TC70 + 
              SMK + HRX + DM + race + cohort, data = coxdata),
      coxph(Surv(hardcvdtime, hardcvd) ~ onsetage*SEX + HR70 + BMIR70 + SBP70 + TC70 + 
              SMK + HRX + DM + race + cohort, data = coxdata),
      test = "LRT")






