library(rms)
library(survival)
library(tidyverse)
library(magrittr)

signum <- function(x){x = ifelse(x < 0.001,"<0.001", ifelse(x < 0.01, sprintf("%.03f", x), ifelse(x < 0.06 ,sprintf("%.03f", x), sprintf("%.02f", x))))}

comb_clin <- readRDS("data/comb_clin.rds") %>% 
  mutate(race = ifelse(race=="1","1","0")) 

md <- comb_clin %>% 
  mutate(hardcvdtime = hardcvd_age - AGE,
         hardcvdtime10 = ifelse(hardcvdtime>30,30,hardcvdtime),
         hardcvd10 = ifelse(hardcvd==1 & hardcvdtime<=30,1,0),
         mitime10 = ifelse(mitime>30,30,mitime),
         mi10 = ifelse(mi==1 & mitime<=30,1,0),
         chftime10 = ifelse(chftime>30,30,chftime),
         chf10 = ifelse(chf==1 & chftime<=30,1,0),
         strktime10 = ifelse(strktime>30,30,strktime),
         strk10 = ifelse(strk==1 & strktime<=30,1,0),
         OB = ifelse(BMI>=30,1,0)) %>% 
  mutate(htngroup = ifelse(SBP<110,0,ifelse(SBP<120,1,ifelse(SBP<130,2,ifelse(SBP < 140,3,ifelse(SBP < 150,4,5))))),
         sbp_tile = ntile(SBP,5),
         age_tile = ifelse(AGE < 47, 1, ifelse(AGE < 57,2, 3)),
         htngroup = as.factor(htngroup),
         age_tile = as.factor(age_tile),
         sbp_tile = as.factor(sbp_tile),
         SBP10 = SBP/10) %>%
  filter(AGE>=18)

bsldat <- md %>% 
  dplyr::select(id,"AGE","BMI","SBP","HRX","SMK","DM","TC","HDL","race","cohort","hardcvd","mi","chf","strk","dth") %>%
  filter(AGE >= 18) %>% 
  na.omit()

nohxchd <- md %>% 
  filter(id %in% bsldat$id) %>%
  mutate(intergroup = paste(htngroup,age_tile,sep = "_"),
         intergroup = relevel(factor(intergroup), ref = "0_1"),
         inter_race = paste(htngroup,race,sep = "_"),
         inter_race = relevel(factor(inter_race), ref = "0_0"))
nohxchd1 <- nohxchd %>% filter(SEX==1)
nohxchd2 <- nohxchd %>% filter(SEX==2) 

nohxchd1$pred <- nohxchd1$SBP
nohxchd2$pred <- nohxchd2$SBP
nohxchd$pred <- nohxchd$SBP

term = "hardcvd"

nohxchd1$outcome <- nohxchd1 %>% dplyr::select(term) %>% pull()
nohxchd2$outcome <- nohxchd2 %>% dplyr::select(term) %>% pull()
nohxchd$outcome <- nohxchd %>% dplyr::select(term) %>% pull()

nohxchd1$outcometime <- nohxchd1 %>% dplyr::select(paste(term,"time",sep = "")) %>% pull()
nohxchd2$outcometime <- nohxchd2 %>% dplyr::select(paste(term,"time",sep = "")) %>% pull()
nohxchd$outcometime <- nohxchd %>% dplyr::select(paste(term,"time",sep = "")) %>% pull()

ref = "0"

cate_cvd_m <- coxph(Surv(hardcvdtime, hardcvd) ~ relevel(htngroup, ref = ref) + DBP + 
                      AGE + BMI + DM + HRX + SMK + HDL + TC + race + cohort, data = nohxchd1)
cate_cvd_f <- coxph(Surv(hardcvdtime, hardcvd) ~ relevel(htngroup, ref = ref) + DBP + 
                      AGE + BMI + DM + HRX + SMK + HDL + TC + race + cohort, data = nohxchd2)
inter_cvd <- anova(coxph(Surv(hardcvdtime, hardcvd) ~ htngroup + SEX + 
                           AGE + BMI + DM + HRX + SMK + HDL + TC + race + cohort, data = nohxchd),
                   coxph(Surv(hardcvdtime, hardcvd) ~ htngroup*SEX + 
                           AGE + BMI + DM + HRX + SMK + HDL + TC + race + cohort, data = nohxchd),
                   test = "LRT")


cate_mi_m <- coxph(Surv(mitime, mi) ~ relevel(htngroup, ref = ref) + DBP + 
                     AGE + BMI + DM + HRX + SMK + HDL + TC + race + cohort, data = nohxchd1)
cate_mi_f <- coxph(Surv(mitime, mi) ~ relevel(htngroup, ref = ref) + DBP + 
                     AGE + BMI + DM + HRX + SMK + HDL + TC + race + cohort, data = nohxchd2)
inter_mi <- anova(coxph(Surv(mitime, mi) ~ htngroup + SEX + 
                          AGE + BMI + DM + HRX + SMK + HDL + TC + race + cohort, data = nohxchd),
                  coxph(Surv(mitime, mi) ~ htngroup*SEX + 
                          AGE + BMI + DM + HRX + SMK + HDL + TC + race + cohort, data = nohxchd),
                  test = "LRT")



cate_chf_m <- coxph(Surv(chftime, chf) ~ relevel(htngroup, ref = ref) + DBP + 
                      AGE + BMI + DM + HRX + SMK + HDL + TC + race + cohort, data = nohxchd1)
cate_chf_f <- coxph(Surv(chftime, chf) ~ relevel(htngroup, ref = ref) + DBP + 
                      AGE + BMI + DM + HRX + SMK + HDL + TC + race + cohort, data = nohxchd2)
inter_chf <- anova(coxph(Surv(chftime, chf) ~ htngroup + SEX + 
                           AGE + BMI + DM + HRX + SMK + HDL + TC + race + cohort, data = nohxchd),
                   coxph(Surv(chftime, chf) ~ htngroup*SEX + 
                           AGE + BMI + DM + HRX + SMK + HDL + TC + race + cohort, data = nohxchd),
                   test = "LRT")


cate_strk_m <- coxph(Surv(strktime, strk) ~ relevel(htngroup, ref = ref) + DBP + 
                       AGE + BMI + DM + HRX + SMK + HDL + TC + race + cohort, data = nohxchd1)
cate_strk_f <- coxph(Surv(strktime, strk) ~ relevel(htngroup, ref = ref) + DBP + 
                       AGE + BMI + DM + HRX + SMK + HDL + TC + race + cohort, data = nohxchd2)

inter_strk <- anova(coxph(Surv(strktime, strk) ~ htngroup + SEX +
                            AGE + BMI + DM + HRX + SMK + HDL + TC + race + cohort, data = nohxchd),
                    coxph(Surv(strktime, strk) ~ htngroup*SEX + 
                            AGE + BMI + DM + HRX + SMK + HDL + TC + race + cohort, data = nohxchd),
                    test = "LRT")



tab_cvd1 <- summary(cate_cvd_m)$coefficients %>% 
  as.data.frame() %>%
  rownames_to_column("term") %>%
  filter(grepl('htngroup', term)) %>%
  mutate(SEX=1, 
         hr = exp(coef),
         low = exp(coef - 1.96*`se(coef)`),
         up = exp(coef + 1.96*`se(coef)`),
         conf = paste(sprintf("%.02f",hr),"(",sprintf("%.02f",low),",",sprintf("%.02f",up),")",sep = ""),
         p = (`Pr(>|z|)`))


tab_cvd2 <- summary(cate_cvd_f)$coefficients %>% 
  as.data.frame() %>%
  rownames_to_column("term") %>%
  filter(grepl('htngroup', term)) %>%
  mutate(SEX=1, 
         hr = exp(coef),
         low = exp(coef - 1.96*`se(coef)`),
         up = exp(coef + 1.96*`se(coef)`),
         conf = paste(sprintf("%.02f",hr),"(",sprintf("%.02f",low),",",sprintf("%.02f",up),")",sep = ""),
         p = (`Pr(>|z|)`))


tab_mi1 <- summary(cate_mi_m)$coefficients %>% 
  as.data.frame() %>%
  rownames_to_column("term") %>%
  filter(grepl('htngroup', term)) %>%
  mutate(SEX=1, 
         hr = exp(coef),
         low = exp(coef - 1.96*`se(coef)`),
         up = exp(coef + 1.96*`se(coef)`),
         conf = paste(sprintf("%.02f",hr),"(",sprintf("%.02f",low),",",sprintf("%.02f",up),")",sep = ""),
         p = (`Pr(>|z|)`))


tab_mi2 <- summary(cate_mi_f)$coefficients %>% 
  as.data.frame() %>%
  rownames_to_column("term") %>%
  filter(grepl('htngroup', term)) %>%
  mutate(SEX=1, 
         hr = exp(coef),
         low = exp(coef - 1.96*`se(coef)`),
         up = exp(coef + 1.96*`se(coef)`),
         conf = paste(sprintf("%.02f",hr),"(",sprintf("%.02f",low),",",sprintf("%.02f",up),")",sep = ""),
         p = (`Pr(>|z|)`))



tab_chf1 <- summary(cate_chf_m)$coefficients %>% 
  as.data.frame() %>%
  rownames_to_column("term") %>%
  filter(grepl('htngroup', term)) %>%
  mutate(SEX=1, 
         hr = exp(coef),
         low = exp(coef - 1.96*`se(coef)`),
         up = exp(coef + 1.96*`se(coef)`),
         conf = paste(sprintf("%.02f",hr),"(",sprintf("%.02f",low),",",sprintf("%.02f",up),")",sep = ""),
         p = (`Pr(>|z|)`))


tab_chf2 <- summary(cate_chf_f)$coefficients %>% 
  as.data.frame() %>%
  rownames_to_column("term") %>%
  filter(grepl('htngroup', term)) %>%
  mutate(SEX=1, 
         hr = exp(coef),
         low = exp(coef - 1.96*`se(coef)`),
         up = exp(coef + 1.96*`se(coef)`),
         conf = paste(sprintf("%.02f",hr),"(",sprintf("%.02f",low),",",sprintf("%.02f",up),")",sep = ""),
         p = (`Pr(>|z|)`))


tab_strk1 <- summary(cate_strk_m)$coefficients %>% 
  as.data.frame() %>%
  rownames_to_column("term") %>%
  filter(grepl('htngroup', term)) %>%
  mutate(SEX=1, 
         hr = exp(coef),
         low = exp(coef - 1.96*`se(coef)`),
         up = exp(coef + 1.96*`se(coef)`),
         conf = paste(sprintf("%.02f",hr),"(",sprintf("%.02f",low),",",sprintf("%.02f",up),")",sep = ""),
         p = (`Pr(>|z|)`))


tab_strk2 <- summary(cate_strk_f)$coefficients %>% 
  as.data.frame() %>%
  rownames_to_column("term") %>%
  filter(grepl('htngroup', term)) %>%
  mutate(SEX=1, 
         hr = exp(coef),
         low = exp(coef - 1.96*`se(coef)`),
         up = exp(coef + 1.96*`se(coef)`),
         conf = paste(sprintf("%.02f",hr),"(",sprintf("%.02f",low),",",sprintf("%.02f",up),")",sep = ""),
         p = (`Pr(>|z|)`))


catechf_tab <- tab_chf2 %>% 
  dplyr::select(conf_f = conf, p_f = p, coef_f = coef, se_f = `se(coef)`) %>% 
  cbind(tab_chf1 %>% 
          dplyr::select(conf_m = conf, p_m = p, coef_m = coef, se_m = `se(coef)`)) %>%
  mutate(pinter = inter_chf$`P(>|Chi|)`[2])

catemi_tab <- tab_mi2 %>% 
  dplyr::select(conf_f = conf, p_f = p, coef_f = coef, se_f = `se(coef)`) %>% 
  cbind(tab_mi1 %>% 
          dplyr::select(conf_m = conf, p_m = p, coef_m = coef, se_m = `se(coef)`)) %>%
  mutate(pinter = inter_mi$`P(>|Chi|)`[2])

catestrk_tab <- tab_strk2 %>% 
  dplyr::select(conf_f = conf, p_f = p, coef_f = coef, se_f = `se(coef)`) %>% 
  cbind(tab_strk1 %>% 
          dplyr::select(conf_m = conf, p_m = p, coef_m = coef, se_m = `se(coef)`)) %>%
  mutate(pinter = inter_strk$`P(>|Chi|)`[2])

catecvd_tab <- tab_cvd2 %>% 
  dplyr::select(conf_f = conf, p_f = p, coef_f = coef, se_f = `se(coef)`) %>% 
  cbind(tab_cvd1 %>% 
          dplyr::select(conf_m = conf, p_m = p, coef_m = coef, se_m = `se(coef)`)) %>%
  mutate(pinter = inter_cvd$`P(>|Chi|)`[2])


adddat<-data.frame(conf_f=NA,p_f=NA,coef_f=NA,
                   se_f=NA,conf_m=NA,p_m=NA,coef_m=NA,se_m=NA,pinter=NA,low_m=NA,up_m=NA,mean_m=1,low_f=NA,up_f=NA,mean_f=1,
                   n =6,color_f="red2",color_m="blue2")
nseq = c(5:1)
maknum = 1
datatab_cvd <- catecvd_tab %>% 
  mutate(low_m = exp(coef_m - 1.96*se_m),
         up_m = exp(coef_m + 1.96*se_m),
         mean_m = exp(coef_m),
         low_f = exp(coef_f - 1.96*se_f),
         up_f = exp(coef_f + 1.96*se_f),
         mean_f = exp(coef_f),
         n = nseq,
         color_f = ifelse(p_f<0.05/maknum, "red4","red2"),
         color_m = ifelse(p_m<0.05/maknum, "blue4","blue2"),
         p_f = signum(p_f),
         p_m = signum(p_m)) %>%
  rbind(adddat) #%>% filter(n>=4) %>% mutate(n = n-3)

gap = 10
datatab_mi <- catemi_tab %>% 
  mutate(low_m = exp(coef_m - 1.96*se_m) * gap,
         up_m = exp(coef_m + 1.96*se_m) * gap,
         mean_m = exp(coef_m) * gap,
         low_f = exp(coef_f - 1.96*se_f) * gap,
         up_f = exp(coef_f + 1.96*se_f) * gap,
         mean_f = exp(coef_f) * gap,
         n = nseq,
         color_f = ifelse(p_f<0.05/maknum, "red4","red2"),
         color_m = ifelse(p_m<0.05/maknum, "blue4","blue2"),
         p_f = signum(p_f),
         p_m = signum(p_m)) %>%
  rbind(adddat %>% mutate(mean_m = gap^1, mean_f = gap^1)) #%>% filter(n>=4) %>% mutate(n = n-3)



datatab_chf <- catechf_tab %>% 
  mutate(low_m = exp(coef_m - 1.96*se_m) *gap^2,
         up_m = exp(coef_m + 1.96*se_m) *gap^2,
         mean_m = exp(coef_m) *gap^2,
         low_f = exp(coef_f - 1.96*se_f) *gap^2,
         up_f = exp(coef_f + 1.96*se_f) *gap^2,
         mean_f = exp(coef_f) *gap^2,
         n = nseq,
         color_f = ifelse(p_f<0.05/maknum, "red4","red2"),
         color_m = ifelse(p_m<0.05/maknum, "blue4","blue2"),
         p_f = signum(p_f),
         p_m = signum(p_m)) %>%
  rbind(adddat %>% mutate(mean_m = gap^2, mean_f = gap^2)) #%>% filter(n>=4) %>% mutate(n = n-3)



datatab_strk <- catestrk_tab %>% 
  mutate(low_m = exp(coef_m - 1.96*se_m) *gap^3,
         up_m = exp(coef_m + 1.96*se_m) *gap^3,
         mean_m = exp(coef_m) *gap^3,
         low_f = exp(coef_f - 1.96*se_f) *gap^3,
         up_f = exp(coef_f + 1.96*se_f) *gap^3,
         mean_f = exp(coef_f) *gap^3,
         n = nseq,
         color_f = ifelse(p_f<0.05/maknum, "red4","red2"),
         color_m = ifelse(p_m<0.05/maknum, "blue4","blue2"),
         p_f = signum(p_f),
         p_m = signum(p_m)) %>%
  rbind(adddat %>% mutate(mean_m = gap^3, mean_f = gap^3)) #%>% filter(n>=4) %>% mutate(n = n-3)


write.csv(datatab_cvd,"output/datatab_cvd.csv")
write.csv(datatab_mi,"output/datatab_mi.csv")
write.csv(datatab_chf,"output/datatab_chf.csv")
write.csv(datatab_strk,"output/datatab_strk.csv")


ff1 <- c('bold',
         rep('bold', 6))

lab1 <- c('Systolic Blood Pressure',
          "~ 109 mm Hg",
          paste(seq(110,140,10)," - ",seq(119,149,10), " mm Hg",sep = ""),
          "150 ~ mm Hg")

tsize1 <- 5

textdata1 <- data.frame(ff1 = ff1, 
                        tsize1 = tsize1, 
                        lab1 = lab1) %>% 
  mutate(n = c(7:1)) 

cateplot<-ggplot() +
  coord_cartesian(xlim = c(gap^(-2),gap^4)) +
  geom_errorbarh(data = datatab_cvd, aes(xmin = low_m, xmax = up_m, y = n - 0.15, color = color_m), height = 0, size = 1.0, show.legend = FALSE) +
  geom_errorbarh(data = datatab_cvd, aes(xmin = low_f, xmax = up_f, y = n + 0.15, color = color_f),  height = 0, size = 1.0, show.legend = FALSE) +
  geom_point(data = datatab_cvd, aes(x = mean_m, y = n-0.15, color = color_m), size = 3, shape = 15) +
  geom_point(data = datatab_cvd, aes(x = mean_f, y = n+0.15, color = color_f), size = 3, shape = 15) +
  
  geom_errorbarh(data = datatab_mi, aes(xmin = low_m, xmax = up_m, y = n - 0.15, color = color_m), height = 0, size = 1.0, show.legend = FALSE) +
  geom_errorbarh(data = datatab_mi, aes(xmin = low_f, xmax = up_f, y = n + 0.15, color = color_f),  height = 0, size = 1.0, show.legend = FALSE) +
  geom_point(data = datatab_mi, aes(x = mean_m, y = n-0.15, color = color_m), size = 3, shape = 15) +
  geom_point(data = datatab_mi, aes(x = mean_f, y = n+0.15, color = color_f), size = 3, shape = 15) +
  
  geom_errorbarh(data = datatab_chf, aes(xmin = low_m, xmax = up_m, y = n - 0.15, color = color_m), height = 0, size = 1.0, show.legend = T) +
  geom_errorbarh(data = datatab_chf, aes(xmin = low_f, xmax = up_f, y = n + 0.15, color = color_f),  height = 0, size = 1.0, show.legend = T) +
  geom_point(data = datatab_chf, aes(x = mean_m, y = n-0.15, color = color_m), size = 3, shape = 15) +
  geom_point(data = datatab_chf, aes(x = mean_f, y = n+0.15, color = color_f), size = 3, shape = 15) +
  
  geom_errorbarh(data = datatab_strk, aes(xmin = low_m, xmax = up_m, y = n - 0.15, color = color_m), height = 0, size = 1.0, show.legend = FALSE) +
  geom_errorbarh(data = datatab_strk, aes(xmin = low_f, xmax = up_f, y = n + 0.15, color = color_f),  height = 0, size = 1.0, show.legend = FALSE) +
  geom_point(data = datatab_strk, aes(x = mean_m, y = n-0.15, color = color_m), size = 3, shape = 15) +
  geom_point(data = datatab_strk, aes(x = mean_f, y = n+0.15, color = color_f), size = 3, shape = 15) +
  
  geom_text(data = textdata1, aes(label = lab1, y = n, x = gap^(-0.8), fontface = ff1), size = tsize1, hjust = c(1, rep(1, nrow(textdata1)-1)), colour = 'black') +
  geom_vline(xintercept = (gap)^seq(0,3), size = 1, linetype = 2) +
  geom_hline(yintercept = seq(1.5, 6+1.5,1), size = 0.5, alpha = 0.3, linetype = 1) +
  ggtitle("CVD               Myocardial Infarction         Heart Failure                Stroke")+
  scale_x_continuous(name = "\n <- Lower Risk       Hazard Ratio (95% CI)      Higher Risk ->",
                     # limits = c(-0.5, 8.5),
                     breaks = c(c(0.5,1,2),c(0.5,1,2)*gap,c(0.5,1,2)*gap^2,c(0.5,1,2)*gap^3),
                     labels = rep(c("0.5","1","2"),4),
                     trans = "log2") + #c(10^(-log10(xrange))
  scale_color_manual(name = "",
                     values = c("blue4" = "#2166ac", "red4" = "#b2182b", "blue2" = "#d1e5f0", "red2"="#ffcccc"),
                     labels = c("blue4" = "Significant in Men", "red4" = "Significant in Women", "blue2" = "Non-Significant in Men", "red2" = "Non-Significant in Women")) +
  theme_bw() + # use a white background
  theme(
    axis.line.x = element_line(colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.title.x = element_text(colour = "#434443", size = 15,hjust = 0.65,face="bold"),
    axis.title.y =  element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(colour="#434443",size= 15,angle = 0,vjust=0.5,face="bold"),
    plot.title = element_text(colour = "#434443", size = 15,face="bold",hjust = 0.65),
    axis.ticks.y = element_blank(),
    legend.text = element_text(colour = "black", size = 15,face="bold"),
    legend.title = element_text(colour = "black", size = 15,face="bold"),
    legend.position = "bottom"
  ) + guides(fill = F, 
             # color = F, 
             shape = FALSE,
             color = guide_legend(nrow = 1))

cateplot


pdf("output/baseline_cate_10.pdf", width=14, height=5)

cateplot

dev.off()
