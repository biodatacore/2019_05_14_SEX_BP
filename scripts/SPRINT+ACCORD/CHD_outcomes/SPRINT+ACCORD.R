
# ACCORD + SPRINT
accord_data <- readRDS("data/accord_data.rds")
sprint_data <- readRDS("data/sprint_data.rds")

signum <- function(x){x = ifelse(x < 0.001,"<0.001", ifelse(x < 0.01, sprintf("%.03f", x), ifelse(x < 0.06 ,sprintf("%.03f", x), sprintf("%.02f", x))))}

comb_sprint_accord <- 
  rbind(sprint_data %>% dplyr::select(id = MASKID, SEX, RANDASSIGN, AGE, chdtime, chd, chddeath, chddeathtime, statin, aspirin=ASPIRIN, chf, chftime, hd, hdtime, cvddeath, cvddeathtime, nfchd, nfchdtime, death, deathtime, cvd_hx_baseline) %>% mutate(source="sprint"),
        accord_data %>% dplyr::select(id = MaskID, SEX, RANDASSIGN = bp_trt, AGE = baseline_age, chdtime, chd, chddeath, chddeathtime, statin, aspirin, chf, chftime, hd, hdtime, cvddeath, cvddeathtime, nfchd, nfchdtime, death, deathtime, cvd_hx_baseline) %>% mutate(source="accord")) %>%
  mutate(AGE65= ifelse(AGE >=70,1,0)) %>%
  mutate(chd65 = ifelse(chd == 1 & AGE + chdtime<70,1,0),
         chd65time = ifelse(chd == 1 & AGE + chdtime<70,chdtime,70),
         nfchd65 = ifelse(nfchd == 1 & AGE + nfchdtime<70,1,0),
         nfchd65time = ifelse(nfchd == 1 & AGE + nfchdtime<70,nfchdtime,70)) %>%
  filter(AGE<70) 

saveRDS(comb_sprint_accord,"data/comb_sprint_accord.rds")

# comb_sprint_accord$chdtime_cr <- with(comb_sprint_accord, ifelse(chd==0, deathtime, chdtime))
# comb_sprint_accord$chd_cr <- with(comb_sprint_accord, ifelse(chd==0, 2*death, chd))
# comb_sprint_accord$chd_cr <- factor(comb_sprint_accord$chd_cr, 0:2, labels=c("censor", "1", "death"))
# 
# comb_sprint_accord$nfchdtime_cr <- with(comb_sprint_accord, ifelse(nfchd==0, deathtime, nfchdtime))
# comb_sprint_accord$nfchd_cr <- with(comb_sprint_accord, ifelse(nfchd==0, 2*death, nfchd))
# comb_sprint_accord$nfchd_cr <- factor(comb_sprint_accord$nfchd_cr, 0:2, labels=c("censor", "1", "death"))
# 
# comb_sprint_accord$chddeathtime_cr <- with(comb_sprint_accord, ifelse(chddeath == 0, deathtime, chddeathtime))
# comb_sprint_accord$chddeath_cr <- with(comb_sprint_accord, ifelse(chddeath == 0, 2*death, chddeath))
# comb_sprint_accord$chddeath_cr <- factor(comb_sprint_accord$chddeath_cr, 0:2, labels=c("censor", "1", "death"))

filter(comb_sprint_accord, source == "sprint")$RANDASSIGN %>% as.factor() %>% summary()
filter(comb_sprint_accord, source == "accord")$RANDASSIGN %>% as.factor() %>% summary()

comb_sprint_accord$chd_cr %>% as.factor() %>% summary()
comb_sprint_accord %>% nrow()
comb_sprint_accord$chdtime %>% summary()

# PH model ----
term = "chd"

comb_sprint_accord$outcome <- comb_sprint_accord %>% dplyr::select(term) %>% pull()
comb_sprint_accord$outcometime <- comb_sprint_accord %>% dplyr::select(paste(term,"time",sep = "")) %>% pull()

young2 <- coxph(Surv(outcometime, outcome) ~ RANDASSIGN*factor(SEX, levels = c(2,1)) + strata(source) + cluster(id), data = comb_sprint_accord)
young1 <- coxph(Surv(outcometime, outcome) ~ RANDASSIGN*factor(SEX, levels = c(1,2)) + strata(source) + cluster(id), data = comb_sprint_accord)

young2
young1

anova(coxph(Surv(outcometime, outcome) ~ RANDASSIGN*SEX + strata(source) + cluster(id), data = comb_sprint_accord),
      coxph(Surv(outcometime, outcome) ~ RANDASSIGN + SEX + strata(source) + cluster(id), data = comb_sprint_accord),
      test = "LRT")

coxph(Surv(outcometime, outcome) ~ RANDASSIGN*SEX + strata(source) + cluster(id), data = comb_sprint_accord) %>% cox.zph()

comb_sprint_accord$RANDASSIGN %>% as.factor() %>% summary()

# # Competing Risk model ----
# term = "nfchd"
# 
# comb_sprint_accord$outcome %>% as.factor() %>% summary()
# 
# comb_sprint_accord$outcome <- comb_sprint_accord %>% dplyr::select(paste(term,"_cr",sep = "")) %>% pull()
# comb_sprint_accord$outcometime <- comb_sprint_accord %>% dplyr::select(paste(term,"time_cr",sep = "")) %>% pull()
# 
# pdata <- finegray(Surv(outcometime, outcome) ~ ., data = comb_sprint_accord %>% filter(!is.na(outcome)))
# young2 <- coxph(Surv(fgstart, fgstop, fgstatus) ~ RANDASSIGN*factor(SEX, levels = c(2,1)) + strata(source) + cluster(id), data=pdata)
# young1 <- coxph(Surv(fgstart, fgstop, fgstatus) ~ RANDASSIGN*factor(SEX, levels = c(1,2)) + strata(source) + cluster(id), data=pdata)
# 
# young2
# young1
# 
# anova(coxph(Surv(fgstart, fgstop, fgstatus) ~ RANDASSIGN*SEX + strata(source) + cluster(id), data = pdata),
#       coxph(Surv(fgstart, fgstop, fgstatus) ~ RANDASSIGN + SEX + strata(source) + cluster(id), data = pdata),
#       test = "LRT")


coxph(Surv(outcometime, outcome) ~ RANDASSIGN*SEX + strata(source) + cluster(id), data = comb_sprint_accord) %>% cox.zph()

all_coefs <- function(model_list){
  purrr::map(model_list, function(model){
    new_coef_tib <- as_tibble(summary(model)$coefficients) %>% 
      mutate(term = rownames(summary(model)$coefficients)) %>% 
      dplyr::select(term, everything())
  }) %>% 
    bind_rows() 
}


tab <- list(
  young2, 
  young1
)

eventnum <- comb_sprint_accord %>% 
  filter(outcometime >= 0) %>%
  group_by(AGE65, SEX, RANDASSIGN) %>% 
  dplyr::summarise(nevent = sum(outcome=="1", na.rm = T),
                   total = n()) %>%
  arrange(AGE65, desc(SEX)) %>%
  as.data.frame() %>%
  mutate(eventrate = paste(nevent,total,sep = " / "))

tab_sum <- all_coefs(tab) %>%
  mutate(hr = exp(coef),
         low = exp(coef-1.96*`robust se`),
         up = exp(coef+1.96*`robust se`),
         conf=paste(sprintf("%.02f",hr)," (",sprintf("%.02f",low),",",sprintf("%.02f",up),")",sep = "")) %>%
  filter(term == "RANDASSIGN")

write.csv(tab_sum, "output/tab_sum.csv")
write.csv(eventnum, "output/sum_event.csv")


# coxph(Surv(outcometime, outcome) ~ RANDASSIGN*factor(SEX, levels = c(2,1)) + RANDASSIGN*factor(source, levels = c("sprint","accord")) + cluster(id), data = comb_sprint_accord) %>%
#   cox.zph()
# 
# tab2 <- list(coxph(Surv(outcometime, outcome) ~ RANDASSIGN*factor(SEX, levels = c(2,1)) + RANDASSIGN*factor(source, levels = c("sprint","accord")) + cluster(id), data = comb_sprint_accord),
#              coxph(Surv(outcometime, outcome) ~ RANDASSIGN*factor(SEX, levels = c(2,1)) + RANDASSIGN*factor(source, levels = c("accord","sprint")) + cluster(id), data = comb_sprint_accord),
#              coxph(Surv(outcometime, outcome) ~ RANDASSIGN*factor(SEX, levels = c(1,2)) + RANDASSIGN*factor(source, levels = c("sprint","accord")) + cluster(id), data = comb_sprint_accord),
#              coxph(Surv(outcometime, outcome) ~ RANDASSIGN*factor(SEX, levels = c(1,2)) + RANDASSIGN*factor(source, levels = c("accord","sprint")) + cluster(id), data = comb_sprint_accord))
# 
# anova(coxph(Surv(outcometime, outcome) ~ RANDASSIGN*factor(SEX, levels = c(2,1)) + RANDASSIGN*factor(source, levels = c("sprint","accord")) + cluster(id), data = comb_sprint_accord),
#       coxph(Surv(outcometime, outcome) ~ RANDASSIGN + factor(SEX, levels = c(2,1)) + RANDASSIGN*factor(source, levels = c("sprint","accord")) + cluster(id), data = comb_sprint_accord))
# 
# 
# eventnum2 <- comb_sprint_accord %>% 
#   group_by(source, SEX, RANDASSIGN) %>% 
#   dplyr::summarise(nevent = sum(outcome=="1", na.rm = T),
#                    total = n()) %>%
#   arrange(desc(SEX), desc(source)) %>%
#   as.data.frame() %>%
#   mutate(eventrate = paste(nevent,total,sep = " / "))
# 
# tab_sum2 <- all_coefs(tab2) %>%
#   mutate(hr = exp(coef),
#          low = exp(coef-1.96*`robust se`),
#          up = exp(coef+1.96*`robust se`),
#          conf=paste(sprintf("%.02f",hr)," (",sprintf("%.02f",low),",",sprintf("%.02f",up),")",sep = "")) %>%
#   filter(term == "RANDASSIGN") %>%
#   mutate(SEX = c(2,2,1,1),
#          source = c("sprint","accord","sprint","accord")) %>%
#   arrange(desc(SEX), desc(source))
# 
# write.csv(tab_sum2, "output/tab_sum2.csv")
# write.csv(eventnum2, "output/sum_event2.csv")

kmdata1 <- comb_sprint_accord %>% filter(SEX==2 & AGE65==0 & outcometime >= 0)
kmdata3 <- comb_sprint_accord %>% filter(SEX==1 & AGE65==0 & outcometime >= 0)

kmtest1 <- survdiff(Surv(outcometime, outcome) ~ RANDASSIGN + strata(source), data = kmdata1, subset = outcometime>=0)
kmtest3 <- survdiff(Surv(outcometime, outcome) ~ RANDASSIGN + strata(source), data = kmdata3, subset = outcometime>=0)

p.val1 <- 1 - pchisq(kmtest1$chisq, length(kmtest1$n) - 1)
p.val3 <- 1 - pchisq(kmtest3$chisq, length(kmtest3$n) - 1)

library(ggkm) # install.packages("ggkm")
# install.packages("devtools")
# library(devtools)
# install_github("michaelway/ggkm")
# library(ggkm)
library(ggquickeda) # install.packages("ggquickeda")

surv_curv1 <- ggplot() + 
  coord_cartesian(xlim = c(0,4), ylim = c(0,0.10)) +
  geom_km(aes(time = outcometime, status = outcome, color = factor(RANDASSIGN)), trans = "cumhaz", size = 1,data = kmdata1) +
  scale_color_manual(name = "",
                     label = c("0" = "Standard Treatment", "1" = "Intensive Treatment", "3"="120-129","4"="130-139","5"="140 ~"),
                     values = c("1" = "#a42f3e","0" = "#e1743c", "3"="#edb847","4"="#1c7265","5"="black")) +
  scale_fill_manual(name = "",
                    label = c("0" = "Standard Treatment", "1" = "Intensive Treatment", "3"="120-129","4"="130-139","5"="140 ~"),
                    values = c("1" = "#a42f3e","0" = "#e1743c", "3"="#edb847","4"="#1c7265","5"="black")) +
  scale_x_continuous(breaks = seq(0,5,1), name = "\nYears") +
  scale_y_continuous(name = "Cumulative Hazard") +
  theme_classic() +
  theme(axis.title.y.left = element_text(color = "black",size =15,face="bold"),
        axis.text = element_text(color = "black",size =15,face="bold"),
        title = element_text(color = "black",size =15,face="bold"),
        legend.text = element_text(color = "black",size =15,face="bold"),
        legend.title = element_blank(),
        legend.position = c(0.25,0.9)) +
  annotate("text",0, 0.065, label=ifelse(p.val1<0.001,paste("Log-rank test, P<0.001",sep = ""),
                                         paste("Log-rank test, P = ",signum(p.val1),sep="")),
           fontface = 1, col = c('black'), size = 5, hjust = 0) +
  annotate("text",0, 0.075, label=paste("HR (95%CI): ", tab_sum$conf[1], sep = ""),
           fontface = 1, col = c('black'), size = 5, hjust = 0) +
  guides(color = guide_legend(nrow = 2, title.position ="left")) 


surv_curv1


surv_curv3 <- ggplot() + 
  coord_cartesian(xlim = c(0,4), ylim = c(0,0.10)) +
  geom_km(aes(time = outcometime, status = outcome, color = factor(RANDASSIGN)), trans = "cumhaz", size = 1,data = kmdata3) +
  scale_color_manual(name = "",
                     label = c("0" = "Standard Treatment", "1" = "Intensive Treatment", "3"="120-129","4"="130-139","5"="140 ~"),
                     values = c("1" = "#003366","0" = "#3399FF", "3"="#edb847","4"="#1c7265","5"="black")) +
  scale_fill_manual(name = "",
                    label = c("0" = "Standard Treatment", "1" = "Intensive Treatment", "3"="120-129","4"="130-139","5"="140 ~"),
                    values = c("1" = "#003366","0" = "#3399FF", "3"="#edb847","4"="#1c7265","5"="black")) +
  scale_x_continuous(breaks = seq(0,5,1), name = "\nYears") +
  scale_y_continuous(name = "Cumulative Hazard") +
  theme_classic() +
  theme(axis.title.y.left = element_text(color = "black",size =15,face="bold"),
        axis.text = element_text(color = "black",size =15,face="bold"),
        title = element_text(color = "black",size =15,face="bold"),
        legend.text = element_text(color = "black",size =15,face="bold"),
        legend.title = element_blank(),
        legend.position = c(0.25,0.9)) +
  annotate("text",0, 0.065, label=ifelse(p.val3<0.001,paste("Log-rank test, P<0.001",sep = ""),
                                         paste("Log-rank test, P = ",signum(p.val3),sep="")),
           fontface = 1, col = c('black'), size = 5, hjust = 0) +
  annotate("text",0, 0.075, label=paste("HR (95%CI): ", tab_sum$conf[2], sep = ""),
           fontface = 1, col = c('black'), size = 5, hjust = 0) +
  guides(color = guide_legend(nrow = 2, title.position ="left")) 


surv_curv3


library(survminer)

table1 <- ggsurvplot(survfit(Surv(outcometime, outcome) ~ RANDASSIGN, data = kmdata1),
                     data = kmdata1,
                     break.x.by = 1,
                     fun = "cumhaz",
                     risk.table = TRUE)$data.survtable

risktable1 <- ggplot() +
  geom_text(aes(x = time, y = strata, label = n.risk), size = 5, data = table1) +
  scale_x_continuous(limits = c(0,5), breaks = seq(0,5,1)) +
  scale_y_discrete(name = "", labels = c("RANDASSIGN=1" = "Intensive", "RANDASSIGN=0" = "Standard")) +
  ggtitle("No. at Risk") + 
  theme_classic() +
  theme(axis.title.y.left = element_text(color = "black",size =18,face="bold"),
        axis.text.y.left = element_text(color = "black",size =15,face="bold"),
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        title = element_text(color = "black",size =15,face="bold"),
        legend.text = element_text(color = "black",size =15,face="bold"))

risktable1

table3 <- ggsurvplot(survfit(Surv(outcometime, outcome) ~ RANDASSIGN, data = kmdata3),
                     data = kmdata3,
                     break.x.by = 1,
                     fun = "cumhaz",
                     risk.table = TRUE)$data.survtable

risktable3 <- ggplot() +
  geom_text(aes(x = time, y = strata, label = n.risk), size = 5, data = table3) +
  scale_x_continuous(limits = c(0,5), breaks = seq(0,5,1)) +
  scale_y_discrete(name = "", labels = c("RANDASSIGN=1" = "Intensive", "RANDASSIGN=0" = "Standard")) +
  ggtitle("No. at Risk") + 
  theme_classic() +
  theme(axis.title.y.left = element_text(color = "black",size =18,face="bold"),
        axis.text.y.left = element_text(color = "black",size =15,face="bold"),
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        title = element_text(color = "black",size =15,face="bold"),
        legend.text = element_text(color = "black",size =15,face="bold"))

risktable3

setEPS()
postscript("output/KM1_rct.eps")
# cairo_ps(file = "output/KM.eps", onefile = FALSE, fallback_resolution = 600)
cowplot::plot_grid(surv_curv1, risktable1, rel_heights=c(2.5, 0.8), ncol = 1, align = "v")
dev.off()




setEPS()
postscript("output/KM3_rct.eps")
# cairo_ps(file = "output/KM.eps", onefile = FALSE, fallback_resolution = 600)
cowplot::plot_grid(surv_curv3, risktable3, rel_heights=c(2.5, 0.8), ncol = 1, align = "v")
dev.off()



event_sum <- comb_sprint_accord %>% 
  group_by(RANDASSIGN, AGE65, SEX) %>%
  dplyr::summarise(sum = sum(outcome, na.rm = T),
                   n = n()) %>%
  mutate(eventrate = paste(sum, n, sep = " / ")) %>%
  arrange(RANDASSIGN, AGE65, desc(SEX))

write.csv(event_sum,"output/event_sum.csv")

# bp trajectory----
bps_sprint <- readRDS("data/bps_sprint.rds")
bps_accord <- read.csv("/Users/Ji/Documents/biolincc/biolincc_data/ACCORD_2017b_2/Main_Study/3-Data_Sets-Analysis/3a-Analysis_Data_Sets/csv/bloodpressure.csv")

sprint_bps <- bps_sprint %>% 
  dplyr::select(MASKID, SEATSYS, visit) %>%
  rbind(sprint_data %>% dplyr::select(MASKID, SEATSYS = SEATSYS_0M) %>% mutate(visit = 0)) %>%
  left_join(sprint_data %>% dplyr::select(MASKID, RANDASSIGN, AGE65, SEX), by = "MASKID")

accord_bps<-bps_accord %>% 
  mutate(Visit = as.character(Visit)) %>%
  filter(Visit!="EXIT" & MaskID %in% accord_data$MaskID) %>%
  mutate(Visit = ifelse(Visit == "BLR", "F0", Visit)) %>%
  mutate(visit = as.numeric(gsub("F","",Visit))) %>%
  dplyr::select(MASKID = MaskID, SEATSYS = sbp, visit) %>%
  left_join(accord_data %>% 
              dplyr::select(MASKID = MaskID, RANDASSIGN = bp_trt, baseline_age, SEX) %>% 
              mutate(AGE65 = ifelse(baseline_age>=70,1,0)), by = "MASKID") %>%
  dplyr::select(MASKID,SEATSYS,visit,RANDASSIGN,AGE65,SEX)

comb_bps <- sprint_bps %>% 
  filter(visit %in% unique(accord_bps$visit[which(accord_bps$visit %in% sprint_bps$visit)])) %>%
  rbind(filter(accord_bps, visit %in% unique(accord_bps$visit[which(accord_bps$visit %in% sprint_bps$visit)]))) %>%
  group_by(RANDASSIGN, AGE65, SEX, visit) %>%
  dplyr::summarise(mean = mean(SEATSYS, na.rm = T),
                   n = n(),
                   se = sd(SEATSYS, na.rm = T)/sqrt(n)) %>%
  as.data.frame() %>%
  filter(AGE65==0) %>%
  mutate(visit = visit/12,
         group = ifelse(AGE65 == 0 & SEX==1, "Men", ifelse(AGE65 == 0 & SEX==2, "Women", ifelse(AGE65 == 1 & SEX==1, "Age 70 or older, Men", "Age 70 or older, Women"))))
# 
# data1y <-filter(comb_bps, visit == 1)
# 
# mean_f <- filter(data1y, group == "Women" & RANDASSIGN==0)$mean - filter(data1y, group == "Women" & RANDASSIGN==1)$mean
# se_f <- (filter(data1y, group == "Women" & RANDASSIGN==0)$se^2 + filter(data1y, group == "Women" & RANDASSIGN==1)$se^2)^0.5
# 
# mean_m <- filter(data1y, group == "Men" & RANDASSIGN==0)$mean - filter(data1y, group == "Men" & RANDASSIGN==1)$mean
# se_m <- (filter(data1y, group == "Men" & RANDASSIGN==0)$se^2 + filter(data1y, group == "Men" & RANDASSIGN==1)$se^2)^0.5
# 
# paste(sprintf("%.01f",mean_f)," (",sprintf("%.01f",mean_f-1.96*se_f),",",sprintf("%.01f",mean_f+1.96*se_f),")",sep = "")
# paste(sprintf("%.01f",mean_m)," (",sprintf("%.01f",mean_m-1.96*se_m),",",sprintf("%.01f",mean_m+1.96*se_m),")",sep = "")
# 2*pnorm(-abs((mean_f - mean_m)/(se_f^2 + se_m^2)^0.5))

library(gridExtra)
library(grid)

bps_data <- sprint_bps %>% 
  filter(visit %in% unique(accord_bps$visit[which(accord_bps$visit %in% sprint_bps$visit)])) %>%
  rbind(filter(accord_bps, visit %in% unique(accord_bps$visit[which(accord_bps$visit %in% sprint_bps$visit)]))) 

# 1year between-treatment diff -----

anova(glm(SEATSYS ~ RANDASSIGN + SEX, data = bps_data %>% filter(visit == 12)),
      glm(SEATSYS ~ RANDASSIGN * SEX, data = bps_data %>% filter(visit == 12)), 
      test = "LRT")

effect_men_1 <- summary(glm(SEATSYS ~ RANDASSIGN*factor(SEX, levels = c("1","2")), data = bps_data %>% filter(visit == 12)))
effect_women_1 <- summary(glm(SEATSYS ~ RANDASSIGN*factor(SEX, levels = c("2","1")), data = bps_data %>% filter(visit == 12)))

effect_men_1$coefficients %>%
  as.data.frame() %>%
  filter(rownames(.) == "RANDASSIGN") %>%
  mutate(conf = paste(sprintf("%.01f",-Estimate)," (",sprintf("%.01f",-(Estimate+1.96*`Std. Error`)),",",sprintf("%.01f",-(Estimate-1.96*`Std. Error`)),")",sep = ""))

effect_women_1$coefficients %>%
  as.data.frame() %>%
  filter(rownames(.) == "RANDASSIGN") %>%
  mutate(conf = paste(sprintf("%.01f",-Estimate)," (",sprintf("%.01f",-(Estimate+1.96*`Std. Error`)),",",sprintf("%.01f",-(Estimate-1.96*`Std. Error`)),")",sep = ""))

# overall between-treatment diff -----

library(lme4)

anova(lmer(SEATSYS ~ RANDASSIGN + SEX + (1|MASKID), data = bps_data),
      lmer(SEATSYS ~ RANDASSIGN * SEX + (1|MASKID), data = bps_data), 
      test = "LRT")

effect_men <- lmer(SEATSYS ~ RANDASSIGN * factor(SEX, levels = c("1","2")) + (1|MASKID), data = bps_data) %>% summary()
effect_women <- lmer(SEATSYS ~ RANDASSIGN * factor(SEX, levels = c("2","1")) + (1|MASKID), data = bps_data) %>% summary()


effect_men$coefficients %>%
  as.data.frame() %>%
  filter(rownames(.) == "RANDASSIGN") %>%
  mutate(conf = paste(sprintf("%.01f",-Estimate)," (",sprintf("%.01f",-(Estimate+1.96*`Std. Error`)),",",sprintf("%.01f",-(Estimate-1.96*`Std. Error`)),")",sep = ""))

effect_women$coefficients %>%
  as.data.frame() %>%
  filter(rownames(.) == "RANDASSIGN") %>%
  mutate(conf = paste(sprintf("%.01f",-Estimate)," (",sprintf("%.01f",-(Estimate+1.96*`Std. Error`)),",",sprintf("%.01f",-(Estimate-1.96*`Std. Error`)),")",sep = ""))

sbptraj1 <- ggplot(comb_bps) +
  geom_hline(yintercept = seq(115,145,5), color = "grey90") +
  coord_cartesian(ylim = c(115,145), clip="off") +
  geom_errorbar(aes(ymin = mean - 1.96*se, ymax = mean + 1.96*se, x = visit, color = factor(RANDASSIGN)), width = 0.1, show.legend = FALSE) +
  geom_point(aes(x = visit, y = mean, color = factor(RANDASSIGN)), size = 1, shape = 15) +
  geom_text(data = comb_bps %>% filter(visit == 0 | visit >=0.5), aes(x = visit, y = ifelse(RANDASSIGN == 1, 105.5,107), label = paste("N=",n,sep = ""), color = factor(RANDASSIGN)), size = 3) +
  geom_line(aes(x = visit, y = mean, color = factor(RANDASSIGN)), size = 0.5) +
  facet_wrap(. ~ factor(group, levels = c("Women","Men","Age 70 or older, Women","Age 70 or older, Men")), scales = "free", ncol = 2) +
  theme_classic() +
  scale_color_manual(name = "",
                     label = c("0" = "Standard Treatment", "1" = "Intensive Treatment", "3"="120-129","4"="130-139","5"="140 ~"),
                     values = c("1" = "black","0" = "grey50", "3"="#edb847","4"="#1c7265","5"="black")) +
  scale_x_continuous(name = "\nYears") +
  scale_y_continuous(name = "Systolic Blood Pressure, mm Hg\n",breaks = seq(115,145,5)) +
  theme(axis.title.y.left = element_text(color = "black",size =12,face="bold"),
        axis.text.y.left = element_text(color = "black",size =12,face="bold"),
        axis.text.x = element_text(color = "black",size =12,face="bold"),
        strip.text = element_text(color = "black",size =12,face="bold"),
        title = element_text(color = "black",size =12,face="bold"),
        legend.text = element_text(color = "black",size =12,face="bold"),
        strip.background = element_blank(),
        plot.margin = unit(c(1,1,5,1), "lines"))

sbptraj1

setEPS()
postscript("output/sbptraj.eps", width = 17, height = 6)
sbptraj1
dev.off()


# table 1----
library(tableone)
bsl_sprint_accord$DM %>% as.factor() %>% summary()
bsl_sprint_accord <- 
  rbind(sprint_data %>% 
          dplyr::select(id=MASKID, RANDASSIGN, AGE65, SEX, AGE,SBP = SBPAVG,DBP = DBPAVG,race = RACE_WHITE,BMI,SMK,HDL,LDL,TC,statin,aspirin=ASPIRIN,GFR = GFRESTIMATE,SERUMCREAT) %>%
          mutate(DM = 0, source="sprint"),
        accord_data %>% 
          dplyr::select(id=MaskID, RANDASSIGN=bp_trt, SEX, AGE = baseline_age,SBP = sbp, DBP = dbp, race = raceclass,HDL=hdl,LDL=ldl,TC=chol,statin,aspirin,GFR = gfr,SERUMCREAT=ucreat) %>% 
          mutate(DM = 1, source="accord", BMI = NA, SMK = NA, race = ifelse(race=="White",1,0), SERUMCREAT = SERUMCREAT/88.4, AGE65 = ifelse(AGE >= 70,1,0)))  %>% filter(AGE<70)

youngtab1 <- CreateTableOne(vars = c("AGE", "SBP","DBP", "race", "BMI", "SMK", "DM","HDL", "LDL", "TC", "statin","aspirin", "GFR","SERUMCREAT","source"),
                            factorVars = c("race", "DM","SMK", "aspirin", "statin","source"),
                            argsNonNormal = "SERUMCREAT",
                            strata = c("RANDASSIGN"),
                            data = bsl_sprint_accord %>% filter(AGE65==0 & SEX==1)) %>% print(smd = T, nonnormal = "SERUMCREAT")

youngtab2 <- CreateTableOne(vars = c("AGE", "SBP","DBP", "race", "BMI", "SMK", "DM","HDL", "LDL", "TC", "statin","aspirin", "GFR","SERUMCREAT","source"),
                            factorVars = c("race", "DM","SMK", "aspirin", "statin","source"),
                            strata = c("RANDASSIGN"),
                            data = bsl_sprint_accord %>% filter(AGE65==0 & SEX==2)) %>% print(smd = T, nonnormal = "SERUMCREAT")

alltab1 <- CreateTableOne(vars = c("AGE", "SBP","DBP", "race", "BMI", "SMK", "DM","HDL", "LDL", "TC", "statin","aspirin", "GFR","SERUMCREAT","source"),
                          factorVars = c("race", "DM","SMK", "aspirin", "statin"),
                          strata = c("SEX"),
                          data = bsl_sprint_accord,
                          addOverall = T) %>% print(nonnormal = "SERUMCREAT")

write.csv(cbind(youngtab1, youngtab2), "output/youngtab.csv")

write.csv(alltab1, "output/alltab_bysex.csv")

saveRDS(bsl_sprint_accord,"data/bsl_sprint_accord.rds")

# 
# # early onset events----
# 
# comb_sprint_accord_early <- 
#   rbind(sprint_data %>% dplyr::select(SEX, RANDASSIGN, AGE, chdtime, chd, chddeath, chddeathtime, statin, aspirin=ASPIRIN, chf, chftime, hd, hdtime, cvddeath, cvddeathtime, nfchd, nfchdtime, death, deathtime, cvd_hx_baseline) %>% mutate(source="sprint"),
#         accord_data %>% dplyr::select(SEX, RANDASSIGN = bp_trt, AGE = baseline_age, chdtime, chd, chddeath, chddeathtime, statin, aspirin, chf, chftime, hd, hdtime, cvddeath, cvddeathtime, nfchd, nfchdtime, death, deathtime, cvd_hx_baseline) %>% mutate(source="accord")) %>%
#   mutate(AGE65= ifelse(AGE >=70,1,0),
#          id = rownames(.)) %>%
#   filter(AGE<70) 
# 
# term = "nfchd65"
# 
# comb_sprint_accord_early$outcome <- comb_sprint_accord_early %>% dplyr::select(term) %>% pull()
# comb_sprint_accord_early$outcometime <- comb_sprint_accord_early %>% dplyr::select(paste(term,"time",sep = "")) %>% pull()
# 
# coxph(Surv(outcometime, outcome) ~ RANDASSIGN*factor(SEX, levels = c(2,1)) + strata(source) + cluster(id), data = comb_sprint_accord_early)
# coxph(Surv(outcometime, outcome) ~ RANDASSIGN*factor(SEX, levels = c(1,2)) + strata(source) + cluster(id), data = comb_sprint_accord_early)
# 
# anova(coxph(Surv(outcometime, outcome) ~ RANDASSIGN*factor(SEX, levels = c(2,1)) + strata(source) + cluster(id), data = comb_sprint_accord_early),
#       coxph(Surv(outcometime, outcome) ~ RANDASSIGN + factor(SEX, levels = c(1,2)) + strata(source) + cluster(id), data = comb_sprint_accord_early))
# 
# 
# comb_sprint_accord_all <- 
#   rbind(sprint_data %>% dplyr::select(SEX, RANDASSIGN, AGE, chdtime, chd, chddeath, chddeathtime, statin, aspirin=ASPIRIN, chf, chftime, hd, hdtime, cvddeath, cvddeathtime, nfchd, nfchdtime, death, deathtime, cvd_hx_baseline) %>% mutate(source="sprint"),
#         accord_data %>% dplyr::select(SEX, RANDASSIGN = bp_trt, AGE = baseline_age, chdtime, chd, chddeath, chddeathtime, statin, aspirin, chf, chftime, hd, hdtime, cvddeath, cvddeathtime, nfchd, nfchdtime, death, deathtime, cvd_hx_baseline) %>% mutate(source="accord")) %>%
#   mutate(AGE65= ifelse(AGE >=70,1,0),
#          id = rownames(.)) %>%
#   mutate(chd65 = ifelse(chd == 1 & AGE + chdtime<70,1,0),
#          chd65time = ifelse(chd == 1 & AGE + chdtime<70,chdtime,70),
#          nfchd65 = ifelse(nfchd == 1 & AGE + nfchdtime<70,1,0),
#          nfchd65time = ifelse(nfchd == 1 & AGE + nfchdtime<70,nfchdtime,70)) 
# 
# test<-comb_sprint_accord_all %>% 
#   filter(death == 1) %>%
#   mutate(death_age = AGE + deathtime,
#          AGE = round(AGE))
# 
# glm(death_age ~ SEX, data = test) %>% summary()
# 
# table(test$AGE, test$death_age)
# 
# test2 <- rbind(sprint_data %>% dplyr::select(SEX, RANDASSIGN, AGE, chdtime, chd, chddeath, chddeathtime, statin, aspirin=ASPIRIN, chf, chftime, hd, hdtime, cvddeath, cvddeathtime, nfchd, nfchdtime, death, deathtime, cvd_hx_baseline) %>% mutate(source="sprint"),
#                accord_data %>% dplyr::select(SEX, RANDASSIGN = bp_trt, AGE = baseline_age, chdtime, chd, chddeath, chddeathtime, statin, aspirin, chf, chftime, hd, hdtime, cvddeath, cvddeathtime, nfchd, nfchdtime, death, deathtime, cvd_hx_baseline) %>% mutate(source="accord")) %>%
#   mutate(AGE65= ifelse(AGE >=70,1,0),
#          id = rownames(.)) %>%
#   mutate(chd65 = ifelse(chd == 1 & AGE + chdtime<70,1,0),
#          chd65time = ifelse(chd == 1 & AGE + chdtime<70,chdtime,70),
#          nfchd65 = ifelse(nfchd == 1 & AGE + nfchdtime<70,1,0),
#          nfchd65time = ifelse(nfchd == 1 & AGE + nfchdtime<70,nfchdtime,70),
#          death_age = AGE + deathtime,
#          AGE = round(AGE)) %>% 
#   filter(AGE >= 50) %>%
#   mutate(deathtime = ifelse(death == 1, deathtime, NA)) %>%
#   group_by(AGE, RANDASSIGN, SEX) %>%
#   dplyr::summarise(death = sum(death, na.rm = T),
#                    all = n(),
#                    surv_time = mean(deathtime, na.rm = T),
#                    eventrate = death / all)
# 
# glm(eventrate ~ SEX*RANDASSIGN, data = test2) %>% summary()




