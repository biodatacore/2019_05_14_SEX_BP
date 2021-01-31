library(haven)
library(MASS)
library(reshape2)
library(reshape)
library(dplyr) 
library(magrittr)
library(survival)
library(rms)

comb_visits <- readRDS("data/comb_visits.rds") %>% 
  filter(
    (cohort == "FHS" & visit == 2)|
    (cohort == "MESA" & visit == 2)|
    (cohort == "ARIC" & visit == 2)|
    (cohort == "CARDIA" & visit == 4)|
    # (cohort == "JHS" & visit == 1)|
    (cohort == "CHS" & visit == 1)
  )

comb_event <- readRDS("data/comb_event.rds")


mddata <- comb_event %>% 
  dplyr::select(-cohort) %>%
  left_join(comb_visits, by = "id") %>%
  filter(!is.na(visit))
nrow(mddata)

mddata %<>% na.omit()
nrow(mddata)

mddata %<>%
  mutate(race = ifelse(race==1,1,0),
         SBP10 = - SBP/10,
         chdtime = chd_age - AGE,
         nfchdtime = nfchd_age - AGE,
         chddeathtime = chddeath_age - AGE,
         SBPgroup = ifelse(SBP<120,1,0)) %>%
  filter(AGE < 70 & HRX==1 & SBP < 140) %>%
  filter(chdtime >= 0 & nfchdtime >= 0 & chddeathtime >= 0)
nrow(mddata)

mddata$SBPgroup %>% as.factor() %>% summary()
mddata$AGE %>% summary()
mddata$AGE %>% sd()
mddata$SEX %>% summary()
mddata$race %>% summary()
mddata$chd %>% sum()
mddata$chdtime %>% summary()
mddata$chdtime %>% sd()
mddata$cohort %>% as.factor() %>% summary()
plot(factor(mddata$cohort), mddata$LDL)
plot(factor(mddata$cohort), mddata$HDL)
plot(factor(mddata$cohort), mddata$TC)

# Table 1 -------

library(tableone)

cohort_tab1 <- CreateTableOne(vars = c("AGE", "SBP", "DBP", "BMI", "SMK", "DM", "HDL", "LDL", "TC", "CHOLMED", "race","cohort"),
                           factorVars = c("HRX", "SMK", "DM", "CHOLMED", "race", "cohort"),
                           strata = c("SBPgroup","SEX"),
                           data = mddata %>% filter(SEX==1)) %>% print(smd = T)

write.csv(cohort_tab1, "output/cohort_tab1.csv")


cohort_tab2 <- CreateTableOne(vars = c("AGE", "SBP", "DBP", "BMI", "SMK", "DM", "HDL", "LDL", "TC", "CHOLMED", "race","cohort"),
                              factorVars = c("HRX", "SMK", "DM", "CHOLMED", "race", "cohort"),
                              strata = c("SBPgroup","SEX"),
                              data = mddata %>% filter(SEX==2)) %>% print(smd = T)

write.csv(cohort_tab2, "output/cohort_tab2.csv")


# between treatment diff-------

glm(SBP ~ SBPgroup*factor(SEX, levels = c(1,2)), data = mddata) %>% summary()
glm(SBP ~ SBPgroup*factor(SEX, levels = c(2,1)), data = mddata) %>% summary()

summary(glm(SBP ~ SBPgroup*factor(SEX, levels = c(1,2)), data = mddata))$coefficients %>%
  as.data.frame() %>%
  filter(rownames(.) == "SBPgroup") %>%
  mutate(conf = paste(sprintf("%.01f",-Estimate)," (",sprintf("%.01f",-(Estimate+1.96*`Std. Error`)),",",sprintf("%.01f",-(Estimate-1.96*`Std. Error`)),")",sep = ""))

summary(glm(SBP ~ SBPgroup*factor(SEX, levels = c(2,1)), data = mddata))$coefficients %>%
  as.data.frame() %>%
  filter(rownames(.) == "SBPgroup") %>%
  mutate(conf = paste(sprintf("%.01f",-Estimate)," (",sprintf("%.01f",-(Estimate+1.96*`Std. Error`)),",",sprintf("%.01f",-(Estimate-1.96*`Std. Error`)),")",sep = ""))

anova(glm(SBP ~ SBPgroup*SEX, data = mddata),
      glm(SBP ~ SBPgroup + SEX, data = mddata),
      test = "LRT")


# PH model ----
term = "chd"

mddata$outcome <- mddata %>% dplyr::select(term) %>% pull()
mddata$outcometime <- mddata %>% dplyr::select(paste(term,"time",sep = "")) %>% pull()

young2 <- coxph(Surv(outcometime, outcome) ~ factor(SBPgroup)*factor(SEX, levels = c(2,1)) + AGE + BMI + SMK + DM + LDL + HDL + TC + CHOLMED + race + strata(cohort) + cluster(id), data = mddata)
young1 <- coxph(Surv(outcometime, outcome) ~ factor(SBPgroup)*factor(SEX, levels = c(1,2)) + AGE + BMI + SMK + DM + LDL + HDL + TC + CHOLMED + race + strata(cohort) + cluster(id), data = mddata)

young2
young1

anova(coxph(Surv(outcometime, outcome) ~ SBPgroup*factor(SEX, levels = c(2,1)) + AGE + BMI + SMK + DM + LDL + HDL + TC + CHOLMED + race + strata(cohort) + cluster(id), data = mddata),
      coxph(Surv(outcometime, outcome) ~ SBPgroup + factor(SEX, levels = c(2,1)) + AGE + BMI + SMK + DM + LDL + HDL + TC + CHOLMED + race + strata(cohort) + cluster(id), data = mddata),
      test = "LRT")

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

tab_sum <- all_coefs(tab) %>%
  mutate(hr = exp(coef),
         low = exp(coef-1.96*`robust se`),
         up = exp(coef+1.96*`robust se`),
         conf=paste(sprintf("%.02f",hr)," (",sprintf("%.02f",low),",",sprintf("%.02f",up),")",sep = "")) %>%
  filter(grepl("SBPgroup", term) & !grepl("SEX", term))

write.csv(tab_sum, "output/tab_sum_cohort.csv")


ddist <- datadist(mddata)
options(datadist="ddist")
survplot(cph(Surv(outcometime, outcome) ~ SBPgroup, data = mddata, subset = SEX==1, x=TRUE, y=TRUE), type = "kaplan-meier")
survplot(cph(Surv(outcometime, outcome) ~ SBPgroup, data = mddata, subset = SEX==2, x=TRUE, y=TRUE), type = "kaplan-meier")

eventnum <- mddata %>% 
  group_by(SEX, SBPgroup) %>% 
  dplyr::summarise(nevent = sum(outcome=="1", na.rm = T),
                   total = n()) %>%
  arrange(desc(SEX)) %>%
  as.data.frame() %>%
  mutate(eventrate = paste(nevent,total,sep = " / "))

write.csv(eventnum, "output/eventnum_cohort.csv")

kmdata1 <- mddata %>% filter(SEX==1)
kmdata2 <- mddata %>% filter(SEX==2)

kmtest1 <- survdiff(Surv(outcometime, outcome) ~ SBPgroup, data = kmdata1)
kmtest2 <- survdiff(Surv(outcometime, outcome) ~ SBPgroup, data = kmdata2)

p.val1 <- 1 - pchisq(kmtest1$chisq, length(kmtest1$n) - 1)
p.val2 <- 1 - pchisq(kmtest2$chisq, length(kmtest2$n) - 1)

library(ggkm) # install.packages("ggkm")
# install.packages("devtools")
# library(devtools)
# install_github("michaelway/ggkm")
# library(ggkm)
library(ggquickeda) # install.packages("ggquickeda")

signum <- function(x){x = ifelse(x < 0.001,"<0.001", ifelse(x < 0.01, sprintf("%.03f", x), ifelse(x < 0.06 ,sprintf("%.03f", x), sprintf("%.02f", x))))}

surv_curv1 <- ggplot() + 
  coord_cartesian(xlim = c(0,25), ylim = c(0,1)) +
  geom_km(aes(time = outcometime, status = outcome, color = factor(SBPgroup)), trans = "cumhaz", size = 1,data = kmdata1) +
  scale_color_manual(name = "",
                     label = c("0" = "Systolic BP 120-139 mm Hg", "1" = "Systolic BP <120 mm Hg", "3"="120-129","4"="130-139","5"="140 ~"),
                     values = c("1" = "#003366","0" = "#3399FF", "3"="#edb847","4"="#1c7265","5"="black")) +
  scale_fill_manual(name = "",
                    label = c("0" = "Systolic BP 120-139 mm Hg", "1" = "Systolic BP <120 mm Hg", "3"="120-129","4"="130-139","5"="140 ~"),
                    values = c("1" = "#003366","0" = "#3399FF", "3"="#edb847","4"="#1c7265","5"="black")) +
  scale_x_continuous(breaks = seq(0,25,5), name = "\nYears") +
  scale_y_continuous(name = "Cumulative Hazard") +
  theme_classic() +
  theme(axis.title.y.left = element_text(color = "black",size =15,face="bold"),
        axis.text = element_text(color = "black",size =15,face="bold"),
        title = element_text(color = "black",size =15,face="bold"),
        legend.text = element_text(color = "black",size =15,face="bold"),
        legend.title = element_blank(),
        legend.position = c(0.3,0.9)) +
  annotate("text",0, 0.65, label=ifelse(p.val1<0.001,paste("Log-rank test, P<0.001",sep = ""),
                                         paste("Log-rank test, P = ",signum(p.val1),sep="")),
           fontface = 1, col = c('black'), size = 5, hjust = 0) +
  annotate("text",0, 0.75, label=paste("Adjusted HR (95%CI): ", tab_sum$conf[2],sep = ""),
           fontface = 1, col = c('black'), size = 5, hjust = 0) +
  guides(color = guide_legend(nrow = 2, title.position ="left")) 


surv_curv1

surv_curv2 <- ggplot() + 
  coord_cartesian(xlim = c(0,25), ylim = c(0,1)) +
  geom_km(aes(time = outcometime, status = outcome, color = factor(SBPgroup)), trans = "cumhaz", size = 1,data = kmdata2) +
  scale_color_manual(name = "",
                     label = c("0" = "Systolic BP 120-139 mm Hg", "1" = "Systolic BP <120 mm Hg", "3"="120-129","4"="130-139","5"="140 ~"),
                     values = c("1" = "#a42f3e","0" = "#e1743c", "3"="#edb847","4"="#1c7265","5"="black")) +
  scale_fill_manual(name = "",
                    label = c("0" = "Systolic BP 120-139 mm Hg", "1" = "Systolic BP <120 mm Hg", "3"="120-129","4"="130-139","5"="140 ~"),
                    values = c("1" = "#a42f3e","0" = "#e1743c", "3"="#edb847","4"="#1c7265","5"="black")) +
  scale_x_continuous(breaks = seq(0,30,5), name = "\nYears") +
  scale_y_continuous(name = "Cumulative Hazard") +
  theme_classic() +
  theme(axis.title.y.left = element_text(color = "black",size =15,face="bold"),
        axis.text = element_text(color = "black",size =15,face="bold"),
        title = element_text(color = "black",size =15,face="bold"),
        legend.text = element_text(color = "black",size =15,face="bold"),
        legend.title = element_blank(),
        legend.position = c(0.3,0.9)) +
  annotate("text",0, 0.65, label=ifelse(p.val2<0.001,paste("Log-rank test, P<0.001",sep = ""),
                                       paste("Log-rank test, P = ",signum(p.val2),sep="")),
           fontface = 1, col = c('black'), size = 5, hjust = 0) +
  annotate("text",0, 0.75, label=paste("Adjusted HR (95%CI): ", tab_sum$conf[1],sep = ""),
           fontface = 1, col = c('black'), size = 5, hjust = 0) +
  guides(color = guide_legend(nrow = 2, title.position ="left")) 


surv_curv2

library(survminer)

table1 <- ggsurvplot(survfit(Surv(outcometime, outcome) ~ SBPgroup, data = kmdata1),
                     data = kmdata1,
                     break.x.by = 1,
                     fun = "cumhaz",
                     risk.table = TRUE)$data.survtable

risktable1 <- ggplot() +
  geom_text(aes(x = time, y = strata, label = n.risk), size = 5, data = table1) +
  scale_x_continuous(limits = c(0,5), breaks = seq(0,5,1)) +
  scale_y_discrete(name = "", labels = c("SBPgroup=1" = "Intensive", "SBPgroup=0" = "Standard")) +
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

table2 <- ggsurvplot(survfit(Surv(outcometime, outcome) ~ SBPgroup, data = kmdata2),
                     data = kmdata2,
                     break.x.by = 1,
                     fun = "cumhaz",
                     risk.table = TRUE)$data.survtable

risktable2 <- ggplot() +
  geom_text(aes(x = time, y = strata, label = n.risk), size = 5, data = table2) +
  scale_x_continuous(limits = c(0,5), breaks = seq(0,5,1)) +
  scale_y_discrete(name = "", labels = c("SBPgroup=1" = "Intensive", "SBPgroup=0" = "Standard")) +
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

risktable2


setEPS()
postscript("output/KM1.eps")
cowplot::plot_grid(surv_curv1, risktable1, rel_heights=c(2.5, 0.8), ncol = 1, align = "v")
dev.off()

setEPS()
postscript("output/KM2.eps")
cowplot::plot_grid(surv_curv2, risktable2, rel_heights=c(2.5, 0.8), ncol = 1, align = "v")
dev.off()


