library(dplyr)
library(magrittr)
library(ggplot2)

# fhs

fhsclin4568 <- readRDS("data/fhsclin_ex4568.rds")

dat1 <- fhsclin4568 %>%
  dplyr::select(AGE = AGE1, SEX = SEX, DM = curr_diab1, SBP = SBP1, DBP = DBP1, HRX = HRX1, BMI = BMI1, ID, SMK = CURRSMK1, FG = fasting_bg1) %>%
  mutate(TP = 1)
dat2 <- fhsclin4568 %>%
  dplyr::select(AGE = AGE2, SEX = SEX, DM = curr_diab2, SBP = SBP2, DBP = DBP2, HRX = HRX2, BMI = BMI2, ID, SMK = CURRSMK2, FG = fasting_bg2) %>%
  mutate(TP = 2)
dat3 <- fhsclin4568 %>%
  dplyr::select(AGE = AGE3, SEX = SEX, DM = curr_diab3, SBP = SBP3, DBP = DBP3, HRX = HRX3, BMI = BMI3, ID, SMK = CURRSMK3, FG = FASTING_BG3) %>%
  mutate(TP = 3)
dat4 <- fhsclin4568 %>%
  dplyr::select(AGE = AGE4, SEX = SEX, DM = curr_diab4, SBP = SBP4, DBP = DBP4, HRX = HRX4, BMI = BMI4, ID, SMK = CURRSMK4, FG = FASTING_BG4) %>%
  mutate(TP = 4)
dat5 <- fhsclin4568 %>%
  dplyr::select(AGE = AGE5, SEX = SEX, DM = curr_diab5, SBP = SBP5, DBP = DBP5, HRX = HRX5, BMI = BMI5, ID, SMK = CURRSMK5, FG = FASTING_BG5) %>%
  mutate(TP = 5)
dat6 <- fhsclin4568 %>%
  dplyr::select(AGE = AGE6, SEX = SEX, DM = curr_diab6, SBP = SBP6, DBP = DBP6, HRX = HRX6, BMI = BMI6, ID, SMK = CURRSMK6, FG = FASTING_BG6) %>%
  mutate(TP = 6)
dat7 <- fhsclin4568 %>%
  dplyr::select(AGE = AGE7, SEX = SEX, DM = curr_diab7, SBP = SBP7, DBP = DBP7, HRX = HRX7, BMI = BMI7, ID, SMK = CURRSMK7, FG = FASTING_BG7) %>%
  mutate(TP = 7)
dat8 <- fhsclin4568 %>%
  dplyr::select(AGE = AGE8, SEX = SEX, DM = curr_diab8, SBP = SBP8, DBP = DBP8, HRX = HRX8, BMI = BMI8, ID, SMK = CURRSMK8, FG = FASTING_BG8) %>%
  mutate(TP = 8)
dat9 <- fhsclin4568 %>%
  dplyr::select(AGE = AGE9, SEX = SEX, DM = curr_diab9, SBP = SBP9, DBP = DBP9, HRX = HRX9, BMI = BMI9, ID, SMK = CURRSMK9, FG = FASTING_BG9) %>%
  mutate(TP = 9)

dat <- rbind(dat1, dat2, dat3, dat4, dat5, dat6, dat7, dat8, dat9) 

rm(dat1, dat2, dat3, dat4, dat5, dat6, dat7, dat8, dat9)

fhs <- dat[,c("SBP","DBP","SEX","AGE","HRX","ID","TP")] %>% 
  mutate(id=ID, 
         cohort = "fhs",
         TP = paste("fhs",TP,sep = "")) %>% 
  dplyr::select(-ID) %>% na.omit()

rm(dat)

# MESA

library(haven)

MESA_ALT <- read.csv("data/mesa_cleaned.csv")
MESA_ALT <- MESA_ALT[,c("SBP","DBP","SEX","AGE","HRX","id","TP")] %>% 
  mutate(SEX = ifelse(SEX=="F",2,1), 
         cohort = "MESA",
         SBP = SBP + 0.5, 
         DBP = DBP + 2.9,
         TP = paste("mesa",TP,sep = "")) %>%
  na.omit() 

# CARDIA

cardia <- read.csv("data/cardia_cleaned.csv")
cardia <- cardia[,c("SBP","DBP","SEX","AGE","HRX","id","TP")] %>% 
  mutate(SEX = ifelse(SEX=="F",2,1), 
         cohort = "cardia",
         SBP = ifelse(TP == 20 | TP == 25, 3.74 + 0.96*SBP, SBP),
         DBP = ifelse(TP == 20 | TP == 25, 1.30 + 0.97*DBP, DBP),
         SBP = SBP + 2.6,
         DBP = DBP + 6.2,
         HRX = HRX -1,
         TP = paste("cardia",TP,sep = "")) %>% 
  na.omit()


# ARIC 

ARIC <- read_dta("data/ARIC_main-for-PAR.dta")
ARIC_dat1 <- ARIC %>% dplyr::select(AGE = v1age, SEX = gender, SBP = v1sbp, DBP = v1dbp, HRX = v1htnmed, id = id) %>% 
  mutate(TP = 1)
ARIC_dat2 <- ARIC %>% dplyr::select(AGE = v2age, SEX = gender, SBP = v2sbp, DBP = v2dbp, HRX = v2htnmed, id = id) %>% 
  mutate(TP = 2)
ARIC_dat3 <- ARIC %>% dplyr::select(AGE = v3age, SEX = gender, SBP = v3sbp, DBP = v3dbp, HRX = v3htnmed, id = id) %>% 
  mutate(TP = 3)
ARIC_dat4 <- ARIC %>% dplyr::select(AGE = v4age, SEX = gender, SBP = v4sbp, DBP = v4dbp, HRX = v4htnmed, id = id) %>% 
  mutate(TP = 4) 


rm(ARIC)
ARIC_comb <- rbind(ARIC_dat1, ARIC_dat2, ARIC_dat3, ARIC_dat4) %>% mutate(SEX = ifelse(SEX=="F",2,1), 
                                                                          SBP = SBP + 2.6,
                                                                          DBP = DBP + 6.2,
                                                                          cohort = "ARIC",
                                                                          TP = paste("aric",TP,sep = "")) %>% na.omit()

comb_dat <- rbind(cardia, fhs, MESA_ALT, ARIC_comb) %>% 
  mutate(SBP = ifelse(HRX==1, SBP + 10 , SBP),
         DBP = ifelse(HRX==1, DBP + 5 , DBP),
         PP = SBP - DBP,
         MAP = DBP + 1/3*PP,
         AGE = round(AGE))

saveRDS(comb_dat, "data/comb_dat.rds")

# fhs[,"id"] %>% unique() %>% nrow()
# ARIC_comb[,"id"] %>% unique() %>% nrow()
# cardia[which(cardia$TP=="cardia0"),]
# cardia[,"id"] %>% unique() %>% length()
# MESA_ALT[,"id"] %>% unique() %>% length()
# MESA_ALT[which(MESA_ALT$TP=="mesa1"),]$id %>% unique() %>% length()
# 
# unique(fhs[,c("id","SEX")])$SEX %>% as.factor() %>% summary()
# unique(ARIC_comb[,c("id","SEX")])$SEX %>% as.factor() %>% summary()
# unique(cardia[,c("id","SEX")])$SEX %>% as.factor() %>% summary()
# unique(MESA_ALT[,c("id","SEX")])$SEX %>% as.factor() %>% summary()

comb_dat <- readRDS("data/comb_dat.rds")


library(lme4)
library(splines)
library(rms)
library(nlme)
library(merTools) # install.packages("merTools")
library(plotrix)
library(bootpredictlme4) # devtools::install_github("remkoduursma/bootpredictlme4")

# get cental 99% range of age

plot_x_min <- max(c(quantile(comb_dat[which(comb_dat$SEX==1),]$AGE, 0.005,na.rm = T),quantile(comb_dat[which(comb_dat$SEX==2),]$AGE, 0.005,na.rm = T)))

plot_x_max <- min(c(quantile(comb_dat[which(comb_dat$SEX==1),]$AGE, 0.995,na.rm = T),quantile(comb_dat[which(comb_dat$SEX==2),]$AGE, 0.995,na.rm = T)))


# SBP

modm1 <- lmer(SBP ~ bs(AGE,3) + TP + (1|id), data = comb_dat %>% filter(SEX==1))
modf1 <- lmer(SBP ~ bs(AGE,3) + TP + (1|id), data = comb_dat %>% filter(SEX==2))

boot_m1 <- predict(modm1, newdata = datm1, re.form=NA, se.fit=TRUE, nsim=100)
boot_f1 <- predict(modf1, newdata = datf1, re.form=NA, se.fit=TRUE, nsim=100)

fit_m1 <- data.frame(AGE = datm1$AGE,
                    fit = boot_m1$fit,
                    se = boot_m1$se.fit) %>%
  mutate(upper = fit + 1.96*se,
         lower = fit - 1.96*se)
fit_m1$dif <- c(diff(fit_m1$fit), NA)

fit_f1 <- data.frame(AGE = datf1$AGE,
                    fit = boot_f1$fit,
                    se = boot_f1$se.fit) %>%
  mutate(upper = fit + 1.96*se,
         lower = fit - 1.96*se)
fit_f1$dif <- c(diff(fit_f1$fit), NA)


base1 <- ggplot() +
  coord_cartesian(xlim = c(plot_x_min,plot_x_max)) +
  geom_line(aes(x = AGE, y = fit, color = "blue4"), data = fit_m1) +
  geom_ribbon(aes(x = AGE, ymin = lower, ymax = upper, fill = "blue4"), alpha = 0.2, data = fit_m1) +
  geom_line(aes(x = AGE, y = fit, color = "red4"), data = fit_f1) +
  geom_ribbon(aes(x = AGE, ymin = lower, ymax = upper, fill = "red4"), alpha = 0.2, data = fit_f1) +
  scale_color_manual(name = "Sex", values = c("red4"="red4", "blue4"="blue4"), labels = c("red4"="Women","blue4"="Men")) +
  scale_fill_manual(name = "Sex", values = c("red4"="red4", "blue4"="blue4"), labels = c("red4"="Women","blue4"="Men")) +
  scale_y_continuous(name = "SBP, mm Hg") + 
  scale_x_continuous(breaks = seq(from = 20, to = 80, by = 10)) + 
  ggtitle("Systolic Blood Pressure") +
  # ggtitle("Unadjusted SBP") +
  theme_bw() +
  theme(axis.title = element_text(color = "#434443",size =15,face="bold"),
        axis.text = element_text(color = "#434443",size =12,face="bold"),
        title = element_text(color = "#434443",size =16,face="bold"),
        legend.text = element_text(color = "#434443",size =10,face="bold"))

ggplot() +
  coord_cartesian(xlim = c(plot_x_min,plot_x_max)) +
  geom_line(aes(x = AGE, y = dif, color = "blue4"), data = fit_m1) +
  geom_line(aes(x = AGE, y = dif, color = "red4"), data = fit_f1) +
  scale_color_manual(name = "Sex", values = c("red4"="red4", "blue4"="blue4"), labels = c("red4"="Women","blue4"="Men")) +
  scale_fill_manual(name = "Sex", values = c("red4"="red4", "blue4"="blue4"), labels = c("red4"="Women","blue4"="Men")) +
  scale_y_continuous(name = "SBP Increase Rate, mmHg/year") + 
  scale_x_continuous(breaks = seq(from = 20, to = 80, by = 10)) + 
  ggtitle("SBP Increase Rate, mmHg/year") +
  theme_bw() +
  theme(axis.title = element_text(color = "#434443",size =15,face="bold"),
        axis.text = element_text(color = "#434443",size =12,face="bold"),
        title = element_text(color = "#434443",size =16,face="bold"),
        legend.text = element_text(color = "#434443",size =10,face="bold"))

y_base_m1 <- fit_m1[1,"fit"]
y_base_f1 <- fit_f1[1,"fit"]

adj1 <- ggplot() +
  coord_cartesian(xlim = c(plot_x_min, plot_x_max)) +
  geom_line(aes(x = AGE, y = fit - y_base_m1, color = "blue4"), data = fit_m1) +
  geom_ribbon(aes(x = AGE, ymin = lower - y_base_m1, ymax = upper - y_base_m1, fill = "blue4"), alpha = 0.2, data = fit_m1) +
  geom_line(aes(x = AGE, y = fit - y_base_f1, color = "red4"), data = fit_f1) +
  geom_ribbon(aes(x = AGE, ymin = lower - y_base_f1, ymax = upper - y_base_f1, fill = "red4"), alpha = 0.2, data = fit_f1) +
  scale_color_manual(name = "Sex", values = c("red4"="red4", "blue4"="blue4"), labels = c("red4"="Women","blue4"="Men")) +
  scale_fill_manual(name = "Sex", values = c("red4"="red4", "blue4"="blue4"), labels = c("red4"="Women","blue4"="Men")) +
  scale_y_continuous(name = "SBP Elevation, mm Hg") + 
  scale_x_continuous(breaks = seq(from = 20, to = 80, by = 10)) + 
  ggtitle("SBP Elevation from Baseline") +
  # ggtitle("Baseline Adjusted SBP") +
  theme_bw() +
  theme(axis.title = element_text(color = "#434443",size =15,face="bold"),
        axis.text = element_text(color = "#434443",size =12,face="bold"),
        title = element_text(color = "#434443",size =16,face="bold"),
        legend.text = element_text(color = "#434443",size =10,face="bold"))



# DBP

modm2 <- lmer(DBP ~ bs(AGE,3) + TP + (1|id), data = comb_dat %>% filter(SEX==1))
modf2 <- lmer(DBP ~ bs(AGE,3) + TP + (1|id), data = comb_dat %>% filter(SEX==2))
datm2 <- data.frame(AGE = plot_x_min:plot_x_max, TP = "cardia0", id = "1")
datf2 <- data.frame(AGE = plot_x_min:plot_x_max, TP = "cardia0", id = "1")

boot_m2 <- predict(modm2, newdata = datm2, re.form=NA, se.fit=TRUE, nsim=100)
boot_f2 <- predict(modf2, newdata = datf2, re.form=NA, se.fit=TRUE, nsim=100)

fit_m2 <- data.frame(AGE = datm2$AGE,
                    fit = boot_m2$fit,
                    se = boot_m2$se.fit) %>%
  mutate(upper = fit + 1.96*se,
         lower = fit - 1.96*se)
fit_m2$dif <- c(diff(fit_m2$fit), NA)

fit_f2 <- data.frame(AGE = datf2$AGE,
                    fit = boot_f2$fit,
                    se = boot_f2$se.fit) %>%
  mutate(upper = fit + 1.96*se,
         lower = fit - 1.96*se)
fit_f2$dif <- c(diff(fit_f2$fit), NA)


base2 <- ggplot() +
  coord_cartesian(xlim = c(plot_x_min,plot_x_max)) +
  geom_line(aes(x = AGE, y = fit, color = "blue4"), data = fit_m2) +
  geom_ribbon(aes(x = AGE, ymin = lower, ymax = upper, fill = "blue4"), alpha = 0.2, data = fit_m2) +
  geom_line(aes(x = AGE, y = fit, color = "red4"), data = fit_f2) +
  geom_ribbon(aes(x = AGE, ymin = lower, ymax = upper, fill = "red4"), alpha = 0.2, data = fit_f2) +
  scale_color_manual(name = "Sex", values = c("red4"="red4", "blue4"="blue4"), labels = c("red4"="Women","blue4"="Men")) +
  scale_fill_manual(name = "Sex", values = c("red4"="red4", "blue4"="blue4"), labels = c("red4"="Women","blue4"="Men")) +
  scale_y_continuous(name = "DBP, mm Hg") + 
  scale_x_continuous(breaks = seq(from = 20, to = 80, by = 10)) + 
  ggtitle("Diastolic Blood Pressure") +
  # ggtitle("Unadjusted DBP") +
  theme_bw() +
  theme(axis.title = element_text(color = "#434443",size =15,face="bold"),
        axis.text = element_text(color = "#434443",size =12,face="bold"),
        title = element_text(color = "#434443",size =16,face="bold"),
        legend.text = element_text(color = "#434443",size =10,face="bold"))

ggplot() +
  coord_cartesian(xlim = c(plot_x_min,plot_x_max)) +
  geom_line(aes(x = AGE, y = dif, color = "blue4"), data = fit_m2) +
  geom_line(aes(x = AGE, y = dif, color = "red4"), data = fit_f2) +
  scale_color_manual(name = "Sex", values = c("red4"="red4", "blue4"="blue4"), labels = c("red4"="Women","blue4"="Men")) +
  scale_fill_manual(name = "Sex", values = c("red4"="red4", "blue4"="blue4"), labels = c("red4"="Women","blue4"="Men")) +
  scale_y_continuous(name = "DBP Increase Rate, mmHg/year") + 
  scale_x_continuous(breaks = seq(from = 20, to = 80, by = 10)) + 
  ggtitle("DBP Increase Rate, mmHg/year") +
  theme_bw() +
  theme(axis.title = element_text(color = "#434443",size =15,face="bold"),
        axis.text = element_text(color = "#434443",size =12,face="bold"),
        title = element_text(color = "#434443",size =16,face="bold"),
        legend.text = element_text(color = "#434443",size =10,face="bold"))

y_base_m2 <- fit_m2[1,"fit"]
y_base_f2 <- fit_f2[1,"fit"]

adj2 <- ggplot() +
  coord_cartesian(xlim = c(plot_x_min, plot_x_max)) +
  geom_line(aes(x = AGE, y = fit - y_base_m2, color = "blue4"), data = fit_m2) +
  geom_ribbon(aes(x = AGE, ymin = lower - y_base_m2, ymax = upper - y_base_m2, fill = "blue4"), alpha = 0.2, data = fit_m2) +
  geom_line(aes(x = AGE, y = fit - y_base_f2, color = "red4"), data = fit_f2) +
  geom_ribbon(aes(x = AGE, ymin = lower - y_base_f2, ymax = upper - y_base_f2, fill = "red4"), alpha = 0.2, data = fit_f2) +
  scale_color_manual(name = "Sex", values = c("red4"="red4", "blue4"="blue4"), labels = c("red4"="Women","blue4"="Men")) +
  scale_fill_manual(name = "Sex", values = c("red4"="red4", "blue4"="blue4"), labels = c("red4"="Women","blue4"="Men")) +
  scale_y_continuous(name = "DBP Elevation, mm Hg") + 
  scale_x_continuous(breaks = seq(from = 20, to = 80, by = 10)) + 
  ggtitle("DBP Elevation from Baseline") +
  theme_bw() +
  theme(axis.title = element_text(color = "#434443",size =15,face="bold"),
        axis.text = element_text(color = "#434443",size =12,face="bold"),
        title = element_text(color = "#434443",size =16,face="bold"),
        legend.text = element_text(color = "#434443",size =10,face="bold"))


# MAP
modm3 <- lmer(MAP ~ bs(AGE) + TP + (1|id), data = comb_dat %>% filter(SEX==1))
modf3 <- lmer(MAP ~ bs(AGE) + TP + (1|id), data = comb_dat %>% filter(SEX==2))

datm3 <- data.frame(AGE = plot_x_min:plot_x_max, TP = "cardia0", id = "1")
datf3 <- data.frame(AGE = plot_x_min:plot_x_max, TP = "cardia0", id = "1")

boot_m3 <- predict(modm3, newdata = datm3, re.form=NA, se.fit=TRUE, nsim=100)
boot_f3 <- predict(modf3, newdata = datf3, re.form=NA, se.fit=TRUE, nsim=100)

fit_m3 <- data.frame(AGE = datm3$AGE,
                     fit = boot_m3$fit,
                     se = boot_m3$se.fit) %>%
  mutate(upper = fit + 1.96*se,
         lower = fit - 1.96*se)
fit_m3$dif <- c(diff(fit_m3$fit), NA)

fit_f3 <- data.frame(AGE = datf3$AGE,
                     fit = boot_f3$fit,
                     se = boot_f3$se.fit) %>%
  mutate(upper = fit + 1.96*se,
         lower = fit - 1.96*se)
fit_f3$dif <- c(diff(fit_f3$fit), NA)

base3 <- ggplot() +
  coord_cartesian(xlim = c(plot_x_min,plot_x_max)) +
  geom_line(aes(x = AGE, y = fit, color = "blue4"), data = fit_m3) +
  geom_ribbon(aes(x = AGE, ymin = lower, ymax = upper, fill = "blue4"), alpha = 0.2, data = fit_m3) +
  geom_line(aes(x = AGE, y = fit, color = "red4"), data = fit_f3) +
  geom_ribbon(aes(x = AGE, ymin = lower, ymax = upper, fill = "red4"), alpha = 0.2, data = fit_f3) +
  scale_color_manual(name = "Sex", values = c("red4"="red4", "blue4"="blue4"), labels = c("red4"="Women","blue4"="Men")) +
  scale_fill_manual(name = "Sex", values = c("red4"="red4", "blue4"="blue4"), labels = c("red4"="Women","blue4"="Men")) +
  scale_y_continuous(name = "MAP, mm Hg") + 
  scale_x_continuous(breaks = seq(from = 20, to = 80, by = 10)) + 
  ggtitle("Mean Arterial Pressure") +
  # ggtitle("Unadjusted MAP") +
  theme_bw() +
  theme(axis.title = element_text(color = "#434443",size =15,face="bold"),
        axis.text = element_text(color = "#434443",size =12,face="bold"),
        title = element_text(color = "#434443",size =16,face="bold"),
        legend.text = element_text(color = "#434443",size =10,face="bold"))

ggplot() +
  coord_cartesian(xlim = c(plot_x_min,plot_x_max)) +
  geom_line(aes(x = AGE, y = dif, color = "blue4"), data = fit_m3) +
  geom_line(aes(x = AGE, y = dif, color = "red4"), data = fit_f3) +
  scale_color_manual(name = "Sex", values = c("red4"="red4", "blue4"="blue4"), labels = c("red4"="Women","blue4"="Men")) +
  scale_fill_manual(name = "Sex", values = c("red4"="red4", "blue4"="blue4"), labels = c("red4"="Women","blue4"="Men")) +
  scale_y_continuous(name = "MAP Increase Rate, mmHg/year") + 
  scale_x_continuous(breaks = seq(from = 20, to = 80, by = 10)) + 
  ggtitle("MAP Increase Rate, mmHg/year") +
  theme_bw() +
  theme(axis.title = element_text(color = "#434443",size =15,face="bold"),
        axis.text = element_text(color = "#434443",size =12,face="bold"),
        title = element_text(color = "#434443",size =16,face="bold"),
        legend.text = element_text(color = "#434443",size =10,face="bold"))


y_base_m3 <- fit_m3[1,"fit"]
y_base_f3 <- fit_f3[1,"fit"]

adj3 <- ggplot() +
  coord_cartesian(xlim = c(plot_x_min, plot_x_max)) +
  geom_line(aes(x = AGE, y = fit - y_base_m3, color = "blue4"), data = fit_m3) +
  geom_ribbon(aes(x = AGE, ymin = lower - y_base_m3, ymax = upper - y_base_m3, fill = "blue4"), alpha = 0.2, data = fit_m3) +
  geom_line(aes(x = AGE, y = fit - y_base_f3, color = "red4"), data = fit_f3) +
  geom_ribbon(aes(x = AGE, ymin = lower - y_base_f3, ymax = upper - y_base_f3, fill = "red4"), alpha = 0.2, data = fit_f3) +
  scale_color_manual(name = "Sex", values = c("red4"="red4", "blue4"="blue4"), labels = c("red4"="Women","blue4"="Men")) +
  scale_fill_manual(name = "Sex", values = c("red4"="red4", "blue4"="blue4"), labels = c("red4"="Women","blue4"="Men")) +
  scale_y_continuous(name = "MAP Elevation, mm Hg") + 
  scale_x_continuous(breaks = seq(from = 20, to = 80, by = 10)) + 
  ggtitle("MAP Elevation from Baseline") +
  theme_bw() +
  theme(axis.title = element_text(color = "#434443",size =15,face="bold"),
        axis.text = element_text(color = "#434443",size =12,face="bold"),
        title = element_text(color = "#434443",size =16,face="bold"),
        legend.text = element_text(color = "#434443",size =10,face="bold"))


pct3 <- ggplot() +
  coord_cartesian(xlim = c(plot_x_min, plot_x_max)) +
  geom_line(aes(x = AGE, y = (fit - y_base_m3)/y_base_m3, color = "blue4"), data = fit_m3) +
  geom_ribbon(aes(x = AGE, ymin = (lower - y_base_m3)/y_base_m3, ymax = (upper - y_base_m3)/y_base_m3, fill = "blue4"), alpha = 0.2, data = fit_m3) +
  geom_line(aes(x = AGE, y = (fit - y_base_f3)/y_base_f3, color = "red4"), data = fit_f3) +
  geom_ribbon(aes(x = AGE, ymin = (lower - y_base_f3)/y_base_f3, ymax = (upper - y_base_f3)/y_base_f3, fill = "red4"), alpha = 0.2, data = fit_f3) +
  scale_color_manual(name = "Sex", values = c("red4"="red4", "blue4"="blue4"), labels = c("red4"="Women","blue4"="Men")) +
  scale_fill_manual(name = "Sex", values = c("red4"="red4", "blue4"="blue4"), labels = c("red4"="Women","blue4"="Men")) +
  scale_y_continuous(name = "MAP Elevation, mm Hg") + 
  scale_x_continuous(breaks = seq(from = 20, to = 80, by = 10)) + 
  ggtitle("MAP Elevation from Baseline") +
  theme_bw() +
  theme(axis.title = element_text(color = "#434443",size =15,face="bold"),
        axis.text = element_text(color = "#434443",size =12,face="bold"),
        title = element_text(color = "#434443",size =16,face="bold"),
        legend.text = element_text(color = "#434443",size =10,face="bold"))



# PP 
modm4 <- lmer(PP ~ bs(AGE) + TP + (1|id), data = comb_dat %>% filter(SEX==1))
modf4 <- lmer(PP ~ bs(AGE) + TP + (1|id), data = comb_dat %>% filter(SEX==2))

lrtest(lmer(PP ~ bs(AGE) + TP + (1|id), data = comb_dat %>% filter(SEX==1)),
       lmer(PP ~ bs(AGE) + TP + (1|id), data = comb_dat %>% filter(SEX==1)))

datm4 <- data.frame(AGE = plot_x_min:plot_x_max, TP = "cardia0", id = "1")
datf4 <- data.frame(AGE = plot_x_min:plot_x_max, TP = "cardia0", id = "1")

boot_m4 <- predict(modm4, newdata = datm4, re.form=NA, se.fit=TRUE, nsim=100)
boot_f4 <- predict(modf4, newdata = datm4, re.form=NA, se.fit=TRUE, nsim=100)

fit_m4 <- data.frame(AGE = datm4$AGE,
                     fit = boot_m4$fit,
                     se = boot_m4$se.fit) %>%
  mutate(upper = fit + 1.96*se,
         lower = fit - 1.96*se)
fit_m4$dif <- c(diff(fit_m4$fit), NA)

fit_f4 <- data.frame(AGE = datf4$AGE,
                     fit = boot_f4$fit,
                     se = boot_f4$se.fit) %>%
  mutate(upper = fit + 1.96*se,
         lower = fit - 1.96*se)
fit_f4$dif <- c(diff(fit_f4$fit), NA)


base4 <- ggplot() +
  coord_cartesian(xlim = c(plot_x_min,plot_x_max)) +
  geom_line(aes(x = AGE, y = fit, color = "blue4"), data = fit_m4) +
  geom_ribbon(aes(x = AGE, ymin = lower, ymax = upper, fill = "blue4"), alpha = 0.2, data = fit_m4) +
  geom_line(aes(x = AGE, y = fit, color = "red4"), data = fit_f4) +
  geom_ribbon(aes(x = AGE, ymin = lower, ymax = upper, fill = "red4"), alpha = 0.2, data = fit_f4) +
  scale_color_manual(name = "Sex", values = c("red4"="red4", "blue4"="blue4"), labels = c("red4"="Women","blue4"="Men")) +
  scale_fill_manual(name = "Sex", values = c("red4"="red4", "blue4"="blue4"), labels = c("red4"="Women","blue4"="Men")) +
  scale_y_continuous(name = "PP, mm Hg") + 
  scale_x_continuous(breaks = seq(from = 20, to = 80, by = 10)) + 
  ggtitle("Pulse Pressure") +
  # ggtitle("Unadjusted PP") +
  theme_bw() +
  theme(axis.title = element_text(color = "#434443",size =15,face="bold"),
        axis.text = element_text(color = "#434443",size =12,face="bold"),
        title = element_text(color = "#434443",size =16,face="bold"),
        legend.text = element_text(color = "#434443",size =10,face="bold"))

ggplot() +
  coord_cartesian(xlim = c(plot_x_min,plot_x_max)) +
  geom_line(aes(x = AGE, y = dif, color = "blue4"), data = fit_m4) +
  geom_line(aes(x = AGE, y = dif, color = "red4"), data = fit_f4) +
  scale_color_manual(name = "Sex", values = c("red4"="red4", "blue4"="blue4"), labels = c("red4"="Women","blue4"="Men")) +
  scale_fill_manual(name = "Sex", values = c("red4"="red4", "blue4"="blue4"), labels = c("red4"="Women","blue4"="Men")) +
  scale_y_continuous(name = "PP Increase Rate, mmHg/year") + 
  scale_x_continuous(breaks = seq(from = 20, to = 80, by = 10)) + 
  ggtitle("PP Increase Rate, mmHg/year") +
  theme_bw() +
  theme(axis.title = element_text(color = "#434443",size =15,face="bold"),
        axis.text = element_text(color = "#434443",size =12,face="bold"),
        title = element_text(color = "#434443",size =16,face="bold"),
        legend.text = element_text(color = "#434443",size =10,face="bold"))

y_base_m4 <- fit_m4[1,"fit"]
y_base_f4 <- fit_f4[1,"fit"]

adj4 <- ggplot() +
  coord_cartesian(xlim = c(plot_x_min, plot_x_max)) +
  geom_line(aes(x = AGE, y = fit - y_base_m4, color = "blue4"), data = fit_m4) +
  geom_ribbon(aes(x = AGE, ymin = lower - y_base_m4, ymax = upper - y_base_m4, fill = "blue4"), alpha = 0.2, data = fit_m4) +
  geom_line(aes(x = AGE, y = fit - y_base_f4, color = "red4"), data = fit_f4) +
  geom_ribbon(aes(x = AGE, ymin = lower - y_base_f4, ymax = upper - y_base_f4, fill = "red4"), alpha = 0.2, data = fit_f4) +
  scale_color_manual(name = "Sex", values = c("red4"="red4", "blue4"="blue4"), labels = c("red4"="Women","blue4"="Men")) +
  scale_fill_manual(name = "Sex", values = c("red4"="red4", "blue4"="blue4"), labels = c("red4"="Women","blue4"="Men")) +
  scale_y_continuous(name = "PP Elevation, mm Hg") + 
  scale_x_continuous(breaks = seq(from = 20, to = 80, by = 10)) + 
  ggtitle("PP Elevation from Baseline") +
  theme_bw() +
  theme(axis.title = element_text(color = "#434443",size =15,face="bold"),
        axis.text = element_text(color = "#434443",size =12,face="bold"),
        title = element_text(color = "#434443",size =16,face="bold"),
        legend.text = element_text(color = "#434443",size =10,face="bold"))



library(gridExtra)

par(mai=c(0,0,0,0),xaxs="i",yaxs="i")

grid.arrange(
  base1,
  adj1,
  base2,
  adj2,
  base3,
  adj3,
  base4,
  adj4,
  nrow = 4,
  widths = c(1,1.1)
)

base1
base2
base3
base4
adj1
adj2
adj3
adj4


lrt_sbp <- anova(
  lmer(SBP ~ bs(AGE) + SEX + TP + (1|id), data = comb_dat),
  lmer(SBP ~ bs(AGE) + SEX + bs(AGE)*SEX + TP + (1|id), data = comb_dat),
  test = "LRT"
)

lrt_dbp <- anova(
  lmer(DBP ~ bs(AGE) + SEX + TP + (1|id), data = comb_dat),
  lmer(DBP ~ bs(AGE) + SEX + bs(AGE)*SEX + TP + (1|id), data = comb_dat),
  test = "LRT"
)

lrt_map <- anova(
  lmer(MAP ~ bs(AGE) + SEX + TP + (1|id), data = comb_dat),
  lmer(MAP ~ bs(AGE) + SEX + bs(AGE)*SEX + TP + (1|id), data = comb_dat),
  test = "LRT"
)

lrt_pp <- anova(
  lmer(PP ~ bs(AGE) + SEX + TP + (1|id), data = comb_dat),
  lmer(PP ~ bs(AGE) + SEX + bs(AGE)*SEX + TP + (1|id), data = comb_dat),
  test = "LRT"
)



