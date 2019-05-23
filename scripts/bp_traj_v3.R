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

m <- comb_dat %>% filter(SEX==1)
f <- comb_dat %>% filter(SEX==2)
# SBP

modm1 <- lmer(SBP ~ AGE + TP + (1|id), data = m)
modf1 <- lmer(SBP ~ AGE + TP + (1|id), data = f)

m$pred_SBP <- predict(modm1)
f$pred_SBP <- predict(modf1)

base1 <- ggplot() +
  geom_smooth(aes(x = AGE, y = pred_SBP, color = "red4", fill = "red4"), alpha = 0.2, data = f, method = lm, formula = y ~ splines::bs(x), se = T) +
  geom_smooth(aes(x = AGE, y = pred_SBP, color = "blue4", fill = "blue4"), alpha = 0.2, data = m, method = lm, formula = y ~ splines::bs(x), se = T) +
  scale_color_manual(name = "Sex", values = c("red4"="red4", "blue4"="blue4"), labels = c("red4"="Women","blue4"="Men")) +
  scale_fill_manual(name = "Sex", values = c("red4"="red4", "blue4"="blue4"), labels = c("red4"="Women","blue4"="Men")) +
  scale_y_continuous(name = "SBP, mm Hg") + 
  scale_x_continuous(breaks = seq(from = 20, to = 80, by = 10), limits = c(plot_x_min,plot_x_max)) + 
  ggtitle("Systolic Blood Pressure") +
  # ggtitle("Unadjusted SBP") +
  theme_bw() +
  theme(axis.title = element_text(color = "#434443",size =15,face="bold"),
        axis.text = element_text(color = "#434443",size =12,face="bold"),
        title = element_text(color = "#434443",size =16,face="bold"),
        legend.text = element_text(color = "#434443",size =10,face="bold"))

ym1 <- ggplot_build(base1)$data[[2]]$y[1]
yf1 <- ggplot_build(base1)$data[[1]]$y[1]

adj1 <- ggplot() +
  geom_smooth(aes(x = AGE, y = pred_SBP-yf1, color = "red4", fill = "red4"), alpha = 0.2, data = f, method = lm, formula = y ~ splines::bs(x), se = T) +
  geom_smooth(aes(x = AGE, y = pred_SBP-ym1, color = "blue4", fill = "blue4"), alpha = 0.2, data = m, method = lm, formula = y ~ splines::bs(x), se = T) +
  scale_color_manual(name = "Sex", values = c("red4"="red4", "blue4"="blue4"), labels = c("red4"="Women","blue4"="Men")) +
  scale_fill_manual(name = "Sex", values = c("red4"="red4", "blue4"="blue4"), labels = c("red4"="Women","blue4"="Men")) +
  scale_y_continuous(name = "SBP Elevation, mm Hg") + 
  scale_x_continuous(breaks = seq(from = 20, to = 80, by = 10), limits = c(plot_x_min, plot_x_max)) + 
  ggtitle("SBP Elevation from Baseline") +
  # ggtitle("Baseline Adjusted SBP") +
  theme_bw() +
  theme(axis.title = element_text(color = "#434443",size =15,face="bold"),
        axis.text = element_text(color = "#434443",size =12,face="bold"),
        title = element_text(color = "#434443",size =16,face="bold"),
        legend.text = element_text(color = "#434443",size =10,face="bold"))



# DBP

modm2 <- lmer(DBP ~ AGE + TP + (1|id), data = m)
modf2 <- lmer(DBP ~ AGE + TP + (1|id), data = f)

m$pred_DBP <- predict(modm2)
f$pred_DBP <- predict(modf2)

base2 <- ggplot() +
  geom_smooth(aes(x = AGE, y = pred_DBP, color = "red4", fill = "red4"), alpha = 0.2, data = f, method = lm, formula = y ~ splines::bs(x), se = T) +
  geom_smooth(aes(x = AGE, y = pred_DBP, color = "blue4", fill = "blue4"), alpha = 0.2, data = m, method = lm, formula = y ~ splines::bs(x), se = T) +
  scale_color_manual(name = "Sex", values = c("red4"="red4", "blue4"="blue4"), labels = c("red4"="Women","blue4"="Men")) +
  scale_fill_manual(name = "Sex", values = c("red4"="red4", "blue4"="blue4"), labels = c("red4"="Women","blue4"="Men")) +
  scale_y_continuous(name = "DBP, mm Hg") + 
  scale_x_continuous(breaks = seq(from = 20, to = 80, by = 10), limits = c(plot_x_min,plot_x_max)) + 
  ggtitle("Diastolic Blood Pressure") +
  theme_bw() +
  theme(axis.title = element_text(color = "#434443",size =15,face="bold"),
        axis.text = element_text(color = "#434443",size =12,face="bold"),
        title = element_text(color = "#434443",size =16,face="bold"),
        legend.text = element_text(color = "#434443",size =10,face="bold"))

ym2 <- ggplot_build(base2)$data[[2]]$y[1]
yf2 <- ggplot_build(base2)$data[[1]]$y[1]

adj2 <- ggplot() +
  geom_smooth(aes(x = AGE, y = pred_DBP-yf2, color = "red4", fill = "red4"), alpha = 0.2, data = f, method = lm, formula = y ~ splines::bs(x), se = T) +
  geom_smooth(aes(x = AGE, y = pred_DBP-ym2, color = "blue4", fill = "blue4"), alpha = 0.2, data = m, method = lm, formula = y ~ splines::bs(x), se = T) +
  scale_color_manual(name = "Sex", values = c("red4"="red4", "blue4"="blue4"), labels = c("red4"="Women","blue4"="Men")) +
  scale_fill_manual(name = "Sex", values = c("red4"="red4", "blue4"="blue4"), labels = c("red4"="Women","blue4"="Men")) +
  scale_y_continuous(name = "DBP Elevation, mm Hg") + 
  scale_x_continuous(breaks = seq(from = 20, to = 80, by = 10), limits = c(plot_x_min, plot_x_max)) + 
  ggtitle("DBP Elevation from Baseline") +
  theme_bw() +
  theme(axis.title = element_text(color = "#434443",size =15,face="bold"),
        axis.text = element_text(color = "#434443",size =12,face="bold"),
        title = element_text(color = "#434443",size =16,face="bold"),
        legend.text = element_text(color = "#434443",size =10,face="bold"))


# MAP
modm3 <- lmer(MAP ~ AGE + TP + (1|id), data = m)
modf3 <- lmer(MAP ~ AGE + TP + (1|id), data = f)

m$pred_MAP <- predict(modm3)
f$pred_MAP <- predict(modf3)

base3 <- ggplot() +
  geom_smooth(aes(x = AGE, y = pred_MAP, color = "red4", fill = "red4"), alpha = 0.2, data = f, method = lm, formula = y ~ splines::bs(x), se = T) +
  geom_smooth(aes(x = AGE, y = pred_MAP, color = "blue4", fill = "blue4"), alpha = 0.2, data = m, method = lm, formula = y ~ splines::bs(x), se = T) +
  scale_color_manual(name = "Sex", values = c("red4"="red4", "blue4"="blue4"), labels = c("red4"="Women","blue4"="Men")) +
  scale_fill_manual(name = "Sex", values = c("red4"="red4", "blue4"="blue4"), labels = c("red4"="Women","blue4"="Men")) +
  scale_y_continuous(name = "MAP, mm Hg") + 
  scale_x_continuous(breaks = seq(from = 20, to = 80, by = 10), limits = c(plot_x_min,plot_x_max)) + 
  ggtitle("Mean Arterial Pressure") +
  theme_bw() +
  theme(axis.title = element_text(color = "#434443",size =15,face="bold"),
        axis.text = element_text(color = "#434443",size =12,face="bold"),
        title = element_text(color = "#434443",size =16,face="bold"),
        legend.text = element_text(color = "#434443",size =10,face="bold"))

ym3 <- ggplot_build(base3)$data[[2]]$y[1]
yf3 <- ggplot_build(base3)$data[[1]]$y[1]

adj3 <- ggplot() +
  geom_smooth(aes(x = AGE, y = pred_MAP-yf3, color = "red4", fill = "red4"), alpha = 0.2, data = f, method = lm, formula = y ~ splines::bs(x), se = T) +
  geom_smooth(aes(x = AGE, y = pred_MAP-ym3, color = "blue4", fill = "blue4"), alpha = 0.2, data = m, method = lm, formula = y ~ splines::bs(x), se = T) +
  scale_color_manual(name = "Sex", values = c("red4"="red4", "blue4"="blue4"), labels = c("red4"="Women","blue4"="Men")) +
  scale_fill_manual(name = "Sex", values = c("red4"="red4", "blue4"="blue4"), labels = c("red4"="Women","blue4"="Men")) +
  scale_y_continuous(name = "MAP Elevation, mm Hg") + 
  scale_x_continuous(breaks = seq(from = 20, to = 80, by = 10), limits = c(plot_x_min, plot_x_max)) + 
  ggtitle("MAP Elevation from Baseline") +
  theme_bw() +
  theme(axis.title = element_text(color = "#434443",size =15,face="bold"),
        axis.text = element_text(color = "#434443",size =12,face="bold"),
        title = element_text(color = "#434443",size =16,face="bold"),
        legend.text = element_text(color = "#434443",size =10,face="bold"))


# PP 
modm4 <- lmer(PP ~ AGE + TP + (1|id), data = m)
modf4 <- lmer(PP ~ AGE + TP + (1|id), data = f)

m$pred_PP <- predict(modm4)
f$pred_PP <- predict(modf4)

base4 <- ggplot() +
  geom_smooth(aes(x = AGE, y = pred_PP, color = "red4", fill = "red4"), alpha = 0.2, data = f, method = lm, formula = y ~ splines::bs(x), se = T) +
  geom_smooth(aes(x = AGE, y = pred_PP, color = "blue4", fill = "blue4"), alpha = 0.2, data = m, method = lm, formula = y ~ splines::bs(x), se = T) +
  scale_color_manual(name = "Sex", values = c("red4"="red4", "blue4"="blue4"), labels = c("red4"="Women","blue4"="Men")) +
  scale_fill_manual(name = "Sex", values = c("red4"="red4", "blue4"="blue4"), labels = c("red4"="Women","blue4"="Men")) +
  scale_y_continuous(name = "MAP, mm Hg") + 
  scale_x_continuous(breaks = seq(from = 20, to = 80, by = 10), limits = c(plot_x_min,plot_x_max)) + 
  ggtitle("Pulse Pressure") +
  theme_bw() +
  theme(axis.title = element_text(color = "#434443",size =15,face="bold"),
        axis.text = element_text(color = "#434443",size =12,face="bold"),
        title = element_text(color = "#434443",size =16,face="bold"),
        legend.text = element_text(color = "#434443",size =10,face="bold"))

ym4 <- ggplot_build(base4)$data[[2]]$y[1]
yf4 <- ggplot_build(base4)$data[[1]]$y[1]

adj4 <- ggplot() +
  geom_smooth(aes(x = AGE, y = pred_PP-yf4, color = "red4", fill = "red4"), alpha = 0.2, data = f, method = lm, formula = y ~ splines::bs(x), se = T) +
  geom_smooth(aes(x = AGE, y = pred_PP-ym4, color = "blue4", fill = "blue4"), alpha = 0.2, data = m, method = lm, formula = y ~ splines::bs(x), se = T) +
  scale_color_manual(name = "Sex", values = c("red4"="red4", "blue4"="blue4"), labels = c("red4"="Women","blue4"="Men")) +
  scale_fill_manual(name = "Sex", values = c("red4"="red4", "blue4"="blue4"), labels = c("red4"="Women","blue4"="Men")) +
  scale_y_continuous(name = "PP Elevation, mm Hg") + 
  scale_x_continuous(breaks = seq(from = 20, to = 80, by = 10), limits = c(plot_x_min, plot_x_max)) + 
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



