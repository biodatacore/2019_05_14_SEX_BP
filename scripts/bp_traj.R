library(dplyr)
library(magrittr)

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

dat <- rbind(dat1, dat2, dat3, dat4, dat5, dat6, dat7, dat8, dat9) %>% 
  mutate(PP = SBP - DBP,
         MAP = DBP + 1/3*PP) 

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

library(ggplot2)

cardia <- read.csv("data/cardia_cleaned.csv")
cardia <- cardia[,c("SBP","DBP","SEX","AGE","HRX","id","TP")] %>% 
  mutate(SEX = ifelse(SEX=="F",2,1), 
         cohort = "cardia",
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

saveRDS(rbind(cardia, fhs, MESA_ALT, ARIC_comb), "data/comb_dat.rds")

comb_dat <- rbind(cardia, fhs, MESA_ALT, ARIC_comb) %>% 
  mutate(SBP = ifelse(HRX==1, SBP + 10 , SBP),
         DBP = ifelse(HRX==1, DBP + 5 , DBP),
         PP = SBP - DBP,
         MAP = DBP + 1/3*PP)

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



library(lme4)
library(splines)
library(rms)
comb_dat$pred_SBP <- predict(lmer(SBP ~ AGE + 
                                    SEX + 
                                    HRX +
                                    cohort +
                                    TP +
                                    (1|id), 
                                  data = comb_dat))


plot_x_min <- max(c(quantile(comb_dat[which(comb_dat$SEX==1),]$AGE, 0.005,na.rm = T),quantile(comb_dat[which(comb_dat$SEX==2),]$AGE, 0.005,na.rm = T)))

plot_x_max <- min(c(quantile(comb_dat[which(comb_dat$SEX==1),]$AGE, 0.995,na.rm = T),quantile(comb_dat[which(comb_dat$SEX==2),]$AGE, 0.995,na.rm = T)))

fitmethod <- "loess"

group1_sbp <- ggplot(aes(x=AGE,y=pred_SBP),data= comb_dat %>% filter(SEX ==1, AGE>=plot_x_min)) + geom_smooth(method = lm, formula = y ~ splines::bs(x), se = T)
group2_sbp <- ggplot(aes(x=AGE,y=pred_SBP),data= comb_dat %>% filter(SEX ==2, AGE>=plot_x_min)) + geom_smooth(method = lm, formula = y ~ splines::bs(x), se = T)

y1<-as.data.frame(ggplot_build(group1_sbp)$data)$y[1]
y2<-as.data.frame(ggplot_build(group2_sbp)$data)$y[1]
y_adj1<-y1-y2

base1 <- ggplot() +
  coord_cartesian(xlim = c(plot_x_min,plot_x_max)) +
  geom_smooth(aes(x = AGE, y = pred_SBP, color = "red4"), data = comb_dat %>% filter(SEX==2, AGE>=plot_x_min), method = lm, formula = y ~ splines::bs(x), se = T) +
  geom_smooth(aes(x = AGE, y = pred_SBP, color = "blue4"), data = comb_dat %>% filter(SEX==1, AGE>=plot_x_min), method = lm, formula = y ~ splines::bs(x), se = T) +
  scale_color_manual(name = "Sex", values = c("red4"="red4", "blue4"="blue4"), labels = c("red4"="Women","blue4"="Men")) +
  scale_y_continuous(name = "SBP, mm Hg") + 
  scale_x_continuous(breaks = seq(from = 20, to = 80, by = 10)) + 
  # ggtitle("Systolic Blood Pressure") +
  ggtitle("Unadjusted SBP") +
  theme(axis.title = element_text(color = "#434443",size =15,face="bold"),
        axis.text = element_text(color = "#434443",size =12,face="bold"),
        title = element_text(color = "#434443",size =16,face="bold"))

adj1 <- ggplot() +
  coord_cartesian(xlim = c(plot_x_min,plot_x_max)) +
  geom_smooth(aes(x = AGE, y = pred_SBP + y_adj1, color = "red4"), data = comb_dat %>% filter(SEX==2, AGE>=plot_x_min), method = lm, formula = y ~ splines::bs(x), se = T) +
  geom_smooth(aes(x = AGE, y = pred_SBP, color = "blue4"), data = comb_dat %>% filter(SEX==1, AGE>=plot_x_min), method = lm, formula = y ~ splines::bs(x), se = T) +
  scale_color_manual(name = "Sex", values = c("red4"="red4", "blue4"="blue4"), labels = c("red4"="Women","blue4"="Men")) +
  scale_y_continuous(name = "SBP [Male], mm Hg", sec.axis = sec_axis(~. - y_adj1, name = "SBP [Female], mm Hg")) + 
  scale_x_continuous(breaks = seq(from = 20, to = 80, by = 10)) + 
  # ggtitle("Systolic Blood Pressure") +
  ggtitle("Baseline Adjusted SBP") +
  theme(axis.title = element_text(color = "#434443",size =15,face="bold"),
        axis.text = element_text(color = "#434443",size =12,face="bold"),
        title = element_text(color = "#434443",size =16,face="bold"))


comb_dat$pred_DBP <- predict(lmer(DBP ~ AGE + 
                                    SEX + 
                                    HRX +
                                    cohort +
                                    TP +
                                    (1|id), 
                                  data = comb_dat))


group1_dbp <- ggplot(aes(x=AGE,y=pred_DBP),data= comb_dat %>% filter(SEX ==1, AGE>=plot_x_min)) + geom_smooth(method = lm, formula = y ~ splines::bs(x), se = T)
group2_dbp <- ggplot(aes(x=AGE,y=pred_DBP),data= comb_dat %>% filter(SEX ==2, AGE>=plot_x_min)) + geom_smooth(method = lm, formula = y ~ splines::bs(x), se = T)

y1<-as.data.frame(ggplot_build(group1_dbp)$data)$y[1]
y2<-as.data.frame(ggplot_build(group2_dbp)$data)$y[1]
y_adj2<-y1-y2

base2 <- ggplot() +
  coord_cartesian(xlim = c(plot_x_min, plot_x_max)) +
  geom_smooth(aes(x = AGE, y = pred_DBP, color = "red4"), data = comb_dat %>% filter(SEX==2, AGE>=plot_x_min), method = lm, formula = y ~ splines::bs(x), se = T) +
  geom_smooth(aes(x = AGE, y = pred_DBP, color = "blue4"), data = comb_dat %>% filter(SEX==1, AGE>=plot_x_min), method = lm, formula = y ~ splines::bs(x), se = T) +
  scale_color_manual(name = "Sex", values = c("red4"="red4", "blue4"="blue4"), labels = c("red4"="Women","blue4"="Men")) +
  scale_y_continuous(name = "DBP, mm Hg") + 
  scale_x_continuous(breaks = seq(from = 20, to = 80, by = 10)) + 
  # ggtitle("Diastolic Blood Pressure") +
  ggtitle("Unadjusted DBP") +
  theme(axis.title = element_text(color = "#434443",size =15,face="bold"),
        axis.text = element_text(color = "#434443",size =12,face="bold"),
        title = element_text(color = "#434443",size =16,face="bold"))

adj2 <- ggplot() +
  coord_cartesian(xlim = c(plot_x_min, plot_x_max)) +
  geom_smooth(aes(x = AGE, y = pred_DBP + y_adj2, color = "red4"), data = comb_dat %>% filter(SEX==2, AGE>=plot_x_min), method = lm, formula = y ~ splines::bs(x), se = T) +
  geom_smooth(aes(x = AGE, y = pred_DBP, color = "blue4"), data = comb_dat %>% filter(SEX==1, AGE>=plot_x_min), method = lm, formula = y ~ splines::bs(x), se = T) +
  scale_color_manual(name = "Sex", values = c("red4"="red4", "blue4"="blue4"), labels = c("red4"="Women","blue4"="Men")) +
  scale_y_continuous(name = "DBP [Male], mm Hg", sec.axis = sec_axis(~. - y_adj2, name = "DBP [Female], mm Hg")) + 
  scale_x_continuous(breaks = seq(from = 20, to = 80, by = 10)) + 
  # ggtitle("Diastolic Blood Pressure") +
  ggtitle("Baseline Adjusted DBP") +
  theme(axis.title = element_text(color = "#434443",size =15,face="bold"),
        axis.text = element_text(color = "#434443",size =12,face="bold"),
        title = element_text(color = "#434443",size =16,face="bold"))


comb_dat$pred_MAP <- predict(lmer(MAP ~ AGE + 
                                    SEX + 
                                    HRX +
                                    cohort +
                                    TP +
                                    (1|id), 
                                  data = comb_dat))

group1_map <- ggplot(aes(x=AGE,y=pred_MAP),data= comb_dat %>% filter(SEX ==1, AGE>=plot_x_min)) + geom_smooth(method = lm, formula = y ~ splines::bs(x), se = T)
group2_map <- ggplot(aes(x=AGE,y=pred_MAP),data= comb_dat %>% filter(SEX ==2, AGE>=plot_x_min)) + geom_smooth(method = lm, formula = y ~ splines::bs(x), se = T)

y1<-as.data.frame(ggplot_build(group1_map)$data)$y[1]
y2<-as.data.frame(ggplot_build(group2_map)$data)$y[1]
y_adj3<-y1-y2

base3 <- ggplot() +
  coord_cartesian(xlim = c(plot_x_min, plot_x_max)) +
  geom_smooth(aes(x = AGE, y = pred_MAP, color = "red4"), data = comb_dat %>% filter(SEX==2, AGE>=plot_x_min), method = lm, formula = y ~ splines::bs(x), se = T) +
  geom_smooth(aes(x = AGE, y = pred_MAP, color = "blue4"), data = comb_dat %>% filter(SEX==1, AGE>=plot_x_min), method = lm, formula = y ~ splines::bs(x), se = T) +
  scale_color_manual(name = "Sex", values = c("red4"="red4", "blue4"="blue4"), labels = c("red4"="Women","blue4"="Men")) +
  scale_y_continuous(name = "MAP, mm Hg") + 
  scale_x_continuous(breaks = seq(from = 20, to = 80, by = 10)) + 
  # ggtitle("Mean Arterial Pressure") +
  ggtitle("Unadjusted MAP") +
  theme(axis.title = element_text(color = "#434443",size =15,face="bold"),
        axis.text = element_text(color = "#434443",size =12,face="bold"),
        title = element_text(color = "#434443",size =16,face="bold"))

adj3 <- ggplot() +
  coord_cartesian(xlim = c(plot_x_min, plot_x_max)) +
  geom_smooth(aes(x = AGE, y = pred_MAP + y_adj3, color = "red4"), data = comb_dat %>% filter(SEX==2, AGE>=plot_x_min), method = lm, formula = y ~ splines::bs(x), se = T) +
  geom_smooth(aes(x = AGE, y = pred_MAP, color = "blue4"), data = comb_dat %>% filter(SEX==1, AGE>=plot_x_min), method = lm, formula = y ~ splines::bs(x), se = T) +
  scale_color_manual(name = "Sex", values = c("red4"="red4", "blue4"="blue4"), labels = c("red4"="Women","blue4"="Men")) +
  scale_y_continuous(name = "MAP [Male], mm Hg", sec.axis = sec_axis(~. - y_adj3, name = "MAP [Female], mm Hg")) + 
  scale_x_continuous(breaks = seq(from = 20, to = 80, by = 10)) + 
  # ggtitle("Mean Arterial Pressure") +
  ggtitle("Baseline Adjusted MAP") +
  theme(axis.title = element_text(color = "#434443",size =15,face="bold"),
        axis.text = element_text(color = "#434443",size =12,face="bold"),
        title = element_text(color = "#434443",size =16,face="bold"))



comb_dat$pred_PP <- predict(lmer(PP ~ AGE + 
                                   SEX + 
                                   HRX +
                                   cohort +
                                   TP +
                                   (1|id), 
                                 data = comb_dat))


group1_pp <- ggplot(aes(x=AGE,y=pred_PP),data= comb_dat %>% filter(SEX ==1, AGE>=plot_x_min)) + geom_smooth(method = lm, formula = y ~ splines::bs(x), se = T)
group2_pp <- ggplot(aes(x=AGE,y=pred_PP),data= comb_dat %>% filter(SEX ==2, AGE>=plot_x_min)) + geom_smooth(method = lm, formula = y ~ splines::bs(x), se = T)

y1<-as.data.frame(ggplot_build(group1_pp)$data)$y[1]
y2<-as.data.frame(ggplot_build(group2_pp)$data)$y[1]
y_adj4<-y1-y2

base4 <- ggplot() +
  coord_cartesian(xlim = c(plot_x_min, plot_x_max)) +
  geom_smooth(aes(x = AGE, y = pred_PP, color = "red4"), data = comb_dat %>% filter(SEX==2, AGE>=plot_x_min), method = lm, formula = y ~ splines::bs(x), se = T) +
  geom_smooth(aes(x = AGE, y = pred_PP, color = "blue4"), data = comb_dat %>% filter(SEX==1, AGE>=plot_x_min), method = lm, formula = y ~ splines::bs(x), se = T) +
  scale_color_manual(name = "Sex", values = c("red4"="red4", "blue4"="blue4"), labels = c("red4"="Women","blue4"="Men")) +
  scale_y_continuous(name = "PP, mm Hg") + 
  scale_x_continuous(breaks = seq(from = 20, to = 80, by = 10)) + 
  # ggtitle("Pulse Pressure") +
  ggtitle("Unadjusted PP") +
  theme(axis.title = element_text(color = "#434443",size =15,face="bold"),
        axis.text = element_text(color = "#434443",size =12,face="bold"),
        title = element_text(color = "#434443",size =16,face="bold"))

adj4 <- ggplot() +
  coord_cartesian(xlim = c(plot_x_min, plot_x_max)) +
  geom_smooth(aes(x = AGE, y = pred_PP + y_adj4, color = "red4"), data = comb_dat %>% filter(SEX==2, AGE>=plot_x_min), method = lm, formula = y ~ splines::bs(x), se = T) +
  geom_smooth(aes(x = AGE, y = pred_PP, color = "blue4"), data = comb_dat %>% filter(SEX==1, AGE>=plot_x_min), method = lm, formula = y ~ splines::bs(x), se = T) +
  scale_color_manual(name = "Sex", values = c("red4"="red4", "blue4"="blue4"), labels = c("red4"="Women","blue4"="Men")) +
  scale_y_continuous(name = "PP [Male], mm Hg", sec.axis = sec_axis(~. - y_adj4, name = "PP [Female], mm Hg")) + 
  scale_x_continuous(breaks = seq(from = 20, to = 80, by = 10)) + 
  # ggtitle("Pulse Pressure") +
  ggtitle("Baseline Adjusted PP") +
  theme(axis.title = element_text(color = "#434443",size =15,face="bold"),
        axis.text = element_text(color = "#434443",size =12,face="bold"),
        title = element_text(color = "#434443",size =16,face="bold"))


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




library(tableone)
tabledata <- comb_dat
listVars <- c("AGE","SBP","DBP","PP","MAP","HRX")

# inctod elements------

tabledata$SEX<-factor(tabledata$SEX)
tabledata$HRX<-factor(tabledata$HRX)

table1 <- CreateTableOne(vars = listVars, data = tabledata %>% filter(cohort == "ARIC"), strata = "SEX", testNonNormal = kruskal.test)
table2 <- CreateTableOne(vars = listVars, data = tabledata %>% filter(cohort == "cardia"), strata = "SEX", testNonNormal = kruskal.test)
table3 <- CreateTableOne(vars = listVars, data = tabledata %>% filter(cohort == "fhs"), strata = "SEX", testNonNormal = kruskal.test)
table4 <- CreateTableOne(vars = listVars, data = tabledata %>% filter(cohort == "MESA"), strata = "SEX", testNonNormal = kruskal.test)


print(table2)
print(table3)
print(table4)

table_comb  <- cbind(print(table1), print(table2), print(table3), print(table4))

write.csv(table_comb,"output/table_comb.csv")


sexdat$id %>% unique()%>% length()
sexdat <- comb_dat[,c("id","SEX")] %>% unique()

sexdat$SEX %>% as.factor() %>% summary()


f_m<-NULL
f_m$AGE <- 19:plot_x_max
f_m$f_sbp<- predict(lm(pred_SBP ~ bs(AGE), data = comb_dat %>% filter(SEX==2)), newdata = data.frame(AGE = f_m$AGE+1, SEX=2))-
  predict(lm(pred_SBP ~ bs(AGE), data = comb_dat %>% filter(SEX==2)), newdata = data.frame(AGE = f_m$AGE, SEX=2))
f_m$m_sbp <-predict(lm(pred_SBP ~ bs(AGE), data = comb_dat %>% filter(SEX==1)), newdata = data.frame(AGE = f_m$AGE+1, SEX=1))-
  predict(lm(pred_SBP ~ bs(AGE), data = comb_dat %>% filter(SEX==1)), newdata = data.frame(AGE = f_m$AGE, SEX=1))
f_m$f_dbp<- predict(lm(pred_DBP ~ bs(AGE), data = comb_dat %>% filter(SEX==2)), newdata = data.frame(AGE = f_m$AGE+1, SEX=2))-
  predict(lm(pred_DBP ~ bs(AGE), data = comb_dat %>% filter(SEX==2)), newdata = data.frame(AGE = f_m$AGE, SEX=2))
f_m$m_dbp <-predict(lm(pred_DBP ~ bs(AGE), data = comb_dat %>% filter(SEX==1)), newdata = data.frame(AGE = f_m$AGE+1, SEX=1))-
  predict(lm(pred_DBP ~ bs(AGE), data = comb_dat %>% filter(SEX==1)), newdata = data.frame(AGE = f_m$AGE, SEX=1))
f_m$f_map<- predict(lm(pred_MAP ~ bs(AGE), data = comb_dat %>% filter(SEX==2)), newdata = data.frame(AGE = f_m$AGE+1, SEX=2))-
  predict(lm(pred_MAP ~ bs(AGE), data = comb_dat %>% filter(SEX==2)), newdata = data.frame(AGE = f_m$AGE, SEX=2))
f_m$m_map <-predict(lm(pred_MAP ~ bs(AGE), data = comb_dat %>% filter(SEX==1)), newdata = data.frame(AGE = f_m$AGE+1, SEX=1))-
  predict(lm(pred_MAP ~ bs(AGE), data = comb_dat %>% filter(SEX==1)), newdata = data.frame(AGE = f_m$AGE, SEX=1))
f_m$f_pp<- predict(lm(pred_PP ~ bs(AGE), data = comb_dat %>% filter(SEX==2)), newdata = data.frame(AGE = f_m$AGE+1, SEX=2))-
  predict(lm(pred_PP ~ bs(AGE), data = comb_dat %>% filter(SEX==2)), newdata = data.frame(AGE = f_m$AGE, SEX=2))
f_m$m_pp <-predict(lm(pred_PP ~ bs(AGE), data = comb_dat %>% filter(SEX==1)), newdata = data.frame(AGE = f_m$AGE+1, SEX=1))-
  predict(lm(pred_PP ~ bs(AGE), data = comb_dat %>% filter(SEX==1)), newdata = data.frame(AGE = f_m$AGE, SEX=1))

f_m_dif <- f_m %>% as.data.frame() %>% mutate(dif_sbp = (f_sbp-m_sbp),
                                              dif_dbp = (f_dbp-m_dbp),
                                              dif_map = (f_map-m_map),
                                              dif_pp = (f_pp-m_pp))


gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}
n = 8
cols = gg_color_hue(n)


detplot<- ggplot(data = f_m_dif) +
  geom_point(aes(x = AGE, y = dif_sbp, color = "sbp")) + 
  geom_point(aes(x = AGE, y = dif_dbp, color = "dbp")) + 
  geom_point(aes(x = AGE, y = dif_map, color = "map")) + 
  geom_point(aes(x = AGE, y = dif_pp, color = "pp")) +
  scale_color_manual(name = "BP measures", 
                     values = c("sbp" = "blue4", "dbp" = cols[2], "map" = cols[6], "pp" = cols[1]),
                     labels = c("sbp" ="SBP","dbp" = "DBP", "map" = "MAP","pp" = "PP")) +
  geom_hline(yintercept=0, lty=1, lwd=1, col="black") +  
  theme_bw() +
  theme(axis.text= element_text(hjust = 0.5, size= 12,colour="black",face="bold"),
        axis.title = element_text(hjust = 0.5, size= 16,colour="black",face="bold"),
        plot.title = element_text(hjust = 0.5, size= 16,colour="black",face="bold"),
        legend.position = c(0.2, 0.8),
        legend.text = element_text(hjust = 0.5, size= 12,colour="black",face="bold"),
        legend.title = element_text(hjust = 0.5, size= 12,colour="black",face="bold"),
        legend.background = element_rect(fill="white",
                                         size=0.5, linetype="solid", 
                                         colour ="black")
  ) +
  scale_x_continuous(name=c("Age"),   breaks = seq(from = 20, to = 80, by = 10)) + # change #
  scale_y_continuous(name="Deterioration Rate [F vs. M], mmHg/year") + 
  ggtitle("")
detplot 
  

plot(f_m_dif$AGE,f_m_dif$dif_sbp)
plot(f_m_dif$AGE,f_m_dif$dif_dbp)
plot(f_m_dif$AGE,f_m_dif$dif_map)
plot(f_m_dif$AGE,f_m_dif$dif_pp)




comb_dat$AGE %>% summary

library(tibble)
age10 <- comb_dat %>% mutate(age10 = AGE/10)

sbpci <- summary(lm(pred_SBP ~ age10*SEX, data = age10))$coefficients %>% 
  as.data.frame() %>%
  mutate(lower = Estimate - 1.96*`Std. Error`,
         upper = Estimate + 1.96*`Std. Error`,
         term = "sbp")
dbpci <- summary(lm(pred_DBP ~ age10*SEX, data = age10 %>% filter(AGE < 60)))$coefficients %>% 
  as.data.frame() %>%
  mutate(lower = Estimate - 1.96*`Std. Error`,
         upper = Estimate + 1.96*`Std. Error`,
         term = "dbp")
mapci <- summary(lm(pred_MAP ~ age10*SEX, data = age10 %>% filter(AGE < 60)))$coefficients %>% 
  as.data.frame() %>%
  mutate(lower = Estimate - 1.96*`Std. Error`,
         upper = Estimate + 1.96*`Std. Error`,
         term = "map")
ppci <- summary(lm(pred_PP ~ age10*SEX, data = age10))$coefficients %>% 
  as.data.frame() %>%
  mutate(lower = Estimate - 1.96*`Std. Error`,
         upper = Estimate + 1.96*`Std. Error`,
         term = "pp")

bpci <- rbind(sbpci,dbpci,mapci,ppci) %>% 
  rownames_to_column("num") %>% 
  filter(num %in% c(4,8,12,16)) %>%
  mutate(CI = paste(sprintf("%.3f", Estimate),"(",sprintf("%.3f", lower),", ",sprintf("%.3f", upper),")",sep = ""),
         dura = c("All ages","< 60 yrs"," < 60 yrs","All ages"))

write.csv(bpci,"output/bpci.csv")



barplot<- ggplot(data=bpci, aes(x=term, y=Estimate)) +
  geom_bar(aes(fill = term), position=position_dodge(), stat="identity", width = 0.3, show.legend = F) +
  geom_errorbar(aes(ymin=lower, ymax=upper), color = "grey40", width=.2 , size = 1) +
  coord_flip() +  # flip coordinates (puts labels on y axis)
  scale_y_continuous(breaks = c(0.6,0.8,1,1.2,1.4,1.6,1.8)) +
  scale_x_discrete(labels = c("sbp" ="SBP (All ages)","dbp" = "DBP (< 60 yrs)", "map" = "MAP (< 60 yrs)","pp" = "PP (All ages)")) +
  scale_fill_manual(name = "BP measures", 
                     values = c("sbp" = "blue4", "dbp" = cols[2], "map" = cols[6], "pp" = cols[1]),
                     labels = c("sbp" ="SBP","dbp" = "DBP", "map" = "MAP","pp" = "PP")) +
  theme_bw() + # use a white background
  theme(
    axis.title = element_blank(),
    axis.text.y = element_text(colour = "#434443", size = 12,face="bold"),
    axis.text.x = element_text(colour="#434443",size= 12,angle = 0,vjust=0.5,face="bold"),
    plot.title = element_text(colour = "black", size = 15,face="bold",hjust = 0.5),
    axis.ticks = element_blank(),
    legend.text = element_text(colour = "black", size = 14,face="bold"),
    legend.title = element_text(colour = "black", size = 14,face="bold"),
    panel.border = element_rect(colour = "grey80", size=1.5, linetype=1)
  ) +
  ggtitle("Coefficients for Women vs. Men, mmHg/10y") 






range <- plot_x_min:plot_x_max
boot.fun1 <- function(data, indices){
  data <-data[indices,]	#this does the random resampling of rows
  f_m<-NULL
  f_m$AGE <- range
  f_m$f_bp<- predict(lm(pred_SBP ~ bs(AGE), data = data %>% filter(SEX==2)), newdata = data.frame(AGE = f_m$AGE+1, SEX=2))-
    predict(lm(pred_SBP ~ bs(AGE), data = data %>% filter(SEX==2)), newdata = data.frame(AGE = f_m$AGE, SEX=2))
  f_m$m_bp <-predict(lm(pred_SBP ~ bs(AGE), data = data %>% filter(SEX==1)), newdata = data.frame(AGE = f_m$AGE+1, SEX=1))-
    predict(lm(pred_SBP ~ bs(AGE), data = data %>% filter(SEX==1)), newdata = data.frame(AGE = f_m$AGE, SEX=1))
  f_m_dif <- f_m %>% as.data.frame() %>% mutate(dif_bp = (f_bp-m_bp))
  print(indices[[1]])
  f_m_dif$dif_bp
  }

library(boot)
library(reshape2)
boot1000 <- boot(data = comb_dat, statistic = boot.fun1, R=1000)	
# medconfint <- boot.ci(medboot, index=1, conf=(.95), type="perc")

bootdat_1 <- NULL
bootdat_1$AGE <- range
bootdat_1$mean <- boot1000$t %>% as.data.frame() %>%sapply(mean)
bootdat_1$se <- boot1000$t %>% as.data.frame() %>%sapply(sd)
bootdat_1 %<>% as.data.frame()


ggplot() +
  geom_smooth(aes(x = AGE, y = mean), data = bootdat_1) +
  geom_ribbon(aes(x = AGE, ymin = mean -1.96*se, ymax = mean + 1.96*se), data = bootdat_1, alpha = 0.2)


# pvalue = 1 - sum (boot1000$t0*boot1000$t > 0) / (1 + boot1000$R)

eval(parse(text = paste("medlist2[",i,",] <- c(medboot$t0,medconfint$percent[4:5],pvalue)")))


range <- plot_x_min:plot_x_max
boot.fun2 <- function(data, indices){
  data <-data[indices,]	#this does the random resampling of rows
  f_m<-NULL
  f_m$AGE <- range
  f_m$f_bp<- predict(lm(pred_DBP ~ bs(AGE), data = data %>% filter(SEX==2)), newdata = data.frame(AGE = f_m$AGE+1, SEX=2))-
    predict(lm(pred_DBP ~ bs(AGE), data = data %>% filter(SEX==2)), newdata = data.frame(AGE = f_m$AGE, SEX=2))
  f_m$m_bp <-predict(lm(pred_DBP ~ bs(AGE), data = data %>% filter(SEX==1)), newdata = data.frame(AGE = f_m$AGE+1, SEX=1))-
    predict(lm(pred_DBP ~ bs(AGE), data = data %>% filter(SEX==1)), newdata = data.frame(AGE = f_m$AGE, SEX=1))
  f_m_dif <- f_m %>% as.data.frame() %>% mutate(dif_bp = (f_bp-m_bp))
  print(indices[[1]])
  f_m_dif$dif_bp
}

library(boot)
library(reshape2)
boot1000_2 <- boot(data = comb_dat, statistic = boot.fun2, R=1000)	
# medconfint <- boot.ci(medboot, index=1, conf=(.95), type="perc")

bootdat_2 <- NULL
bootdat_2$AGE <- range
bootdat_2$mean <- boot1000_2$t %>% as.data.frame() %>%sapply(mean)
bootdat_2$se <- boot1000_2$t %>% as.data.frame() %>%sapply(sd)
bootdat_2 %<>% as.data.frame()


ggplot() +
  geom_smooth(aes(x = AGE, y = mean), data = bootdat_2) +
  geom_ribbon(aes(x = AGE, ymin = mean -1.96*se, ymax = mean + 1.96*se), data = bootdat_2, alpha = 0.2)



range <- plot_x_min:plot_x_max
boot.fun3 <- function(data, indices){
  data <-data[indices,]	#this does the random resampling of rows
  f_m<-NULL
  f_m$AGE <- range
  f_m$f_bp<- predict(lm(pred_MAP ~ bs(AGE), data = data %>% filter(SEX==2)), newdata = data.frame(AGE = f_m$AGE+1, SEX=2))-
    predict(lm(pred_MAP ~ bs(AGE), data = data %>% filter(SEX==2)), newdata = data.frame(AGE = f_m$AGE, SEX=2))
  f_m$m_bp <-predict(lm(pred_MAP ~ bs(AGE), data = data %>% filter(SEX==1)), newdata = data.frame(AGE = f_m$AGE+1, SEX=1))-
    predict(lm(pred_MAP ~ bs(AGE), data = data %>% filter(SEX==1)), newdata = data.frame(AGE = f_m$AGE, SEX=1))
  f_m_dif <- f_m %>% as.data.frame() %>% mutate(dif_bp = (f_bp-m_bp))
  print(indices[[1]])
  f_m_dif$dif_bp
}

library(boot)
library(reshape2)
boot1000_3 <- boot(data = comb_dat, statistic = boot.fun3, R=1000)	
# medconfint <- boot.ci(medboot, index=1, conf=(.95), type="perc")

bootdat_3 <- NULL
bootdat_3$AGE <- range
bootdat_3$mean <- boot1000_3$t %>% as.data.frame() %>%sapply(mean)
bootdat_3$se <- boot1000_3$t %>% as.data.frame() %>%sapply(sd)
bootdat_3 %<>% as.data.frame()


ggplot() +
  geom_smooth(aes(x = AGE, y = mean), data = bootdat_3) +
  geom_ribbon(aes(x = AGE, ymin = mean -1.96*se, ymax = mean + 1.96*se), data = bootdat_3, alpha = 0.2)






range <- plot_x_min:plot_x_max
boot.fun4 <- function(data, indices){
  data <-data[indices,]	#this does the random resampling of rows
  f_m<-NULL
  f_m$AGE <- range
  f_m$f_bp<- predict(lm(pred_PP ~ bs(AGE), data = data %>% filter(SEX==2)), newdata = data.frame(AGE = f_m$AGE+1, SEX=2))-
    predict(lm(pred_PP ~ bs(AGE), data = data %>% filter(SEX==2)), newdata = data.frame(AGE = f_m$AGE, SEX=2))
  f_m$m_bp <-predict(lm(pred_PP ~ bs(AGE), data = data %>% filter(SEX==1)), newdata = data.frame(AGE = f_m$AGE+1, SEX=1))-
    predict(lm(pred_PP ~ bs(AGE), data = data %>% filter(SEX==1)), newdata = data.frame(AGE = f_m$AGE, SEX=1))
  f_m_dif <- f_m %>% as.data.frame() %>% mutate(dif_bp = (f_bp-m_bp))
  print(indices[[1]])
  f_m_dif$dif_bp
}

library(boot)
library(reshape2)
library(plyr)
library(plotrix)

boot1000_4 <- boot(data = comb_dat, statistic = boot.fun4, R=1000)	
medconfint <- boot.ci(boot1000_4, index=1, conf=(.95), type="perc")



bootdat_4 <- NULL
bootdat_4$AGE <- range
bootdat_4$mean <- boot1000_4$t %>% as.data.frame() %>%sapply(mean)
bootdat_4$se <- boot1000_4$t %>% as.data.frame() %>%sapply(sd)
bootdat_4 %<>% as.data.frame()

ggplot() +
  geom_smooth(aes(x = AGE, y = mean), data = bootdat_4) +
  geom_ribbon(aes(x = AGE, ymin = mean -1.96*se, ymax = mean + 1.96*se), data = bootdat_4, alpha = 0.2)
  


ggplot() +
  geom_smooth(aes(x = AGE, y = mean, color = "sbp"), data = bootdat_1, method = lm, formula = y ~ bs(x)) + 
  geom_ribbon(aes(x = AGE, ymin = mean -1.96*se, ymax = mean + 1.96*se, fill = "sbp"), data = bootdat_1, alpha = 0.2) +
  geom_smooth(aes(x = AGE, y = mean, color = "dbp"), data = bootdat_2, method = lm, formula = y ~ bs(x)) + 
  geom_ribbon(aes(x = AGE, ymin = mean -1.96*se, ymax = mean + 1.96*se, fill = "dbp"), data = bootdat_2, alpha = 0.2) +
  geom_smooth(aes(x = AGE, y = mean, color = "map"), data = bootdat_3, method = lm, formula = y ~ bs(x)) + 
  geom_ribbon(aes(x = AGE, ymin = mean -1.96*se, ymax = mean + 1.96*se, fill = "map"), data = bootdat_3, alpha = 0.2) +
  geom_smooth(aes(x = AGE, y = mean, color = "pp"), data = bootdat_4, method = lm, formula = y ~ bs(x)) +
  geom_ribbon(aes(x = AGE, ymin = mean -1.96*se, ymax = mean + 1.96*se, fill = "pp"), data = bootdat_4, alpha = 0.2) +
  scale_color_manual(name = "BP measures", 
                     breaks = c("sbp","dbp","map","pp"),
                     values = c("sbp" = "blue4", "dbp" = cols[2], "map" = cols[6], "pp" = cols[1]),
                     labels = c("sbp" ="SBP","dbp" = "DBP", "map" = "MAP","pp" = "PP")) +
  scale_fill_manual(name = "BP measures", 
                    breaks = c("sbp","dbp","map","pp"),
                    values = c("sbp" = "blue4", "dbp" = cols[2], "map" = cols[6], "pp" = cols[1]),
                     labels = c("sbp" ="SBP","dbp" = "DBP", "map" = "MAP","pp" = "PP")) +
  geom_hline(yintercept=0, lty=1, lwd=1, col="black") +  
  theme_bw() +
  theme(axis.text= element_text(hjust = 0.5, size= 12,colour="black",face="bold"),
        axis.title = element_text(hjust = 0.5, size= 16,colour="black",face="bold"),
        plot.title = element_text(hjust = 0.5, size= 16,colour="black",face="bold"),
        legend.position = c(0.2, 0.85),
        legend.text = element_text(hjust = 0.5, size= 12,colour="black",face="bold"),
        legend.title = element_text(hjust = 0.5, size= 12,colour="black",face="bold"),
        legend.background = element_rect(fill="white",
                                         size=0.5, linetype="solid", 
                                         colour ="black")
  ) +
  scale_x_continuous(name=c("Age"),   breaks = seq(from = 20, to = 80, by = 10)) + # change #
  scale_y_continuous(name="Deterioration Rate [F vs. M], mmHg/year") + 
  ggtitle("") +
  guides(fill = guide_legend(override.aes = list(alpha = 0.2)))

  
  