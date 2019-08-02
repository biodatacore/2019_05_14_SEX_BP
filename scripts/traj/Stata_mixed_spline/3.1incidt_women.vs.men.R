cardia_clin <- readRDS("data/cardia_clin.rds")
MESA_clin <- readRDS("data/MESA_clin.rds")
ARIC_clin <- readRDS("data/ARIC_clin.rds")
FHS_clin <- readRDS("data/FHS_clin.rds")

cardia_clin <- cardia_clin %>% dplyr::select(id, AGE, SEX, SBP, DBP, TC, HDL, BMI, SMK, DM, HRX, hardcvd, hardcvd_age, race) %>% mutate(cohort = "CARDIA")
MESA_clin <- MESA_clin %>% dplyr::select(id, AGE, SEX, SBP, DBP, TC, HDL, BMI, SMK, DM, HRX, hardcvd, hardcvd_age, race) %>% mutate(cohort = "MESA")
ARIC_clin <- ARIC_clin %>% dplyr::select(id, AGE, SEX, SBP, DBP, TC, HDL, BMI, SMK, DM, HRX, hardcvd, hardcvd_age, race) %>% mutate(cohort = "ARIC")
FHS_clin <- FHS_clin %>% dplyr::select(id, AGE, SEX, SBP, DBP, TC, HDL, BMI, SMK, DM, HRX, hardcvd, hardcvd_age, race) %>% mutate(cohort = "FHS")

comb_clin <- rbind(cardia_clin, MESA_clin, ARIC_clin, FHS_clin) %>%
  mutate(age_cate = ifelse(AGE < 30, 1, ifelse(AGE < 40, 2, ifelse(AGE < 50, 3, ifelse(AGE < 60, 4, ifelse(AGE < 70, 5, ifelse(AGE < 80, 6, 7)))))),
         age_cate2 = ifelse(AGE < 40, 1, ifelse(AGE < 50, 2, ifelse(AGE < 60, 3, ifelse(AGE < 70, 4, ifelse(AGE < 80, 5, 6))))),
         age_round = round(AGE),
         cvdage_cate = ifelse(hardcvd_age < 50, 3, ifelse(hardcvd_age < 60, 4, ifelse(hardcvd_age < 70, 5, ifelse(hardcvd_age < 80, 6, 7)))),
         hardcvd10 = if_else(hardcvd_age <= AGE + 10 & hardcvd_age != AGE, 1, 0, missing = 0))

saveRDS(comb_clin, "data/comb_clin.rds")

num = as.numeric(table(comb_clin$cvdage_cate))

# sum_m <- comb_clin %>%
#   filter(SEX==1) %>%
#   filter(!is.na(hardcvd_age)) %>%
#   group_by(cvdage_cate) %>%
#   summarize(INCIDENT_CVD = sum(hardcvd)) %>%
#   mutate(rate = INCIDENT_CVD/num*1000)
# 
# 
# sum_f <- comb_clin %>%
#   filter(SEX==2) %>%
#   filter(!is.na(hardcvd_age)) %>%
#   group_by(cvdage_cate) %>%
#   summarize(INCIDENT_CVD = sum(hardcvd)) %>%
#   mutate(rate = INCIDENT_CVD/num*1000)


bslage_m <- comb_clin %>%
  filter(SEX==1) %>%
  group_by(age_cate) %>%
  summarize(INCIDENT_CVD = sum(hardcvd10),
            num = as.numeric(table(age_cate))) %>%
  mutate(rate = INCIDENT_CVD/num*1000,
         se = (INCIDENT_CVD/(num^2))^0.5,
         category = c(25,35,45,55,65,75,85))

bslage_m2 <- comb_clin %>%
  filter(SEX==1) %>%
  group_by(age_cate2) %>%
  summarize(INCIDENT_CVD = sum(hardcvd10),
            num = as.numeric(table(age_cate2))) %>%
  mutate(rate = INCIDENT_CVD/num*1000,
         se = (INCIDENT_CVD/(num^2))^0.5,
         category = c(30,45,55,65,75,85))
bslage_m2 %<>% mutate(rate = rate/bslage_m2$rate[1])

bslage_f <- comb_clin %>%
  filter(SEX==2) %>%
  group_by(age_cate) %>%
  summarize(INCIDENT_CVD = sum(hardcvd10),
            num = as.numeric(table(age_cate))) %>%
  mutate(rate = INCIDENT_CVD/num*1000,
         se = (INCIDENT_CVD/(num^2))^0.5,
         category = c(25,35,45,55,65,75,85))

bslage_f2 <- comb_clin %>%
  filter(SEX==2) %>%
  group_by(age_cate2) %>%
  summarize(INCIDENT_CVD = sum(hardcvd10),
            num = as.numeric(table(age_cate2))) %>%
  mutate(rate = INCIDENT_CVD/num*1000,
         se = (INCIDENT_CVD/(num^2))^0.5,
         category = c(30,45,55,65,75,85))
bslage_f2 %<>% mutate(rate = rate/bslage_f2$rate[1])

saveRDS(bslage_f, "data/bslage_f.rds")
saveRDS(bslage_m, "data/bslage_m.rds")

saveRDS(bslage_f2, "data/bslage_f2.rds")
saveRDS(bslage_m2, "data/bslage_m2.rds")


##########################################

age_m2 <- comb_clin %>%
  filter(SEX==1) %>%
  group_by(age_cate) %>%
  summarize(INCIDENT_CVD = sum(hardcvd10),
            num = as.numeric(table(age_cate2))) %>%
  mutate(rate = INCIDENT_CVD/num*1000,
         se = (INCIDENT_CVD/(num^2))^0.5,
         category = c(25,35,45,55,65,75,85))
age_m2 %<>% mutate(rate = rate/bslage_m$rate[1])
age_f2 <- comb_clin %>%
  filter(SEX==2) %>%
  group_by(age_cate) %>%
  summarize(INCIDENT_CVD = sum(hardcvd10),
            num = as.numeric(table(age_cate2))) %>%
  mutate(rate = INCIDENT_CVD/num*1000,
         se = (INCIDENT_CVD/(num^2))^0.5,
         category = c(25,35,45,55,65,75,85))
age_f2 %<>% mutate(rate = rate/bslage_f$rate[1])


saveRDS(age_f2, "data/age_f2.rds")
saveRDS(age_m2, "data/age_m2.rds")

##########################################

age_m3 <- comb_clin %>%
  filter(SEX==1) %>%
  group_by(age_cate) %>%
  summarize(INCIDENT_CVD = sum(hardcvd10),
            num = as.numeric(table(age_cate2))) %>%
  mutate(rate = INCIDENT_CVD/num*1000,
         se = (INCIDENT_CVD/(num^2))^0.5,
         category = c(25,35,45,55,65,75,85))
age_m3 %<>% mutate(rate = rate/bslage_m$rate[1])
age_f3 <- comb_clin %>%
  filter(SEX==2) %>%
  group_by(age_cate) %>%
  summarize(INCIDENT_CVD = sum(hardcvd10),
            num = as.numeric(table(age_cate2))) %>%
  mutate(rate = INCIDENT_CVD/num*1000,
         se = (INCIDENT_CVD/(num^2))^0.5,
         category = c(25,35,45,55,65,75,85))
age_f3 %<>% mutate(rate = rate/bslage_f$rate[1])


saveRDS(age_f2, "data/age_f2.rds")
saveRDS(age_m2, "data/age_m2.rds")

