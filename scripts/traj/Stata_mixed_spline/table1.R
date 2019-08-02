
comb_dat_id <- readRDS("data/comb_dat_id.rds") # BP measures
comb_clin <- readRDS("data/comb_clin.rds") # events

baselinedat <- comb_dat_id %>% 
  dplyr::select(id, cohort, SEX) %>% unique() %>%
  left_join(comb_clin  [,c("id","hardcvd","hardcvd_age")], by = "id")

baselineage <- matrix(nrow = nrow(baselinedat), ncol = 12) %>% as.data.frame()
colnames(baselineage) <- c("id","AGE","SBP","DBP","PP","MAP","HRX","SMK","BMI","DM","TC","race")
baselineage$id <- baselinedat$id

for (i in unique(baselineage$id)) {
  pt <- comb_dat_id[which(comb_dat_id$id == i),]
  where1 <- pt$visit==min(pt$visit, na.rm = T)
  AGE <- pt$AGE[where1]
  SBP <- pt$SBP[where1]
  DBP <- pt$DBP[where1]
  PP <- pt$PP[where1]
  MAP <- pt$MAP[where1]
  HRX <- pt$HRX[where1]
  
  SMK <- pt$SMK[where1]
  BMI <- pt$BMI[where1]
  DM <- pt$DM[where1]
  TC <- pt$TC[where1]
  race <- pt$race[where1]
  
  
  baselineage[which(baselineage$id==i),2] <- AGE
  baselineage[which(baselineage$id==i),3] <- SBP
  baselineage[which(baselineage$id==i),4] <- DBP
  baselineage[which(baselineage$id==i),5] <- PP
  baselineage[which(baselineage$id==i),6] <- MAP
  baselineage[which(baselineage$id==i),7] <- HRX
  
  baselineage[which(baselineage$id==i),8] <- SMK
  baselineage[which(baselineage$id==i),9] <- BMI
  baselineage[which(baselineage$id==i),10] <- DM
  baselineage[which(baselineage$id==i),11] <- TC
  baselineage[which(baselineage$id==i),12] <- race
  
}

baselinedat <- baselinedat %>% left_join(baselineage, by = "id")

baselinedat <- baselinedat %>% mutate(prev_CVD = if_else(hardcvd_age==AGE,1,0, missing = 0),
                       incidt_CVD = if_else(hardcvd==1 & hardcvd_age != AGE,1,0, missing = 0))

saveRDS(baselinedat,"data/baselinedat_traj.rds")



library(tableone)
library(officer)
library(flextable)

baselinedat <- readRDS("data/baselinedat_traj.rds")

listVars <- c("AGE","SEX","SBP","DBP","MAP","PP","HRX","SMK","BMI","DM","TC","prev_CVD","incidt_CVD","race")

tabledata <- baselinedat

tabledata$SEX <- as.factor(tabledata$SEX)
tabledata$HRX <- as.factor(tabledata$HRX)
tabledata$SMK <- as.factor(tabledata$SMK)
tabledata$DM <- as.factor(tabledata$DM)
tabledata$prev_CVD <- as.factor(tabledata$prev_CVD)
tabledata$incidt_CVD <- as.factor(tabledata$incidt_CVD)
tabledata$race <- as.factor(tabledata$race)

table1 <- CreateTableOne(vars = listVars, data = tabledata , strata = "cohort", testNonNormal = kruskal.test)
table1 <- print(table1)

table2 <- CreateTableOne(vars = listVars, data = tabledata, testNonNormal = kruskal.test)
table2 <- print(table2)

write.csv(table1,"output/table1_traj.csv")
write.csv(table2,"output/table2_traj.csv")

sum(is.na(baselinedat$SMK), na.rm = T)
sum(is.na(baselinedat$BMI), na.rm = T)
sum(is.na(baselinedat$TC), na.rm = T)
sum(is.na(baselinedat$DM), na.rm = T)
sum(is.na(baselinedat$hardcvd), na.rm = T)


test <- baselinedat %>% dplyr::select(-hardcvd_age) %>% na.omit
