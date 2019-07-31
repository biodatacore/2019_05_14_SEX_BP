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
library(segmented) # install.packages("segmented")
library(nlme)
library(haven)
library(boot)

baselinedat <- readRDS("data/baselinedat.rds") %>% mutate(onsetage = ifelse(onsetage == 0 , NA, onsetage))

library(tableone)
library(officer)
library(flextable)

listVars <- c("AGE","SBP","DBP","MAP","PP","HRX","onsetage")

tabledata <- baselinedat
 
tabledata$SEX <- as.factor(tabledata$gender)
tabledata$HRX <- as.factor(tabledata$HRX)

table1 <- CreateTableOne(vars = listVars, data = tabledata , strata = "SEX", testNonNormal = kruskal.test)
table1 <- print(table1)

write.csv(table1, "output/table1.csv")

tabledata$onsetage %>% summary()

tabledata$onsetage %>% length()

tabledata[which(!is.na(tabledata$onsetage)),]$SEX %>% as.factor() %>% summary()

sum(comb_dat$category_3!="NO_HTN" & comb_dat$SEX==2)

