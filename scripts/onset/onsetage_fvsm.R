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

comb_dat <- readRDS("data/comb_dat.rds")
comb_dat %<>% mutate(AGE = round(AGE),
                     id = as.numeric(id))
head(comb_dat)
# Filter out those who were under 18 at baseline --------------------------

u1850 <-
  comb_dat %>%
  filter(visit == 1) %>%
  distinct(AGE, id) %>%
  filter(AGE < 18)

baselinehtn <-
  comb_dat %>%
  filter(visit == 1) %>% 
  distinct(HRX, SBP, DBP, AGE, id) %>%
  filter((HRX == 1 | SBP > 140 | DBP > 90) & AGE > 45)

num_visit <-
  comb_dat %>%
  group_by(id) %>%
  dplyr::summarise(n = n()) %>%
  filter(n < 4)

comb_dat %<>%
  anti_join(u1850, by = 'id')%>%
  anti_join(num_visit, by = 'id') %>%
  anti_join(baselinehtn, by = 'id') 


rm(u1850, baselinehtn, num_visit)

# Filter out unmeasured exams and exams those never diagnosed with hptn --------

onset <- comb_dat %>% 
  mutate(onsetage = ifelse(SBP >= 140 | DBP >=90 | HRX == 1, AGE, NA)) %>%
  setDT() %>%
  dcast(id ~ visit, value.var = c("onsetage")) 

onset$onsetage <- apply(dplyr::select(onset, -id), 1, FUN = min, na.rm = T)

onset %<>% mutate(onsetage = ifelse(onsetage == Inf, NA, onsetage))
onset$onsetage %>% hist()

htnonset <- comb_dat %>% 
  filter(visit == 1) %>%
  left_join(onset, by = "id")

t.test(onsetage ~ SEX, data = htnonset)

library(tableone)
library(officer)
library(flextable)

listVars <- c("AGE","SBP","DBP","MAP","PP","HRX","onsetage")

tabledata<-htnonset

tabledata$SEX <- as.factor(tabledata$SEX)
tabledata$HRX <- as.factor(tabledata$HRX)

table1 <- CreateTableOne(vars = listVars, data = tabledata , strata = "SEX", testNonNormal = kruskal.test)
table1 <- print(table1)

write.csv(table1, "output/table1.csv")
