
library(dplyr)
library(magrittr)
library(data.table)
library(purrr)

FHS_clin <- readRDS("data/FHS_clin.rds")
ARIC_clin <- readRDS("data/ARIC_clin.rds")
MESA_clin <- readRDS("data/MESA_clin.rds")
cardia_clin <- readRDS("data/CARDIA_clin.rds")

cardia_clin <- cardia_clin %>% dplyr::select(id, AGE, SEX, SBP, DBP, TC, HDL, BMI, SMK, DM, HRX, hardcvd, hardcvd_age, race) %>% mutate(cohort = "CARDIA")
MESA_clin <- MESA_clin %>% dplyr::select(id, AGE, SEX, SBP, DBP, TC, HDL, BMI, SMK, DM, HRX, hardcvd, hardcvd_age, race) %>% mutate(cohort = "MESA")
ARIC_clin <- ARIC_clin %>% dplyr::select(id, AGE, SEX, SBP, DBP, TC, HDL, BMI, SMK, DM, HRX, hardcvd, hardcvd_age, race) %>% mutate(cohort = "ARIC")
FHS_clin <- FHS_clin %>% dplyr::select(id, AGE, SEX, SBP, DBP, TC, HDL, BMI, SMK, DM, HRX, hardcvd, hardcvd_age, race) %>% mutate(cohort = "FHS")

comb_clin <- rbind(cardia_clin, FHS_clin, MESA_clin, ARIC_clin)
comb_dat <- readRDS("data/comb_dat_id.rds")
comb_dat %<>% mutate(AGE = round(AGE))
head(comb_dat)
# Filter out those who were under 18 or HTN at baseline --------------------------

u1850 <-
  comb_dat %>%
  filter(visit == 1) %>%
  distinct(AGE, id) %>%
  filter(AGE < 18)

baselinehtn <-
  comb_dat %>%
  filter(visit == 1) %>% 
  distinct(HRX, SBP, DBP, AGE, id) %>%
  filter((HRX == 1 | SBP >= 140 | DBP >= 90))


comb_dat %<>%
  anti_join(u1850, by = 'id')%>%
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

onset_NA <- onset %>% filter(is.na(onsetage))
onset_age <- onset %>% filter(!is.na(onsetage)) 

# Custom Age Buckets ------------------------------------------------------

# Teemu requested age_categories
age_breaks <-
  list(
    data_frame(
      start = c(seq(15.01, 85.01, 10)),
      end   = start + 10,
      category = paste(start, end, sep = '-')
    ),
    data_frame(
      start = c(seq(15.01, 75.01, 15)),
      end   = start + 15,
      category = paste(start, end, sep = '-')
    ),
    data_frame(
      start = c(15,45,55,65),
      end   = c(45,55,65,95),
      category = paste(start-0.01, end-0.01, sep = '-')
    )
  )


multi_ifelse <- function(x, breaks) {
  level <- 1
  output <- rep(0, length(x))
  
  if ('tbl_df' %in% class(breaks)) {
    breaks %<>% as.data.frame()
  }
  
  # Later categroy takes the intersections
  for (i in seq_len(nrow(breaks))) {
    output <-
      ifelse(between(x, breaks[i, 'start'], breaks[i, 'end']), breaks$category[level], output)
    level <- level + 1
  }
  
  output <-
    as.factor(output)
  
  return(output)
}


# AGE CATEGORIES

age_categories <-
  onset_age %>%
  distinct(id, onsetage) %>%
  mutate(
    category_quintile = cut(onsetage, breaks = quantile(onsetage, probs = seq(0,1,0.2))),
    category_1       = multi_ifelse(onsetage, age_breaks[[1]]),
    category_2       = multi_ifelse(onsetage, age_breaks[[2]]),
    category_3       = multi_ifelse(onsetage, age_breaks[[3]])
  )

sum(age_categories$category_3=="0")

na_id <-
  onset_NA %>%
  distinct(id)

age_categories %<>% 
  add_row(id = na_id[[1]]) %>%
  mutate_at(vars(category_quintile:category_3), funs(ifelse(id %in% na_id[[1]], 'NO_HTN', .))) %>%
  mutate_at(vars(category_quintile:category_3), as.factor)

onset_age %<>% bind_rows(onset_NA)

# Fix Mislabeled Category_Quintile -------------------------------------------------------
# The automatically generated breaks sometimes excludes respondents below the minimum age cutoff.
# We assign missing people to the lowest category.

age_categories %<>%
  mutate(category_quintile = ifelse(
    is.na(category_quintile),
    levels(category_quintile)[1],
    as.character(category_quintile)
  )) %>%
  mutate(category_quintile = as.factor(category_quintile))

stopifnot(!any(is.na(age_categories$category_quintile)))

# Group Sizes -------------------------------------------------------------
age_category_names <- grep('^cat', names(age_categories), value = T)

age_category_sizes <- map(age_category_names, function(category) {
  age_categories %>%
    group_by_(category) %>%
    dplyr::summarise(n =  n()) %>%
    set_names(c('category', 'n'))
}) %>%
  set_names(age_category_names) %>%
  bind_rows(.id = 'category_type')


# Format Data For Modeling -------------------------------------------------------

comb_dat %<>%
  left_join(age_categories, by = "id") %>% # diag_age
  filter(onsetage <= 75 | is.na(onsetage)) %>%
  filter(AGE <= onsetage | is.na(onsetage))

num_visit <-
  comb_dat %>%
  group_by(id) %>%
  dplyr::summarise(n = n()) %>%
  filter(n < 2)

comb_dat %<>%
  anti_join(num_visit, by = 'id')

head(comb_dat)

saveRDS(comb_dat, "data/comb_dat_id_onset.rds")

onsetgroup <- comb_dat %>% dplyr::select(id, onsetgroup = category_3) %>% 
  unique() %>% 
  na.omit() 

dat <- onsetgroup %>% left_join(comb_clin, by ="id")

saveRDS(dat, "data/comb_clin")

