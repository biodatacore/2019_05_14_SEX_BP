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

# Filter out those who were under 18 at baseline --------------------------

u1850 <-
  comb_dat %>%
  filter(visit == 1 | visit ==0) %>%
  distinct(AGE, id) %>%
  filter(AGE <= 18 | AGE > 50)

num_visit <-
  comb_dat %>%
  group_by(id) %>%
  dplyr::summarise(n = n()) %>%
  filter(n<4)

comb_dat %<>%
  anti_join(u1850, by = 'id') %>%
  anti_join(num_visit, by = 'id') 


rm(u1850, num_visit)

# Filter out unmeasured exams and exams those never diagnosed with hptn --------

onset <- comb_dat %>% 
  mutate(SBP = ifelse(HRX==1, SBP - 10 , SBP),
         DBP = ifelse(HRX==1, DBP - 5 , DBP),
         AGE = round(AGE)) %>%
  mutate(onsetage = ifelse(SBP >= 140 | DBP >=90 | HRX == 1, AGE, NA),
         onsetvisit = ifelse(SBP >= 140 | DBP >=90 | HRX == 1, visit, NA)) %>%
  setDT() %>%
  dcast(id ~ visit, value.var = c("onsetage")) 


onset$onsetage <- apply(dplyr::select(onset, -id), 1, FUN = min, na.rm = T)
onset %<>% mutate(onsetage = ifelse(onsetage == Inf, NA, onsetage))


onset_NA <- onset %>% filter(is.na(onsetage))
onset_age <- onset %>% filter(!is.na(onsetage)) 

# Custom Age Buckets ------------------------------------------------------

# Teemu requested age_categories
age_breaks <-
  list(
    data_frame(
      start = c(seq(40, 60, 10)),
      end   = start + 10,
      category = paste(start, end, sep = '-')
    ),
    data_frame(
      start = c(seq(35, 65, 10)),
      end   = start + 10,
      category = paste(start, end, sep = '-')
    ),
    data_frame(
      start = c(seq(35, 65, 15)),
      end   = start + 15,
      category = paste(start, end, sep = '-')
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
    category_quintile = cut(onsetage, breaks = quantile(onsetage, probs = seq(0, 1, 1/2))),
    category_1       = multi_ifelse(onsetage, age_breaks[[1]]),
    category_2       = multi_ifelse(onsetage, age_breaks[[2]]),
    category_3       = multi_ifelse(onsetage, age_breaks[[3]])
  )

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
    arrange(category_quintile) %>%
    group_by_(category) %>%
    dplyr::summarise(n =  n()) %>%
    set_names(c('category', 'n'))
}) %>%
  set_names(age_category_names) %>%
  bind_rows(.id = 'category_type')


# Format Data For Modeling -------------------------------------------------------

comb_dat %<>%
  left_join(age_categories, by = "id")  # diag_age

comb_dat$category_quintile %>% as.factor() %>% summary()

comb_dats <-
  map(age_category_names, function(n) {
    split(comb_dat, comb_dat[[n]])
  }) %>%
  set_names(age_category_names)

# Model Function ----------------------------------------------------------

ctrl <- lmeControl(opt = 'optim')

seg_model <- function(d) {
  segmented.lme(
    lme(
      SBP ~ AGE,
      control = ctrl,
      data = d,
      random =  list(id = ~ 1)
    ),
    Z = AGE,
    random = list(id = pdDiag(~ 1))
  )
}


# Segmented models --------------------------------------------------------
segmentedLMs <-
  comb_dats %>%
  at_depth(2, function(x)
    NA)

for (i in seq_len(length(comb_dats))) {
  print(names(comb_dats[i]))
  for (j in seq_len(length(comb_dats[[i]]))) {
    print(names(comb_dats[[i]][j]))
    
    
    d <- comb_dats[[i]][[j]]
    
    output <- NA
    try(output <- seg_model(d))
    
    segmentedLMs[[i]][[j]] <- output
  }
}


segmentedLMs %<>%
  map(function(x) {
    discard(x, ~ length(.) == 1)
  })


# Plotting Things ---------------------------------------------------------

test<-segmentedLMs %>%
  at_depth(2, 'lme.fit') %>%
  at_depth(2, summary)

# Extract slopes and changepoints from models  ------------------------------------------------------
# The way ggplot works is that it is easiest to add different elements 
# lines, labels, CIs, etc when each is in its own dataset. These chunks
# beneath make those separate datasets that will later be graphed.
linslopes <-
  segmentedLMs %>%
  at_depth(2, 'lme.fit') %>%
  at_depth(2, summary) %>%
  at_depth(2, function(x) {
    minx <- min(range(x$data$AGE))
    maxx <- max(range(x$data$AGE))
    
    plot_min <- minx + (0.05) * (maxx - minx)
    plot_max <- maxx - (0.05) * (maxx - minx)
    
    x <- x$tTable
    rnames <- rownames(x)
    
    estimates = x[, 1]
    int = estimates[rnames == '(Intercept)']
    slope = estimates[rnames == 'AGE']
    deltaSlope = estimates[rnames == 'U']
    changepoint = estimates[rnames == 'G0']
    
    newX1 = seq(plot_min, changepoint, length.out = 2)
    newX2 = seq(changepoint, plot_max, length.out = 2)
    
    
    newY1 = int + newX1 * slope
    newY2 = max(newY1) + (newX2 - changepoint) * (slope + deltaSlope)
    
    return(list(
      x = unique(c(newX1, newX2)),
      y = unique(c(newY1, newY2)),
      cpx = changepoint,
      cpy = max(newY1)
    ))
  } )

text <-
  linslopes %>%
  at_depth(2, function(d) {
    data_frame(x = d$cpx, y = d$cpy)
  }) %>%
  map( ~ bind_rows(., .id = 'category')) %>%
  bind_rows(.id = 'category_type')

linslopes %<>%
  at_depth(2, function(d) {
    data_frame(x = d$x, y = d$y)
  }) %>%
  map( ~ bind_rows(., .id = 'category')) %>%
  bind_rows(.id = 'category_type')

age_category_sizes %<>%
  semi_join(text) %>% # remove NA row and failed model row
  left_join(linslopes %>%
              group_by(category_type, category) %>%
              filter(x == max(x)) %>%
              ungroup())


# CI Calculation ----------------------------------------------------------

# cis <-
#   segmentedLMs %>%
#   at_depth(2, function(m) {
#     intervals(m$lme.fit, which = 'fixed')
#   }) %>%
#   at_depth(2, 'fixed')

# Export CIs
# cis %>%
#   at_depth(2, function(x) {
#     rnames <- rownames(x)
#     
#     x <- as_data_frame(x)
#     
#     x$term <- rnames
#     
#     x
#   }) %>%
#   map( ~ bind_rows(., .id = 'category')) %>%
#   bind_rows(.id = 'category_type') %>%
#   write_tsv(paste0('all_cis_', VISITSBEFOREHTN, '.tsv'))


# CIs for plotting --------------------------------------------------------


# cp_cis <-
#   cis %>%
#   at_depth(2, function(x) {
#     rnames <- rownames(x)
#     
#     
#     x[rnames == 'G0',] %>%
#       as_data_frame() %>%
#       mutate(which = c('lower', 'est', 'upper'))
#   }) %>%
#   map( ~ bind_rows(., .id = 'category')) %>%
#   bind_rows(.id = 'category_type') %>%
#   spread(which, value)  %>%
#   left_join(text, by = c('category_type', 'category'))

# age_breaks %<>%
#   bind_rows(.id = 'category_type') %>%
#   mutate(
#     category = as.character(category),
#     category_type = paste0('category_', category_type)
#   ) %>%
#   mutate(text = paste(start, '-', end)) %>%
#   bind_rows(
#     age_categories %>%
#       distinct(category_quintile) %>%
#       filter(!is.na(category_quintile)) %>%
#       mutate(category = as.character(1:n()),
#              text = category_quintile) %>%
#       dplyr::select(-category_quintile)
#   )  %>%
#   mutate(category_type = ifelse(is.na(category_type), 'category_quintile', category_type)) %>%
#   left_join(
#     linslopes %>%
#       group_by(category_type, category) %>%
#       dplyr::summarise(y = y[x == min(x)], x = min(x)) %>%
#       ungroup()
#   )

# Group Size filtering ----------------------------------------------------

age_category_sizes %<>%
  filter(n > 50)

plotDset <-
  comb_dat %>%
  mutate(SBP = ifelse(HRX==1, SBP + 10 , SBP),
         DBP = ifelse(HRX==1, DBP + 5 , DBP)) %>%
  select(AGE, SBP, category_quintile:category_3) %>% ###
  gather('category_type', 'category', -c(1:2)) %>%
  group_by(category_type, category) %>%
  filter(AGE >= min(AGE) + 0.025 * (max(AGE) - min(AGE)) &
           AGE <= max(AGE) - 0.025 * (max(AGE) - min(AGE))) %>%
  semi_join(age_category_sizes)

# cp_cis %<>%
#   semi_join(age_category_sizes, by = c('category_type', 'category'))

# text %<>%
#   semi_join(age_category_sizes, by = c('category_type', 'category'))

linslopes %<>%
  semi_join(age_category_sizes, by = c('category_type', 'category'))

# age_breaks %<>%
#   semi_join(age_category_sizes, by = c('category_type', 'category'))

# CI Plot -----------------------------------------------------------------

# pdf(
#   paste0('catplot', VISITSBEFOREHTN, '.pdf'),
#   width = 4 * 8,
#   height = 2 * 8
# )
library(splines)
library(lspline) # install.packages("lspline")

ggplot() +
  geom_smooth(
    aes(x = AGE, y = SBP, colour = category),
    method = "loess",
    data = plotDset %>% filter(category_type == "category_2")
  ) +
  # geom_errorbarh(
  #   aes(y = y, x = est, xmax = upper, xmin = lower, colour = category),
  #   height = 4, 
  #   data = cp_cis
  # ) +
  geom_line(
    aes(x = x, y = y, colour = category), 
    data = linslopes %>% filter(category_type == "category_2")
  ) # +
  # geom_text(
  #   aes(x = x, y = y, label = round(y, 2), colour = category), 
  #   vjust = 1, 
  #   data = text
  # ) +
  # geom_text(
  #   aes(x = x, y = y, label = n, colour = category), 
  #   vjust = 1, 
  #   data = age_category_sizes) +
  # geom_text(
  #   aes(x = x, y = y, label = text, colour = category), 
#   vjust = 1, 
#   data = age_breaks) +
# facet_wrap( ~ category_type)

# dev.off()
