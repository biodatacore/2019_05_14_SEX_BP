###################################################################################
## make sure "mixed_spline.ado" was put in the root path of current project
## The output will be extracted files (fitted spline) in a path that we specified
###################################################################################

# install.packages('RStata')
library(RStata)
options("RStata.StataPath" = "/Applications/Stata/StataSE.app/Contents/MacOS/stata-se") # STATA path in MAC
options("RStata.StataVersion" = 15) # version of STATA

# ARIC, cardia, fhs, MESA, comb_dat
x <- readRDS("data/comb_dat_id.rds") # input for fitting mixed model spline
x %<>% filter(AGE >=18 & AGE <=85)

######## STATA code ######## 
# mixed_spline: function, which was put in the root path of current project
# SBP: as response
# AGE: as predictor
# id(variable): random effects
# by(variable): group
# trim(lowerlimit upperlimit): predictor range
# knots(integer): specify knots of fitted spline 
# path(string): specify where to save fitted spline
# savename(string): specify the name of spline data (.csv should be omitted)
############################ 

stata_src1 <- "

mixed_spline SBP BMI DM SMK TC AGE, id(id) by(SEX) trim(0.01 99.99) knots(4) path(/Users/Ji/Documents/2019_05_14_SEX_BP) savename(sbp_spline)

"
stata(stata_src1, data.in = x)


stata_src2 <- "

mixed_spline DBP BMI DM SMK TC AGE, id(id) by(SEX) trim(0.01 99.99) knots(4) path(/Users/Ji/Documents/2019_05_14_SEX_BP) savename(dbp_spline)

"
stata(stata_src2, data.in = x)


stata_src3 <- "

mixed_spline MAP BMI DM SMK TC AGE, id(id) by(SEX) trim(0.01 99.99) knots(4) path(/Users/Ji/Documents/2019_05_14_SEX_BP) savename(map_spline)

"
stata(stata_src3, data.in = x)


stata_src4 <- "

mixed_spline PP BMI DM SMK TC AGE, id(id) by(SEX) trim(0.01 99.99) knots(4) path(/Users/Ji/Documents/2019_05_14_SEX_BP) savename(pp_spline)

"
stata(stata_src4, data.in = x)



library(lme4)

lrt_sbp <- anova(
  lmer(SBP ~ bs(AGE) + SEX + BMI + DM + SMK + TC + (1|id), data = x),
  lmer(SBP ~ bs(AGE) + SEX + BMI + DM + SMK + TC + bs(AGE)*SEX + (1|id), data = x),
  test = "LRT"
)

lrt_dbp <- anova(
  lmer(DBP ~ bs(AGE) + SEX + BMI + DM + SMK + TC + (1|id), data = x),
  lmer(DBP ~ bs(AGE) + SEX + BMI + DM + SMK + TC + bs(AGE)*SEX + (1|id), data = x ),
  test = "LRT"
)

lrt_map <- anova(
  lmer(MAP ~ bs(AGE) + SEX + BMI + DM + SMK + TC + (1|id), data = x),
  lmer(MAP ~ bs(AGE) + SEX + BMI + DM + SMK + TC + bs(AGE)*SEX + (1|id), data = x),
  test = "LRT"
)

lrt_pp <- anova(
  lmer(PP ~ bs(AGE) + SEX + BMI + DM + SMK + TC + (1|id), data = x),
  lmer(PP ~ bs(AGE) + SEX + BMI + DM + SMK + TC + bs(AGE)*SEX + (1|id), data = x),
  test = "LRT"
)



