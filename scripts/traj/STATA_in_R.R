# install.packages('RStata')
library(RStata)
options("RStata.StataPath" = "/Applications/Stata/StataSE.app/Contents/MacOS/stata-se")  
options("RStata.StataVersion" = 15)
options("RStata.StataPath")
options("RStata.StataVersion")

x <- readRDS("data/comb_dat.rds") 
x %<>% filter(AGE >=18 & AGE <=85)

stata_src1 <- "

mixed_spline SBP AGE, id(id) by(SEX) trim(0.01 99.99) knots(4) path(/Users/Ji/Documents/2019_05_14_SEX_BP) savename(sbp_spline)

"
stata(stata_src1, data.in = x)


stata_src2 <- "

mixed_spline DBP AGE, id(id) by(SEX) trim(0.01 99.99) knots(4) path(/Users/Ji/Documents/2019_05_14_SEX_BP) savename(dbp_spline)

"
stata(stata_src2, data.in = x)


stata_src3 <- "

mixed_spline MAP AGE, id(id) by(SEX) trim(0.01 99.99) knots(4) path(/Users/Ji/Documents/2019_05_14_SEX_BP) savename(map_spline)

"
stata(stata_src3, data.in = x)


stata_src4 <- "

mixed_spline PP AGE, id(id) by(SEX) trim(0.01 99.99) knots(4) path(/Users/Ji/Documents/2019_05_14_SEX_BP) savename(pp_spline)

"
stata(stata_src4, data.in = x)

