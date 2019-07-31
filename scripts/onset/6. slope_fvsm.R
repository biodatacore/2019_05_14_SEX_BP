slopeci_f <- readRDS("data/slopeci_f.rds")
slopeci_m <- readRDS("data/slopeci_m.rds")

colnames(slopeci_f) <- c("f_slope","f_low","f_up","f_n","f_est","f_se","f_group")
colnames(slopeci_m) <- c("m_slope","m_low","m_up","m_n","m_est","m_se","m_group")

slope <- cbind(slopeci_f, slopeci_m) %>%
  mutate(slopeci_f = paste(sprintf("%.3f",f_est),"(",sprintf("%.3f",f_low),",",sprintf("%.3f",f_up),")", sep = ""),
         slopeci_m = paste(sprintf("%.3f",m_est),"(",sprintf("%.3f",m_low),",",sprintf("%.3f",m_up),")", sep = ""),
         
         pval_f = 2*pnorm(-abs(f_est/f_se)),
         pval_m = 2*pnorm(-abs(m_est/m_se)),
         
         sd_f = f_se * f_n^0.5,
         sd_m = m_se * m_n^0.5,
         
         slope_z = (f_est-m_est)/(f_se^2 + m_se^2)^0.5,
         
         dif = f_est-m_est,
         se = (f_se^2 + m_se^2)^0.5,
         df = (f_n - 1) + (m_n - 1),
         difci = paste(sprintf("%.3f",dif),"(",sprintf("%.3f",dif - 1.96*se),",",sprintf("%.3f",dif + 1.96*se),")", sep = ""),
         pval = 2*pnorm(-abs(slope_z))
  )

beforeslp <- slope %>% filter(f_slope == "before_breakpoint")
afterslp <- slope %>% filter(f_slope == "after_breakpoint")

deltaslope <- data.frame(
  dif_f = afterslp$f_est - beforeslp$f_est,
  se_f = (afterslp$f_se^2 + beforeslp$f_se^2)^0.5,
  dif_m = afterslp$m_est - beforeslp$m_est,
  se_m = (afterslp$m_se^2 + beforeslp$m_se^2)^0.5
  ) %>%
  mutate(difci_f = paste(sprintf("%.3f",dif_f),"(",sprintf("%.3f",dif_f - 1.96*se_f),",",sprintf("%.3f",dif_f + 1.96*se_f),")", sep = ""),
         difci_m = paste(sprintf("%.3f",dif_m),"(",sprintf("%.3f",dif_m - 1.96*se_m),",",sprintf("%.3f",dif_m + 1.96*se_m),")", sep = ""),
         slope_z = (dif_f-dif_m)/(se_f^2 + se_m^2)^0.5,
         pval = 2*pnorm(-abs(slope_z)))
  

write.csv(beforeslp, "output/beforeslp.csv")
write.csv(afterslp, "output/afterslp.csv")
write.csv(deltaslope,"output/deltaslope.csv")
