slopeci_f <- readRDS("data/slopeci_f.rds")
slopeci_m <- readRDS("data/slopeci_m.rds")

colnames(slopeci_f) <- c("f_slope","f_low","f_up","f_est","f_se","f_group")
colnames(slopeci_m) <- c("m_slope","m_low","m_up","m_est","m_se","m_group")

slope <- cbind(slopeci_f, slopeci_m) %>%
  mutate(slopeci_f = paste(sprintf("%.3f",f_est),"(",sprintf("%.3f",f_low),",",sprintf("%.3f",f_up),")", sep = ""),
         slopeci_m = paste(sprintf("%.3f",m_est),"(",sprintf("%.3f",m_low),",",sprintf("%.3f",m_up),")", sep = ""),
         
         pval_f = 2*pnorm(-abs(f_est/f_se)),
         pval_m = 2*pnorm(-abs(m_est/m_se)),
         
         slope_z = (f_est-m_est)/(f_se^2 + m_se^2)^0.5,
         pval = 2*pnorm(-abs(slope_z))
  )

beforeslp <- slope %>% filter(f_slope == "before_breakpoint")
afterslp <- slope %>% filter(f_slope == "after_breakpoint")

write.csv(beforeslp, "output/beforeslp.csv")
write.csv(afterslp, "output/afterslp.csv")



