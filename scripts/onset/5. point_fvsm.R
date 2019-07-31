point_f <- readRDS("data/pointci_f.rds")
point_m <- readRDS("data/pointci_m.rds")

point_f %<>% dplyr::select(p_f = Value, se_f = `Std.Error`)
point_m %<>% dplyr::select(p_m = Value, se_m = `Std.Error`, group)

point <- cbind(point_f, point_m) %>%
  mutate(point_f = paste(sprintf("%.3f",p_f),"(",sprintf("%.3f",p_f - 1.96*se_f),",",sprintf("%.3f",p_f + 1.96*se_f),")", sep = ""),
         point_m = paste(sprintf("%.3f",p_m),"(",sprintf("%.3f",p_m - 1.96*se_m),",",sprintf("%.3f",p_m + 1.96*se_m),")", sep = ""),

         point_z = (p_f-p_m)/(se_f^2 + se_m^2)^0.5,
         dif = p_f-p_m,
         se = (se_f^2 + se_m^2)^0.5,
         difci = paste(sprintf("%.3f",dif),"(",sprintf("%.3f",dif - 1.96*se),",",sprintf("%.3f",dif + 1.96*se),")", sep = ""),
         pval = 2*pnorm(-abs(point_z))
  )

write.csv(point, "output/point.csv")



