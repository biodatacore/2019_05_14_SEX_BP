
sbpplot_data <- read.csv("sbp_spline.csv")

sbp_line0 <- sbpplot_data %>% dplyr::select(spvar, pred_0, lb_0, ub_0) %>% na.omit() %>% unique()
sbp_line1 <- sbpplot_data %>% dplyr::select(spvar, pred_1, lb_1, ub_1) %>% na.omit() %>% unique()

sbp_line0 <- sbp_line0[order(sbp_line0$spvar),] %>% mutate(se_0 = (ub_0 - lb_0)/(2*1.96))
sbp_line1 <- sbp_line1[order(sbp_line1$spvar),] %>% mutate(se_1 = (ub_1 - lb_1)/(2*1.96))

sbp_y0<- sbp_line0[1,"pred_0"] 
sbp_y1<- sbp_line1[1,"pred_1"]

sbpline <- sbp_line0 %>% 
  left_join(sbp_line1, by = "spvar") %>%
  mutate(dif = (pred_1-sbp_y1) - (pred_0-sbp_y0),
         difse = (se_0^2 + se_1^2)^0.5)


dbpplot_data <- read.csv("dbp_spline.csv")

dbp_line0 <- dbpplot_data %>% dplyr::select(spvar, pred_0, lb_0, ub_0) %>% na.omit() %>% unique()
dbp_line1 <- dbpplot_data %>% dplyr::select(spvar, pred_1, lb_1, ub_1) %>% na.omit() %>% unique()

dbp_line0 <- dbp_line0[order(dbp_line0$spvar),] %>% mutate(se_0 = (ub_0 - lb_0)/(2*1.96))
dbp_line1 <- dbp_line1[order(dbp_line1$spvar),] %>% mutate(se_1 = (ub_1 - lb_1)/(2*1.96))

dbp_y0<- dbp_line0[1,"pred_0"] 
dbp_y1<- dbp_line1[1,"pred_1"]

dbpline <- dbp_line0 %>% 
  left_join(dbp_line1, by = "spvar") %>%
  mutate(dif = (pred_1-dbp_y1) - (pred_0-dbp_y0),
         difse = (se_0^2 + se_1^2)^0.5)


mapplot_data <- read.csv("map_spline.csv")

map_line0 <- mapplot_data %>% dplyr::select(spvar, pred_0, lb_0, ub_0) %>% na.omit() %>% unique()
map_line1 <- mapplot_data %>% dplyr::select(spvar, pred_1, lb_1, ub_1) %>% na.omit() %>% unique()

map_line0 <- map_line0[order(map_line0$spvar),] %>% mutate(se_0 = (ub_0 - lb_0)/(2*1.96))
map_line1 <- map_line1[order(map_line1$spvar),] %>% mutate(se_1 = (ub_1 - lb_1)/(2*1.96))

map_y0<- map_line0[1,"pred_0"] 
map_y1<- map_line1[1,"pred_1"]

mapline <- map_line0 %>% 
  left_join(map_line1, by = "spvar") %>%
  mutate(dif = (pred_1-map_y1) - (pred_0-map_y0),
         difse = (se_0^2 + se_1^2)^0.5)



ppplot_data <- read.csv("pp_spline.csv")

pp_line0 <- ppplot_data %>% dplyr::select(spvar, pred_0, lb_0, ub_0) %>% na.omit() %>% unique()
pp_line1 <- ppplot_data %>% dplyr::select(spvar, pred_1, lb_1, ub_1) %>% na.omit() %>% unique()

pp_line0 <- pp_line0[order(pp_line0$spvar),] %>% mutate(se_0 = (ub_0 - lb_0)/(2*1.96))
pp_line1 <- pp_line1[order(pp_line1$spvar),] %>% mutate(se_1 = (ub_1 - lb_1)/(2*1.96))

pp_y0<- pp_line0[1,"pred_0"] 
pp_y1<- pp_line1[1,"pred_1"]

ppline <- pp_line0 %>% 
  left_join(pp_line1, by = "spvar") %>%
  mutate(dif = (pred_1-pp_y1) - (pred_0-pp_y0),
         difse = (se_0^2 + se_1^2)^0.5)



gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}
n = 8
cols = gg_color_hue(n)


ggplot() +
  geom_line(aes(x = spvar, y = dif, color = "sbp"), data = sbpline) + 
  geom_ribbon(aes(x = spvar, ymin = dif - 1.96*difse, ymax = dif + 1.96*difse, fill = "sbp"), alpha = 0.2, data = sbpline) + 
  geom_line(aes(x = spvar, y = dif, color = "dbp"), data = dbpline) + 
  geom_ribbon(aes(x = spvar, ymin = dif - 1.96*difse, ymax = dif + 1.96*difse, fill = "dbp"), alpha = 0.2, data = dbpline) + 
  geom_line(aes(x = spvar, y = dif, color = "map"), data = mapline) + 
  geom_ribbon(aes(x = spvar, ymin = dif - 1.96*difse, ymax = dif + 1.96*difse, fill = "map"), alpha = 0.2, data = mapline) + 
  geom_line(aes(x = spvar, y = dif, color = "pp"), data = ppline) + 
  geom_ribbon(aes(x = spvar, ymin = dif - 1.96*difse, ymax = dif + 1.96*difse, fill = "pp"), alpha = 0.2, data = ppline) + 
  scale_color_manual(name = "BP measures", 
                     breaks = c("sbp","dbp","map","pp"),
                     values = c("sbp" = "blue4", "dbp" = cols[2], "map" = cols[6], "pp" = cols[1]),
                     labels = c("sbp" ="SBP","dbp" = "DBP", "map" = "MAP","pp" = "PP")) +
  scale_fill_manual(name = "BP measures", 
                    breaks = c("sbp","dbp","map","pp"),
                    values = c("sbp" = "blue4", "dbp" = cols[2], "map" = cols[6], "pp" = cols[1]),
                    labels = c("sbp" ="SBP","dbp" = "DBP", "map" = "MAP","pp" = "PP")) +
  geom_hline(yintercept=0, lty=1, lwd=1, col="black") +  
  theme_bw() +
  theme(axis.text= element_text(hjust = 0.5, size= 12,colour="black",face="bold"),
        axis.title = element_text(hjust = 0.5, size= 16,colour="black",face="bold"),
        plot.title = element_text(hjust = 0.5, size= 16,colour="black",face="bold"),
        legend.position = c(0.2, 0.8),
        legend.text = element_text(hjust = 0.5, size= 12,colour="black",face="bold"),
        legend.title = element_text(hjust = 0.5, size= 12,colour="black",face="bold"),
        legend.background = element_rect(fill="white",
                                         size=0.5, linetype="solid", 
                                         colour ="black")
  ) +
  scale_x_continuous(name=c("Age"), breaks = seq(from = 20, to = 80, by = 10)) + # change #
  scale_y_continuous(name="Increment From Baseline [F vs. M], mmHg") + 
  ggtitle("")


