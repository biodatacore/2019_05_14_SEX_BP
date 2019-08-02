
library(haven)
library(dplyr)
library(magrittr)
library(ggplot2)

bslage_f <- readRDS("data/bslage_f.rds")
bslage_m <- readRDS("data/bslage_m.rds")

# bslage_f2 <- readRDS("data/bslage_f2.rds")
# bslage_m2 <- readRDS("data/bslage_m2.rds")

bslage_f2 <- readRDS("data/age_f2.rds")
bslage_m2 <- readRDS("data/age_m2.rds")

# SBP

sbpplot_data <- read.csv("sbp_spline.csv")
sbpplot_data %<>% filter(spvar >=18 & spvar <=85)

sbp_line0 <- sbpplot_data %>% dplyr::select(spvar, pred_0, lb_0, ub_0) %>% na.omit() %>% unique()
sbp_line1 <- sbpplot_data %>% dplyr::select(spvar, pred_1, lb_1, ub_1) %>% na.omit() %>% unique()

ggplot() +
  geom_line(aes(x = spvar, y = pred_0, color = "blue4"), data = sbp_line0) +
  geom_ribbon(aes(x = spvar, ymin = lb_0, ymax = ub_0, fill = "blue4"), alpha = 0.2, data = sbp_line0) +
  geom_line(aes(x = spvar, y = pred_1, color = "red4"), data = sbp_line1) +
  geom_ribbon(aes(x = spvar, ymin = lb_1, ymax = ub_1, fill = "red4"), alpha = 0.2, data = sbp_line1) +
  scale_color_manual(name = "Sex", values = c("red4"="red4", "blue4"="blue4"), labels = c("red4"="Women","blue4"="Men")) +
  scale_fill_manual(name = "Sex", values = c("red4"="red4", "blue4"="blue4"), labels = c("red4"="Women","blue4"="Men")) +
  scale_y_continuous(name = "SBP, mm Hg") + 
  scale_x_continuous(name = "Age", breaks = seq(from = 20, to = 80, by = 10)) + 
  ggtitle("Systolic Blood Pressure") +
  theme_bw() +
  theme(axis.title = element_text(color = "#434443",size =15,face="bold"),
        axis.text = element_text(color = "#434443",size =12,face="bold"),
        title = element_text(color = "#434443",size =16,face="bold"),
        legend.text = element_text(color = "#434443",size =10,face="bold"))


ggplot() +
  coord_cartesian(ylim = c(100, 150)) +
  geom_line(aes(x = spvar, y = pred_0, color = "blue4"), data = sbp_line0) +
  geom_ribbon(aes(x = spvar, ymin = lb_0, ymax = ub_0, fill = "blue4"), alpha = 0.2, data = sbp_line0) +
  geom_line(aes(x = spvar, y = pred_1, color = "red4"), data = sbp_line1) +
  geom_ribbon(aes(x = spvar, ymin = lb_1, ymax = ub_1, fill = "red4"), alpha = 0.2, data = sbp_line1) +
  
  geom_col(aes(x = category+1.5, y = rate/15+100, fill = "blue4"), position = "dodge", width=3, alpha = 0.2, data = bslage_m) +
  geom_col(aes(x = category-1.5, y = rate/15+100, fill = "red4"), position = "dodge", width=3, alpha = 0.2, data = bslage_f) +
  
  scale_color_manual(name = "Sex", values = c("red4"="red4", "blue4"="blue4"), labels = c("red4"="Women","blue4"="Men")) +
  scale_fill_manual(name = "Sex", values = c("red4"="red4", "blue4"="blue4"), labels = c("red4"="Women","blue4"="Men")) +
  scale_y_continuous(expand = c(0,0), name = "SBP, mm Hg", position = "left", 
                     sec.axis = sec_axis(~., name = "10-year Incident CVD\n(cases per 1000 pers.)", breaks = seq(100, 150, 10), labels = seq(0, 750, 150))) +
  scale_x_continuous(name = "Age", breaks = seq(from = 20, to = 80, by = 10)) + 
  ggtitle("Systolic Blood Pressure") +
  theme_bw() +
  theme(axis.title.y.left = element_text(color = "#434443",size =15,face="bold"),
        axis.title.y.right = element_text(color = "#434443",size =12,face="bold"),
        axis.text.y.left = element_text(color = "#434443",size =12,face="bold"),
        axis.text.y.right = element_text(color = "#434443",size =8,face="bold"),
        title = element_text(color = "#434443",size =16,face="bold"),
        legend.text = element_text(color = "#434443",size =10,face="bold"))



sbp_y0<- sbp_line0[order(sbp_line0$spvar),][1,"pred_0"] 
sbp_y1<- sbp_line1[order(sbp_line1$spvar),][1,"pred_1"]

ggplot() +
  geom_line(aes(x = spvar, y = pred_0 - sbp_y0, color = "blue4"), data = sbp_line0) +
  geom_ribbon(aes(x = spvar, ymin = lb_0 - sbp_y0, ymax = ub_0 - sbp_y0, fill = "blue4"), alpha = 0.2, data = sbp_line0) +
  geom_line(aes(x = spvar, y = pred_1 - sbp_y1, color = "red4"), data = sbp_line1) +
  geom_ribbon(aes(x = spvar, ymin = lb_1 - sbp_y1, ymax = ub_1 - sbp_y1, fill = "red4"), alpha = 0.2, data = sbp_line1) +
  scale_color_manual(name = "Sex", values = c("red4"="red4", "blue4"="blue4"), labels = c("red4"="Women","blue4"="Men")) +
  scale_fill_manual(name = "Sex", values = c("red4"="red4", "blue4"="blue4"), labels = c("red4"="Women","blue4"="Men")) +
  scale_y_continuous(name = "SBP Elevation, mm Hg") + 
  scale_x_continuous(name = "Age", breaks = seq(from = 20, to = 80, by = 10)) + 
  ggtitle("SBP Elevation from Baseline") +
  theme_bw() +
  theme(axis.title = element_text(color = "#434443",size =15,face="bold"),
        axis.text = element_text(color = "#434443",size =12,face="bold"),
        title = element_text(color = "#434443",size =16,face="bold"),
        legend.text = element_text(color = "#434443",size =10,face="bold"))


ggplot() +
  coord_cartesian(ylim = c(0, 45)) +
  geom_line(aes(x = spvar, y = pred_0 - sbp_y0, color = "blue4"), data = sbp_line0) +
  geom_ribbon(aes(x = spvar, ymin = lb_0 - sbp_y0, ymax = ub_0 - sbp_y0, fill = "blue4"), alpha = 0.2, data = sbp_line0) +
  geom_line(aes(x = spvar, y = pred_1 - sbp_y1, color = "red4"), data = sbp_line1) +
  geom_ribbon(aes(x = spvar, ymin = lb_1 - sbp_y1, ymax = ub_1 - sbp_y1, fill = "red4"), alpha = 0.2, data = sbp_line1) +
  
  geom_col(aes(x = category+1.5, y = rate/1.5, fill = "blue4"), position = "dodge", width=3, alpha = 0.2, data = bslage_m2) +
  geom_col(aes(x = category-1.5, y = rate/1.5, fill = "red4"), position = "dodge", width=3, alpha = 0.2, data = bslage_f2) +
  
  scale_color_manual(name = "Sex", values = c("red4"="red4", "blue4"="blue4"), labels = c("red4"="Women","blue4"="Men")) +
  scale_fill_manual(name = "Sex", values = c("red4"="red4", "blue4"="blue4"), labels = c("red4"="Women","blue4"="Men")) +
  scale_y_continuous(expand = c(0,0), name = "SBP Elevation, mm Hg", position = "left", 
                     sec.axis = sec_axis(~., name = "10-year Incident CVD\n(Fold change from Ref)", 
                                         breaks = c(1/1.5,10,20,30,40), labels = c(1,15,30,45,60))) +
  scale_x_continuous(name = "Age", breaks = seq(from = 20, to = 80, by = 10)) + 
  ggtitle("SBP Elevation from Baseline") +
  theme_bw() +
  theme(axis.title.y.left = element_text(color = "#434443",size =15,face="bold"),
        axis.title.y.right = element_text(color = "#434443",size =12,face="bold"),
        axis.text.y.left = element_text(color = "#434443",size =12,face="bold"),
        axis.text.y.right = element_text(color = "#434443",size =8,face="bold"),
        title = element_text(color = "#434443",size =16,face="bold"),
        legend.text = element_text(color = "#434443",size =10,face="bold"))




# DBP

dbpplot_data <- read.csv("dbp_spline.csv")
dbpplot_data %<>% filter(spvar >=18 & spvar <=85)

dbp_line0 <- dbpplot_data %>% dplyr::select(spvar, pred_0, lb_0, ub_0) %>% na.omit() %>% unique()
dbp_line1 <- dbpplot_data %>% dplyr::select(spvar, pred_1, lb_1, ub_1) %>% na.omit() %>% unique()

ggplot() +
  geom_line(aes(x = spvar, y = pred_0, color = "blue4"), data = dbp_line0) +
  geom_ribbon(aes(x = spvar, ymin = lb_0, ymax = ub_0, fill = "blue4"), alpha = 0.2, data = dbp_line0) +
  geom_line(aes(x = spvar, y = pred_1, color = "red4"), data = dbp_line1) +
  geom_ribbon(aes(x = spvar, ymin = lb_1, ymax = ub_1, fill = "red4"), alpha = 0.2, data = dbp_line1) +
  scale_color_manual(name = "Sex", values = c("red4"="red4", "blue4"="blue4"), labels = c("red4"="Women","blue4"="Men")) +
  scale_fill_manual(name = "Sex", values = c("red4"="red4", "blue4"="blue4"), labels = c("red4"="Women","blue4"="Men")) +
  scale_y_continuous(name = "DBP, mm Hg") + 
  scale_x_continuous(name = "Age", breaks = seq(from = 20, to = 80, by = 10)) + 
  ggtitle("Diastolic Blood Pressure") +
  theme_bw() +
  theme(axis.title = element_text(color = "#434443",size =15,face="bold"),
        axis.text = element_text(color = "#434443",size =12,face="bold"),
        title = element_text(color = "#434443",size =16,face="bold"),
        legend.text = element_text(color = "#434443",size =10,face="bold"))

dbp_y0<- dbp_line0[order(dbp_line0$spvar),][1,"pred_0"] 
dbp_y1<- dbp_line1[order(dbp_line1$spvar),][1,"pred_1"]

ggplot() +
  geom_line(aes(x = spvar, y = pred_0 - dbp_y0, color = "blue4"), data = dbp_line0) +
  geom_ribbon(aes(x = spvar, ymin = lb_0 - dbp_y0, ymax = ub_0 - dbp_y0, fill = "blue4"), alpha = 0.2, data = dbp_line0) +
  geom_line(aes(x = spvar, y = pred_1 - dbp_y1, color = "red4"), data = dbp_line1) +
  geom_ribbon(aes(x = spvar, ymin = lb_1 - dbp_y1, ymax = ub_1 - dbp_y1, fill = "red4"), alpha = 0.2, data = dbp_line1) +
  scale_color_manual(name = "Sex", values = c("red4"="red4", "blue4"="blue4"), labels = c("red4"="Women","blue4"="Men")) +
  scale_fill_manual(name = "Sex", values = c("red4"="red4", "blue4"="blue4"), labels = c("red4"="Women","blue4"="Men")) +
  scale_y_continuous(name = "DBP Elevation, mm Hg") + 
  scale_x_continuous(name = "Age", breaks = seq(from = 20, to = 80, by = 10)) + 
  ggtitle("DBP Elevation from Baseline") +
  theme_bw() +
  theme(axis.title = element_text(color = "#434443",size =15,face="bold"),
        axis.text = element_text(color = "#434443",size =12,face="bold"),
        title = element_text(color = "#434443",size =16,face="bold"),
        legend.text = element_text(color = "#434443",size =10,face="bold"))


# MAP


mapplot_data <- read.csv("map_spline.csv")
mapplot_data %<>% filter(spvar >=18 & spvar <=85)

map_line0 <- mapplot_data %>% dplyr::select(spvar, pred_0, lb_0, ub_0) %>% na.omit() %>% unique()
map_line1 <- mapplot_data %>% dplyr::select(spvar, pred_1, lb_1, ub_1) %>% na.omit() %>% unique()

ggplot() +
  geom_line(aes(x = spvar, y = pred_0, color = "blue4"), data = map_line0) +
  geom_ribbon(aes(x = spvar, ymin = lb_0, ymax = ub_0, fill = "blue4"), alpha = 0.2, data = map_line0) +
  geom_line(aes(x = spvar, y = pred_1, color = "red4"), data = map_line1) +
  geom_ribbon(aes(x = spvar, ymin = lb_1, ymax = ub_1, fill = "red4"), alpha = 0.2, data = map_line1) +
  scale_color_manual(name = "Sex", values = c("red4"="red4", "blue4"="blue4"), labels = c("red4"="Women","blue4"="Men")) +
  scale_fill_manual(name = "Sex", values = c("red4"="red4", "blue4"="blue4"), labels = c("red4"="Women","blue4"="Men")) +
  scale_y_continuous(name = "MAP, mm Hg") + 
  scale_x_continuous(name = "Age", breaks = seq(from = 20, to = 80, by = 10)) + 
  ggtitle("Mean Arterial Pressure") +
  theme_bw() +
  theme(axis.title = element_text(color = "#434443",size =15,face="bold"),
        axis.text = element_text(color = "#434443",size =12,face="bold"),
        title = element_text(color = "#434443",size =16,face="bold"),
        legend.text = element_text(color = "#434443",size =10,face="bold"))

map_y0<- map_line0[order(map_line0$spvar),][1,"pred_0"] 
map_y1<- map_line1[order(map_line1$spvar),][1,"pred_1"]

ggplot() +
  geom_line(aes(x = spvar, y = pred_0 - map_y0, color = "blue4"), data = map_line0) +
  geom_ribbon(aes(x = spvar, ymin = lb_0 - map_y0, ymax = ub_0 - map_y0, fill = "blue4"), alpha = 0.2, data = map_line0) +
  geom_line(aes(x = spvar, y = pred_1 - map_y1, color = "red4"), data = map_line1) +
  geom_ribbon(aes(x = spvar, ymin = lb_1 - map_y1, ymax = ub_1 - map_y1, fill = "red4"), alpha = 0.2, data = map_line1) +
  scale_color_manual(name = "Sex", values = c("red4"="red4", "blue4"="blue4"), labels = c("red4"="Women","blue4"="Men")) +
  scale_fill_manual(name = "Sex", values = c("red4"="red4", "blue4"="blue4"), labels = c("red4"="Women","blue4"="Men")) +
  scale_y_continuous(name = "MAP Elevation, mm Hg") + 
  scale_x_continuous(name = "Age", breaks = seq(from = 20, to = 80, by = 10)) + 
  ggtitle("MAP Elevation from Baseline") +
  theme_bw() +
  theme(axis.title = element_text(color = "#434443",size =15,face="bold"),
        axis.text = element_text(color = "#434443",size =12,face="bold"),
        title = element_text(color = "#434443",size =16,face="bold"),
        legend.text = element_text(color = "#434443",size =10,face="bold"))



# PP

ppplot_data <- read.csv("pp_spline.csv")
ppplot_data %<>% filter(spvar >=18 & spvar <=85)

pp_line0 <- ppplot_data %>% dplyr::select(spvar, pred_0, lb_0, ub_0) %>% na.omit() %>% unique()
pp_line1 <- ppplot_data %>% dplyr::select(spvar, pred_1, lb_1, ub_1) %>% na.omit() %>% unique()

ggplot() +
  geom_line(aes(x = spvar, y = pred_0, color = "blue4"), data = pp_line0) +
  geom_ribbon(aes(x = spvar, ymin = lb_0, ymax = ub_0, fill = "blue4"), alpha = 0.2, data = pp_line0) +
  geom_line(aes(x = spvar, y = pred_1, color = "red4"), data = pp_line1) +
  geom_ribbon(aes(x = spvar, ymin = lb_1, ymax = ub_1, fill = "red4"), alpha = 0.2, data = pp_line1) +
  scale_color_manual(name = "Sex", values = c("red4"="red4", "blue4"="blue4"), labels = c("red4"="Women","blue4"="Men")) +
  scale_fill_manual(name = "Sex", values = c("red4"="red4", "blue4"="blue4"), labels = c("red4"="Women","blue4"="Men")) +
  scale_y_continuous(name = "PP, mm Hg") + 
  scale_x_continuous(name = "Age", breaks = seq(from = 20, to = 80, by = 10)) + 
  ggtitle("Pulse Pressure") +
  theme_bw() +
  theme(axis.title = element_text(color = "#434443",size =15,face="bold"),
        axis.text = element_text(color = "#434443",size =12,face="bold"),
        title = element_text(color = "#434443",size =16,face="bold"),
        legend.text = element_text(color = "#434443",size =10,face="bold"))

pp_y0<- pp_line0[order(pp_line0$spvar),][1,"pred_0"] 
pp_y1<- pp_line1[order(pp_line1$spvar),][1,"pred_1"]

ggplot() +
  geom_line(aes(x = spvar, y = pred_0 - pp_y0, color = "blue4"), data = pp_line0) +
  geom_ribbon(aes(x = spvar, ymin = lb_0 - pp_y0, ymax = ub_0 - pp_y0, fill = "blue4"), alpha = 0.2, data = pp_line0) +
  geom_line(aes(x = spvar, y = pred_1 - pp_y1, color = "red4"), data = pp_line1) +
  geom_ribbon(aes(x = spvar, ymin = lb_1 - pp_y1, ymax = ub_1 - pp_y1, fill = "red4"), alpha = 0.2, data = pp_line1) +
  scale_color_manual(name = "Sex", values = c("red4"="red4", "blue4"="blue4"), labels = c("red4"="Women","blue4"="Men")) +
  scale_fill_manual(name = "Sex", values = c("red4"="red4", "blue4"="blue4"), labels = c("red4"="Women","blue4"="Men")) +
  scale_y_continuous(name = "PP Elevation, mm Hg") + 
  scale_x_continuous(name = "Age", breaks = seq(from = 20, to = 80, by = 10)) + 
  ggtitle("PP Elevation from Baseline") +
  theme_bw() +
  theme(axis.title = element_text(color = "#434443",size =15,face="bold"),
        axis.text = element_text(color = "#434443",size =12,face="bold"),
        title = element_text(color = "#434443",size =16,face="bold"),
        legend.text = element_text(color = "#434443",size =10,face="bold"))




cvd1 <- ggplot() +
  coord_cartesian(ylim = c(0, 700)) +

  geom_col(aes(x = category+1.5, y = rate, fill = "blue4"), position = "dodge", width=3, alpha = 0.2, data = bslage_m) +
  geom_col(aes(x = category-1.5, y = rate, fill = "red4"), position = "dodge", width=3, alpha = 0.2, data = bslage_f) +

  scale_color_manual(name = "Sex", values = c("red4"="red4", "blue4"="blue4"), labels = c("red4"="Women","blue4"="Men")) +
  scale_fill_manual(name = "Sex", values = c("red4"="red4", "blue4"="blue4"), labels = c("red4"="Women","blue4"="Men")) +
  scale_y_continuous(expand = c(0,0), name = "Event Rate / 1000", position = "left") +
  scale_x_continuous(name = "Age", breaks = seq(from = 20, to = 80, by = 10)) + 
  # ggtitle("Hard CVD Events") +
  theme_bw() +
  theme(axis.title.y.left = element_text(color = "#434443",size =15,face="bold"),
        axis.title.y.right = element_text(color = "#434443",size =12,face="bold"),
        axis.text.y.left = element_text(color = "#434443",size =12,face="bold"),
        axis.text.y.right = element_text(color = "#434443",size =8,face="bold"),
        title = element_text(color = "#434443",size =16,face="bold"),
        legend.text = element_text(color = "#434443",size =10,face="bold"))


cvd2 <- ggplot() +
  coord_cartesian(ylim = c(0, 60)) +
  
  geom_col(aes(x = category+1.5, y = rate, fill = "blue4"), position = "dodge", width=3, alpha = 0.2, data = bslage_m2) +
  geom_col(aes(x = category-1.5, y = rate, fill = "red4"), position = "dodge", width=3, alpha = 0.2, data = bslage_f2) +
  
  scale_color_manual(name = "Sex", values = c("red4"="red4", "blue4"="blue4"), labels = c("red4"="Women","blue4"="Men")) +
  scale_fill_manual(name = "Sex", values = c("red4"="red4", "blue4"="blue4"), labels = c("red4"="Women","blue4"="Men")) +
  scale_y_continuous(expand = c(0,0), name = "Fold Change in\nEvent Rate / 1000", position = "left",
                     breaks = c(1, seq(10,70,10))) +
  scale_x_continuous(name = "Age", breaks = seq(from = 20, to = 80, by = 10)) + 
  # ggtitle("Hard CVD Events") +
  theme_bw() +
  theme(axis.title.y.left = element_text(color = "#434443",size =15,face="bold"),
        axis.title.y.right = element_text(color = "#434443",size =12,face="bold"),
        axis.text.y.left = element_text(color = "#434443",size =12,face="bold"),
        axis.text.y.right = element_text(color = "#434443",size =8,face="bold"),
        title = element_text(color = "#434443",size =16,face="bold"),
        legend.text = element_text(color = "#434443",size =10,face="bold")) +
  annotate("text", x = 25, y = 5, 
           label = "Ref", 
           size=4, color="#434443", fontface="bold")


library(gridExtra)

par(mai=c(0,0,0,0), xaxs="i", yaxs="i")

grid.arrange(
  cvd1,
  cvd2,
  nrow = 2,
  widths = c(1)
)

library(gtable)
library(grid)
g2 <- ggplotGrob(cvd1)
g3 <- ggplotGrob(cvd2)
g <- rbind(g2, g3, size = "first")
g$widths <- unit.pmax(g2$widths, g3$widths)
grid.newpage()
grid.draw(g)
