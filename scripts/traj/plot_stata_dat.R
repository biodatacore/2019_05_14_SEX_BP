
library(haven)
library(dplyr)
library(magrittr)
library(ggplot2)

# SBP

sbpplot_data <- read.csv("sbp_spline.csv")

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



# DBP

dbpplot_data <- read.csv("dbp_spline.csv")

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




