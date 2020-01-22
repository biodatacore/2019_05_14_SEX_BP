# Forestplot
varnum <- 11

datatab1 <- savetab_mi %>% mutate(low_m = exp(coef_m - 1.96*se_m),
                                  up_m = exp(coef_m + 1.96*se_m),
                                  mean_m = exp(coef_m),
                                  low_f = exp(coef_f - 1.96*se_f),
                                  up_f = exp(coef_f + 1.96*se_f),
                                  mean_f = exp(coef_f),
                                  n = c(varnum:1),
                                  color_f = ifelse(p_f<0.05, "red4","red2"),
                                  color_m = ifelse(p_m<0.05, "blue4","blue2"))
datatab2 <- savetab_chf %>% mutate(low_m = exp(coef_m - 1.96*se_m) + 1,
                                   up_m = exp(coef_m + 1.96*se_m) + 1,
                                   mean_m = exp(coef_m) + 1,
                                   low_f = exp(coef_f - 1.96*se_f) + 1,
                                   up_f = exp(coef_f + 1.96*se_f) + 1,
                                   mean_f = exp(coef_f) + 1,
                                   n = c(varnum:1),
                                   color_f = ifelse(p_f<0.05, "red4","red2"),
                                   color_m = ifelse(p_m<0.05, "blue4","blue2"))
datatab3 <- savetab_strk %>% mutate(low_m = exp(coef_m - 1.96*se_m) + 2,
                                    up_m = exp(coef_m + 1.96*se_m) + 2,
                                    mean_m = exp(coef_m) + 2,
                                    low_f = exp(coef_f - 1.96*se_f) + 2,
                                    up_f = exp(coef_f + 1.96*se_f) + 2,
                                    mean_f = exp(coef_f) + 2,
                                    n = c(varnum:1),
                                    color_f = ifelse(p_f<0.05, "red4","red2"),
                                    color_m = ifelse(p_m<0.05, "blue4","blue2"))

datatab_burden <- savetab %>% mutate(low_m = exp(coef_m - 1.96*se_m),
                                     up_m = exp(coef_m + 1.96*se_m),
                                     mean_m = exp(coef_m),
                                     low_f = exp(coef_f - 1.96*se_f),
                                     up_f = exp(coef_f + 1.96*se_f),
                                     mean_f = exp(coef_f),
                                     n = c(varnum:1),
                                     color_f = ifelse(p_f<0.05, "red4","red2"),
                                     color_m = ifelse(p_m<0.05, "blue4","blue2"))




ff1 <- c('bold',
         rep('plain', varnum))

lab1 <- c('Per S.D. Increase in SBP burden',
          paste("SBP >",seq(100,150,5),"mmHg Burden"))

tsize1 <- 4

lab2 <- c('Presence vs. Absence of SBP burden',
          paste("SBP >",seq(100,150,5),"mmHg Burden"))


textdata1 <- data.frame(ff1 = ff1, 
                        tsize1 = tsize1, 
                        lab1 = lab1,
                        lab2 = lab2) %>% 
  mutate(n = c(varnum+1,varnum:1))

onsetplot<-ggplot() +
  geom_errorbarh(data = datatab1, aes(xmin = low_m, xmax = up_m, y = n - 0.15, color = color_m), height = 0, size = 1.0, show.legend = FALSE) +
  geom_errorbarh(data = datatab1, aes(xmin = low_f, xmax = up_f, y = n + 0.15, color = color_f),  height = 0, size = 1.0, show.legend = FALSE) +
  geom_point(data = datatab1, aes(x = mean_m, y = n-0.15, color = color_m), size = 4, shape = 15) +
  geom_point(data = datatab1, aes(x = mean_f, y = n+0.15, color = color_f), size = 4, shape = 15) +
  
  geom_errorbarh(data = datatab2, aes(xmin = low_m, xmax = up_m, y = n - 0.15, color = color_m), height = 0, size = 1.0, show.legend = T) +
  geom_errorbarh(data = datatab2, aes(xmin = low_f, xmax = up_f, y = n + 0.15, color = color_f),  height = 0, size = 1.0, show.legend = T) +
  geom_point(data = datatab2, aes(x = mean_m, y = n-0.15, color = color_m), size = 4, shape = 15) +
  geom_point(data = datatab2, aes(x = mean_f, y = n+0.15, color = color_f), size = 4, shape = 15) +
  
  geom_errorbarh(data = datatab3, aes(xmin = low_m, xmax = up_m, y = n - 0.15, color = color_m), height = 0, size = 1.0, show.legend = FALSE) +
  geom_errorbarh(data = datatab3, aes(xmin = low_f, xmax = up_f, y = n + 0.15, color = color_f),  height = 0, size = 1.0, show.legend = FALSE) +
  geom_point(data = datatab3, aes(x = mean_m, y = n-0.15, color = color_m), size = 4, shape = 15) +
  geom_point(data = datatab3, aes(x = mean_f, y = n+0.15, color = color_f), size = 4, shape = 15) +
  
  geom_text(data = textdata1, aes(label = lab1, y = n, x = 0.3, fontface = ff1), size = tsize1, hjust = c(0.8, rep(1, nrow(textdata1)-1)), colour = 'black') +
  # geom_text(data = textdata1, aes(label = lab2, y = n, x = 5, fontface = ff1), size = tsize1, hjust = c(0.9, rep(1, nrow(textdata1)-1)), colour = 'black') +
  geom_vline(xintercept = seq(1,3,1), size = 1, linetype = 2) +
  geom_hline(yintercept = seq(1.5, varnum+1.5,1), size = 0.5, alpha = 0.3, linetype = 1) +
  ggtitle("Myocardial Infarction                   Heart Failure                             Stroke")+
  scale_x_continuous(name = "\n <- Earlier Onset       Odds Ratio (95% CI)      Later Onset ->",
                     limits = c(-0.5, 3.5),
                     breaks = c(0.75,1,1.25,1.75,2,2.25,2.75,3,3.25),
                     labels = c(0.75,1,1.25,0.75,1,1.25,0.75,1,1.25)) + #c(10^(-log10(xrange))
  scale_color_manual(name = "",
                     values = c("blue4" = "#2166ac", "red4" = "#b2182b", "blue2" = "#d1e5f0", "red2"="#ffcccc"),
                     labels = c("blue4" = "Sig. in Men", "red4" = "Sig. in Women", "blue2" = "Non-Sig. in Men", "red2" = "Non-Sig. in Women")) +
  theme_bw() + # use a white background
  theme(
    axis.line.x = element_line(colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.title.x = element_text(colour = "#434443", size = 12,hjust = 0.65,face="bold"),
    axis.title.y =  element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(colour="#434443",size= 12,angle = 0,vjust=0.5,face="bold"),
    plot.title = element_text(colour = "#434443", size = 15,face="bold",hjust = 0.7),
    axis.ticks.y = element_blank(),
    legend.text = element_text(colour = "black", size = 12,face="bold"),
    legend.title = element_text(colour = "black", size = 12,face="bold"),
    legend.position = "bottom"
  ) + guides(fill = F, 
             # color = F, 
             shape = FALSE) 
onsetplot

burdenplot<-ggplot() +
  geom_errorbarh(data = datatab_burden, aes(xmin = low_m, xmax = up_m, y = n - 0.15, color = color_m), height = 0, size = 1.0, show.legend = FALSE) +
  geom_errorbarh(data = datatab_burden, aes(xmin = low_f, xmax = up_f, y = n + 0.15, color = color_f),  height = 0, size = 1.0, show.legend = FALSE) +
  geom_point(data = datatab_burden, aes(x = mean_m, y = n-0.15, color = color_m), size = 4, shape = 15) +
  geom_point(data = datatab_burden, aes(x = mean_f, y = n+0.15, color = color_f), size = 4, shape = 15) +

  geom_text(data = textdata1, aes(label = lab2, y = n, x = 0.3, fontface = ff1), size = tsize1, hjust = c(0.8, rep(1, nrow(textdata1)-1)), colour = 'black') +
  # geom_text(data = textdata1, aes(label = lab2, y = n, x = 5, fontface = ff1), size = tsize1, hjust = c(0.9, rep(1, nrow(textdata1)-1)), colour = 'black') +
  geom_vline(xintercept = 1, size = 1, linetype = 2) +
  geom_hline(yintercept = seq(1.5, varnum+1.5,1), size = 0.5, alpha = 0.3, linetype = 1) +
  ggtitle("")+
  scale_x_continuous(name = "\n <- Lower Risk       Hazard Ratio (95% CI)      Higher Risk ->",
                     limits = c(-0.5, 2),
                     breaks = c(0.75,1,1.25),
                     labels = c(0.75,1,1.25)) + #c(10^(-log10(xrange))
  scale_color_manual(name = "",
                     values = c("blue4" = "#2166ac", "red4" = "#b2182b", "blue2" = "#d1e5f0", "red2"="#ffcccc"),
                     labels = c("blue4" = "Sig. in Men", "red4" = "Sig. in Women", "blue2" = "Non-Sig. in Men", "red2" = "Non-Sig. in Women")) +
  theme_bw() + # use a white background
  theme(
    axis.line.x = element_line(colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.title.x = element_text(colour = "#434443", size = 12,hjust = 0.65,face="bold"),
    axis.title.y =  element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(colour="#434443",size= 12,angle = 0,vjust=0.5,face="bold"),
    plot.title = element_text(colour = "#434443", size = 15,face="bold",hjust = 0.7),
    axis.ticks.y = element_blank(),
    legend.text = element_text(colour = "black", size = 12,face="bold"),
    legend.title = element_text(colour = "black", size = 12,face="bold"),
    legend.position = "bottom"
  ) + guides(fill = F, 
             # color = F, 
             shape = FALSE) 
burdenplot

library(gridExtra)
library(grid)
grid_arrange_shared_legend <- function(..., ncol = length(list(...)), nrow = 1, position = c("bottom", "right")) {
  plots <- list(...)
  position <- match.arg(position)
  g <- ggplotGrob(plots[[1]] + 
                    theme(legend.position = position))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  lwidth <- sum(legend$width)
  gl <- lapply(plots, function(x) x +
                 theme(legend.position = "none"))
  gl <- c(gl, ncol = ncol, nrow = nrow)
  
  combined <- switch(position,
                     "bottom" = arrangeGrob(do.call(arrangeGrob, gl), 
                                            legend,ncol = 1,
                                            heights = unit.c(unit(1, "npc") - lheight, lheight)),
                     "right" = arrangeGrob(do.call(arrangeGrob, gl),
                                           legend, ncol = 2,
                                           widths = unit.c(unit(1, "npc") - lwidth, lwidth)))
  
  grid.newpage()
  grid.draw(combined)
  
  # return gtable invisibly
  invisible(combined)
}



pdf("output/Onset.pdf", width=12, height=5)
# grid_arrange_shared_legend(SBPdif, DBPdif, MAPdif, PPdif, ncol = 1, nrow = 4)
onsetplot

dev.off()

pdf("output/burden_forest.pdf", width=12, height=5)
# grid_arrange_shared_legend(SBPdif, DBPdif, MAPdif, PPdif, ncol = 1, nrow = 4)
burdenplot

dev.off()


