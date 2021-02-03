

# meta-analysis ------
library(metafor)
library(meta)
tab_sum <- read.csv("output/tab_sum.csv")
tab_sum_cohort <- read.csv("output/tab_sum_cohort.csv")

mega <- rbind(tab_sum_cohort,tab_sum) %>%
  mutate(Sex = ifelse(X == "1","Females","Males"),
         conf = as.character(conf),
         term = ifelse(term =="RANDASSIGN","RCTs","Cohorts")#,
         # term = paste(term, ", ", Sex, sep = "")
  )
mega

meta1 <- metagen(coef,
                 `robust.se`,
                 byvar = Sex,
                 sm = "HR",
                 studlab = term,
                 data = mega)

setEPS()
postscript("output/forest.eps", width = 15, height = 6)
meta1 %>% forest(overall=F,
                 leftlabs = c("Study","Estimate","S.E."),
                 leftcols = c("studlab","TE","seTE"),
                 rightlabs = c("HR", "95% CI","Weight\n(fixed)", " Weight \n(random)"),
                 rightcols = c("effect", "ci","w.fixed", "w.random"),
                 test.subgroup.random=TRUE,
                 test.subgroup.fixed=TRUE,
                 label.test.subgroup.fixed="Test for subgroup differences (fixed effect): ",
                 label.test.subgroup.random="Test for subgroup differences (random effect): ",
                 print.Q.subgroup=T,
                 overall.hetstat = F,
                 colgap = grid::unit(7, "mm"),
                 colgap.left = grid::unit(20, "mm"),
                 col.i=c("#a42f3e","#003366","#a42f3e","#003366"),
                 col.square=c("#a42f3e","#003366","#a42f3e","#003366"))
dev.off()


lab1 <- c("",
          "Females",
          "Cohorts",
          "RCTs",
          "Fixed effects model",
          "Random effects model", 
          "",
          "Males",
          "Cohorts",
          "RCTs",
          "Fixed effects model",
          "Random effects model",
          "","","")

lab1_exp <- c(as.expression(bquote("Heterogeneity: " ~ italic("I")^2 ~ "=" ~ .(sprintf("%1.0f%%", 100*meta1$I2.w[1])) ~ ", " ~ "tau"^2 ~ "=" ~ .(signif(meta1$tau2.w[1],2)) ~ ", " ~ italic("P") ~ "=" ~ .(signum(meta1$pval.Q.w[1])))),
              as.expression(bquote("Heterogeneity: " ~ italic("I")^2 ~ "=" ~ .(sprintf("%1.0f%%", 100*meta1$I2.w[2])) ~ ", " ~ "tau"^2 ~ "=" ~ .(signif(meta1$tau2.w[2],2)) ~ ", " ~ italic("P") ~ "=" ~ .(signum(meta1$pval.Q.w[2])))),
              as.expression(bquote("Test for sex difference (fixed effect): " ~ "X"^2 ~ "=" ~ .(signif(meta1$Q.b.fixed,3)) ~ ", " ~ italic("P") ~ "=" ~ .(signum(meta1$pval.Q.b.fixed[1])))),
              as.expression(bquote("Test for sex difference (random effect): " ~ "X"^2 ~ "=" ~ .(signif(meta1$Q.b.random,3)) ~ ", " ~ italic("P") ~ "=" ~ .(signum(meta1$pval.Q.b.random[1])))))

lab2 <- c("HR (95% CI)",
          "",
          mega$conf[c(1,3)],
          paste(sprintf("%.02f",exp(meta1$TE.fixed.w[1]))," (",sprintf("%.02f",exp(meta1$TE.fixed.w[1]-1.96*meta1$seTE.fixed.w[1])),",",sprintf("%.02f",exp(meta1$TE.fixed.w[1]+1.96*meta1$seTE.fixed.w[1])),")",sep = ""),
          paste(sprintf("%.02f",exp(meta1$TE.random.w[1]))," (",sprintf("%.02f",exp(meta1$TE.random.w[1]-1.96*meta1$seTE.random.w[1])),",",sprintf("%.02f",exp(meta1$TE.random.w[1]+1.96*meta1$seTE.random.w[1])),")",sep = ""),
          "","",
          mega$conf[c(2,4)],
          paste(sprintf("%.02f",exp(meta1$TE.fixed.w[2]))," (",sprintf("%.02f",exp(meta1$TE.fixed.w[2]-1.96*meta1$seTE.fixed.w[2])),",",sprintf("%.02f",exp(meta1$TE.fixed.w[2]+1.96*meta1$seTE.fixed.w[2])),")",sep = ""),
          paste(sprintf("%.02f",exp(meta1$TE.random.w[2]))," (",sprintf("%.02f",exp(meta1$TE.random.w[2]-1.96*meta1$seTE.random.w[2])),",",sprintf("%.02f",exp(meta1$TE.random.w[2]+1.96*meta1$seTE.random.w[2])),")",sep = ""),
          "","","")


lab3 <- c("Weight\n(fixed)","",sprintf("%1.1f%%", 100*meta1$w.fixed[c(1,3)]/sum(meta1$w.fixed[c(1,3)])), "100%","--","","",sprintf("%1.1f%%", 100*meta1$w.fixed[c(2,4)]/sum(meta1$w.fixed[c(2,4)])), "100%","--","","","")


lab4 <- c("Weight\n(random)","",sprintf("%1.1f%%", 100*meta1$w.random[c(1,3)]/sum(meta1$w.random[c(1,3)])), "--","100%","","",sprintf("%1.1f%%", 100*meta1$w.random[c(2,4)]/sum(meta1$w.random[c(2,4)])),"--","100%","","","")

estimate <- c(NA,NA,
              mega$coef[c(1,3)],
              meta1$TE.fixed.w[1],
              meta1$TE.random.w[1],
              NA,NA,
              mega$coef[c(2,4)],
              meta1$TE.fixed.w[2],
              meta1$TE.random.w[2],
              NA,NA,NA)

est_low <- c(NA,NA,
             mega$coef[c(1,3)] - 1.96*mega$robust.se[c(1,3)],
             meta1$TE.fixed.w[1] - 1.96*meta1$seTE.fixed.w[1],
             meta1$TE.random.w[1] - 1.96*meta1$seTE.random.w[1],
             NA,NA,
             mega$coef[c(2,4)] - 1.96*mega$robust.se[c(2,4)],
             meta1$TE.fixed.w[2] - 1.96*meta1$seTE.fixed.w[2],
             meta1$TE.random.w[2] - 1.96*meta1$seTE.random.w[2],
             NA,NA,NA)

est_up <- c(NA,NA,
            mega$coef[c(1,3)] + 1.96*mega$robust.se[c(1,3)],
            meta1$TE.fixed.w[1] + 1.96*meta1$seTE.fixed.w[1],
            meta1$TE.random.w[1] + 1.96*meta1$seTE.random.w[1],
            NA,NA,
            mega$coef[c(2,4)] + 1.96*mega$robust.se[c(2,4)],
            meta1$TE.fixed.w[2] + 1.96*meta1$seTE.fixed.w[2],
            meta1$TE.random.w[2] + 1.96*meta1$seTE.random.w[2],
            NA,NA,NA)

ypos <- c(16:10,8:1)
tsize1 <- 3

est_tab <- 
  tibble(term = c(NA,NA,"female","female","sum_f","sum_f",
                  NA,NA,"male","male","sum_m","sum_m",
                  NA,NA,NA),
         est = estimate,
         est_low = est_low,
         est_up = est_up,
         n = ypos)

est_tab_line <- est_tab %>% 
  filter(term == "female" | term == "male")

est_tab_seg <- est_tab %>% 
  filter(term == "sum_f" | term == "sum_m")

tab1_sum <- rbind(est_tab_seg %>% mutate(x = exp(est), y = n + 0.2),
                  est_tab_seg %>% mutate(x = exp(est_up), y = n),
                  est_tab_seg %>% mutate(x = exp(est), y = n - 0.2),
                  est_tab_seg %>% mutate(x = exp(est_low), y = n))

xpos1 <- 0.065
xpos2 <- 1.7
xpos3 <- 4
xpos4 <- 6

pointsize <- c(meta1$w.fixed[c(1,3)]/sum(meta1$w.fixed[c(1,3)]),meta1$w.fixed[c(2,4)]/sum(meta1$w.fixed[c(2,4)]))

forest_summary <- ggplot() +
  coord_cartesian(xlim = c(0.065,7), clip="off", ylim = c(1,16)) +
  geom_segment(aes(x = 1, xend = 1, y = 0, yend = 15), size = 0.2, show.legend = F) +
  geom_point(data = est_tab_line, aes(x = exp(est), y = n, color = factor(term)), size = pointsize^0.5*7, shape = 15) +
  geom_linerange(data = est_tab_line, aes(xmax = exp(est_up), xmin = exp(est_low), y = n, color = factor(term)), size = 1.0, show.legend = F) +
  geom_polygon(data = tab1_sum, aes(x = x, y = y, group = factor(n), fill = factor(term)), show.legend = F) +
  geom_text(data = tibble(lab = lab4, n = ypos), 
            aes(label = lab, y = n, x = xpos4), 
            hjust = 0, 
            fontface = c("bold", "plain", rep("plain",5), "plain", rep("plain",7)), 
            size = c(tsize1,tsize1,rep(tsize1,5),tsize1,rep(tsize1,7)), 
            colour = 'black') + 
  geom_text(data = tibble(lab = lab3, n = ypos), 
            aes(label = lab, y = n, x = xpos3), 
            hjust = 0, 
            fontface = c("bold", "plain", rep("plain",5), "plain", rep("plain",7)), 
            size = c(tsize1,tsize1,rep(tsize1,5),tsize1,rep(tsize1,7)), 
            colour = 'black') + 
  geom_text(data = tibble(lab = lab2, n = ypos), 
            aes(label = lab, y = n, x = xpos2), 
            hjust = 0, 
            fontface = c("bold", "plain", rep("plain",5), "plain", rep("plain",7)), 
            size = c(tsize1,tsize1,rep(tsize1,5),tsize1,rep(tsize1,7)), 
            colour = 'black') + 
  geom_text(data = tibble(lab = lab1, n = ypos), 
            aes(label = lab, y = n, x = xpos1), 
            hjust = 0, 
            fontface = c("bold", "italic", rep("bold",5), "italic", rep("bold",7)), 
            size = c(tsize1,tsize1,rep(tsize1,5),tsize1,rep(tsize1,7)), 
            colour = 'black') + 
  annotate('text', x =  xpos1, y = ypos[7], 
           label = lab1_exp[1], parse = TRUE,size=tsize1,hjust = 0) +
  annotate('text', x =  xpos1, y = ypos[13], 
           label = lab1_exp[2], parse = TRUE,size=tsize1,hjust = 0) +
  annotate('text', x =  xpos1, y = ypos[14], 
           label = lab1_exp[3], parse = TRUE,size=tsize1,hjust = 0) +
  annotate('text', x =  xpos1, y = ypos[15], 
           label = lab1_exp[4], parse = TRUE,size=tsize1,hjust = 0) +
  scale_x_continuous(name = "\nHazard Ratio for Incident Coronary Heart Disease", trans = "log", breaks = c(0.6,0.8,1,1.2,1.4)) +
  scale_y_continuous(name = "", breaks = NULL) +
  # scale_fill_manual(values = c("female"="#a42f3e", "male"="#003366","sum_f"="#a42f3e", "sum_m"="#003366")) +
  scale_fill_manual(values = c("female"="#e1743c", "male"="#3399FF","sum_f"="#e1743c", "sum_m"="#3399FF")) +
  # scale_color_manual(name = "Sex", values = c("female"="#a42f3e", "male"="#003366"), labels = c("female" = "Females","male" = "Males")) +
  scale_color_manual(name = "Sex", values = c("female"="#e1743c", "male"="#3399FF"), labels = c("female" = "Females","male" = "Males")) +
  theme_bw() + # use a white background
  theme(
    axis.line.x = element_line(colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.title.x = element_text(colour = "black", size = 10,hjust = 0.55,face="bold"),
    axis.title.y =  element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(colour="black",size= 10,angle = 0,vjust=0.55,face="bold"),
    plot.title = element_text(colour = "black", size = 12,face="bold",hjust = 0.55),
    axis.ticks.y = element_blank(),
    legend.text = element_text(colour = "black", size = 10,face="bold"),
    legend.title = element_text(colour = "black", size = 10,face="bold"),
    legend.position = "top",
    plot.margin = unit(c(1,1,1,1), "lines")
  ) + 
  guides(#fill = F, 
    # color = F,
    shape = FALSE,
    color = guide_legend(ncol = 2,
                         override.aes = list(size=5)))
forest_summary
setEPS()
postscript("output/forest_summary.eps", width = 10, height = 4.5)
forest_summary
dev.off()

COMB <-mddata %>%
  dplyr::select(AGE, id, SEX) %>%
  rbind(comb_sprint_accord %>% dplyr::select(AGE, id, SEX)) 

COMB$SEX %>% summary()
COMB$AGE %>% summary()
COMB$AGE %>% sd()



