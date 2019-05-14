# incremental BP from baseline


f_m<-NULL
f_m$AGE <- plot_x_min:plot_x_max
f_m$f_sbp<- predict(lm(pred_SBP ~ bs(AGE), data = comb_dat %>% filter(SEX==2)), newdata = data.frame(AGE = f_m$AGE, SEX=2))-
  predict(lm(pred_SBP ~ bs(AGE), data = comb_dat %>% filter(SEX==2)), newdata = data.frame(AGE = plot_x_min, SEX=2))
f_m$m_sbp <-predict(lm(pred_SBP ~ bs(AGE), data = comb_dat %>% filter(SEX==1)), newdata = data.frame(AGE = f_m$AGE, SEX=1))-
  predict(lm(pred_SBP ~ bs(AGE), data = comb_dat %>% filter(SEX==1)), newdata = data.frame(AGE = plot_x_min, SEX=1))
f_m$f_dbp<- predict(lm(pred_DBP ~ bs(AGE), data = comb_dat %>% filter(SEX==2)), newdata = data.frame(AGE = f_m$AGE, SEX=2))-
  predict(lm(pred_DBP ~ bs(AGE), data = comb_dat %>% filter(SEX==2)), newdata = data.frame(AGE = plot_x_min, SEX=2))
f_m$m_dbp <-predict(lm(pred_DBP ~ bs(AGE), data = comb_dat %>% filter(SEX==1)), newdata = data.frame(AGE = f_m$AGE, SEX=1))-
  predict(lm(pred_DBP ~ bs(AGE), data = comb_dat %>% filter(SEX==1)), newdata = data.frame(AGE = plot_x_min, SEX=1))
f_m$f_map<- predict(lm(pred_MAP ~ bs(AGE), data = comb_dat %>% filter(SEX==2)), newdata = data.frame(AGE = f_m$AGE, SEX=2))-
  predict(lm(pred_MAP ~ bs(AGE), data = comb_dat %>% filter(SEX==2)), newdata = data.frame(AGE = plot_x_min, SEX=2))
f_m$m_map <-predict(lm(pred_MAP ~ bs(AGE), data = comb_dat %>% filter(SEX==1)), newdata = data.frame(AGE = f_m$AGE, SEX=1))-
  predict(lm(pred_MAP ~ bs(AGE), data = comb_dat %>% filter(SEX==1)), newdata = data.frame(AGE = plot_x_min, SEX=1))
f_m$f_pp<- predict(lm(pred_PP ~ bs(AGE), data = comb_dat %>% filter(SEX==2)), newdata = data.frame(AGE = f_m$AGE, SEX=2))-
  predict(lm(pred_PP ~ bs(AGE), data = comb_dat %>% filter(SEX==2)), newdata = data.frame(AGE = plot_x_min, SEX=2))
f_m$m_pp <-predict(lm(pred_PP ~ bs(AGE), data = comb_dat %>% filter(SEX==1)), newdata = data.frame(AGE = f_m$AGE, SEX=1))-
  predict(lm(pred_PP ~ bs(AGE), data = comb_dat %>% filter(SEX==1)), newdata = data.frame(AGE = plot_x_min, SEX=1))

f_m_dif <- f_m %>% as.data.frame() %>% mutate(dif_sbp = (f_sbp-m_sbp),
                                              dif_dbp = (f_dbp-m_dbp),
                                              dif_map = (f_map-m_map),
                                              dif_pp = (f_pp-m_pp))

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}
n = 8
cols = gg_color_hue(n)


increplot<- ggplot(data = f_m_dif) +
  geom_point(aes(x = AGE, y = dif_sbp, color = "sbp")) + 
  geom_point(aes(x = AGE, y = dif_dbp, color = "dbp")) + 
  geom_point(aes(x = AGE, y = dif_map, color = "map")) + 
  geom_point(aes(x = AGE, y = dif_pp, color = "pp")) +
  scale_color_manual(name = "BP measures", 
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
  scale_x_continuous(name=c("Age"),   breaks = seq(from = 20, to = 80, by = 10)) + # change #
  scale_y_continuous(name="Increment From Baseline [F vs. M], mmHg") + 
  ggtitle("")
increplot 


# bootstrap


range <- plot_x_min:plot_x_max
boot.fun1 <- function(data, indices){
  data <-data[indices,]	
  f_m<-NULL
  f_m$AGE <- range
  f_m$f_bp<- predict(lm(pred_SBP ~ bs(AGE), data = data %>% filter(SEX==2)), newdata = data.frame(AGE = f_m$AGE+1, SEX=2))-
    predict(lm(pred_SBP ~ bs(AGE), data = data %>% filter(SEX==2)), newdata = data.frame(AGE = plot_x_min, SEX=2))
  f_m$m_bp <-predict(lm(pred_SBP ~ bs(AGE), data = data %>% filter(SEX==1)), newdata = data.frame(AGE = f_m$AGE+1, SEX=1))-
    predict(lm(pred_SBP ~ bs(AGE), data = data %>% filter(SEX==1)), newdata = data.frame(AGE = plot_x_min, SEX=1))
  f_m_dif <- f_m %>% as.data.frame() %>% mutate(dif_bp = (f_bp-m_bp))
  print(indices[[1]])
  f_m_dif$dif_bp
}

library(boot)
library(reshape2)
boot1000 <- boot(data = comb_dat, statistic = boot.fun1, R=1000)	

bootdat_1 <- NULL
bootdat_1$AGE <- range
bootdat_1$mean <- boot1000$t %>% as.data.frame() %>%sapply(mean)
bootdat_1$se <- boot1000$t %>% as.data.frame() %>%sapply(sd)
bootdat_1 %<>% as.data.frame()


ggplot() +
  geom_line(aes(x = AGE, y = mean), data = bootdat_1) +
  geom_ribbon(aes(x = AGE, ymin = mean -1.96*se, ymax = mean + 1.96*se), data = bootdat_1, alpha = 0.2)




range <- plot_x_min:plot_x_max
boot.fun2 <- function(data, indices){
  data <-data[indices,]	
  f_m<-NULL
  f_m$AGE <- range
  f_m$f_bp<- predict(lm(pred_DBP ~ bs(AGE), data = data %>% filter(SEX==2)), newdata = data.frame(AGE = f_m$AGE+1, SEX=2))-
    predict(lm(pred_DBP ~ bs(AGE), data = data %>% filter(SEX==2)), newdata = data.frame(AGE = plot_x_min, SEX=2))
  f_m$m_bp <-predict(lm(pred_DBP ~ bs(AGE), data = data %>% filter(SEX==1)), newdata = data.frame(AGE = f_m$AGE+1, SEX=1))-
    predict(lm(pred_DBP ~ bs(AGE), data = data %>% filter(SEX==1)), newdata = data.frame(AGE = plot_x_min, SEX=1))
  f_m_dif <- f_m %>% as.data.frame() %>% mutate(dif_bp = (f_bp-m_bp))
  print(indices[[1]])
  f_m_dif$dif_bp
}

library(boot)
library(reshape2)
boot1000_2 <- boot(data = comb_dat, statistic = boot.fun2, R=1000)	

bootdat_2 <- NULL
bootdat_2$AGE <- range
bootdat_2$mean <- boot1000_2$t %>% as.data.frame() %>%sapply(mean)
bootdat_2$se <- boot1000_2$t %>% as.data.frame() %>%sapply(sd)
bootdat_2 %<>% as.data.frame()


ggplot() +
  geom_line(aes(x = AGE, y = mean), data = bootdat_2) +
  geom_ribbon(aes(x = AGE, ymin = mean -1.96*se, ymax = mean + 1.96*se), data = bootdat_2, alpha = 0.2)



range <- plot_x_min:plot_x_max
boot.fun3 <- function(data, indices){
  data <-data[indices,]	
  f_m<-NULL
  f_m$AGE <- range
  f_m$f_bp<- predict(lm(pred_MAP ~ bs(AGE), data = data %>% filter(SEX==2)), newdata = data.frame(AGE = f_m$AGE+1, SEX=2))-
    predict(lm(pred_MAP ~ bs(AGE), data = data %>% filter(SEX==2)), newdata = data.frame(AGE = plot_x_min, SEX=2))
  f_m$m_bp <-predict(lm(pred_MAP ~ bs(AGE), data = data %>% filter(SEX==1)), newdata = data.frame(AGE = f_m$AGE+1, SEX=1))-
    predict(lm(pred_MAP ~ bs(AGE), data = data %>% filter(SEX==1)), newdata = data.frame(AGE = plot_x_min, SEX=1))
  f_m_dif <- f_m %>% as.data.frame() %>% mutate(dif_bp = (f_bp-m_bp))
  print(indices[[1]])
  f_m_dif$dif_bp
}

library(boot)
library(reshape2)
boot1000_3 <- boot(data = comb_dat, statistic = boot.fun3, R=1000)	

bootdat_3 <- NULL
bootdat_3$AGE <- range
bootdat_3$mean <- boot1000_3$t %>% as.data.frame() %>%sapply(mean)
bootdat_3$se <- boot1000_3$t %>% as.data.frame() %>%sapply(sd)
bootdat_3 %<>% as.data.frame()


ggplot() +
  geom_smooth(aes(x = AGE, y = mean), data = bootdat_3) +
  geom_ribbon(aes(x = AGE, ymin = mean -1.96*se, ymax = mean + 1.96*se), data = bootdat_3, alpha = 0.2)






range <- plot_x_min:plot_x_max
boot.fun4 <- function(data, indices){
  data <-data[indices,]	
  f_m<-NULL
  f_m$AGE <- range
  f_m$f_bp<- predict(lm(pred_PP ~ bs(AGE), data = data %>% filter(SEX==2)), newdata = data.frame(AGE = f_m$AGE+1, SEX=2))-
    predict(lm(pred_PP ~ bs(AGE), data = data %>% filter(SEX==2)), newdata = data.frame(AGE = plot_x_min, SEX=2))
  f_m$m_bp <-predict(lm(pred_PP ~ bs(AGE), data = data %>% filter(SEX==1)), newdata = data.frame(AGE = f_m$AGE+1, SEX=1))-
    predict(lm(pred_PP ~ bs(AGE), data = data %>% filter(SEX==1)), newdata = data.frame(AGE = plot_x_min, SEX=1))
  f_m_dif <- f_m %>% as.data.frame() %>% mutate(dif_bp = (f_bp-m_bp))
  print(indices[[1]])
  f_m_dif$dif_bp
}

library(boot)
library(reshape2)
library(plyr)
library(plotrix)

boot1000_4 <- boot(data = comb_dat, statistic = boot.fun4, R=1000)	
medconfint <- boot.ci(boot1000_4, index=1, conf=(.95), type="perc")



bootdat_4 <- NULL
bootdat_4$AGE <- range
bootdat_4$mean <- boot1000_4$t %>% as.data.frame() %>%sapply(mean)
bootdat_4$se <- boot1000_4$t %>% as.data.frame() %>%sapply(sd)
bootdat_4 %<>% as.data.frame()

ggplot() +
  geom_smooth(aes(x = AGE, y = mean), data = bootdat_4) +
  geom_ribbon(aes(x = AGE, ymin = mean -1.96*se, ymax = mean + 1.96*se), data = bootdat_4, alpha = 0.2)



ggplot() +
  geom_line(aes(x = AGE, y = mean, color = "sbp"), data = bootdat_1) + 
  geom_ribbon(aes(x = AGE, ymin = mean -1.96*se, ymax = mean + 1.96*se, fill = "sbp"), data = bootdat_1, alpha = 0.2) +
  geom_line(aes(x = AGE, y = mean, color = "dbp"), data = bootdat_2) + 
  geom_ribbon(aes(x = AGE, ymin = mean -1.96*se, ymax = mean + 1.96*se, fill = "dbp"), data = bootdat_2, alpha = 0.2) +
  geom_line(aes(x = AGE, y = mean, color = "map"), data = bootdat_3) + 
  geom_ribbon(aes(x = AGE, ymin = mean -1.96*se, ymax = mean + 1.96*se, fill = "map"), data = bootdat_3, alpha = 0.2) +
  geom_line(aes(x = AGE, y = mean, color = "pp"), data = bootdat_4) +
  geom_ribbon(aes(x = AGE, ymin = mean -1.96*se, ymax = mean + 1.96*se, fill = "pp"), data = bootdat_4, alpha = 0.2) +
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
  scale_x_continuous(name=c("Age"),   breaks = seq(from = 20, to = 80, by = 10)) + # change #
  scale_y_continuous(name="Increment From Baseline [F vs. M], mmHg") + 
  ggtitle("") +
  guides(fill = guide_legend(override.aes = list(alpha = 0.2)))


