
## make the plots
#######################################FIGURES##################################
#######################################FIGURES##################################
#######################################FIGURES##################################
#######################################FIGURES##################################
#######################################FIGURES##################################
library(tidyverse)
library(brms)
library(readr)
library(scales)
##  make new data frame
husbands_new <- read.csv("C:/Users/robert/Dropbox/Github/Intensive extensive kin networks/data/husbands_new.csv", header = T, sep = ",")

data1 <-  husbands_new %>% filter (Respondent_a=="Wife" | Respondent_a=="Husband")
data2 <-  husbands_new %>% filter (Respondent_a=="Wife")
data3 <-  husbands_new %>% filter (Respondent_a=="Husband")


## total NW size
d <- data1[c(2,28,6,8,9,25:27)] 
d <- d[complete.cases(d), ] 

###HERE!!!!

## make the top plot
Sex_seq <- rep(0:1)

#read in  the model

M1 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_sex_diffs/Results/NW_total_lognormal_intx_sex.rds")
attach(d)
newdata <- tidyr::crossing(
  gender_F0_M1_a = c(0,1),
  MI_geo_proximity=mean(MI_geo_proximity),
  MI_economic_capital=mean(MI_economic_capital),
  MI_human_capital=mean(MI_human_capital),
  Kids_a=mean(Kids_a),
  Mothers_kids_a=mean(Mothers_kids_a)) %>%
  as.data.frame()
detach(d)


mu_summary <-
  fitted(M1, 
         newdata = newdata, allow_new_levels=TRUE, probs=c(0.11,0.89)) %>%
  as_tibble() %>%
   bind_cols(Sex_seq)

mu_summary

colnames(mu_summary) <- c('Estimate','Error','Q5','Q95','Sex')

mu_summary

Data1 <- d %>% left_join (mu_summary, by =c("gender_F0_M1_a"="Sex"))

library(plotrix)
library(magrittr)
library(dplyr)
library(ggplot2)
library(ggstance)
library(rstan)
library(tidybayes)
library(emmeans)
library(broom)
library(brms)
library(modelr)
library(forcats)
library(ggdist)


cols <- c("0" = "#000000", "1" = "#0072B2") #,

show_col("#000000")
show_col("#0072B2")

plot1 <- ggplot(data=Data1, aes(x=gender_F0_M1_a, y=NW_total,group=factor(gender_F0_M1_a),
                            fill=factor(gender_F0_M1_a),
                            color=factor(gender_F0_M1_a)))+
  #geom_jitter(alpha=0.5,size=0.8,width=0.1) +
  geom_violin(alpha = 0.5) +   
  #geom_boxplot(width=.1,fill='#A4A4A4', col="darkred",position="dodge") +
  geom_errorbar(position="dodge",size=1.2,width=0.6,aes(x = gender_F0_M1_a, ymin = Q5, ymax = Q95,
                color=factor(gender_F0_M1_a)),Data1)+
  
  scale_color_manual(name = "Model Predictions",
                     values = cols,
                     breaks=c(1),
                     labels = c("95% Credibility Interval"),
                     guide = guide_legend(override.aes = list(linetype = c(1),
                                                              shape = c(NA),
                                                              color = "black")))+
  
  scale_fill_manual(name="Observed data", values=cols,
                    breaks=c(0,1),
                    labels=c("Women","Men")) +
  scale_x_continuous(
    "",limits=c(-0.5,1.5),breaks=c(0.0,1.0),labels = c("Women", "Men"))+
  scale_y_continuous(name="Total network size",
                     breaks=c(1,5,6,7,8,9,10,11,12,13,14,15,20,28),
                     limits=c(2,28),
                     labels=c("1","5","6","7","8","9","10","11","12","13","14","15",
                              "20","28")) +
  
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size=16,face="bold"),
        legend.title = element_text(size=12,face="bold"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.key.size = unit(0.15, "in"),
        legend.text = element_text(size = 12),
        axis.text.x = element_text(colour="grey20",size=10,angle=0,face="bold"),
        axis.text.y = element_text(colour="grey20",size=10,angle=0,hjust=0,vjust=0,face="bold"),  
        axis.title.x = element_text(colour="black",size=12,angle=0,hjust=.5,vjust=0,face="bold"),
        axis.title.y = element_text(colour="black",size=12,angle=90,hjust=.5,vjust=.5,face="bold"))
 
 
plot1
## save it

setwd("C:/Users/robert/Dropbox/Github/Kin_networks_sex_diffs/Figures")
png("~/graph.png") ; par(mar=c(6, 4, 4, 2) + 0.1)
ggsave(plot1, filename = "Figure 1 (Total NW size).png", width = 24, height = 24, dpi = 600,units = "cm")

### percent kin
d <- data1[c(2,38,6,8,9,25:27)] 
d <- d[complete.cases(d), ] 

#read in  the model

M1 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_sex_diffs/Results/percent_relatives_in_NW_lognormal_intx_sex.rds")
attach(d)
newdata <- tidyr::crossing(
  gender_F0_M1_a = c(0.6,1.8),
  MI_geo_proximity=mean(MI_geo_proximity),
  MI_economic_capital=mean(MI_economic_capital),
  MI_human_capital=mean(MI_human_capital),
  Kids_a=mean(Kids_a),
  Mothers_kids_a=mean(Mothers_kids_a)) %>%
  as.data.frame()
detach(d)


mu_summary <-
  fitted(M1, 
         newdata = newdata, allow_new_levels=TRUE, probs=c(0.11,0.89)) %>%
  as_tibble() %>%
  bind_cols(Sex_seq)

mu_summary


colnames(mu_summary) <- c('Estimate','Error','Q5','Q95','Sex')

mu_summary



Data1 <- d %>% left_join (mu_summary, by =c("gender_F0_M1_a"="Sex"))

library(plotrix)
library(magrittr)
library(dplyr)
library(ggplot2)
library(ggstance)
library(rstan)
library(tidybayes)
library(emmeans)
library(broom)
library(brms)
library(modelr)
library(forcats)
library(ggdist)

cols <- c("0" = "#000000", "1" = "#0072B2") #,


#percent_rels_in_NW
plot2 <- ggplot(data=Data1, aes(x=gender_F0_M1_a, y=percent_rels_in_NW,group=factor(gender_F0_M1_a),
                                fill=factor(gender_F0_M1_a),
                                color=factor(gender_F0_M1_a)))+
  #geom_jitter(alpha=0.5,size=0.8,width=0.1) +
  geom_violin(alpha = 0.5) +   
  #geom_boxplot(width=.1,fill='#A4A4A4', col="darkred",position="dodge") +
  geom_errorbar(position="dodge",size=1.2,width=0.6,aes(x = gender_F0_M1_a, ymin = Q5, ymax = Q95,
                                                        color=factor(gender_F0_M1_a)),Data1)+
  
  scale_color_manual(name = "Model Predictions",
                     values = cols,
                     breaks=c(1),
                     labels = c("95% Credibility Interval"),
                     guide = guide_legend(override.aes = list(linetype = c(1),
                                                              shape = c(NA),
                                                              color = "black")))+
  
  scale_fill_manual(name="Observed data", values=cols,
                    breaks=c(0,1),
                    labels=c("Women","Men")) +
  scale_x_continuous(
    "",limits=c(-0.5,1.5),breaks=c(0.0,1.0),labels = c("Women", "Men"))+
  scale_y_continuous(name="Percentage of relatives in Network",breaks=c(0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,1),limits=c(0.4,1.05),
                     labels=c("40%","45%","50%","55%","60%","65%","70%","75%","80%","85%","90%","95%","100%")) +
  
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size=16,face="bold"),
        legend.title = element_text(size=12,face="bold"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.key.size = unit(0.15, "in"),
        legend.text = element_text(size = 12),
        axis.text.x = element_text(colour="grey20",size=10,angle=0,face="bold"),
        axis.text.y = element_text(colour="grey20",size=10,angle=0,hjust=0,vjust=0,face="bold"),  
        axis.title.x = element_text(colour="black",size=12,angle=0,hjust=.5,vjust=0,face="bold"),
        axis.title.y = element_text(colour="black",size=12,angle=90,hjust=.5,vjust=.5,face="bold"))


plot2

setwd("C:/Users/robert/Dropbox/Github/Kin_networks_sex_diffs/Figures")
png("~/graph.png") ; par(mar=c(6, 4, 4, 2) + 0.1)
ggsave(plot2, filename = "Figure 2 (Kin density).png", width = 24, height = 24, dpi = 600,units = "cm")


### Relatives
d <- data1[c(2,37,6,8,9,25:27)] 
d <- d[complete.cases(d), ] 

#read inthe model

M1 <-readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_sex_diffs/Results/relatives_in_NW_lognormal_intx_sex.rds")
attach(d)
newdata <- tidyr::crossing(
  gender_F0_M1_a = c(0,1),
  MI_geo_proximity=mean(MI_geo_proximity),
  MI_economic_capital=mean(MI_economic_capital),
  MI_human_capital=mean(MI_human_capital),
  Kids_a=mean(Kids_a),
  Mothers_kids_a=mean(Mothers_kids_a)) %>%
  as.data.frame()
detach(d)


mu_summary <-
  fitted(M1, 
         newdata = newdata, allow_new_levels=TRUE, probs=c(0.11,0.89)) %>%
  as_tibble() %>%
  bind_cols(Sex_seq)

mu_summary

colnames(mu_summary) <- c('Estimate','Error','Q5','Q95','Sex')

mu_summary

Data1 <- d %>% left_join (mu_summary, by =c("gender_F0_M1_a"="Sex"))

library(plotrix)
library(magrittr)
library(dplyr)
library(ggplot2)
library(ggstance)
library(rstan)
library(tidybayes)
library(emmeans)
library(broom)
library(brms)
library(modelr)
library(forcats)
library(ggdist)

cols <- c("0" = "#000000", "1" = "#0072B2") #,


#rels_in_NW
plot3 <- ggplot(data=Data1, aes(x=gender_F0_M1_a, y=rels_in_NW,group=factor(gender_F0_M1_a),
                                fill=factor(gender_F0_M1_a),
                                color=factor(gender_F0_M1_a)))+
  #geom_jitter(alpha=0.5,size=0.8,width=0.1) +
  geom_violin(alpha = 0.5) +   
  #geom_boxplot(width=.1,fill='#A4A4A4', col="darkred",position="dodge") +
  geom_errorbar(position="dodge",size=1.2,width=0.6,aes(x = gender_F0_M1_a, ymin = Q5, ymax = Q95,
                                                        color=factor(gender_F0_M1_a)),Data1)+
  
  scale_color_manual(name = "Model Predictions",
                     values = cols,
                     breaks=c(1),
                     labels = c("95% Credibility Interval"),
                     guide = guide_legend(override.aes = list(linetype = c(1),
                                                              shape = c(NA),
                                                              color = "black")))+
  
  scale_fill_manual(name="Observed data", values=cols,
                    breaks=c(0,1),
                    labels=c("Women","Men")) +
  scale_x_continuous(
    "",limits=c(-0.5,1.5),breaks=c(0.0,1.0),labels = c("Women", "Men"))+
  scale_y_continuous(name="Number of Relatives in Network",breaks=c(1,5,6,7,8,9,10,15),limits=c(0,16),
                     labels=c("1","5","6","7","8","9","10","15")) +
  
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size=16,face="bold"),
        legend.title = element_text(size=12,face="bold"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.key.size = unit(0.15, "in"),
        legend.text = element_text(size = 12),
        axis.text.x = element_text(colour="grey20",size=10,angle=0,face="bold"),
        axis.text.y = element_text(colour="grey20",size=10,angle=0,hjust=0,vjust=0,face="bold"),  
        axis.title.x = element_text(colour="black",size=12,angle=0,hjust=.5,vjust=0,face="bold"),
        axis.title.y = element_text(colour="black",size=12,angle=90,hjust=.5,vjust=.5,face="bold"))


plot3

setwd("C:/Users/robert/Dropbox/Github/Kin_networks_sex_diffs/Figures")
png("~/graph.png") ; par(mar=c(6, 4, 4, 2) + 0.1)
ggsave(plot3, filename = "Figure 3 (Number of relatives).png", width = 24, height = 24, dpi = 600,units = "cm")


#### Non-relatives
d <- data1[c(2,29,6,8,9,25:27)] 
d <- d[complete.cases(d), ] 

#read in  the model

M1 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_sex_diffs/Results/non_relatives_in_NW_neg_bin_intx_sex.rds")
attach(d)
newdata <- tidyr::crossing(
  gender_F0_M1_a = c(0,1),
  MI_geo_proximity=mean(MI_geo_proximity),
  MI_economic_capital=mean(MI_economic_capital),
  MI_human_capital=mean(MI_human_capital),
  Kids_a=mean(Kids_a),
  Mothers_kids_a=mean(Mothers_kids_a)) %>%
  as.data.frame()
detach(d)


mu_summary <-
  fitted(M1, 
         newdata = newdata, allow_new_levels=TRUE, probs=c(0.11,0.89)) %>%
  as_tibble() %>%
  bind_cols(Sex_seq)

mu_summary

colnames(mu_summary) <- c('Estimate','Error','Q5','Q95','Sex')

mu_summary

Data1 <- d %>% left_join (mu_summary, by =c("gender_F0_M1_a"="Sex"))

library(plotrix)
library(magrittr)
library(dplyr)
library(ggplot2)
library(ggstance)
library(rstan)
library(tidybayes)
library(emmeans)
library(broom)
library(brms)
library(modelr)
library(forcats)
library(ggdist)

cols <- c("0" = "#000000", "1" = "#0072B2") #,


#non_rels_in_NW
plot4 <- ggplot(data=Data1, aes(x=gender_F0_M1_a, y=non_rels,
                                group=factor(gender_F0_M1_a),
                                fill=factor(gender_F0_M1_a),
                                color=factor(gender_F0_M1_a)))+

  geom_violin(alpha = 0.5) +   
 
  geom_errorbar(position="dodge",size=1.2,width=0.6,aes(x = gender_F0_M1_a, ymin = Q5, ymax = Q95,
                                                        color=factor(gender_F0_M1_a)),Data1)+
  
  scale_color_manual(name = "Model Predictions",
                     values = cols,
                     breaks=c(1),
                     labels = c("95% Credibility Interval"),
                     guide = guide_legend(override.aes = list(linetype = c(1),
                                                              shape = c(NA),
                                                              color = "black")))+
  
  scale_fill_manual(name="Observed data", values=cols,
                    breaks=c(0,1),
                    labels=c("Women","Men")) +
  scale_x_continuous(
    "",limits=c(-0.5,1.5),breaks=c(0.0,1.0),labels = c("Women", "Men"))+
  scale_y_continuous(name="Number of Non-relatives in Network",
                     breaks=c(1,2,3,4,5,6,7,8,9),limits=c(0,10),
                     labels=c("1","2","3","4","5","6","7","8","9")) +
  
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size=16,face="bold"),
        legend.title = element_text(size=12,face="bold"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.key.size = unit(0.15, "in"),
        legend.text = element_text(size = 12),
        axis.text.x = element_text(colour="grey20",size=10,angle=0,face="bold"),
        axis.text.y = element_text(colour="grey20",size=10,angle=0,hjust=0,vjust=0,face="bold"),  
        axis.title.x = element_text(colour="black",size=12,angle=0,hjust=.5,vjust=0,face="bold"),
        axis.title.y = element_text(colour="black",size=12,angle=90,hjust=.5,vjust=.5,face="bold"))


plot(plot4)

setwd("C:/Users/robert/Dropbox/Github/Kin_networks_sex_diffs/Figures")
png("~/graph.png") ; par(mar=c(6, 4, 4, 2) + 0.1)
ggsave(plot4, filename = "Figure 4 (Number of non-relatives).png", width = 24, height = 24, dpi = 600,units = "cm")



#### Some aspects of market integration, particularly education and wealth, 
#increase the number of non-relatives in the networks of both men and women, 
#increase the kin density of female networks, 
##but decrease the kin density of male networks.


### Do education first for non_relatives
### then wealth for non_relatives
#### Non-relatives
library(tidyverse)
library(brms)
library(readr)
library(scales)
##  make new data frame
husbands_new <- read.csv("C:/Users/robert/Dropbox/Github/Intensive extensive kin networks/data/husbands_new.csv", header = T, sep = ",")

data1 <-  husbands_new %>% filter (Respondent_a=="Wife" | Respondent_a=="Husband")
data2 <-  husbands_new %>% filter (Respondent_a=="Wife")
data3 <-  husbands_new %>% filter (Respondent_a=="Husband")
d <- data1[c(2,29,6,8,9,25:27)] 
d <- d[complete.cases(d), ] 
d<-d %>% mutate(rank=ntile(d$MI_human_capital,10))
library(ggthemes)

## make the top plot
Sex_seq <- rep(0:1, each=10)
MI_seq <- rep(1:10,2)
#read in  the model

M1 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_sex_diffs/Results/non_relatives_in_NW_neg_bin_intx_sex.rds")
attach(d)
newdata <- tidyr::crossing(
  gender_F0_M1_a = c(0,1),
  MI_geo_proximity=mean(MI_geo_proximity),
  MI_economic_capital=mean(MI_economic_capital),
  MI_human_capital=c(-2.0,-1.5,-1,-0.5,0,0.5,1.0,1.5,2.0,2.5),
  Kids_a=mean(Kids_a),
  Mothers_kids_a=mean(Mothers_kids_a)) %>%
  as.data.frame()
detach(d)


mu_summary <-
  fitted(M1, 
         newdata = newdata, allow_new_levels=TRUE, probs=c(0.11,0.89)) %>%
  as_tibble() %>%
  bind_cols(Sex_seq) %>%
  bind_cols(MI_seq)

mu_summary

colnames(mu_summary) <- c('Estimate','Error','Q5','Q95','Sex',"Education")

mu_summary

Data1 <- d %>% left_join (mu_summary, by =c("gender_F0_M1_a"="Sex","rank"="Education"))

library(plotrix)
library(magrittr)
library(dplyr)
library(ggplot2)
library(ggstance)
library(rstan)
library(tidybayes)
library(emmeans)
library(broom)
library(brms)
library(modelr)
library(forcats)
library(ggdist)


cols <- c("0" = "#000000", "1" = "#0072B2", "2" = "#000000", "3" = "#0072B2") #,

show_col("#000000")
show_col("#0072B2")
# relabel sex from facet grid
new <- c("Men", "Women")
names(new) <- c(1,0)


plot1<- ggplot(Data1, aes(x=rank, y=Estimate,group=factor(gender_F0_M1_a),
                          fill=factor(gender_F0_M1_a),
                          color=factor(gender_F0_M1_a))) +
  
  geom_ribbon(aes(ymin = Q5, ymax = Q95,alpha=0.7)) +
  
  geom_line() +
  
  geom_jitter(data=Data1,shape=1,size=0.9,width=0.6,aes(group=factor(gender_F0_M1_a),colour=factor(gender_F0_M1_a),
                                                        x = rank, y = non_rels)) +
  
  facet_wrap(~gender_F0_M1_a, ncol = 2, labeller=labeller(gender_F0_M1_a=new))+
  
  
  # scale_color_manual(name=NULL, breaks=c(0,1),values=cols,
  #                    labels=c("Predicted (95% CI)","Observed")) +
  
  scale_color_manual(name = NULL,
                     values = cols,
                     breaks=c(1,0),
                     labels = c("Predicted (95% CI)", "Observed"),
                     guide = guide_legend(override.aes = list(linetype = c(1, 0),
                                                              shape = c(NA, 1),
                                                              color = "grey")))+
  
  scale_fill_manual(name="", breaks=c(0),values=cols,
                    labels=c("Predicted (95% CI)")) +
  scale_x_continuous(name="Human Capital",limits=c(0.0,10.0),breaks=c(2.5,7.5),
                     labels=c("Low","High")) +
  scale_y_continuous(name="Number of Non-relatives in Network",breaks=c(1,2,3,4,5,6,7,8,9,10),limits=c(0,12),
                     labels=c("1","2","3","4","5","6","7","8","9","10")) +
  
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size=16,face="bold"),
        legend.title = element_text(size=12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.key.size = unit(0.15, "in"),
        legend.text = element_text(size = 12, face = "bold"),
        strip.text.x = element_text(
          size = 12, color = "black", face = "bold"),
        strip.text.y = element_text(
          size = 12, color = "black", face = "bold"),
        
        axis.text.x = element_text(colour="grey20",size=10,angle=0,face="bold"),
        axis.text.y = element_text(colour="grey20",size=10,angle=0,hjust=0,vjust=0,face="bold"),  
        axis.title.x = element_text(colour="black",size=12,angle=0,hjust=.5,vjust=0,face="bold"),
        axis.title.y = element_text(colour="black",size=12,angle=90,hjust=.5,vjust=.5,face="bold"))+
  
  guides(alpha="none")+
  guides(fill="none")
plot1


library(ggthemes)

## make the bottom plot
d <- data1[c(2,29,6,8,9,25:27)] 
d <- d[complete.cases(d), ] 
d<-d %>% mutate(rank=ntile(d$MI_economic_capital,10))
library(ggthemes)

## make the top plot
Sex_seq <- rep(0:1, each=10)
MI_seq <- rep(1:10,2)
#read in  the model

M1 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_sex_diffs/Results/non_relatives_in_NW_neg_bin_intx_sex.rds")
attach(d)
newdata <- tidyr::crossing(
  gender_F0_M1_a = c(0,1),
  MI_geo_proximity=mean(MI_geo_proximity),
  MI_human_capital=mean(MI_human_capital),
  MI_economic_capital=c(-2.0,-1,0,1,2,3,4,5,6,7),
  Kids_a=mean(Kids_a),
  Mothers_kids_a=mean(Mothers_kids_a)) %>%
  as.data.frame()
detach(d)


mu_summary <-
  fitted(M1, 
         newdata = newdata, allow_new_levels=TRUE, probs=c(0.11,0.89)) %>%
  as_tibble() %>%
  bind_cols(Sex_seq) %>%
  bind_cols(MI_seq)

mu_summary

colnames(mu_summary) <- c('Estimate','Error','Q5','Q95','Sex',"Wealth")

mu_summary


Data2 <- d %>% left_join (mu_summary, by =c("rank"="Wealth","gender_F0_M1_a"="Sex"))

library(plotrix)
library(magrittr)
library(dplyr)
library(ggplot2)
library(ggstance)
library(rstan)
library(tidybayes)
library(emmeans)
library(broom)
library(brms)
library(modelr)
library(forcats)
library(ggdist)
#0072B2
cols <- c("0" = "#000000", "1" = "#0072B2", "2" = "#000000", "3" = "#0072B2")
plot1a <-  ggplot(Data2, aes(x=rank, y=Estimate,group=factor(gender_F0_M1_a),
                             fill=factor(gender_F0_M1_a),
                             color=factor(gender_F0_M1_a))) +
  
  geom_ribbon(aes(ymin = Q5, ymax = Q95,alpha=0.7)) +
  
  geom_line() +
  
  geom_jitter(data=Data1,shape=1,size=0.9,width=0.6,aes(group=factor(gender_F0_M1_a),colour=factor(gender_F0_M1_a),
                                                        x = rank, y = non_rels)) +
  
  facet_wrap(~gender_F0_M1_a, ncol = 2, labeller=labeller(gender_F0_M1_a=new))+
  
  
  # scale_color_manual(name=NULL, breaks=c(0,1),values=cols,
  #                    labels=c("Predicted (95% CI)","Observed")) +
  
  scale_color_manual(name = NULL,
                     values = cols,
                     breaks=c(1,0),
                     labels = c("Predicted (95% CI)", "Observed"),
                     guide = guide_legend(override.aes = list(linetype = c(1, 0),
                                                              shape = c(NA, 1),
                                                              color = "grey")))+
  
  scale_fill_manual(name="", breaks=c(0),values=cols,
                    labels=c("Predicted (95% CI)")) +
  scale_x_continuous(name="Economic Capital",limits=c(0.0,10.0),breaks=c(2.5,7.5),
                     labels=c("Low","High")) +
  scale_y_continuous(name="Number of Non-relatives in Network",breaks=c(1,2,3,4,5,6,7,8,9,10,11,12),limits=c(0,14),
                     labels=c("1","2","3","4","5","6","7","8","9","10","11","12")) +
  
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size=16,face="bold"),
        legend.title = element_text(size=12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.key.size = unit(0.15, "in"),
        legend.text = element_text(size = 12, face = "bold"),
        strip.text.x = element_text(
          size = 12, color = "black", face = "bold"),
        strip.text.y = element_text(
          size = 12, color = "black", face = "bold"),
        
        axis.text.x = element_text(colour="grey20",size=10,angle=0,face="bold"),
        axis.text.y = element_text(colour="grey20",size=10,angle=0,hjust=0,vjust=0,face="bold"),  
        axis.title.x = element_text(colour="black",size=12,angle=0,hjust=.5,vjust=0,face="bold"),
        axis.title.y = element_text(colour="black",size=12,angle=90,hjust=.5,vjust=.5,face="bold"))+
  
  guides(alpha="none")+
  guides(fill="none")
plot1a


library("gridExtra")
library(ggpubr)

m <- ggarrange(plot1, plot1a, 
               labels = c("A", "B"),
               ncol = 1, nrow = 2)
# m1 <-annotate_figure(m,
#                      top = text_grob("Women who are higher in religiosity\n have larger overall social networks", color = "black", face = "bold",
#                                      size = 14))
require(grid)   # for the textGrob() function

figure <- ggarrange(plot1 + rremove("ylab"), plot1a + rremove("ylab"),
                    labels = NULL,
                    ncol = 1, nrow = 2,
                    common.legend = TRUE, legend = "bottom",
                    align = "hv", 
                    font.label = list(size = 10, color = "black", face = "bold", family = NULL, position = "top"))

figure1 <- annotate_figure(figure, left = textGrob("Number of non-relatives", rot = 90, vjust = 1, gp = gpar(cex = 1.2)))


setwd("C:/Users/robert/Dropbox/Github/Kin_networks_sex_diffs/Figures")
png("~/graph.png") ; par(mar=c(6, 4, 4, 2) + 0.1)
ggsave(figure1, filename = "Figure 5 (Number of non-relatives with MI indices).png", width = 24, height = 24, dpi = 600,units = "cm")


#### Some aspects of market integration, particularly education and wealth, 
#increase the kin density of female networks, 
##but decrease the kin density of male networks.

library(tidyverse)
library(brms)
library(readr)
library(scales)
##  make new data frame
husbands_new <- read.csv("C:/Users/robert/Dropbox/Github/Intensive extensive kin networks/data/husbands_new.csv", header = T, sep = ",")

data1 <-  husbands_new %>% filter (Respondent_a=="Wife" | Respondent_a=="Husband")
data2 <-  husbands_new %>% filter (Respondent_a=="Wife")
data3 <-  husbands_new %>% filter (Respondent_a=="Husband")
d <- data1[c(2,38,6,8,9,25:27)] 
d <- d[complete.cases(d), ] 
d<-d %>% mutate(rank=ntile(d$MI_human_capital,10))
library(ggthemes)

## make the top plot
Sex_seq <- rep(0:1, each=10)
MI_seq <- rep(1:10,2)
#read in  the model

M1 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_sex_diffs/Results/percent_relatives_in_NW_lognormal_sex.rds")
attach(d)
newdata <- tidyr::crossing(
  gender_F0_M1_a = c(0.1,1.1),
  MI_geo_proximity=mean(MI_geo_proximity),
  MI_economic_capital=mean(MI_economic_capital),
  MI_human_capital=c(-2.0,-1.5,-1,-0.5,0,0.5,1.0,1.5,2.0,2.5),
  Kids_a=mean(Kids_a),
  Mothers_kids_a=mean(Mothers_kids_a)) %>%
  as.data.frame()
detach(d)


mu_summary <-
  fitted(M1, 
         newdata = newdata, allow_new_levels=TRUE, probs=c(0.11,0.89)) %>%
  as_tibble() %>%
  bind_cols(Sex_seq) %>%
  bind_cols(MI_seq)

mu_summary

colnames(mu_summary) <- c('Estimate','Error','Q5','Q95','Sex',"Education")

mu_summary

Data1 <- d %>% left_join (mu_summary, by =c("gender_F0_M1_a"="Sex","rank"="Education"))

library(plotrix)
library(magrittr)
library(dplyr)
library(ggplot2)
library(ggstance)
library(rstan)
library(tidybayes)
library(emmeans)
library(broom)
library(brms)
library(modelr)
library(forcats)
library(ggdist)


cols <- c("0" = "#000000", "1" = "#0072B2", "2" = "#000000", "3" = "#0072B2") #,

show_col("#000000")
show_col("#0072B2")
# relabel sex from facet grid
new <- c("Men", "Women")
names(new) <- c(1,0)


plot1<- ggplot(Data1, aes(x=rank, y=Estimate,group=factor(gender_F0_M1_a),
                          fill=factor(gender_F0_M1_a),
                          color=factor(gender_F0_M1_a))) +
  
  geom_ribbon(aes(ymin = Q5, ymax = Q95,alpha=0.7)) +
  
  geom_line() +
  
  geom_jitter(data=Data1,shape=1,size=0.9,width=0.6,aes(group=factor(gender_F0_M1_a),colour=factor(gender_F0_M1_a),
                                                        x = rank, y = percent_rels_in_NW)) +
  
  facet_wrap(~gender_F0_M1_a, ncol = 2, labeller=labeller(gender_F0_M1_a=new))+
  

  
  scale_color_manual(name = NULL,
                     values = cols,
                     breaks=c(1,0),
                     labels = c("Predicted (95% CI)", "Observed"),
                     guide = guide_legend(override.aes = list(linetype = c(1, 0),
                                                              shape = c(NA, 1),
                                                              color = "grey")))+
  
  scale_fill_manual(name="", breaks=c(0),values=cols,
                    labels=c("Predicted (95% CI)")) +
  scale_x_continuous(name="Human Capital",limits=c(0.0,10.0),breaks=c(2.5,7.5),
                     labels=c("Low","High")) +
  scale_y_continuous(name="Percentage of relatives in Network",breaks=c(0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,1),limits=c(0.4,1.05),
                     labels=c("40%","45%","50%","55%","60%","65%","70%","75%","80%","85%","90%","95%","100%")) +
  
  
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size=16,face="bold"),
        legend.title = element_text(size=12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.key.size = unit(0.15, "in"),
        legend.text = element_text(size = 12, face = "bold"),
        strip.text.x = element_text(
          size = 12, color = "black", face = "bold"),
        strip.text.y = element_text(
          size = 12, color = "black", face = "bold"),
        
        axis.text.x = element_text(colour="grey20",size=10,angle=0,face="bold"),
        axis.text.y = element_text(colour="grey20",size=10,angle=0,hjust=0,vjust=0,face="bold"),  
        axis.title.x = element_text(colour="black",size=12,angle=0,hjust=.5,vjust=0,face="bold"),
        axis.title.y = element_text(colour="black",size=12,angle=90,hjust=.5,vjust=.5,face="bold"))+
  
  guides(alpha="none")+
  guides(fill="none")
plot1


library(ggthemes)

## make the bottom plot
d <- data1[c(2,38,6,8,9,25:27)] 
d <- d[complete.cases(d), ] 
d<-d %>% mutate(rank=ntile(d$MI_economic_capital,10))
library(ggthemes)

## make the top plot
Sex_seq <- rep(0:1, each=10)
MI_seq <- rep(1:10,2)
#read in  the model

M1 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_sex_diffs/Results/percent_relatives_in_NW_lognormal_sex.rds")
attach(d)
newdata <- tidyr::crossing(
  gender_F0_M1_a = c(0.1,1.1),
  MI_geo_proximity=mean(MI_geo_proximity),
  MI_human_capital=mean(MI_human_capital),
  MI_economic_capital=c(-2.0,-1,0,1,2,3,4,5,6,7),
  Kids_a=mean(Kids_a),
  Mothers_kids_a=mean(Mothers_kids_a)) %>%
  as.data.frame()
detach(d)


mu_summary <-
  fitted(M1, 
         newdata = newdata, allow_new_levels=TRUE, probs=c(0.11,0.89)) %>%
  as_tibble() %>%
  bind_cols(Sex_seq) %>%
  bind_cols(MI_seq)

mu_summary

colnames(mu_summary) <- c('Estimate','Error','Q5','Q95','Sex',"Wealth")

mu_summary


Data2 <- d %>% left_join (mu_summary, by =c("rank"="Wealth","gender_F0_M1_a"="Sex"))

library(plotrix)
library(magrittr)
library(dplyr)
library(ggplot2)
library(ggstance)
library(rstan)
library(tidybayes)
library(emmeans)
library(broom)
library(brms)
library(modelr)
library(forcats)
library(ggdist)
#0072B2
cols <- c("0" = "#000000", "1" = "#0072B2", "2" = "#000000", "3" = "#0072B2")
plot1a <-  ggplot(Data2, aes(x=rank, y=Estimate,group=factor(gender_F0_M1_a),
                             fill=factor(gender_F0_M1_a),
                             color=factor(gender_F0_M1_a))) +
  
  geom_ribbon(aes(ymin = Q5, ymax = Q95,alpha=0.7)) +
  
  geom_line() +
  
  geom_jitter(data=Data1,shape=1,size=0.9,width=0.6,aes(group=factor(gender_F0_M1_a),colour=factor(gender_F0_M1_a),
                                                        x = rank, y = percent_rels_in_NW)) +
  
  facet_wrap(~gender_F0_M1_a, ncol = 2, labeller=labeller(gender_F0_M1_a=new))+
  
  
  # scale_color_manual(name=NULL, breaks=c(0,1),values=cols,
  #                    labels=c("Predicted (95% CI)","Observed")) +
  
  scale_color_manual(name = NULL,
                     values = cols,
                     breaks=c(1,0),
                     labels = c("Predicted (95% CI)", "Observed"),
                     guide = guide_legend(override.aes = list(linetype = c(1, 0),
                                                              shape = c(NA, 1),
                                                              color = "grey")))+
  
  scale_fill_manual(name="", breaks=c(0),values=cols,
                    labels=c("Predicted (95% CI)")) +
  scale_x_continuous(name="Economic Capital",limits=c(0.0,10.0),breaks=c(2.5,7.5),
                     labels=c("Low","High")) +
  scale_y_continuous(name="Percentage of relatives in Network",breaks=c(0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,1),limits=c(0.4,1.05),
                     labels=c("40%","45%","50%","55%","60%","65%","70%","75%","80%","85%","90%","95%","100%")) +
  
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size=16,face="bold"),
        legend.title = element_text(size=12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.key.size = unit(0.15, "in"),
        legend.text = element_text(size = 12, face = "bold"),
        strip.text.x = element_text(
          size = 12, color = "black", face = "bold"),
        strip.text.y = element_text(
          size = 12, color = "black", face = "bold"),
        
        axis.text.x = element_text(colour="grey20",size=10,angle=0,face="bold"),
        axis.text.y = element_text(colour="grey20",size=10,angle=0,hjust=0,vjust=0,face="bold"),  
        axis.title.x = element_text(colour="black",size=12,angle=0,hjust=.5,vjust=0,face="bold"),
        axis.title.y = element_text(colour="black",size=12,angle=90,hjust=.5,vjust=.5,face="bold"))+
  
  guides(alpha="none")+
  guides(fill="none")
plot1a


library("gridExtra")
library(ggpubr)

m <- ggarrange(plot1, plot1a, 
               labels = c("A", "B"),
               ncol = 1, nrow = 2)

require(grid)   # for the textGrob() function

figure <- ggarrange(plot1 + rremove("ylab"), plot1a + rremove("ylab"),
                    labels = NULL,
                    ncol = 1, nrow = 2,
                    common.legend = TRUE, legend = "bottom",
                    align = "hv", 
                    font.label = list(size = 10, color = "black", face = "bold", family = NULL, position = "top"))

figure1 <- annotate_figure(figure, left = textGrob("Percentage of relatives in Network", rot = 90, vjust = 1, gp = gpar(cex = 1.2)))


setwd("C:/Users/robert/Dropbox/Github/Kin_networks_sex_diffs/Figures")
png("~/graph.png") ; par(mar=c(6, 4, 4, 2) + 0.1)
ggsave(figure1, filename = "Figure 6 (Kin density with MI indices).png", width = 24, height = 24, dpi = 600,units = "cm")


#### total NW size 

library(tidyverse)
library(brms)
library(readr)
library(scales)
##  make new data frame
husbands_new <- read.csv("C:/Users/robert/Dropbox/Github/Intensive extensive kin networks/data/husbands_new.csv", header = T, sep = ",")

data1 <-  husbands_new %>% filter (Respondent_a=="Wife" | Respondent_a=="Husband")
data2 <-  husbands_new %>% filter (Respondent_a=="Wife")
data3 <-  husbands_new %>% filter (Respondent_a=="Husband")
d <- data1[c(2,28,6,8,9,25:27)] 
d <- d[complete.cases(d), ] 
d<-d %>% mutate(rank=ntile(d$MI_human_capital,10))
library(ggthemes)

## make the top plot
Sex_seq <- rep(0:1, each=10)
MI_seq <- rep(1:10,2)
#read in  the model

M1 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_sex_diffs/Results/NW_total_lognormal_sex.rds")
attach(d)
newdata <- tidyr::crossing(
  gender_F0_M1_a = c(0,1),
  MI_geo_proximity=mean(MI_geo_proximity),
  MI_economic_capital=mean(MI_economic_capital),
  MI_human_capital=c(-2.0,-1.5,-1,-0.5,0,0.5,1.0,1.5,2.0,2.5),
  Kids_a=mean(Kids_a),
  Mothers_kids_a=mean(Mothers_kids_a)) %>%
  as.data.frame()
detach(d)


mu_summary <-
  fitted(M1, 
         newdata = newdata, allow_new_levels=TRUE, probs=c(0.11,0.89)) %>%
  as_tibble() %>%
  bind_cols(Sex_seq) %>%
  bind_cols(MI_seq)

mu_summary

colnames(mu_summary) <- c('Estimate','Error','Q5','Q95','Sex',"Education")

mu_summary

Data1 <- d %>% left_join (mu_summary, by =c("gender_F0_M1_a"="Sex","rank"="Education"))

library(plotrix)
library(magrittr)
library(dplyr)
library(ggplot2)
library(ggstance)
library(rstan)
library(tidybayes)
library(emmeans)
library(broom)
library(brms)
library(modelr)
library(forcats)
library(ggdist)


cols <- c("0" = "#000000", "1" = "#0072B2", "2" = "#000000", "3" = "#0072B2") #,

show_col("#000000")
show_col("#0072B2")
# relabel sex from facet grid
new <- c("Men", "Women")
names(new) <- c(1,0)


plot1<- ggplot(Data1, aes(x=rank, y=Estimate,group=factor(gender_F0_M1_a),
                          fill=factor(gender_F0_M1_a),
                          color=factor(gender_F0_M1_a))) +
  
  geom_ribbon(aes(ymin = Q5, ymax = Q95,alpha=0.7)) +
  
  geom_line() +
  
  geom_jitter(data=Data1,shape=1,size=0.9,width=0.6,aes(group=factor(gender_F0_M1_a),colour=factor(gender_F0_M1_a),
                                                        x = rank, y = NW_total)) +
  
  facet_wrap(~gender_F0_M1_a, ncol = 2, labeller=labeller(gender_F0_M1_a=new))+
  
  
  # scale_color_manual(name=NULL, breaks=c(0,1),values=cols,
  #                    labels=c("Predicted (95% CI)","Observed")) +
  
  scale_color_manual(name = NULL,
                     values = cols,
                     breaks=c(1,0),
                     labels = c("Predicted (95% CI)", "Observed"),
                     guide = guide_legend(override.aes = list(linetype = c(1, 0),
                                                              shape = c(NA, 1),
                                                              color = "grey")))+
  
  scale_fill_manual(name="", breaks=c(0),values=cols,
                    labels=c("Predicted (95% CI)")) +
  scale_x_continuous(name="Human Capital",limits=c(0.0,10.0),breaks=c(2.5,7.5),
                     labels=c("Low","High")) +
  scale_y_continuous(name="Total network size",
                     breaks=c(1,5,6,7,8,9,10,11,12,13,14,15,20,28),
                     limits=c(2,28),
                     labels=c("1","5","6","7","8","9","10","11","12","13","14","15",
                              "20","28")) +
  
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size=16,face="bold"),
        legend.title = element_text(size=12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.key.size = unit(0.15, "in"),
        legend.text = element_text(size = 12, face = "bold"),
        strip.text.x = element_text(
          size = 12, color = "black", face = "bold"),
        strip.text.y = element_text(
          size = 12, color = "black", face = "bold"),
        
        axis.text.x = element_text(colour="grey20",size=10,angle=0,face="bold"),
        axis.text.y = element_text(colour="grey20",size=10,angle=0,hjust=0,vjust=0,face="bold"),  
        axis.title.x = element_text(colour="black",size=12,angle=0,hjust=.5,vjust=0,face="bold"),
        axis.title.y = element_text(colour="black",size=12,angle=90,hjust=.5,vjust=.5,face="bold"))+
  
  guides(alpha="none")+
  guides(fill="none")
plot1


library(ggthemes)

## make the bottom plot
d <- data1[c(2,28,6,8,9,25:27)] 
d <- d[complete.cases(d), ] 
d<-d %>% mutate(rank=ntile(d$MI_economic_capital,10))
library(ggthemes)

## make the top plot
Sex_seq <- rep(0:1, each=10)
MI_seq <- rep(1:10,2)
#read in  the model

M1 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_sex_diffs/Results/NW_total_lognormal_sex.rds")
attach(d)
newdata <- tidyr::crossing(
  gender_F0_M1_a = c(0,1),
  MI_geo_proximity=mean(MI_geo_proximity),
  MI_human_capital=mean(MI_human_capital),
  MI_economic_capital=c(-2.0,-1,0,1,2,3,4,5,6,7),
  Kids_a=mean(Kids_a),
  Mothers_kids_a=mean(Mothers_kids_a)) %>%
  as.data.frame()
detach(d)


mu_summary <-
  fitted(M1, 
         newdata = newdata, allow_new_levels=TRUE, probs=c(0.11,0.89)) %>%
  as_tibble() %>%
  bind_cols(Sex_seq) %>%
  bind_cols(MI_seq)

mu_summary

colnames(mu_summary) <- c('Estimate','Error','Q5','Q95','Sex',"Wealth")

mu_summary


Data2 <- d %>% left_join (mu_summary, by =c("rank"="Wealth","gender_F0_M1_a"="Sex"))

library(plotrix)
library(magrittr)
library(dplyr)
library(ggplot2)
library(ggstance)
library(rstan)
library(tidybayes)
library(emmeans)
library(broom)
library(brms)
library(modelr)
library(forcats)
library(ggdist)
#0072B2
cols <- c("0" = "#000000", "1" = "#0072B2", "2" = "#000000", "3" = "#0072B2")
plot1a <-  ggplot(Data2, aes(x=rank, y=Estimate,group=factor(gender_F0_M1_a),
                             fill=factor(gender_F0_M1_a),
                             color=factor(gender_F0_M1_a))) +
  
  geom_ribbon(aes(ymin = Q5, ymax = Q95,alpha=0.7)) +
  
  geom_line() +
  
  geom_jitter(data=Data1,shape=1,size=0.9,width=0.6,aes(group=factor(gender_F0_M1_a),colour=factor(gender_F0_M1_a),
                                                        x = rank, y = NW_total)) +
  
  facet_wrap(~gender_F0_M1_a, ncol = 2, labeller=labeller(gender_F0_M1_a=new))+
  
  
  # scale_color_manual(name=NULL, breaks=c(0,1),values=cols,
  #                    labels=c("Predicted (95% CI)","Observed")) +
  
  scale_color_manual(name = NULL,
                     values = cols,
                     breaks=c(1,0),
                     labels = c("Predicted (95% CI)", "Observed"),
                     guide = guide_legend(override.aes = list(linetype = c(1, 0),
                                                              shape = c(NA, 1),
                                                              color = "grey")))+
  
  scale_fill_manual(name="", breaks=c(0),values=cols,
                    labels=c("Predicted (95% CI)")) +
  scale_x_continuous(name="Economic Capital",limits=c(0.0,10.0),breaks=c(2.5,7.5),
                     labels=c("Low","High")) +
  scale_y_continuous(name="Total network size",
                     breaks=c(1,5,6,7,8,9,10,11,12,13,14,15,20,28),
                     limits=c(2,28),
                     labels=c("1","5","6","7","8","9","10","11","12","13","14","15",
                              "20","28")) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size=16,face="bold"),
        legend.title = element_text(size=12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.key.size = unit(0.15, "in"),
        legend.text = element_text(size = 12, face = "bold"),
        strip.text.x = element_text(
          size = 12, color = "black", face = "bold"),
        strip.text.y = element_text(
          size = 12, color = "black", face = "bold"),
        
        axis.text.x = element_text(colour="grey20",size=10,angle=0,face="bold"),
        axis.text.y = element_text(colour="grey20",size=10,angle=0,hjust=0,vjust=0,face="bold"),  
        axis.title.x = element_text(colour="black",size=12,angle=0,hjust=.5,vjust=0,face="bold"),
        axis.title.y = element_text(colour="black",size=12,angle=90,hjust=.5,vjust=.5,face="bold"))+
  
  guides(alpha="none")+
  guides(fill="none")
plot1a


library("gridExtra")
library(ggpubr)

m <- ggarrange(plot1, plot1a, 
               labels = c("A", "B"),
               ncol = 1, nrow = 2)
# m1 <-annotate_figure(m,
#                      top = text_grob("Women who are higher in religiosity\n have larger overall social networks", color = "black", face = "bold",
#                                      size = 14))
require(grid)   # for the textGrob() function

figure <- ggarrange(plot1 + rremove("ylab"), plot1a + rremove("ylab"),
                    labels = NULL,
                    ncol = 1, nrow = 2,
                    common.legend = TRUE, legend = "bottom",
                    align = "hv", 
                    font.label = list(size = 10, color = "black", face = "bold", family = NULL, position = "top"))

figure1 <- annotate_figure(figure, left = textGrob("Total Network Size", rot = 90, vjust = 1, gp = gpar(cex = 1.2)))


setwd("C:/Users/robert/Dropbox/Github/Kin_networks_sex_diffs/Figures")
png("~/graph.png") ; par(mar=c(6, 4, 4, 2) + 0.1)
ggsave(figure1, filename = "Figure 7 (Total NW size with MI indices).png", width = 24, height = 24, dpi = 600,units = "cm")



#### Number of relatives

library(tidyverse)
library(brms)
library(readr)
library(scales)
##  make new data frame
husbands_new <- read.csv("C:/Users/robert/Dropbox/Github/Intensive extensive kin networks/data/husbands_new.csv", header = T, sep = ",")

data1 <-  husbands_new %>% filter (Respondent_a=="Wife" | Respondent_a=="Husband")
data2 <-  husbands_new %>% filter (Respondent_a=="Wife")
data3 <-  husbands_new %>% filter (Respondent_a=="Husband")
d <- data1[c(2,37,6,8,9,25:27)] 
d <- d[complete.cases(d), ] 
d<-d %>% mutate(rank=ntile(d$MI_human_capital,10))
library(ggthemes)

## make the top plot
Sex_seq <- rep(0:1, each=10)
MI_seq <- rep(1:10,2)
#read in  the model

M1 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_sex_diffs/Results/relatives_in_NW_lognormal_sex.rds")
attach(d)
newdata <- tidyr::crossing(
  gender_F0_M1_a = c(0,1),
  MI_geo_proximity=mean(MI_geo_proximity),
  MI_economic_capital=mean(MI_economic_capital),
  MI_human_capital=c(-2.0,-1.5,-1,-0.5,0,0.5,1.0,1.5,2.0,2.5),
  Kids_a=mean(Kids_a),
  Mothers_kids_a=mean(Mothers_kids_a)) %>%
  as.data.frame()
detach(d)


mu_summary <-
  fitted(M1, 
         newdata = newdata, allow_new_levels=TRUE, probs=c(0.11,0.89)) %>%
  as_tibble() %>%
  bind_cols(Sex_seq) %>%
  bind_cols(MI_seq)

mu_summary

colnames(mu_summary) <- c('Estimate','Error','Q5','Q95','Sex',"Education")

mu_summary

Data1 <- d %>% left_join (mu_summary, by =c("gender_F0_M1_a"="Sex","rank"="Education"))

library(plotrix)
library(magrittr)
library(dplyr)
library(ggplot2)
library(ggstance)
library(rstan)
library(tidybayes)
library(emmeans)
library(broom)
library(brms)
library(modelr)
library(forcats)
library(ggdist)


cols <- c("0" = "#000000", "1" = "#0072B2", "2" = "#000000", "3" = "#0072B2") #,

show_col("#000000")
show_col("#0072B2")
# relabel sex from facet grid
new <- c("Men", "Women")
names(new) <- c(1,0)


plot1<- ggplot(Data1, aes(x=rank, y=Estimate,group=factor(gender_F0_M1_a),
                          fill=factor(gender_F0_M1_a),
                          color=factor(gender_F0_M1_a))) +
  
  geom_ribbon(aes(ymin = Q5, ymax = Q95,alpha=0.7)) +
  
  geom_line() +
  
  geom_jitter(data=Data1,shape=1,size=0.9,width=0.6,aes(group=factor(gender_F0_M1_a),colour=factor(gender_F0_M1_a),
                                                        x = rank, y = rels_in_NW)) +
  
  facet_wrap(~gender_F0_M1_a, ncol = 2, labeller=labeller(gender_F0_M1_a=new))+
  
  
  # scale_color_manual(name=NULL, breaks=c(0,1),values=cols,
  #                    labels=c("Predicted (95% CI)","Observed")) +
  
  scale_color_manual(name = NULL,
                     values = cols,
                     breaks=c(1,0),
                     labels = c("Predicted (95% CI)", "Observed"),
                     guide = guide_legend(override.aes = list(linetype = c(1, 0),
                                                              shape = c(NA, 1),
                                                              color = "grey")))+
  
  scale_fill_manual(name="", breaks=c(0),values=cols,
                    labels=c("Predicted (95% CI)")) +
  scale_x_continuous(name="Human Capital",limits=c(0.0,10.0),breaks=c(2.5,7.5),
                     labels=c("Low","High")) +
  scale_y_continuous(name="Number of Relatives in Network",breaks=c(1,5,6,7,8,9,10,15),limits=c(0,16),
                     labels=c("1","5","6","7","8","9","10","15")) +
  
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size=16,face="bold"),
        legend.title = element_text(size=12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.key.size = unit(0.15, "in"),
        legend.text = element_text(size = 12, face = "bold"),
        strip.text.x = element_text(
          size = 12, color = "black", face = "bold"),
        strip.text.y = element_text(
          size = 12, color = "black", face = "bold"),
        
        axis.text.x = element_text(colour="grey20",size=10,angle=0,face="bold"),
        axis.text.y = element_text(colour="grey20",size=10,angle=0,hjust=0,vjust=0,face="bold"),  
        axis.title.x = element_text(colour="black",size=12,angle=0,hjust=.5,vjust=0,face="bold"),
        axis.title.y = element_text(colour="black",size=12,angle=90,hjust=.5,vjust=.5,face="bold"))+
  
  guides(alpha="none")+
  guides(fill="none")
plot1


library(ggthemes)

## make the bottom plot
d <- data1[c(2,37,6,8,9,25:27)] 
d <- d[complete.cases(d), ] 
d<-d %>% mutate(rank=ntile(d$MI_economic_capital,10))
library(ggthemes)

## make the top plot
Sex_seq <- rep(0:1, each=10)
MI_seq <- rep(1:10,2)
#read in  the model

M1 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_sex_diffs/Results/relatives_in_NW_lognormal_sex.rds")
attach(d)
newdata <- tidyr::crossing(
  gender_F0_M1_a = c(0,1),
  MI_geo_proximity=mean(MI_geo_proximity),
  MI_human_capital=mean(MI_human_capital),
  MI_economic_capital=c(-2.0,-1,0,1,2,3,4,5,6,7),
  Kids_a=mean(Kids_a),
  Mothers_kids_a=mean(Mothers_kids_a)) %>%
  as.data.frame()
detach(d)


mu_summary <-
  fitted(M1, 
         newdata = newdata, allow_new_levels=TRUE, probs=c(0.11,0.89)) %>%
  as_tibble() %>%
  bind_cols(Sex_seq) %>%
  bind_cols(MI_seq)

mu_summary

colnames(mu_summary) <- c('Estimate','Error','Q5','Q95','Sex',"Wealth")

mu_summary


Data2 <- d %>% left_join (mu_summary, by =c("rank"="Wealth","gender_F0_M1_a"="Sex"))

library(plotrix)
library(magrittr)
library(dplyr)
library(ggplot2)
library(ggstance)
library(rstan)
library(tidybayes)
library(emmeans)
library(broom)
library(brms)
library(modelr)
library(forcats)
library(ggdist)
#0072B2
cols <- c("0" = "#000000", "1" = "#0072B2", "2" = "#000000", "3" = "#0072B2")
plot1a <-  ggplot(Data2, aes(x=rank, y=Estimate,group=factor(gender_F0_M1_a),
                             fill=factor(gender_F0_M1_a),
                             color=factor(gender_F0_M1_a))) +
  
  geom_ribbon(aes(ymin = Q5, ymax = Q95,alpha=0.7)) +
  
  geom_line() +
  
  geom_jitter(data=Data1,shape=1,size=0.9,width=0.6,aes(group=factor(gender_F0_M1_a),colour=factor(gender_F0_M1_a),
                                                        x = rank, y = rels_in_NW)) +
  
  facet_wrap(~gender_F0_M1_a, ncol = 2, labeller=labeller(gender_F0_M1_a=new))+
  
  
  # scale_color_manual(name=NULL, breaks=c(0,1),values=cols,
  #                    labels=c("Predicted (95% CI)","Observed")) +
  
  scale_color_manual(name = NULL,
                     values = cols,
                     breaks=c(1,0),
                     labels = c("Predicted (95% CI)", "Observed"),
                     guide = guide_legend(override.aes = list(linetype = c(1, 0),
                                                              shape = c(NA, 1),
                                                              color = "grey")))+
  
  scale_fill_manual(name="", breaks=c(0),values=cols,
                    labels=c("Predicted (95% CI)")) +
  scale_x_continuous(name="Economic Capital",limits=c(0.0,10.0),breaks=c(2.5,7.5),
                     labels=c("Low","High")) +
  scale_y_continuous(name="Number of Relatives in Network",breaks=c(1,5,6,7,8,9,10,15),limits=c(0,16),
                     labels=c("1","5","6","7","8","9","10","15")) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size=16,face="bold"),
        legend.title = element_text(size=12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.key.size = unit(0.15, "in"),
        legend.text = element_text(size = 12, face = "bold"),
        strip.text.x = element_text(
          size = 12, color = "black", face = "bold"),
        strip.text.y = element_text(
          size = 12, color = "black", face = "bold"),
        
        axis.text.x = element_text(colour="grey20",size=10,angle=0,face="bold"),
        axis.text.y = element_text(colour="grey20",size=10,angle=0,hjust=0,vjust=0,face="bold"),  
        axis.title.x = element_text(colour="black",size=12,angle=0,hjust=.5,vjust=0,face="bold"),
        axis.title.y = element_text(colour="black",size=12,angle=90,hjust=.5,vjust=.5,face="bold"))+
  
  guides(alpha="none")+
  guides(fill="none")
plot1a


library("gridExtra")
library(ggpubr)

m <- ggarrange(plot1, plot1a, 
               labels = c("A", "B"),
               ncol = 1, nrow = 2)
# m1 <-annotate_figure(m,
#                      top = text_grob("Women who are higher in religiosity\n have larger overall social networks", color = "black", face = "bold",
#                                      size = 14))
require(grid)   # for the textGrob() function

figure <- ggarrange(plot1 + rremove("ylab"), plot1a + rremove("ylab"),
                    labels = NULL,
                    ncol = 1, nrow = 2,
                    common.legend = TRUE, legend = "bottom",
                    align = "hv", 
                    font.label = list(size = 10, color = "black", face = "bold", family = NULL, position = "top"))

figure1 <- annotate_figure(figure, left = textGrob("Number of Relative in Network", rot = 90, vjust = 1, gp = gpar(cex = 1.2)))


setwd("C:/Users/robert/Dropbox/Github/Kin_networks_sex_diffs/Figures")
png("~/graph.png") ; par(mar=c(6, 4, 4, 2) + 0.1)
ggsave(figure1, filename = "Figure 8 (Number of relatives with MI indices).png", width = 24, height = 24, dpi = 600,units = "cm")
