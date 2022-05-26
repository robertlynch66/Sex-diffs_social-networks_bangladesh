library(tidyverse)
library(brms)
library(readr)
library(sjPlot)
library(sjmisc)
library(sjlabelled)                             # Install & load scales
library("scales")
####
husbands_new <- read.csv("C:/Users/robert/Dropbox/Github/Intensive extensive kin networks/data/husbands_new.csv", header = T, sep = ",")

husbands_new$percent_rels_in_NW<- as.numeric(husbands_new$percent_rels_in_NW)

data1 <-  husbands_new %>% filter (Respondent_a=="Wife" | Respondent_a=="Husband")


library(readr)

d<-data1[c(2,28,6,8,9,25:27)]

d$NW_total <- as.numeric(d$NW_total)
### try model
model1 <- brm(NW_total ~ Kids_a+
                Mothers_kids_a+
                gender_F0_M1_a*MI_geo_proximity+
                gender_F0_M1_a*MI_economic_capital+
                gender_F0_M1_a*MI_human_capital+
                (1|idwife_a), 
              data=d, 
              family = lognormal(),
              prior = c(set_prior("normal(0,2)", class = "b")),
              warmup = 1000, iter = 5000, chains = 4,
              control = list(adapt_delta = 0.95))

print(summary(model1, prob=0.95,priors=TRUE), digits = 6)

# Population-Level Effects: 
#                                     Estimate Est.Error  l-95% CI u-95% CI     Rhat Bulk_ESS Tail_ESS
# Intercept                           2.057932  0.034594  1.990140 2.125561 0.999969    18238    12472
# Kids_a                              0.003415  0.006726 -0.009952 0.016402 1.000140    17119    12760
# Mothers_kids_a                      0.006963  0.003344  0.000419 0.013469 1.000192    22953    12420
# gender_F0_M1_a                      0.366119  0.019371  0.328018 0.404451 1.001047    19528    11674
# MI_geo_proximity                   -0.017496  0.017652 -0.052306 0.017235 1.000037    15429    12061
# MI_economic_capital                 0.060921  0.014058  0.033622 0.088686 1.000346    15616    12312
# MI_human_capital                    0.036787  0.016712  0.003873 0.069849 1.000422    15831    12103
# gender_F0_M1_a:MI_geo_proximity     0.003603  0.024600 -0.044054 0.051985 1.000248    15063    12269
# gender_F0_M1_a:MI_economic_capital -0.002846  0.021157 -0.044297 0.038198 1.000032    16290    13308
# gender_F0_M1_a:MI_human_capital     0.054973  0.022393  0.011121 0.099116 1.000250    19703    13293


#1) Males have larger networks
#2) Education exacerbates these differences - men are gaining the benefits of Human capital MI
# with expanding social NW's
setwd("C:/Users/robert/Dropbox/Github/Kin_networks_sex_diffs")
path<- (paste0("results/"))
filename <- "NW_total_lognormal_intx_sex.rds"

saveRDS(model1, paste0(path, filename))


### Percent relatives run
d <- data1[c(2,38,6,8,9,25:27)] 
# 
d$percent_rels_in_NW <- as.numeric(d$percent_rels_in_NW)
d$percent_rels_in_NW<- d$percent_rels_in_NW+0.01
d <- d[complete.cases(d), ] 
## run as log normal  
model2 <- brm(percent_rels_in_NW ~ Kids_a+
                Mothers_kids_a+
                gender_F0_M1_a*MI_geo_proximity+
                gender_F0_M1_a*MI_economic_capital+
                gender_F0_M1_a*MI_human_capital+
                (1|idwife_a), 
              data=d, 
              family = lognormal(),
              prior = c(set_prior("normal(0,2)", class = "b")),
              warmup = 1000, iter = 5000, chains = 4,
              control = list(adapt_delta = 0.95,
                             max_treedepth=13))



print(summary(model2, prob=0.95,priors=TRUE), digits = 6)

# Population-Level Effects: 
#                                    Estimate Est.Error  l-95% CI  u-95% CI     Rhat Bulk_ESS Tail_ESS
# Intercept                          -0.177347  0.034366 -0.245237 -0.110244 1.000010    22276    13564
# Kids_a                              0.009264  0.006699 -0.003920  0.022346 1.000396    19541    13539
# Mothers_kids_a                     -0.000635  0.003287 -0.007123  0.005781 1.000054    25458    12601
# gender_F0_M1_a                     -0.387176  0.019640 -0.425631 -0.348446 1.000158    29241    11607
# MI_geo_proximity                   -0.005402  0.017538 -0.039420  0.028665 1.000418    16734    12386
# MI_economic_capital                 0.015799  0.013952 -0.011426  0.042957 1.000380    19795    12827
# MI_human_capital                   -0.011581  0.016738 -0.044501  0.021139 1.000614    16872    12585
# gender_F0_M1_a:MI_geo_proximity     0.014299  0.024547 -0.034403  0.061894 1.000063    17783    12618
# gender_F0_M1_a:MI_economic_capital -0.050179  0.021528 -0.092611 -0.008159 1.000030    21184    12330
# gender_F0_M1_a:MI_human_capital    -0.070654  0.022594 -0.115528 -0.026625 1.000280    19812    12750


#1) Men have a lower percentage of relative in their Networks
#2) Increasing Human capital and wealth reduces this difference between men and women such
# that more integrated households have more similar levels of kin density between men and women
path<- (paste0("results/"))
filename <- "percent_relatives_in_NW_lognormal_intx_sex.rds"
setwd("C:/Users/robert/Dropbox/Github/Kin_networks_sex_diffs")
saveRDS(model2, paste0(path, filename))

# Model 2.1 Relatives in Network

library(tidyverse)
library(brms)
library(readr)

d <- data1[c(2,37,6,8,9,25:27)] 
d <- d[complete.cases(d), ] 

d$rels_in_NW<- d$rels_in_NW+0.01
## run as log normal  
model2.1 <- brm(rels_in_NW ~ Kids_a+
                  Mothers_kids_a+
                  gender_F0_M1_a*MI_geo_proximity+
                  gender_F0_M1_a*MI_economic_capital+
                  gender_F0_M1_a*MI_human_capital+
                  (1|idwife_a), 
                data=d, 
                family = lognormal(),
                prior = c(set_prior("normal(0,2)", class = "b")),
                warmup = 1000, iter = 5000, chains = 4,
                control = list(adapt_delta = 0.95,
                               max_treedepth=13))

print(summary(model2.1, prob=0.95,priors=TRUE), digits = 6)

# Population-Level Effects: 
#                                     Estimate Est.Error  l-95% CI  u-95% CI     Rhat Bulk_ESS Tail_ESS
# Intercept                           1.862970  0.052925  1.759079  1.966258 1.000078    19424    13207
# Kids_a                              0.013928  0.010254 -0.005989  0.033913 1.000212    16213    13036
# Mothers_kids_a                      0.006313  0.005075 -0.003635  0.016229 1.000319    28814    12894
# gender_F0_M1_a                     -0.033352  0.029994 -0.092823  0.025230 0.999931    24809    12600
# MI_geo_proximity                   -0.021335  0.027145 -0.074501  0.031530 0.999997    13640    12146
# MI_economic_capital                 0.079018  0.021804  0.036018  0.121470 1.000099    13472    12259
# MI_human_capital                    0.024133  0.025777 -0.026626  0.074751 1.000287    13055    12759
# gender_F0_M1_a:MI_geo_proximity     0.015495  0.038236 -0.059660  0.090249 1.000271    13679    11748
# gender_F0_M1_a:MI_economic_capital -0.065187  0.033431 -0.131447 -0.000156 1.000129    14503    12638
# gender_F0_M1_a:MI_human_capital    -0.018205  0.034860 -0.086524  0.049544 1.000087    16391    12221

#1) No sex differences in number of relatives

filename <- "relatives_in_NW_lognormal_intx_sex.rds"

saveRDS(model2.1, paste0(path, filename))


# Model 2.2 Non-relatives in Network
library(tidyverse)
library(brms)
library(readr)


d <- data1[c(2,29,6,8,9,25:27)] 
d <- d[complete.cases(d), ] 

#d$non_rels<- d$non_rels+0.01
## run as Negative bionomial
model2.2 <- brm(non_rels ~ Kids_a+
                  Mothers_kids_a+
                  gender_F0_M1_a*MI_geo_proximity+
                  gender_F0_M1_a*MI_economic_capital+
                  gender_F0_M1_a*MI_human_capital+
                  (1|idwife_a), 
                data=d,
                family = negbinomial(link = "log", link_shape = "identity"),
                prior = c(set_prior("normal(0,2)", class = "b")),
                warmup = 1000, iter = 5000, chains = 4,
                control = list(adapt_delta = 0.95,
                               max_treedepth=13))

print(summary(model2.2, prob=0.95,priors=TRUE), digits = 6)

# Population-Level Effects: 
#                                     Estimate Est.Error  l-95% CI u-95% CI     Rhat Bulk_ESS Tail_ESS
# Intercept                          -0.013486  0.098157 -0.207598 0.179614 1.000616    18111    11539
# Kids_a                             -0.016807  0.018809 -0.054167 0.020072 1.000291    17907    12892
# Mothers_kids_a                      0.012913  0.008207 -0.003040 0.028851 0.999936    25569    13104
# gender_F0_M1_a                      1.536917  0.051116  1.437059 1.638088 1.000102    21609    12605
# MI_geo_proximity                    0.053621  0.054748 -0.055623 0.157282 1.000409    13399    11595
# MI_economic_capital                -0.023776  0.046981 -0.117032 0.067135 1.000149    14206    12741
# MI_human_capital                    0.114683  0.053711  0.010321 0.218384 1.000165    12517    12191
# gender_F0_M1_a:MI_geo_proximity    -0.111857  0.067989 -0.243164 0.021294 1.000287    13677    12955
# gender_F0_M1_a:MI_economic_capital  0.108205  0.057008 -0.002813 0.220664 1.000240    15242    12228
# gender_F0_M1_a:MI_human_capital     0.083394  0.059524 -0.033694 0.201644 1.000305    16312    11755

#1) Males have alot more non-rels in their NW's
#2) MI (human capital and econ) may exacerbate this difference
## MI increases non-relatives in males more than in women (positive intx means increasing differences
# with MI)
path<- (paste0("results/"))
filename <- "non_relatives_in_NW_neg_bin_intx_sex.rds"

saveRDS(model2.2, paste0(path, filename))

# Geographic location models
## Distance from non-relatives
#Husband's first
data2 <-  husbands_new %>% filter (Respondent_a=="Wife")
data3 <-  husbands_new %>% filter (Respondent_a=="Husband")
HusbandNW<- read_csv("C:/Users/robert/Dropbox/PSU postdoc/Access text files Bangladesh/HusQnetworkDeID.csv")

# key variables are location and relationship
HusbandNW$relationship <- as.numeric(HusbandNW$relationship)
plyr::count(HusbandNW$relationship)


HusbandNW$relationship[is.na(HusbandNW$relationship)]<- 99
HusbandNW$location[HusbandNW$location == 0] <- NA
HusbandNW$location[HusbandNW$location >5 ] <- NA
plyr::count(HusbandNW$location)

## add id_wife to z
z <- HusbandNW %>% dplyr::select (1,2,3,5,7)

# get location of non relatives
nr <- z %>% filter (relationship==0)
r <- z %>% filter (relationship>0 & relationship<8)

# get non relatives
# join non-rels to data3 (d)
non_rels <- nr %>% left_join (data3, by=c("id_Questionaire"="idhusband"))
#non_rels <- non_rels[complete.cases(non_rels),]

non_rels$location[non_rels$location==1|non_rels$location==2] <- 2
non_rels$location[non_rels$location==3] <- 3
non_rels$location[non_rels$location==4|non_rels$location==5] <- 4
# Location Codes
# 1=khana member,
# 2=near bari/neighbor
# 3=other place in Matlab
# 4=Other Place in Bangladesh
# 5=Abroad
non_rels<- non_rels[c(5,10,29,30,31,12,13,1)]

## Wives next
WifeNW <-  read_csv("C:/Users/robert/Dropbox/PSU postdoc/Access text files Bangladesh/HHQPeopleinNW.csv")

# key variables are location and relationship
WifeNW$relationship <- as.numeric(WifeNW$relationship)
plyr::count(WifeNW$relationship)
WifeNW$relationship[is.na(WifeNW$relationship)]<- 99
WifeNW$location[WifeNW$location == 0] <- NA
WifeNW$location[WifeNW$location >5 ] <- NA
plyr::count(WifeNW$location)


z <- WifeNW %>% dplyr::select (2,3,6,8)

# get location of non relatives
nr <- z %>% filter (relationship==0)
r <- z %>% filter (relationship>0 & relationship<8)

# get non relatives
# join non-rels to data (d)
non_rels2 <- nr %>% left_join (data2, by=c("id_Questionaire"="idwife_a"))
#non_rels2 <- non_rels2[complete.cases(non_rels2),]

non_rels2$location[non_rels2$location==1|non_rels2$location==2] <- 2
non_rels2$location[non_rels2$location==3] <- 3
non_rels2$location[non_rels2$location==4|non_rels2$location==5] <- 4

non_rels2<- non_rels2[c(4,9,28:30,11,12,1)]
### join non-rels to nonrels2 

d <- rbind(non_rels,non_rels2) %>% as.data.frame()
# Make model in brms  
library(brms)
model3.1 <- brm(data = d, 
                family = cumulative("logit"),
                location ~ 1+
                  gender_F0_M1_a*MI_geo_proximity+
                  gender_F0_M1_a*MI_economic_capital+
                  gender_F0_M1_a*MI_human_capital +
                  Kids_a+
                  Mothers_kids_a+
                  (1|id_Questionaire),
                prior = c(prior(normal(0, 1.5), class = Intercept),
                          prior(normal(0, 0.5), class = b)),
                iter = 5000, warmup = 1000, cores = 4, chains = 4,
                control = list(adapt_delta = 0.95,
                               max_treedepth=13))

print(model3.1)
# Population-Level Effects: 
#                                       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept[1]                          -9.27      0.69   -10.67    -7.97 1.00     7397    10159
# Intercept[2]                           2.04      0.39     1.29     2.82 1.00     5175     9360
# Intercept[3]                           3.62      0.40     2.85     4.41 1.00     5271     9262
# gender_F0_M1_a                         0.29      0.21    -0.11     0.70 1.00     6430     9876
# MI_geo_proximity                       0.26      0.20    -0.12     0.65 1.00    11041    12597
# MI_economic_capital                    0.19      0.17    -0.15     0.53 1.00     7668    10012
# MI_human_capital                       0.19      0.20    -0.19     0.58 1.00     6721    10005
# Kids_a                                -0.07      0.08    -0.22     0.08 1.00     5473     9229
# Mothers_kids_a                        -0.04      0.03    -0.10     0.03 1.00     4659     8127
# gender_F0_M1_a:MI_geo_proximity       -0.31      0.25    -0.80     0.18 1.00    10437    10998
# gender_F0_M1_a:MI_economic_capital    -0.01      0.21    -0.42     0.40 1.00     5887     9112
# gender_F0_M1_a:MI_human_capital        0.73      0.22     0.31     1.17 1.00     5503     9126


#1) No gender diffs in distance to non-relatives: Wealthy males or poor women live further from non-relatives 
path<- (paste0("results/"))
filename <- "Geo_distance_non_relatives_intx_sex.rds"

saveRDS(model3.1, paste0(path, filename))
#########################################################################
# Geographic location models
## Distance from relatives
#Husband's first
HusbandNW<- read_csv("C:/Users/robert/Dropbox/PSU postdoc/Access text files Bangladesh/HusQnetworkDeID.csv")

# key variables are location and relationship
HusbandNW$relationship <- as.numeric(HusbandNW$relationship)
plyr::count(HusbandNW$relationship)


HusbandNW$relationship[is.na(HusbandNW$relationship)]<- 99
HusbandNW$location[HusbandNW$location == 0] <- NA
HusbandNW$location[HusbandNW$location >5 ] <- NA
plyr::count(HusbandNW$location)

## add id_wife to z
z <- HusbandNW %>% dplyr::select (1,2,3,5,7)

# get location of non relatives
nr <- z %>% filter (relationship==0)
r <- z %>% filter (relationship>0 & relationship<8)

# get non relatives
# join non-rels to data3 (d)
rels <- r %>% left_join (data3, by=c("id_Questionaire"="idhusband"))
#non_rels <- non_rels[complete.cases(non_rels),]

rels$location[rels$location==1|rels$location==2] <- 2
rels$location[rels$location==3] <- 3
rels$location[rels$location==4|rels$location==5] <- 4
# Location Codes
# 1=khana member,
# 2=near bari/neighbor
# 3=other place in Matlab
# 4=Other Place in Bangladesh
# 5=Abroad
rels<- rels[c(5,10,29,30,31,12,13,1)]

## Wives next
WifeNW <-  read_csv("C:/Users/robert/Dropbox/PSU postdoc/Access text files Bangladesh/HHQPeopleinNW.csv")

# key variables are location and relationship
WifeNW$relationship <- as.numeric(WifeNW$relationship)
plyr::count(WifeNW$relationship)
WifeNW$relationship[is.na(WifeNW$relationship)]<- 99
WifeNW$location[WifeNW$location == 0] <- NA
WifeNW$location[WifeNW$location >5 ] <- NA
plyr::count(WifeNW$location)


z <- WifeNW %>% dplyr::select (2,3,6,8)

# get location of non relatives
nr <- z %>% filter (relationship==0)
r <- z %>% filter (relationship>0 & relationship<8)

# get non relatives
# join non-rels to data (d)
rels2 <- r %>% left_join (data2, by=c("id_Questionaire"="idwife_a"))
#non_rels2 <- non_rels2[complete.cases(non_rels2),]

rels2$location[rels2$location==1|rels2$location==2] <- 2
rels2$location[rels2$location==3] <- 3
rels2$location[rels2$location==4|rels2$location==5] <- 4

rels2<- rels2[c(4,9,28:30,11,12,1)]
### join non-rels to nonrels2 

d <- rbind(rels,rels2) %>% as.data.frame()
# Make model in brms  
library(brms)
model3.2 <- brm(data = d, 
                family = cumulative("logit"),
                location ~ 1+
                  gender_F0_M1_a*MI_geo_proximity+
                  gender_F0_M1_a*MI_economic_capital+
                  gender_F0_M1_a*MI_human_capital +
                  Kids_a+
                  Mothers_kids_a+
                  (1|id_Questionaire),
                prior = c(prior(normal(0, 1.5), class = Intercept),
                          prior(normal(0, 0.5), class = b)),
                iter = 5000, warmup = 1000, cores = 4, chains = 4,
                control = list(adapt_delta = 0.95,
                               max_treedepth=13))

print(model3.2)
# Population-Level Effects: 
#                                       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept[1]                          -7.82      0.52    -8.93    -6.88 1.00     9169     8937
# Intercept[2]                           1.63      0.12     1.39     1.88 1.00     8942    11935
# Intercept[3]                           2.39      0.13     2.14     2.64 1.00     9029    12031
# gender_F0_M1_a                         0.36      0.07     0.22     0.50 1.00     9042    11439
# MI_geo_proximity                       0.08      0.06    -0.04     0.20 1.00     8117    10997
# MI_economic_capital                    0.11      0.05     0.02     0.21 1.00     7353    10992
# MI_human_capital                       0.10      0.06    -0.01     0.22 1.00     7323    10558
# Kids_a                                 0.05      0.02     0.01     0.10 1.00     8901    11341
# Mothers_kids_a                         0.02      0.01    -0.00     0.04 1.00     9384    11585
# gender_F0_M1_a:MI_geo_proximity       -0.15      0.09    -0.32     0.03 1.00     8268    11694
# gender_F0_M1_a:MI_economic_capital    -0.02      0.08    -0.17     0.13 1.00     7612    10266
# gender_F0_M1_a:MI_human_capital       -0.18      0.08    -0.34    -0.03 1.00     8119    10990

#1) ****Males live further from relatives than females****
#2) Wealthier females or poorer males live further from relatives:  
#MI (Wealth and geo proximity) diminishes the male/ female effect

path<- (paste0("results/"))
filename <- "Geo_distance_relatives_intx_sex.rds"

saveRDS(model3.2, paste0(path, filename))

#### Emotional support
library(tidyverse)
library(brms)
library(readr)
library(sjPlot)
library(sjmisc)
library(sjlabelled)                             # Install & load scales
library("scales")

husbands_new <- read.csv("C:/Users/robert/Dropbox/Github/Intensive extensive kin networks/data/husbands_new.csv", header = T, sep = ",")

husbands_new$percent_rels_in_NW<- as.numeric(husbands_new$percent_rels_in_NW)



data1 <-  husbands_new %>% filter (Respondent_a=="Wife" | Respondent_a=="Husband")


library(readr)

d<-data1[c(2,28,6,8,9,25:27)]

d$NW_total <- as.numeric(d$NW_total)
### try model
model1 <- brm(NW_total ~ Kids_a+
                Mothers_kids_a+
                gender_F0_M1_a*MI_geo_proximity+
                gender_F0_M1_a*MI_economic_capital+
                gender_F0_M1_a*MI_human_capital+
                (1|idwife_a), 
              data=d, 
              family = lognormal(),
              prior = c(set_prior("normal(0,2)", class = "b")),
              warmup = 1000, iter = 5000, chains = 4,
              control = list(adapt_delta = 0.95))

print(summary(model1, prob=0.95,priors=TRUE), digits = 6)

# Population-Level Effects: 
#                                     Estimate Est.Error  l-95% CI u-95% CI     Rhat Bulk_ESS Tail_ESS
# Intercept                           2.057932  0.034594  1.990140 2.125561 0.999969    18238    12472
# Kids_a                              0.003415  0.006726 -0.009952 0.016402 1.000140    17119    12760
# Mothers_kids_a                      0.006963  0.003344  0.000419 0.013469 1.000192    22953    12420
# gender_F0_M1_a                      0.366119  0.019371  0.328018 0.404451 1.001047    19528    11674
# MI_geo_proximity                   -0.017496  0.017652 -0.052306 0.017235 1.000037    15429    12061
# MI_economic_capital                 0.060921  0.014058  0.033622 0.088686 1.000346    15616    12312
# MI_human_capital                    0.036787  0.016712  0.003873 0.069849 1.000422    15831    12103
# gender_F0_M1_a:MI_geo_proximity     0.003603  0.024600 -0.044054 0.051985 1.000248    15063    12269
# gender_F0_M1_a:MI_economic_capital -0.002846  0.021157 -0.044297 0.038198 1.000032    16290    13308
# gender_F0_M1_a:MI_human_capital     0.054973  0.022393  0.011121 0.099116 1.000250    19703    13293


#1) Males have larger networks
#2) More educated, wealthier and households closer to markets are all 
#predicted to have larger social networks
#3) Education exacerbates these differences - men are gaining the benefits of Human capital MI
# with expanding social NW's
setwd("C:/Users/robert/Dropbox/Github/Kin_networks_sex_diffs")
path<- (paste0("results/"))
filename <- "NW_total_lognormal_intx_sex.rds"

saveRDS(model1, paste0(path, filename))


### Percent relatives run
d <- data1[c(2,38,6,8,9,25:27)] 
# 
d$percent_rels_in_NW <- as.numeric(d$percent_rels_in_NW)
d$percent_rels_in_NW<- d$percent_rels_in_NW+0.01
d <- d[complete.cases(d), ] 
## run as log normal  
model2 <- brm(percent_rels_in_NW ~ Kids_a+
                Mothers_kids_a+
                gender_F0_M1_a*MI_geo_proximity+
                gender_F0_M1_a*MI_economic_capital+
                gender_F0_M1_a*MI_human_capital+
                (1|idwife_a), 
              data=d, 
              family = lognormal(),
              prior = c(set_prior("normal(0,2)", class = "b")),
              warmup = 1000, iter = 5000, chains = 4,
              control = list(adapt_delta = 0.95,
                             max_treedepth=13))



print(summary(model2, prob=0.95,priors=TRUE), digits = 6)

# Population-Level Effects: 
#                                    Estimate Est.Error  l-95% CI  u-95% CI     Rhat Bulk_ESS Tail_ESS
# Intercept                          -0.177347  0.034366 -0.245237 -0.110244 1.000010    22276    13564
# Kids_a                              0.009264  0.006699 -0.003920  0.022346 1.000396    19541    13539
# Mothers_kids_a                     -0.000635  0.003287 -0.007123  0.005781 1.000054    25458    12601
# gender_F0_M1_a                     -0.387176  0.019640 -0.425631 -0.348446 1.000158    29241    11607
# MI_geo_proximity                   -0.005402  0.017538 -0.039420  0.028665 1.000418    16734    12386
# MI_economic_capital                 0.015799  0.013952 -0.011426  0.042957 1.000380    19795    12827
# MI_human_capital                   -0.011581  0.016738 -0.044501  0.021139 1.000614    16872    12585
# gender_F0_M1_a:MI_geo_proximity     0.014299  0.024547 -0.034403  0.061894 1.000063    17783    12618
# gender_F0_M1_a:MI_economic_capital -0.050179  0.021528 -0.092611 -0.008159 1.000030    21184    12330
# gender_F0_M1_a:MI_human_capital    -0.070654  0.022594 -0.115528 -0.026625 1.000280    19812    12750


#1) Men have a fewer percentage of relative in their Networks
#2) Increasing Human capital and wealth reduces this difference between men and women such
# that more integrated households have more similar levels of kin density between men and women
path<- (paste0("results/"))
filename <- "percent_relatives_in_NW_intx_sex.rds"
setwd("C:/Users/robert/Dropbox/Github/Kin_networks_sex_diffs")
saveRDS(model2, paste0(path, filename))

# Model 2.1 Relatives in Network

library(tidyverse)
library(brms)
library(readr)

d <- data1[c(2,37,6,8,9,25:27)] 
d <- d[complete.cases(d), ] 

d$rels_in_NW<- d$rels_in_NW+0.01
## run as log normal  
model2.1 <- brm(rels_in_NW ~ Kids_a+
                  Mothers_kids_a+
                  gender_F0_M1_a*MI_geo_proximity+
                  gender_F0_M1_a*MI_economic_capital+
                  gender_F0_M1_a*MI_human_capital+
                  (1|idwife_a), 
                data=d, 
                family = lognormal(),
                prior = c(set_prior("normal(0,2)", class = "b")),
                warmup = 1000, iter = 5000, chains = 4,
                control = list(adapt_delta = 0.95,
                               max_treedepth=13))

print(summary(model2.1, prob=0.95,priors=TRUE), digits = 6)

# Population-Level Effects: 
#                                     Estimate Est.Error  l-95% CI  u-95% CI     Rhat Bulk_ESS Tail_ESS
# Intercept                           1.862970  0.052925  1.759079  1.966258 1.000078    19424    13207
# Kids_a                              0.013928  0.010254 -0.005989  0.033913 1.000212    16213    13036
# Mothers_kids_a                      0.006313  0.005075 -0.003635  0.016229 1.000319    28814    12894
# gender_F0_M1_a                     -0.033352  0.029994 -0.092823  0.025230 0.999931    24809    12600
# MI_geo_proximity                   -0.021335  0.027145 -0.074501  0.031530 0.999997    13640    12146
# MI_economic_capital                 0.079018  0.021804  0.036018  0.121470 1.000099    13472    12259
# MI_human_capital                    0.024133  0.025777 -0.026626  0.074751 1.000287    13055    12759
# gender_F0_M1_a:MI_geo_proximity     0.015495  0.038236 -0.059660  0.090249 1.000271    13679    11748
# gender_F0_M1_a:MI_economic_capital -0.065187  0.033431 -0.131447 -0.000156 1.000129    14503    12638
# gender_F0_M1_a:MI_human_capital    -0.018205  0.034860 -0.086524  0.049544 1.000087    16391    12221

#1) No sex differences in number of relatives
#2) Wealthier households have women with more relatives in the networks (and men with fewer relatives in the networks)
path<- (paste0("results/"))
filename <- "relatives_in_NW_lognormal_intx_sex.rds"

saveRDS(model2.1, paste0(path, filename))


# Model 2.2 Non-relatives in Network
library(tidyverse)
library(brms)
library(readr)


d <- data1[c(2,29,6,8,9,25:27)] 
d <- d[complete.cases(d), ] 

#d$non_rels<- d$non_rels+0.01
## run as Negative bionomial
model2.2 <- brm(non_rels ~ Kids_a+
                  Mothers_kids_a+
                  gender_F0_M1_a*MI_geo_proximity+
                  gender_F0_M1_a*MI_economic_capital+
                  gender_F0_M1_a*MI_human_capital+
                  (1|idwife_a), 
                data=d,
                family = negbinomial(link = "log", link_shape = "identity"),
                prior = c(set_prior("normal(0,2)", class = "b")),
                warmup = 1000, iter = 5000, chains = 4,
                control = list(adapt_delta = 0.95,
                               max_treedepth=13))

print(summary(model2.2, prob=0.95,priors=TRUE), digits = 6)

# Population-Level Effects: 
#                                     Estimate Est.Error  l-95% CI u-95% CI     Rhat Bulk_ESS Tail_ESS
# Intercept                          -0.013486  0.098157 -0.207598 0.179614 1.000616    18111    11539
# Kids_a                             -0.016807  0.018809 -0.054167 0.020072 1.000291    17907    12892
# Mothers_kids_a                      0.012913  0.008207 -0.003040 0.028851 0.999936    25569    13104
# gender_F0_M1_a                      1.536917  0.051116  1.437059 1.638088 1.000102    21609    12605
# MI_geo_proximity                    0.053621  0.054748 -0.055623 0.157282 1.000409    13399    11595
# MI_economic_capital                -0.023776  0.046981 -0.117032 0.067135 1.000149    14206    12741
# MI_human_capital                    0.114683  0.053711  0.010321 0.218384 1.000165    12517    12191
# gender_F0_M1_a:MI_geo_proximity    -0.111857  0.067989 -0.243164 0.021294 1.000287    13677    12955
# gender_F0_M1_a:MI_economic_capital  0.108205  0.057008 -0.002813 0.220664 1.000240    15242    12228
# gender_F0_M1_a:MI_human_capital     0.083394  0.059524 -0.033694 0.201644 1.000305    16312    11755

#1) Males have alot more non-rels in their NW's
#2) Education increases the number of non-relatives in networks of men and women
#3) MI (human capital and econ) may exacerbate this difference
## MI increases non-relatives
path<- (paste0("results/"))
filename <- "non_relatives_in_NW_neg_bin_intx_sex.rds"

saveRDS(model2.2, paste0(path, filename))


#### Geo distance relatives
#model 3.1

# Population-Level Effects: 
#                                       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS
# Intercept[1]                          -7.82      0.52    -8.93    -6.88 1.00     9169
# Intercept[2]                           1.63      0.12     1.39     1.88 1.00     8942
# Intercept[3]                           2.39      0.13     2.14     2.64 1.00     9029
# gender_F0_M1_a                         0.36      0.07     0.22     0.50 1.00     9042
# MI_geo_proximity                       0.08      0.06    -0.04     0.20 1.00     8117
# MI_economic_capital                    0.11      0.05     0.02     0.21 1.00     7353
# MI_human_capital                       0.10      0.06    -0.01     0.22 1.00     7323
# Kids_a                                 0.05      0.02     0.01     0.10 1.00     8901
# Mothers_kids_a                         0.02      0.01    -0.00     0.04 1.00     9384
# gender_F0_M1_a:MI_geo_proximity       -0.15      0.09    -0.32     0.03 1.00     8268
# gender_F0_M1_a:MI_economic_capital    -0.02      0.08    -0.17     0.13 1.00     7612
# gender_F0_M1_a:MI_human_capital       -0.18      0.08    -0.34    -0.03 1.00     8119

#model3.2
#### Geo distance non-relatives
# Population-Level Effects: 
#                                     Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS
# Intercept[1]                          -9.27      0.69   -10.67    -7.97 1.00     7397
# Intercept[2]                           2.04      0.39     1.29     2.82 1.00     5175
# Intercept[3]                           3.62      0.40     2.85     4.41 1.00     5271
# gender_F0_M1_a                         0.29      0.21    -0.11     0.70 1.00     6430
# MI_geo_proximity                       0.26      0.20    -0.12     0.65 1.00    11041
# MI_economic_capital                    0.19      0.17    -0.15     0.53 1.00     7668
# MI_human_capital                       0.19      0.20    -0.19     0.58 1.00     6721
# Kids_a                                -0.07      0.08    -0.22     0.08 1.00     5473
# Mothers_kids_a                        -0.04      0.03    -0.10     0.03 1.00     4659
# gender_F0_M1_a:MI_geo_proximity       -0.31      0.25    -0.80     0.18 1.00    10437
# gender_F0_M1_a:MI_economic_capital    -0.01      0.21    -0.42     0.40 1.00     5887
# gender_F0_M1_a:MI_human_capital        0.73      0.22     0.31     1.17 1.00     5503


#### Financial help
library(tidyverse)
library(brms)
library(readr)
library(sjPlot)
library(sjmisc)
library(sjlabelled)                             # Install & load scales
library("scales")

husbands_new <- read.csv("C:/Users/robert/Dropbox/Github/Intensive extensive kin networks/data/husbands_new.csv", header = T, sep = ",")

husbands_new$percent_rels_in_NW<- as.numeric(husbands_new$percent_rels_in_NW)

data1 <-  husbands_new %>% filter (Respondent_a=="Wife" | Respondent_a=="Husband")


library(readr)
### Percent relatives econ help
d<-data1[c(2,41,6,8,9,25:27)]

d$percent_rels_econ_help <- as.numeric(d$percent_rels_econ_help)
d$percent_rels_econ_help<- d$percent_rels_econ_help+0.01
d <- d[complete.cases(d), ] 
### try model
model4 <- brm(percent_rels_econ_help ~ Kids_a+
                Mothers_kids_a+
                gender_F0_M1_a*MI_geo_proximity+
                gender_F0_M1_a*MI_economic_capital+
                gender_F0_M1_a*MI_human_capital+
                (1|idwife_a), 
              data=d, 
              family = lognormal(),
              prior = c(set_prior("normal(0,2)", class = "b")),
              warmup = 1000, iter = 5000, chains = 4,
              control = list(adapt_delta = 0.95))

print(summary(model4, prob=0.95,priors=TRUE), digits = 6)

# Population-Level Effects: 
#                                     Estimate Est.Error  l-95% CI  u-95% CI     Rhat Bulk_ESS
# Intercept                          -0.371322  0.108700 -0.583832 -0.160046 1.000144    25405
# Kids_a                              0.025927  0.020965 -0.015363  0.066818 1.000067    22685
# Mothers_kids_a                     -0.000371  0.010572 -0.020845  0.020404 1.000094    29805
# gender_F0_M1_a                     -0.459546  0.063829 -0.586700 -0.333190 1.000460    31039
# MI_geo_proximity                   -0.004819  0.054915 -0.112918  0.103918 1.000505    21625
# MI_economic_capital                -0.047249  0.044581 -0.134272  0.040580 1.000275    22635
# MI_human_capital                    0.020010  0.052146 -0.083514  0.120951 1.000283    18853
# gender_F0_M1_a:MI_geo_proximity     0.065656  0.078784 -0.088853  0.219323 1.000168    20979
# gender_F0_M1_a:MI_economic_capital  0.005658  0.071221 -0.133667  0.144785 1.000460    22422
# gender_F0_M1_a:MI_human_capital    -0.301154  0.073046 -0.444278 -0.157261 1.000163    25478

#1) Females get a higher percentage of financial support from relatives
#2) More educated  men get a lower percentage of financial support from relatives
setwd("C:/Users/robert/Dropbox/Github/Kin_networks_sex_diffs")
path<- (paste0("results/"))
filename <- "Percent_rels_econ_help_intx_sex.rds"

saveRDS(model4, paste0(path, filename))


## rels econ help
d <- data1[c(2,39,6,8,9,25:27)] 
# 
d$rels_econ_help<-d$rels_econ_help+0.01
d <- d[complete.cases(d), ] 
## run as log normal  
model4.1 <- brm(rels_econ_help~ Kids_a+
                Mothers_kids_a+
                gender_F0_M1_a*MI_geo_proximity+
                gender_F0_M1_a*MI_economic_capital+
                gender_F0_M1_a*MI_human_capital+
                (1|idwife_a), 
              data=d, 
              family = lognormal(),
              prior = c(set_prior("normal(0,2)", class = "b")),
              warmup = 1000, iter = 5000, chains = 4,
              control = list(adapt_delta = 0.95,
                             max_treedepth=13))



print(summary(model4.1, prob=0.95,priors=TRUE), digits = 6)

# Population-Level Effects: 
#                                     Estimate Est.Error  l-95% CI  u-95% CI     Rhat Bulk_ESS
# Intercept                           0.134882  0.224691 -0.304190  0.580557 1.000142    27488
# Kids_a                              0.019448  0.042518 -0.064475  0.102710 0.999970    24382
# Mothers_kids_a                      0.008813  0.021488 -0.032368  0.051114 1.000307    34542
# gender_F0_M1_a                     -0.131211  0.126811 -0.381275  0.113238 1.000330    32567
# MI_geo_proximity                   -0.012435  0.113797 -0.232813  0.212099 1.000164    19050
# MI_economic_capital                -0.106520  0.091435 -0.287003  0.072523 1.000210    20858
# MI_human_capital                    0.499560  0.106088  0.288994  0.706172 1.000108    19139
# gender_F0_M1_a:MI_geo_proximity     0.043322  0.161910 -0.274318  0.360589 1.000063    20486
# gender_F0_M1_a:MI_economic_capital  0.165898  0.141816 -0.116022  0.444964 1.000107    21931
# gender_F0_M1_a:MI_human_capital    -0.722276  0.146738 -1.010707 -0.433276 1.000126    24315

#1) More educated women or less educated men get more financial helps from relatives
path<- (paste0("results/"))
filename <- "relatives_econ_help_intx_sex.rds"
setwd("C:/Users/robert/Dropbox/Github/Kin_networks_sex_diffs")
saveRDS(model4.1, paste0(path, filename))


# Model 4.2 Non-relatives in Network
library(tidyverse)
library(brms)
library(readr)


d <- data1[c(2,40,6,8,9,25:27)] 
d <- d[complete.cases(d), ] 

d$non_rels_econ_help<- d$non_rels_econ_help+0.01
## run as Negative bionomial
model4.2 <- brm(non_rels_econ_help ~ Kids_a+
                  Mothers_kids_a+
                  gender_F0_M1_a*MI_geo_proximity+
                  gender_F0_M1_a*MI_economic_capital+
                  gender_F0_M1_a*MI_human_capital+
                  (1|idwife_a), 
                data=d, 
                family = lognormal(),
                prior = c(set_prior("normal(0,2)", class = "b")),
                warmup = 1000, iter = 5000, chains = 4,
                control = list(adapt_delta = 0.95))

print(summary(model4.2, prob=0.95,priors=TRUE), digits = 6)
# 
# Population-Level Effects: 
#                                      Estimate Est.Error  l-95% CI  u-95% CI     Rhat Bulk_ESS
# Intercept                          -3.544514  0.221040 -3.981166 -3.109000 1.000046    25195
# Kids_a                             -0.092495  0.042523 -0.175370 -0.009819 1.000094    23247
# Mothers_kids_a                      0.030950  0.021084 -0.010673  0.072445 1.000098    31295
# gender_F0_M1_a                      1.431481  0.124078  1.185202  1.672675 1.000682    35461
# MI_geo_proximity                    0.004081  0.109414 -0.212003  0.218163 1.000487    20974
# MI_economic_capital                 0.067250  0.089651 -0.109645  0.243735 1.000165    22291
# MI_human_capital                    0.046310  0.106038 -0.163429  0.252861 1.000084    18990
# gender_F0_M1_a:MI_geo_proximity    -0.184721  0.154247 -0.490157  0.117852 1.000131    20729
# gender_F0_M1_a:MI_economic_capital  0.229387  0.137863 -0.045108  0.499582 1.000172    25780
# gender_F0_M1_a:MI_human_capital     0.609844  0.142034  0.328744  0.889404 0.999895    27187


#1) Males get more financial help from non-relatives
#2) More educated males in particular get even more financial help from non-relatives
path<- (paste0("results/"))
filename <- "non_relatives_econ_help_intx_sex.rds"

saveRDS(model4.2, paste0(path, filename))

#### Childcare/ work help
library(tidyverse)
library(brms)
library(readr)
library(sjPlot)
library(sjmisc)
library(sjlabelled)                     
library("scales")

husbands_new <- read.csv("C:/Users/robert/Dropbox/Github/Intensive extensive kin networks/data/husbands_new.csv", header = T, sep = ",")
data1 <-  husbands_new %>% filter (Respondent_a=="Wife" | Respondent_a=="Husband")

### Percent relatives childcare work support
d <- data1[c(2,47,6,8,9,25:27)] 
# 
d$childcare_work_help_rels_percent <- as.numeric(d$childcare_work_help_rels_percent)
d$childcare_work_help_rels_percent<- d$childcare_work_help_rels_percent+0.01
d <- d[complete.cases(d), ] 
## run as log normal  
model5 <- brm(childcare_work_help_rels_percent ~ Kids_a+
                Mothers_kids_a+
                gender_F0_M1_a*MI_geo_proximity+
                gender_F0_M1_a*MI_economic_capital+
                gender_F0_M1_a*MI_human_capital+
                (1|idwife_a), 
              data=d, 
              family = lognormal(),
              prior = c(set_prior("normal(0,2)", class = "b")),
              warmup = 1000, iter = 5000, chains = 4,
              control = list(adapt_delta = 0.95,
                             max_treedepth=13))

print(summary(model5, prob=0.95,priors=TRUE), digits = 6)

# Population-Level Effects: 
#                                      Estimate Est.Error  l-95% CI  u-95% CI     Rhat
# Intercept                          -0.196094  0.110729 -0.410851  0.020248 1.000266
# Kids_a                             -0.003632  0.021119 -0.044839  0.038805 1.000466
# Mothers_kids_a                      0.017746  0.010853 -0.003531  0.038810 0.999918
# gender_F0_M1_a                     -0.918861  0.068378 -1.052333 -0.784706 0.999944
# MI_geo_proximity                   -0.017182  0.053951 -0.123549  0.087990 1.000139
# MI_economic_capital                -0.031306  0.042527 -0.114710  0.052917 1.000189
# MI_human_capital                   -0.032235  0.052100 -0.134338  0.068913 1.000112
# gender_F0_M1_a:MI_geo_proximity     0.115326  0.099073 -0.078513  0.308477 1.000095
# gender_F0_M1_a:MI_economic_capital  0.022959  0.071193 -0.116141  0.164275 1.000252
# gender_F0_M1_a:MI_human_capital    -0.780803  0.077062 -0.933041 -0.627879 1.000638


#1) Female childcare NW's are more in dense than male work NW's
#2) Wealthier women have more kin dense childcare NW's than men's work NW's??
path<- (paste0("results/"))
filename <- "percent_relatives_childcare_work_help_intx_sex.rds"
setwd("C:/Users/robert/Dropbox/Github/Kin_networks_sex_diffs")
saveRDS(model5, paste0(path, filename))

# Model 5.1 Relatives in Network

library(tidyverse)
library(brms)
library(readr)

d <- data1[c(2,45,6,8,9,25:27)] 
d <- d[complete.cases(d), ] 

d$childcare_work_help_rels<- d$childcare_work_help_rels+0.01
## run as log normal  
model5.1 <- brm(childcare_work_help_rels ~ Kids_a+
                  Mothers_kids_a+
                  gender_F0_M1_a*MI_geo_proximity+
                  gender_F0_M1_a*MI_economic_capital+
                  gender_F0_M1_a*MI_human_capital+
                  (1|idwife_a), 
                data=d, 
                family = lognormal(),
                prior = c(set_prior("normal(0,2)", class = "b")),
                warmup = 1000, iter = 5000, chains = 4,
                control = list(adapt_delta = 0.95,
                               max_treedepth=13))

print(summary(model5.1, prob=0.95,priors=TRUE), digits = 6)

# Population-Level Effects: 
#                                      Estimate Est.Error  l-95% CI  u-95% CI     Rhat
# Intercept                           0.692094  0.213320  0.271722  1.114244 1.000101
# Kids_a                             -0.026974  0.041099 -0.107110  0.053836 0.999906
# Mothers_kids_a                      0.026753  0.020750 -0.013495  0.067719 1.000199
# gender_F0_M1_a                     -2.089739  0.122004 -2.326131 -1.849641 1.000392
# MI_geo_proximity                   -0.186198  0.107434 -0.397781  0.021387 0.999907
# MI_economic_capital                -0.004902  0.086166 -0.177208  0.163990 1.000034
# MI_human_capital                   -0.109681  0.103260 -0.311272  0.091896 0.999929
# gender_F0_M1_a:MI_geo_proximity     0.045168  0.151776 -0.254374  0.337719 1.000148
# gender_F0_M1_a:MI_economic_capital  0.070687  0.134082 -0.189334  0.337644 1.000168
# gender_F0_M1_a:MI_human_capital    -0.186346  0.141374 -0.462206  0.086344 1.000232

#1) Women get more help from relatives for childcare than men do for work
path<- (paste0("results/"))
filename <- "Relatives_childcare_work_help_intx_sex.rds"

saveRDS(model5.1, paste0(path, filename))


library(readr)

d<-data1[c(2,46,6,8,9,25:27)]

d$childcare_work_help_non_rels <- as.numeric(d$childcare_work_help_non_rels)
d$childcare_work_help_non_rels <- d$childcare_work_help_non_rels+0.01
### try model
model5.2 <- brm(childcare_work_help_non_rels ~ Kids_a+
                Mothers_kids_a+
                gender_F0_M1_a*MI_geo_proximity+
                gender_F0_M1_a*MI_economic_capital+
                gender_F0_M1_a*MI_human_capital+
                (1|idwife_a), 
              data=d, 
              family = lognormal(),
              prior = c(set_prior("normal(0,2)", class = "b")),
              warmup = 1000, iter = 5000, chains = 4,
              control = list(adapt_delta = 0.95))

print(summary(model5.2, prob=0.95,priors=TRUE), digits = 6)

# Population-Level Effects: 
#                                     Estimate Est.Error  l-95% CI  u-95% CI     Rhat
# Intercept                          -4.065547  0.185931 -4.431368 -3.696867 1.000770
# Kids_a                             -0.010442  0.035704 -0.080839  0.059757 1.000163
# Mothers_kids_a                     -0.023232  0.017749 -0.057999  0.011047 1.000023
# gender_F0_M1_a                      1.502295  0.106529  1.294896  1.713477 1.000539
# MI_geo_proximity                    0.029415  0.095109 -0.155675  0.219656 1.000176
# MI_economic_capital                 0.015297  0.075285 -0.131168  0.164561 1.000192
# MI_human_capital                    0.060884  0.090721 -0.115171  0.239393 1.000540
# gender_F0_M1_a:MI_geo_proximity    -0.125268  0.135576 -0.388206  0.138974 0.999939
# gender_F0_M1_a:MI_economic_capital  0.032098  0.117826 -0.202428  0.263630 1.000678
# gender_F0_M1_a:MI_human_capital     1.054984  0.123108  0.813408  1.300299 1.000481

#1) No sex difference but more educated  males get more help with work from non rels
#as compared to women getting help from non-rels for childcare
setwd("C:/Users/robert/Dropbox/Github/Kin_networks_sex_diffs")
path<- (paste0("results/"))
filename <- "Non_rels_chilcare_work_help_intx_sex.rds"

saveRDS(model5.2, paste0(path, filename))


### Percent relatives emot support
d <- data1[c(2,44,6,8,9,25:27)] 
# 
d$percent_rels_emot_support <- as.numeric(d$percent_rels_emot_support)
d$percent_rels_emot_support<- d$percent_rels_emot_support+0.01
d <- d[complete.cases(d), ] 
## run as log normal  
model6 <- brm(percent_rels_emot_support ~ Kids_a+
                Mothers_kids_a+
                gender_F0_M1_a*MI_geo_proximity+
                gender_F0_M1_a*MI_economic_capital+
                gender_F0_M1_a*MI_human_capital+
                (1|idwife_a), 
              data=d, 
              family = lognormal(),
              prior = c(set_prior("normal(0,2)", class = "b")),
              warmup = 1000, iter = 5000, chains = 4,
              control = list(adapt_delta = 0.95,
                             max_treedepth=13))

print(summary(model6, prob=0.95,priors=TRUE), digits = 6)
# Population-Level Effects: 
#                                      Estimate Est.Error  l-95% CI  u-95% CI     Rhat
# Intercept                          -0.018942  0.043400 -0.102855  0.066400 1.000366
# Kids_a                             -0.003627  0.008317 -0.019921  0.012574 1.000903
# Mothers_kids_a                     -0.001009  0.004251 -0.009324  0.007198 1.000153
# gender_F0_M1_a                     -0.282197  0.025466 -0.332066 -0.232250 1.000140
# MI_geo_proximity                    0.001280  0.022593 -0.042684  0.046223 1.000076
# MI_economic_capital                 0.005926  0.017696 -0.028636  0.040040 1.000177
# MI_human_capital                   -0.025103  0.020820 -0.066143  0.015303 1.000488
# gender_F0_M1_a:MI_geo_proximity     0.023491  0.031897 -0.038833  0.085392 0.999974
# gender_F0_M1_a:MI_economic_capital -0.061466  0.027554 -0.115447 -0.007183 1.000710
# gender_F0_M1_a:MI_human_capital    -0.033391  0.029019 -0.091168  0.024353 1.000223

#1) Females have more kin dense emotional support NW's


path<- (paste0("results/"))
filename <- "percent_relatives_emot_support_intx_sex.rds"
setwd("C:/Users/robert/Dropbox/Github/Kin_networks_sex_diffs")
saveRDS(model6, paste0(path, filename))

# Model 6.1 Relatives in Network

library(tidyverse)
library(brms)
library(readr)

d <- data1[c(2,42,6,8,9,25:27)] 
d <- d[complete.cases(d), ] 

d$emot_support_rels<- d$emot_support_rels+0.01
## run as log normal  
model6.1 <- brm(emot_support_rels ~ Kids_a+
                  Mothers_kids_a+
                  gender_F0_M1_a*MI_geo_proximity+
                  gender_F0_M1_a*MI_economic_capital+
                  gender_F0_M1_a*MI_human_capital+
                  (1|idwife_a), 
                data=d, 
                family = lognormal(),
                prior = c(set_prior("normal(0,2)", class = "b")),
                warmup = 1000, iter = 5000, chains = 4,
                control = list(adapt_delta = 0.95,
                               max_treedepth=13))

print(summary(model6.1, prob=0.95,priors=TRUE), digits = 6)

# Population-Level Effects: 
#                                      Estimate Est.Error  l-95% CI  u-95% CI     Rhat
# Intercept                           1.656889  0.087414  1.483602  1.829405 1.000283
# Kids_a                             -0.036308  0.016629 -0.069391 -0.003774 1.000054
# Mothers_kids_a                      0.016299  0.008192  0.000185  0.032393 1.000162
# gender_F0_M1_a                     -0.089194  0.049723 -0.187129  0.008039 1.000647
# MI_geo_proximity                   -0.071498  0.044070 -0.159068  0.014956 1.000052
# MI_economic_capital                 0.067503  0.035062 -0.000832  0.136585 1.000055
# MI_human_capital                   -0.003976  0.042261 -0.086770  0.078207 1.000242
# gender_F0_M1_a:MI_geo_proximity     0.120033  0.062929 -0.003031  0.243050 1.000108
# gender_F0_M1_a:MI_economic_capital  0.009371  0.054081 -0.097182  0.113996 1.000044
# gender_F0_M1_a:MI_human_capital    -0.024019  0.056079 -0.134062  0.086043 1.000231

#1) females have somewhat more relatives who provide emotional support
#2) wealthier hh's may have more relatives providing emotional support
#3) More geographically distant men have more rels providign emotional support
path<- (paste0("results/"))
filename <- "relatives_emot_support_intx_sex.rds"

saveRDS(model6.1, paste0(path, filename))


# Model 6.2 Non-relatives emotional support
library(tidyverse)
library(brms)
library(readr)

d<-data1[c(43,2,6,8,9,25:27)]

d$emot_support_non_rels <- as.integer(d$emot_support_non_rels)

## run as Negative bionomial
model6.2 <- brm(emot_support_non_rels ~ Kids_a+
                  Mothers_kids_a+
                  gender_F0_M1_a*MI_geo_proximity+
                  gender_F0_M1_a*MI_economic_capital+
                  gender_F0_M1_a*MI_human_capital+
                  (1|idwife_a), 
                data=d,
                family = negbinomial(link = "log", link_shape = "identity"),
                prior = c(set_prior("normal(0,2)", class = "b")),
                warmup = 1000, iter = 5000, chains = 4,
                control = list(adapt_delta = 0.95,
                               max_treedepth=13))

print(summary(model6.2, prob=0.95,priors=TRUE), digits = 6)

# Population-Level Effects: 
#                                      Estimate Est.Error  l-95% CI  u-95% CI     Rhat
# Intercept                          -2.470846  0.296111 -3.065661 -1.892416 1.000241
# Kids_a                              0.052464  0.049733 -0.045300  0.150319 0.999920
# Mothers_kids_a                      0.036065  0.021453 -0.006249  0.078023 1.000006
# gender_F0_M1_a                      2.284914  0.156918  1.987001  2.597305 1.000134
# MI_geo_proximity                    0.030886  0.182229 -0.347844  0.371025 1.000502
# MI_economic_capital                 0.007339  0.120624 -0.228901  0.244498 0.999992
# MI_human_capital                    0.552720  0.143167  0.274604  0.837171 0.999886
# gender_F0_M1_a:MI_geo_proximity    -0.328214  0.259676 -0.846944  0.173317 1.000315
# gender_F0_M1_a:MI_economic_capital  0.219501  0.150649 -0.078038  0.516920 0.999876
# gender_F0_M1_a:MI_human_capital    -0.134691  0.159349 -0.446662  0.176036 1.000054

#1) human capital predicts more emotional support from non relatives (both sexes)
#2) men receive more emotional support from non-rels

path<- (paste0("results/"))
filename <- "non_relatives_emot_support_intx_sex.rds"

saveRDS(model6.2, paste0(path, filename))