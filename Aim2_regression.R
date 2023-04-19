setwd("/Users/joycelin/Desktop/Gulf/Aim2/GuLF_2")
library(tidyverse)
library(dplyr)
library(Hmisc)
data <- read_csv("CE_blockgroupNEI_tertile.csv")
data <- data[-c(414:1057), ]

data$CdBinary <- as.factor(data$CdBinary) 
data$EDU<- as.factor(data$EDU)
data$CE_housetype<- as.factor(data$CE_housetype)
data$STATEFP<- as.factor(data$STATEFP)
data$Batch<- as.factor(data$Batch)
data$EN_FORMERSMOKER<- as.factor(data$EN_FORMERSMOKER)
data$quartileTHC <- as.factor(data$quartileTHC)
data$CoBinary <- as.factor(data$CoBinary)
data$MoBinary <- as.factor(data$MoBinary)
data$SbTertile <- as.factor(data$SbTertile)
data$VTertile <- as.factor(data$VTertile)
data$race <- as.factor(data$race)
data$STATEFP <- as.factor(data$STATEFP)
data$road50 <- as.factor(data$road50) #roads functional class 1, 2, 3, 4 (highways and secondary highways)
data$road100 <- as.factor(data$road100)
data$medianhouseholdincome <- as.numeric(data$medianhouseholdincome) 
data$road200 <- as.factor(data$road200)
data$HV_L2B_NUM <- as.factor(data$HV_L2B_NUM)
data$passivesmoke <- as.factor(data$passivesmoke)
data$race <- relevel(as.factor(data$race), ref = "White")
data<- data %>% mutate(income = case_when(EN_TOTINCOME >= 5 ~ '>$50,000',
                                          EN_TOTINCOME > 1 & EN_TOTINCOME<= 4 ~ '$20,000 - $49,999',
                                          EN_TOTINCOME ==1 ~ '<$20,000')) # end function

data$income <- relevel(as.factor(data$income), ref = ">$50,000")

datlong<-pivot_longer(data, 6:23, names_to = "Metal", values_to = "Concentration")
datlongmain<-datlong %>% filter(Metal %notin% low_metals) 

data<- data %>% mutate(BMI = case_when(CE_BMI <= 24.9 ~ 'healthy',
                                       CE_BMI >=25 & CE_BMI <=30 ~ 'overweight',
                                       CE_BMI >=30 ~ 'obese')) # end function

data$BMI <- factor(data$BMI)

data<- data %>% mutate(workstat = case_when(workstat == 1 ~ 1,
                                          workstat > 1 ~ 0)) # end function
data$workstat <- as.factor(data$workstat)

data<- data %>% mutate(As_idw = As10_SUMEMISSIONS/AsDistance,
                       Cr_idw = Cr10_SUMEMISSIONS/CrDistance,
                       Pb_idw = Pb10_SUMEMISSIONS/PbDistance,
                       Mn_idw = Mn10_SUMEMISSIONS/MnDistance,
                       Se_idw = Se10_SUMEMISSIONS/SeDistance,
                       Hg_idw = Hg10_SUMEMISSIONS/HgDistance) # end function

data<- data %>% mutate(PbDistance = PbDistance*100,
                       CrDistance = CrDistance*100,
                       AsDistance = AsDistance*100,
                       MnDistance = MnDistance*100,
                       SeDistance = SeDistance*100,
                       HgDistance = HgDistance*100) # end function


data_log <- data %>% mutate(across(c(Mg, Al, Ca, Cr, Mn, Fe, Ni, Cu, Zn, As, Se, Hg, Pb), log10))


# Testing each covariate in univariate models
m1<- lm(Pb ~ PbDistance, data = data_log) 
summary(m1)

m2<- lm(Pb ~ PbDistance, data = data_log) 
summary(m2)

## short cut way using map() in purrr
datapb <- data_log[, c("Pb", "race", "CE_AGE", "THC_CUMULATIVE1", "PbDistance", "Pb10_SUMEMISSIONS", "Pb5_SUMEMISSIONS", "Pb3_SUMEMISSIONS", "Pb10_pointcount","Pb5_pointcount", "Pb3_pointcount","medianyearstructurebuilt", "percentbelowpovertylevel", "medianhouseholdincome", "Totalpop", "percenthispanicllatino", "percentblack", "percentamericanindian", "percentasian", "lessthanhighschool", "owneroccupied", "CE_C1", "EN_TOTINCOME", "EN_FORMERSMOKER", "passivesmoke", "STATEFP", "road50", "road100", "road200", "CE_BMI")]

library(purrr)
library(dplyr)

d<-datapb %>%
  map(~ lm(datapb$Pb ~ ., data=.y)) %>%
  map(summary.lm) %>% map(c("coefficients"))
d

## plot individual univariate models, dot whisker
library(dotwhisker)
library(dplyr)
library(gdata)

mod1<- lm(Pb ~ PbDistance + CE_BMI + CE_AGE + EN_FORMERSMOKER + passivesmoke + road50 + THC_CUMULATIVE1 + workstat + STATEFP, data = data_log)
mod2<- lm(Pb ~ Pb_EMISSIONS + CE_BMI + CE_AGE + EN_FORMERSMOKER + passivesmoke + road50 + THC_CUMULATIVE1 + workstat + STATEFP, data = data_log)
mod3<- lm(Pb ~ Pb10_pointcount + CE_BMI + CE_AGE + EN_FORMERSMOKER + passivesmoke + road50 + THC_CUMULATIVE1 + workstat + STATEFP, data = data_log)
mod4<- lm(Pb ~ Pb10_SUMEMISSIONS + CE_BMI + CE_AGE + EN_FORMERSMOKER + passivesmoke + road50 + THC_CUMULATIVE1 + workstat + STATEFP, data = data_log)
mod5<- lm(Pb ~ Pb5_pointcount + CE_BMI + CE_AGE + EN_FORMERSMOKER + passivesmoke + road50 + THC_CUMULATIVE1 + workstat + STATEFP, data = data_log)
mod6<- lm(Pb ~ Pb5_SUMEMISSIONS + CE_BMI + CE_AGE + EN_FORMERSMOKER + passivesmoke + road50 + THC_CUMULATIVE1 + workstat + STATEFP, data = data_log)
mod7<- lm(Pb ~ Pb3_pointcount + CE_BMI + CE_AGE + EN_FORMERSMOKER + passivesmoke + road50 + THC_CUMULATIVE1 + workstat + STATEFP, data = data_log)
mod8<- lm(Pb ~ Pb3_SUMEMISSIONS + CE_BMI + CE_AGE + EN_FORMERSMOKER + passivesmoke + road50 + THC_CUMULATIVE1 + workstat + STATEFP, data = data_log)

# for plot showing all covariates
Distance <-broom::tidy(mod1) %>% filter(term != "road501")%>% filter(term != "road50")
Emissions <-broom::tidy(mod2)%>% filter(term != "road501")%>% filter(term != "road50")
Count <-broom::tidy(mod3)%>% filter(term != "road501")%>% filter(term != "road50")

plotRace_Pb<- dwplot(Count, dodge_size = 0.5, dot_args = list(aes(colour = model),size = 2), whisker_args = list(aes(colour=model), size = 1)) %>% relabel_predictors(c("raceBlack" = "Black", "PbQuartile2" = "Pb Q2", "PbQuartile3" = "Pb Q3", "PbQuartile4" = "Pb Q4"))
plotRace_Pb
plotRace_Pb1 <- plotRace_Pb + geom_vline(xintercept = 0, colour = "grey60", linetype = 2)  + theme(text = element_text(size = 12)) + labs(x = bquote('Coefficent Estimate with 95% CIs')) + theme(axis.text=element_text(size=12)) + theme(axis.title.x = element_text(margin = margin(t = 12))) 
plotRace_Pb1

## For plots comparing different Pb models

PbDistance <-broom::tidy(mod1) %>% filter(term != "road50") %>% filter(term != "road501") %>% filter(term != "CE_BMI")%>% filter(term != "CE_AGE")%>%filter(term != "EN_FORMERSMOKER1")%>%filter(term != "passivesmoke1")%>%filter(term != "THC_CUMULATIVE1")%>%filter(term != "workstat1")%>%filter(term != "STATEFP12")%>%filter(term != "STATEFP22")%>%filter(term != "STATEFP28")
Pb_EMISSIONS <-broom::tidy(mod2)%>% filter(term != "road50")  %>% filter(term != "road501")%>% filter(term != "CE_BMI")%>% filter(term != "CE_AGE")%>%filter(term != "EN_FORMERSMOKER1")%>%filter(term != "passivesmoke1")%>%filter(term != "THC_CUMULATIVE1")%>%filter(term != "workstat1")%>%filter(term != "STATEFP12")%>%filter(term != "STATEFP22")%>%filter(term != "STATEFP28")
Pb10_pointcount <-broom::tidy(mod3)%>% filter(term != "road50") %>% filter(term != "road501") %>% filter(term != "CE_BMI")%>% filter(term != "CE_AGE")%>%filter(term != "EN_FORMERSMOKER1")%>%filter(term != "passivesmoke1")%>%filter(term != "THC_CUMULATIVE1")%>%filter(term != "workstat1")%>%filter(term != "STATEFP12")%>%filter(term != "STATEFP22")%>%filter(term != "STATEFP28")
Pb10_SUMEMISSIONS <-broom::tidy(mod4)%>% filter(term != "road50")  %>% filter(term != "road501")%>% filter(term != "CE_BMI")%>% filter(term != "CE_AGE")%>%filter(term != "EN_FORMERSMOKER1")%>%filter(term != "passivesmoke1")%>%filter(term != "THC_CUMULATIVE1")%>%filter(term != "workstat1")%>%filter(term != "STATEFP12")%>%filter(term != "STATEFP22")%>%filter(term != "STATEFP28")
Pb5_pointcount <-broom::tidy(mod5)%>% filter(term != "road50")  %>% filter(term != "road501")%>% filter(term != "CE_BMI")%>% filter(term != "CE_AGE")%>%filter(term != "EN_FORMERSMOKER1")%>%filter(term != "passivesmoke1")%>%filter(term != "THC_CUMULATIVE1")%>%filter(term != "workstat1")%>%filter(term != "STATEFP12")%>%filter(term != "STATEFP22")%>%filter(term != "STATEFP28")
Pb5_SUMEMISSIONS <-broom::tidy(mod6)%>% filter(term != "road50")  %>% filter(term != "road501")%>% filter(term != "CE_BMI")%>% filter(term != "CE_AGE")%>%filter(term != "EN_FORMERSMOKER1")%>%filter(term != "passivesmoke1")%>%filter(term != "THC_CUMULATIVE1")%>%filter(term != "workstat1")%>%filter(term != "STATEFP12")%>%filter(term != "STATEFP22")%>%filter(term != "STATEFP28")
Pb3_pointcount <-broom::tidy(mod7)%>% filter(term != "road50") %>% filter(term != "road501") %>% filter(term != "CE_BMI")%>% filter(term != "CE_AGE")%>%filter(term != "EN_FORMERSMOKER1")%>%filter(term != "passivesmoke1")%>%filter(term != "THC_CUMULATIVE1")%>%filter(term != "workstat1")%>%filter(term != "STATEFP12")%>%filter(term != "STATEFP22")%>%filter(term != "STATEFP28")
Pb3_SUMEMISSIONS <-broom::tidy(mod8)%>% filter(term != "road50")  %>% filter(term != "road501")%>% filter(term != "CE_BMI")%>% filter(term != "CE_AGE")%>%filter(term != "EN_FORMERSMOKER1")%>%filter(term != "passivesmoke1")%>%filter(term != "THC_CUMULATIVE1")%>%filter(term != "workstat1")%>%filter(term != "STATEFP12")%>%filter(term != "STATEFP22")%>%filter(term != "STATEFP28")

#Distance plot
all_models <- combine(PbDistance,Pb_EMISSIONS)
plotPb<- dwplot(all_models, dodge_size = 0.5, dot_args = list(aes(colour = model),size = 2), whisker_args = list(aes(colour=model), size = 1)) %>% relabel_predictors(c("Pb10_pointcount" = "Pb 10km", "Pb5_pointcount" = "Pb 5km", "Pb3_pointcount" = "Pb 3km"))
plotPb
plotPb <- plotPb+ ggtitle("Pb Distance (km)") + geom_vline(xintercept = 0, colour = "grey60", linetype = 2) + theme(text = element_text(size = 12)) + labs(x = bquote('Coefficent Estimate with 95% CIs')) + theme(axis.text=element_text(size=12)) + theme(axis.title.x = element_text(margin = margin(t = 12))) 
plotPb

#Count plot
all_models <- combine(Pb10_pointcount,Pb5_pointcount, Pb3_pointcount)
plotPbcount<- dwplot(all_models, dodge_size = 0.5, dot_args = list(aes(colour = model),size = 2), whisker_args = list(aes(colour=model), size = 1)) %>% relabel_predictors(c("Pb10_pointcount" = "Pb 10km", "Pb5_pointcount" = "Pb 5km", "Pb3_pointcount" = "Pb 3km"))
plotPbcount
plotPbcount <- plotPbcount+ ggtitle("Site Count") + geom_vline(xintercept = 0, colour = "grey60", linetype = 2) + theme(text = element_text(size = 12)) + labs(x = bquote('Coefficent Estimate with 95% CIs')) + theme(axis.text=element_text(size=12)) + theme(axis.title.x = element_text(margin = margin(t = 12))) 
plotPbcount

#Volume plot
all_models <- combine(Pb10_SUMEMISSIONS,Pb5_SUMEMISSIONS, Pb3_SUMEMISSIONS)
plotPbvol<- dwplot(all_models, dodge_size = 0.5, dot_args = list(aes(colour = model),size = 2), whisker_args = list(aes(colour=model), size = 1)) %>% relabel_predictors(c("Pb10_SUMEMISSIONS" = "Pb 10km", "Pb5_SUMEMISSIONS" = "Pb 5km", "Pb3_SUMEMISSIONS" = "Pb 3km"))
plotPbvol
plotPbvol <- plotPbvol+ ggtitle("Emissions Volume") + geom_vline(xintercept = 0, colour = "grey60", linetype = 2) + theme(text = element_text(size = 12)) + labs(x = bquote('Coefficent Estimate with 95% CIs')) + theme(axis.text=element_text(size=12)) + theme(axis.title.x = element_text(margin = margin(t = 12)))
plotPbvol

## compile plot
library(patchwork) 
figure1<- plotPb + plotPbcount + plotPbvol + plot_layout(ncol = 3)
figure1


