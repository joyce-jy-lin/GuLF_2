setwd("/Users/joycelin/Desktop/Gulf/Aim2/GuLF_2")
library(tidyverse)
library(dplyr)

data <- read_csv("CE_NEI_TRI_IDW.csv")

data$CE_housetype<- as.factor(data$CE_housetype)
data$EN_FORMERSMOKER<- as.factor(data$EN_FORMERSMOKER)
data$quartileTHC <- as.factor(data$quartileTHC)
data$race <- as.factor(data$race)
data$STATEFP <- as.factor(data$STATEFP)
data$road50 <- as.factor(data$road50) #roads functional class 1, 2, 3, 4 (highways and secondary highways)
data$road100 <- as.factor(data$road100)
data$road200 <- as.factor(data$road200)
data$medianhouseholdincome <- as.numeric(data$medianhouseholdincome) 
data$HV_L2B_NUM <- as.factor(data$HV_L2B_NUM)
data$passivesmoke <- as.factor(data$passivesmoke)
data$race <- relevel(as.factor(data$race), ref = "White")
data$passivesmoke <- as.factor(data$passivesmoke)
data$EDU <- relevel(as.factor(data$EDU), ref = "College or more")
data<- data %>% mutate(income = case_when(EN_TOTINCOME >= 5 ~ '>$50,000',
                                          EN_TOTINCOME > 1 & EN_TOTINCOME<= 4 ~ '$20,000 - $49,999',
                                          EN_TOTINCOME ==1 ~ '<$20,000')) # end function
data$income <- relevel(as.factor(data$income), ref = ">$50,000")

datlong<-pivot_longer(data, 6:23, names_to = "Metal", values_to = "Concentration")

data<- data %>% mutate(BMI = case_when(CE_BMI <= 24.9 ~ 'healthy',
                                       CE_BMI >=25 & CE_BMI <=30 ~ 'overweight',
                                       CE_BMI >=30 ~ 'obese')) # end function
data$BMI <- factor(data$BMI)

data<- data %>% mutate(PbDistance = PbDistance*100,
                       AsDistance = AsDistance*100,
                       MnDistance = MnDistance*100,
                       HgDistance = HgDistance*100,
                       SeDistance = SeDistance*100,
                       CrDistance = CrDistance*100)

datalog <- data %>% mutate(across(c(Mg, Al, Ca, Cr, Mn, Fe, Ni, Cu, Zn, As, Se, Hg, Pb), log10))
#datalog <- datalog[-c(414:416, 394, 364), ] # for old dataset "CE_blockgroupNEI_tertile.csv"

min(datalog$AsDistancekm)

#hispanic ethnicity check
summaryH <- data %>%
  group_by(EN_HISPANIC2) %>%
  summarise(Count = n())
summaryH

## Lily and Ciprian method
datsub <- subset(datalog, select = c(CdBinary, CE_AGE, race, CE_BMI, THC_CUMULATIVE1, STATEFP, CE_C1, EN_FORMERSMOKER, passivesmoke, road50, road100, road200, CdDistance, Cd_EMISSIONS, percenthispanicllatino, percentblack, percentamericanindian, percentasian, medianhouseholdincome, renteroccupied, medianyearstructurebuilt, percentbelowpovertylevel, Cd10_pointcount, Cd10_SUMEMISSIONS, Cd5_pointcount, Cd5_SUMEMISSIONS, Cd3_pointcount, Cd3_SUMEMISSIONS))

datsub %>%
  map(~lm(datsub$CdBinary ~.x, data = datsub)) %>% 
  map(summary.lm) %>% map(c("coefficients"))


datsub %>%
  map(~glm(datsub$CdBinary ~.x, data = datsub, family = binomial)) %>% 
  map(summary.lm) %>% map(c("coefficients"))

##corrlation matrix of covariates
datacor <- data[, c("CE_AGE", "CE_BMI", "THC_CUMULATIVE1", "EN_RACE2", "CE_C1", "EN_TOTINCOME", "percenthispanicllatino", "percentblack", "percentamericanindian", "percentasian", "medianhouseholdincome", "renteroccupied", "medianyearstructurebuilt", "percentbelowpovertylevel")]
library(Hmisc)
cormat = rcorr(as.matrix(datacor))
cormat

library("ggpubr")
ggscatter(datalog, x = "percentbelowpovertylevel", y = "PbDistance", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "% below poverty level", ylab = "Pb Distance (km)")+
  ylim(-0.1, 40)

ggscatter(datalog, x = "medianyearstructurebuilt", y = "Pb", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "% below poverty level", ylab = "Pb Distance (km)")

ggscatter(datalog, x = "CE_C1", y = "EN_TOTINCOME", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Pb Distance", ylab = "Percent below pov level")

## 1.model with all significant covariates in univariate regressions, check vifs for colinearity
mod1 <- lm(Pb ~ CE_C1 + percentbelowpovertylevel + medianyearstructurebuilt + PbDistance, data = datalog)
summary(mod1)
confint(mod1)
(exp(coef(mod1))-1)*100
library(car)
vif(mod1)

## 2. parse down models
mod1 <- lm(Pb ~ CE_C1 + percentbelowpovertylevel + Pb10_pointcount, data = datalog)
summary(mod1)
confint(mod1)
(exp(coef(mod1))-1)*100
library(car)
vif(mod1)

## 
##plot metal concentration disparity by race
# start plot for NEI metals-------------------------------------------------
mod1 <- lm(Mn ~ CE_AGE + THC_CUMULATIVE1 + EN_FORMERSMOKER + passivesmoke + race + CE_BMI + workstat + STATEFP, data = datalog)
mod2 <- lm(Se ~ CE_AGE + THC_CUMULATIVE1 + EN_FORMERSMOKER + passivesmoke + race + CE_BMI+ workstat + STATEFP, data = datalog)
mod3 <- lm(As ~ CE_AGE + THC_CUMULATIVE1 + EN_FORMERSMOKER + passivesmoke + race + CE_BMI+ workstat + STATEFP, data = datalog)
mod4 <- lm(Cr ~ CE_AGE + THC_CUMULATIVE1 + EN_FORMERSMOKER + passivesmoke + race + CE_BMI+ workstat + STATEFP, data = datalog)
mod5 <- lm(Hg ~ CE_AGE + THC_CUMULATIVE1 + EN_FORMERSMOKER + passivesmoke + race + CE_BMI+ workstat + STATEFP, data = datalog)
mod6 <- lm(Pb ~ CE_AGE + THC_CUMULATIVE1 + EN_FORMERSMOKER + passivesmoke + race + CE_BMI+ workstat + STATEFP, data = datalog)

Mn <-broom::tidy(mod1) %>% filter(term != "CE_AGE") %>% filter(term != "Batch")%>% filter(term != "Batch1.5") %>% filter(term != "Batch2") %>% filter(term != "Batch3")%>% filter(term != "Batch4")%>% filter(term != "Batch5")%>% filter(term != "Batch6")%>% filter(term != "Batch6.5")%>% filter(term != "Batch7")%>% filter(term != "THC_CUMULATIVE1") %>% filter(term != "EN_FORMERSMOKER") %>% filter(term != "CE_C1") %>% filter(term != "EN_FORMERSMOKER1") %>% filter(term != "passivesmoke1")%>% filter(term != "passivesmoke")%>% filter(term != "raceOther")%>% filter(term != "CE_BMI")%>% filter(term != "workstat")%>% filter(term != "STATEFP12")%>% filter(term != "STATEFP22")%>% filter(term != "STATEFP28")
Se <-broom::tidy(mod2) %>% filter(term != "CE_AGE") %>% filter(term != "Batch")%>% filter(term != "Batch1.5") %>% filter(term != "Batch2") %>% filter(term != "Batch3")%>% filter(term != "Batch4")%>% filter(term != "Batch5")%>% filter(term != "Batch6")%>% filter(term != "Batch6.5")%>% filter(term != "Batch7")%>% filter(term != "THC_CUMULATIVE1") %>% filter(term != "EN_FORMERSMOKER") %>% filter(term != "CE_C1") %>% filter(term != "EN_FORMERSMOKER1") %>% filter(term != "passivesmoke1")%>% filter(term != "passivesmoke")%>% filter(term != "raceOther")%>% filter(term != "CE_BMI")%>% filter(term != "workstat")%>% filter(term != "STATEFP12")%>% filter(term != "STATEFP22")%>% filter(term != "STATEFP28")
As <-broom::tidy(mod3) %>% filter(term != "CE_AGE") %>% filter(term != "Batch")%>% filter(term != "Batch1.5") %>% filter(term != "Batch2") %>% filter(term != "Batch3")%>% filter(term != "Batch4")%>% filter(term != "Batch5")%>% filter(term != "Batch6")%>% filter(term != "Batch6.5")%>% filter(term != "Batch7")%>% filter(term != "THC_CUMULATIVE1") %>% filter(term != "EN_FORMERSMOKER") %>% filter(term != "CE_C1") %>% filter(term != "EN_FORMERSMOKER1") %>% filter(term != "passivesmoke1")%>% filter(term != "passivesmoke")%>% filter(term != "raceOther")%>% filter(term != "CE_BMI")%>% filter(term != "workstat")%>% filter(term != "STATEFP12")%>% filter(term != "STATEFP22")%>% filter(term != "STATEFP28")
Cr <-broom::tidy(mod4) %>% filter(term != "CE_AGE") %>% filter(term != "Batch")%>% filter(term != "Batch1.5") %>% filter(term != "Batch2") %>% filter(term != "Batch3")%>% filter(term != "Batch4")%>% filter(term != "Batch5")%>% filter(term != "Batch6")%>% filter(term != "Batch6.5")%>% filter(term != "Batch7")%>% filter(term != "THC_CUMULATIVE1") %>% filter(term != "EN_FORMERSMOKER") %>% filter(term != "CE_C1") %>% filter(term != "EN_FORMERSMOKER1") %>% filter(term != "passivesmoke1")%>% filter(term != "passivesmoke")%>% filter(term != "raceOther")%>% filter(term != "CE_BMI")%>% filter(term != "workstat")%>% filter(term != "STATEFP12")%>% filter(term != "STATEFP22")%>% filter(term != "STATEFP28")
Hg <-broom::tidy(mod5) %>% filter(term != "CE_AGE") %>% filter(term != "Batch")%>% filter(term != "Batch1.5") %>% filter(term != "Batch2") %>% filter(term != "Batch3")%>% filter(term != "Batch4")%>% filter(term != "Batch5")%>% filter(term != "Batch6")%>% filter(term != "Batch6.5")%>% filter(term != "Batch7")%>% filter(term != "THC_CUMULATIVE1") %>% filter(term != "EN_FORMERSMOKER") %>% filter(term != "CE_C1") %>% filter(term != "EN_FORMERSMOKER1") %>% filter(term != "passivesmoke1")%>% filter(term != "passivesmoke")%>% filter(term != "raceOther")%>% filter(term != "CE_BMI")%>% filter(term != "workstat")%>% filter(term != "STATEFP12")%>% filter(term != "STATEFP22")%>% filter(term != "STATEFP28")
Pb <-broom::tidy(mod6) %>% filter(term != "CE_AGE") %>% filter(term != "Batch")%>% filter(term != "Batch1.5") %>% filter(term != "Batch2") %>% filter(term != "Batch3")%>% filter(term != "Batch4")%>% filter(term != "Batch5")%>% filter(term != "Batch6")%>% filter(term != "Batch6.5")%>% filter(term != "Batch7")%>% filter(term != "THC_CUMULATIVE1") %>% filter(term != "EN_FORMERSMOKER") %>% filter(term != "CE_C1") %>% filter(term != "EN_FORMERSMOKER1") %>% filter(term != "passivesmoke1")%>% filter(term != "passivesmoke")%>% filter(term != "raceOther")%>% filter(term != "CE_BMI")%>% filter(term != "workstat")%>% filter(term != "STATEFP12")%>% filter(term != "STATEFP22")%>% filter(term != "STATEFP28")

summary(mod1)
(10^(coef(mod6))-1)*100
(10^(confint(mod6))-1)*100

library(dotwhisker)
library(gdata)
library(dplyr)
essential_models <- combine(Se, Mn, Pb, Hg, Cr, As)
colnames(essential_models)[6] ="model"
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#0072B2", "#D55E00", "#CC79A7")


plotRace_essential<- dwplot(essential_models, dodge_size = 1, dot_args = list(aes(color = model),size = 2), whisker_args = list(aes(colour=model), size = 1)) %>% relabel_predictors(c("raceBlack" = "1"))
plotRace_essential
plotRace_essential1 <- plotRace_essential + geom_vline(xintercept = 0, colour = "grey60", linetype = 2) + ggtitle("NEI Metals") + 
  scale_color_manual(values=cbPalette) + theme(text = element_text(size = 12)) +  labs(x = bquote('Percent difference compared to White')) + 
  theme(axis.text=element_text(size=12)) + theme(axis.title.x = element_text(margin = margin(t = 12))) +  
  scale_x_continuous(breaks = log10(pretty(10^(essential_models$estimate),n=5)), labels = pretty(10^(essential_models$estimate),n=5)) + theme_bw() + theme(panel.border = element_rect(fill=NA, colour = "black", size=1))
plotRace_essential1 

##plot metal concentration disparity by income
# start plot for NEI metals-------------------------------------------------
mod1 <- lm(Mn ~ CE_AGE + THC_CUMULATIVE1 + income + EN_FORMERSMOKER + passivesmoke + CE_BMI+ workstat + STATEFP, data = datalog)
mod2 <- lm(Se ~ CE_AGE + THC_CUMULATIVE1 + income + EN_FORMERSMOKER + passivesmoke + CE_BMI+ workstat + STATEFP, data = datalog)
mod3 <- lm(As ~ CE_AGE + THC_CUMULATIVE1 + income + EN_FORMERSMOKER + passivesmoke + CE_BMI+ workstat + STATEFP, data = datalog)
mod4 <- lm(Cr ~ CE_AGE + THC_CUMULATIVE1 + income + EN_FORMERSMOKER + passivesmoke + CE_BMI+ workstat + STATEFP, data = datalog)
mod5 <- lm(Hg ~ CE_AGE + THC_CUMULATIVE1 + income + EN_FORMERSMOKER + passivesmoke + CE_BMI+ workstat + STATEFP, data = datalog)
mod6 <- lm(Pb ~ CE_AGE + THC_CUMULATIVE1 + income + EN_FORMERSMOKER + passivesmoke + CE_BMI+ workstat + STATEFP, data = datalog)

Mn <-broom::tidy(mod1) %>% filter(term != "CE_AGE") %>% filter(term != "Batch")%>% filter(term != "Batch1.5") %>% filter(term != "Batch2") %>% filter(term != "Batch3")%>% filter(term != "Batch4")%>% filter(term != "Batch5")%>% filter(term != "Batch6")%>% filter(term != "Batch6.5")%>% filter(term != "Batch7")%>% filter(term != "THC_CUMULATIVE1") %>% filter(term != "EN_FORMERSMOKER") %>% filter(term != "CE_C1") %>% filter(term != "EN_FORMERSMOKER1") %>% filter(term != "passivesmoke1")%>% filter(term != "passivesmoke")%>% filter(term != "raceOther")%>% filter(term != "CE_BMI")%>% filter(term != "workstat")%>% filter(term != "STATEFP12")%>% filter(term != "STATEFP22")%>% filter(term != "STATEFP28")
Se <-broom::tidy(mod2) %>% filter(term != "CE_AGE") %>% filter(term != "Batch")%>% filter(term != "Batch1.5") %>% filter(term != "Batch2") %>% filter(term != "Batch3")%>% filter(term != "Batch4")%>% filter(term != "Batch5")%>% filter(term != "Batch6")%>% filter(term != "Batch6.5")%>% filter(term != "Batch7")%>% filter(term != "THC_CUMULATIVE1") %>% filter(term != "EN_FORMERSMOKER") %>% filter(term != "CE_C1") %>% filter(term != "EN_FORMERSMOKER1") %>% filter(term != "passivesmoke1")%>% filter(term != "passivesmoke")%>% filter(term != "raceOther")%>% filter(term != "CE_BMI")%>% filter(term != "workstat")%>% filter(term != "STATEFP12")%>% filter(term != "STATEFP22")%>% filter(term != "STATEFP28")
As <-broom::tidy(mod3) %>% filter(term != "CE_AGE") %>% filter(term != "Batch")%>% filter(term != "Batch1.5") %>% filter(term != "Batch2") %>% filter(term != "Batch3")%>% filter(term != "Batch4")%>% filter(term != "Batch5")%>% filter(term != "Batch6")%>% filter(term != "Batch6.5")%>% filter(term != "Batch7")%>% filter(term != "THC_CUMULATIVE1") %>% filter(term != "EN_FORMERSMOKER") %>% filter(term != "CE_C1") %>% filter(term != "EN_FORMERSMOKER1") %>% filter(term != "passivesmoke1")%>% filter(term != "passivesmoke")%>% filter(term != "raceOther")%>% filter(term != "CE_BMI")%>% filter(term != "workstat")%>% filter(term != "STATEFP12")%>% filter(term != "STATEFP22")%>% filter(term != "STATEFP28")
Cr <-broom::tidy(mod4) %>% filter(term != "CE_AGE") %>% filter(term != "Batch")%>% filter(term != "Batch1.5") %>% filter(term != "Batch2") %>% filter(term != "Batch3")%>% filter(term != "Batch4")%>% filter(term != "Batch5")%>% filter(term != "Batch6")%>% filter(term != "Batch6.5")%>% filter(term != "Batch7")%>% filter(term != "THC_CUMULATIVE1") %>% filter(term != "EN_FORMERSMOKER") %>% filter(term != "CE_C1") %>% filter(term != "EN_FORMERSMOKER1") %>% filter(term != "passivesmoke1")%>% filter(term != "passivesmoke")%>% filter(term != "raceOther")%>% filter(term != "CE_BMI")%>% filter(term != "workstat")%>% filter(term != "STATEFP12")%>% filter(term != "STATEFP22")%>% filter(term != "STATEFP28")
Hg <-broom::tidy(mod5) %>% filter(term != "CE_AGE") %>% filter(term != "Batch")%>% filter(term != "Batch1.5") %>% filter(term != "Batch2") %>% filter(term != "Batch3")%>% filter(term != "Batch4")%>% filter(term != "Batch5")%>% filter(term != "Batch6")%>% filter(term != "Batch6.5")%>% filter(term != "Batch7")%>% filter(term != "THC_CUMULATIVE1") %>% filter(term != "EN_FORMERSMOKER") %>% filter(term != "CE_C1") %>% filter(term != "EN_FORMERSMOKER1") %>% filter(term != "passivesmoke1")%>% filter(term != "passivesmoke")%>% filter(term != "raceOther")%>% filter(term != "CE_BMI")%>% filter(term != "workstat")%>% filter(term != "STATEFP12")%>% filter(term != "STATEFP22")%>% filter(term != "STATEFP28")
Pb <-broom::tidy(mod6) %>% filter(term != "CE_AGE") %>% filter(term != "Batch")%>% filter(term != "Batch1.5") %>% filter(term != "Batch2") %>% filter(term != "Batch3")%>% filter(term != "Batch4")%>% filter(term != "Batch5")%>% filter(term != "Batch6")%>% filter(term != "Batch6.5")%>% filter(term != "Batch7")%>% filter(term != "THC_CUMULATIVE1") %>% filter(term != "EN_FORMERSMOKER") %>% filter(term != "CE_C1") %>% filter(term != "EN_FORMERSMOKER1") %>% filter(term != "passivesmoke1")%>% filter(term != "passivesmoke")%>% filter(term != "raceOther")%>% filter(term != "CE_BMI")%>% filter(term != "workstat")%>% filter(term != "STATEFP12")%>% filter(term != "STATEFP22")%>% filter(term != "STATEFP28")

summary(mod4)
(10^(coef(mod5))-1)*100
(10^(confint(mod5))-1)*100

library(dotwhisker)
library(gdata)
library(dplyr)
essential_models <- combine(Se, Mn, Pb, Hg, Cr, As)
scale_models <- combine(Pb, Hg)
colnames(essential_models)[6] ="model"
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

plotRace_essential<- dwplot(essential_models, dodge_size = 1, dot_args = list(aes(color = model),size = 2), whisker_args = list(aes(colour=model), size = 1)) %>% relabel_predictors(c("income<$20,000" = "1", "income$20,000 - $49,999" = "2"))
plotRace_essential
plotRace_essential1 <- plotRace_essential + geom_vline(xintercept = 0, colour = "grey60", linetype = 2) + ggtitle("NEI Metals") + 
  scale_color_manual(values=cbPalette) + theme(text = element_text(size = 12)) +  labs(x = bquote('Percent difference compared to White')) + 
  theme(axis.text=element_text(size=12)) + theme(axis.title.x = element_text(margin = margin(t = 12))) +  
  scale_x_continuous(breaks = log10(pretty(10^(scale_models$estimate),n=6)), labels = pretty(10^(scale_models$estimate),n=6))+ theme_bw() + theme(panel.border = element_rect(fill=NA, colour = "black", size=1))
plotRace_essential1 

(10^(coef(mod6))-1)*100
(10^(confint(mod6))-1)*100

##plot metal concentration disparity by edu
# start plot for NEI metals-------------------------------------------------
mod1 <- lm(Mn ~ CE_AGE + THC_CUMULATIVE1 + EDU + EN_FORMERSMOKER + passivesmoke + CE_BMI+ workstat + STATEFP, data = datalog)
mod2 <- lm(Se ~ CE_AGE + THC_CUMULATIVE1 + EDU + EN_FORMERSMOKER + passivesmoke + CE_BMI+ workstat + STATEFP, data = datalog)
mod3 <- lm(As ~ CE_AGE + THC_CUMULATIVE1 + EDU + EN_FORMERSMOKER + passivesmoke + CE_BMI+ workstat + STATEFP, data = datalog)
mod4 <- lm(Cr ~ CE_AGE + THC_CUMULATIVE1 + EDU + EN_FORMERSMOKER + passivesmoke + CE_BMI+ workstat + STATEFP, data = datalog)
mod5 <- lm(Hg ~ CE_AGE + THC_CUMULATIVE1 + EDU + EN_FORMERSMOKER + passivesmoke + CE_BMI+ workstat + STATEFP, data = datalog)
mod6 <- lm(Pb ~ CE_AGE + THC_CUMULATIVE1 + EDU + EN_FORMERSMOKER + passivesmoke + CE_BMI+ workstat + STATEFP, data = datalog)

Mn <-broom::tidy(mod1) %>% filter(term != "CE_AGE") %>% filter(term != "Batch")%>% filter(term != "Batch1.5") %>% filter(term != "Batch2") %>% filter(term != "Batch3")%>% filter(term != "Batch4")%>% filter(term != "Batch5")%>% filter(term != "Batch6")%>% filter(term != "Batch6.5")%>% filter(term != "Batch7")%>% filter(term != "THC_CUMULATIVE1") %>% filter(term != "EN_FORMERSMOKER") %>% filter(term != "CE_C1") %>% filter(term != "EN_FORMERSMOKER1") %>% filter(term != "passivesmoke1")%>% filter(term != "passivesmoke")%>% filter(term != "raceOther")%>% filter(term != "CE_BMI")%>% filter(term != "workstat")%>% filter(term != "STATEFP12")%>% filter(term != "STATEFP22")%>% filter(term != "STATEFP28")
Se <-broom::tidy(mod2) %>% filter(term != "CE_AGE") %>% filter(term != "Batch")%>% filter(term != "Batch1.5") %>% filter(term != "Batch2") %>% filter(term != "Batch3")%>% filter(term != "Batch4")%>% filter(term != "Batch5")%>% filter(term != "Batch6")%>% filter(term != "Batch6.5")%>% filter(term != "Batch7")%>% filter(term != "THC_CUMULATIVE1") %>% filter(term != "EN_FORMERSMOKER") %>% filter(term != "CE_C1") %>% filter(term != "EN_FORMERSMOKER1") %>% filter(term != "passivesmoke1")%>% filter(term != "passivesmoke")%>% filter(term != "raceOther")%>% filter(term != "CE_BMI")%>% filter(term != "workstat")%>% filter(term != "STATEFP12")%>% filter(term != "STATEFP22")%>% filter(term != "STATEFP28")
As <-broom::tidy(mod3) %>% filter(term != "CE_AGE") %>% filter(term != "Batch")%>% filter(term != "Batch1.5") %>% filter(term != "Batch2") %>% filter(term != "Batch3")%>% filter(term != "Batch4")%>% filter(term != "Batch5")%>% filter(term != "Batch6")%>% filter(term != "Batch6.5")%>% filter(term != "Batch7")%>% filter(term != "THC_CUMULATIVE1") %>% filter(term != "EN_FORMERSMOKER") %>% filter(term != "CE_C1") %>% filter(term != "EN_FORMERSMOKER1") %>% filter(term != "passivesmoke1")%>% filter(term != "passivesmoke")%>% filter(term != "raceOther")%>% filter(term != "CE_BMI")%>% filter(term != "workstat")%>% filter(term != "STATEFP12")%>% filter(term != "STATEFP22")%>% filter(term != "STATEFP28")
Cr <-broom::tidy(mod4) %>% filter(term != "CE_AGE") %>% filter(term != "Batch")%>% filter(term != "Batch1.5") %>% filter(term != "Batch2") %>% filter(term != "Batch3")%>% filter(term != "Batch4")%>% filter(term != "Batch5")%>% filter(term != "Batch6")%>% filter(term != "Batch6.5")%>% filter(term != "Batch7")%>% filter(term != "THC_CUMULATIVE1") %>% filter(term != "EN_FORMERSMOKER") %>% filter(term != "CE_C1") %>% filter(term != "EN_FORMERSMOKER1") %>% filter(term != "passivesmoke1")%>% filter(term != "passivesmoke")%>% filter(term != "raceOther")%>% filter(term != "CE_BMI")%>% filter(term != "workstat")%>% filter(term != "STATEFP12")%>% filter(term != "STATEFP22")%>% filter(term != "STATEFP28")
Hg <-broom::tidy(mod5) %>% filter(term != "CE_AGE") %>% filter(term != "Batch")%>% filter(term != "Batch1.5") %>% filter(term != "Batch2") %>% filter(term != "Batch3")%>% filter(term != "Batch4")%>% filter(term != "Batch5")%>% filter(term != "Batch6")%>% filter(term != "Batch6.5")%>% filter(term != "Batch7")%>% filter(term != "THC_CUMULATIVE1") %>% filter(term != "EN_FORMERSMOKER") %>% filter(term != "CE_C1") %>% filter(term != "EN_FORMERSMOKER1") %>% filter(term != "passivesmoke1")%>% filter(term != "passivesmoke")%>% filter(term != "raceOther")%>% filter(term != "CE_BMI")%>% filter(term != "workstat")%>% filter(term != "STATEFP12")%>% filter(term != "STATEFP22")%>% filter(term != "STATEFP28")
Pb <-broom::tidy(mod6) %>% filter(term != "CE_AGE") %>% filter(term != "Batch")%>% filter(term != "Batch1.5") %>% filter(term != "Batch2") %>% filter(term != "Batch3")%>% filter(term != "Batch4")%>% filter(term != "Batch5")%>% filter(term != "Batch6")%>% filter(term != "Batch6.5")%>% filter(term != "Batch7")%>% filter(term != "THC_CUMULATIVE1") %>% filter(term != "EN_FORMERSMOKER") %>% filter(term != "CE_C1") %>% filter(term != "EN_FORMERSMOKER1") %>% filter(term != "passivesmoke1")%>% filter(term != "passivesmoke")%>% filter(term != "raceOther")%>% filter(term != "CE_BMI")%>% filter(term != "workstat")%>% filter(term != "STATEFP12")%>% filter(term != "STATEFP22")%>% filter(term != "STATEFP28")

summary(mod1)
(exp(coef(mod3))-1)*100
library(dotwhisker)
library(gdata)
library(dplyr)
essential_models <- combine(Se, Mn, Pb, Hg, Cr, As)
colnames(essential_models)[6] ="model"
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

plotRace_essential<- dwplot(essential_models, dodge_size = 1, dot_args = list(aes(color = model),size = 2), whisker_args = list(aes(colour=model), size = 1)) %>% relabel_predictors(c("EDUHighschool" = "2", "EDULess than Highschool" = "1", "EDUSome College" = "3"))
plotRace_essential
plotRace_essential1 <- plotRace_essential + geom_vline(xintercept = 0, colour = "grey60", linetype = 2) + ggtitle("NEI Metals") + 
  scale_color_manual(values=cbPalette) + theme(text = element_text(size = 12)) +  labs(x = bquote('Percent difference compared to White')) + 
  theme(axis.text=element_text(size=12)) + theme(axis.title.x = element_text(margin = margin(t = 12))) +  
  scale_x_continuous(breaks = log(pretty(exp(essential_models$estimate),n=5)), labels = pretty(exp(essential_models$estimate),n=5))
plotRace_essential1 

##corrlation matrix of covariates
datacor <- data[, c("CE_AGE", "THC_CUMULATIVE1", "EN_RACE2", "EN_EDU", "Pb5_SUMEMISSIONS", "medianyearstructurebuilt", "percentbelowpovertylevel", "medianhouseholdincome", "CE_C1", "EN_TOTINCOME", "PbDistance", "Pb5_pointcount")]
library(Hmisc)
cormat = rcorr(as.matrix(datacor))
cormat

## dotwhisker of distance env determinants all metals -----------
library(dotwhisker)
library(dplyr)
library(gdata)
mod1<- lm(As ~ AsDistance + CE_BMI + CE_AGE + EN_FORMERSMOKER + passivesmoke + road50 + THC_CUMULATIVE1 + workstat + STATEFP, data = datalog)
mod2<- lm(Cr ~ CrDistance + CE_BMI + CE_AGE + EN_FORMERSMOKER + passivesmoke + road50 + THC_CUMULATIVE1 + workstat + STATEFP, data = datalog)
mod3<- lm(Hg ~ HgDistance + CE_BMI + CE_AGE + EN_FORMERSMOKER + passivesmoke + road50 + THC_CUMULATIVE1 + workstat + STATEFP, data = datalog)
mod4<- lm(Mn ~ MnDistance + CE_BMI + CE_AGE + EN_FORMERSMOKER + passivesmoke + road50 + THC_CUMULATIVE1 + workstat + STATEFP, data = datalog)
mod5<- lm(Pb ~ PbDistance + CE_BMI + CE_AGE + EN_FORMERSMOKER + passivesmoke + road50 + THC_CUMULATIVE1 + workstat + STATEFP, data = datalog)
mod6<- lm(Se ~ SeDistance + CE_BMI + CE_AGE + EN_FORMERSMOKER + passivesmoke + road50 + THC_CUMULATIVE1 + workstat + STATEFP, data = datalog)
mod7<- lm(As ~ As_Dweight + CE_BMI + CE_AGE + EN_FORMERSMOKER + passivesmoke + road50 + THC_CUMULATIVE1 + workstat + STATEFP, data = datalog)
mod8<- lm(Cr ~ Cr_Dweight + CE_BMI + CE_AGE + EN_FORMERSMOKER + passivesmoke + road50 + THC_CUMULATIVE1 + workstat + STATEFP, data = datalog)
mod9<- lm(Hg ~ Hg_Dweight + CE_BMI + CE_AGE + EN_FORMERSMOKER + passivesmoke + road50 + THC_CUMULATIVE1 + workstat + STATEFP, data = datalog)
mod10<- lm(Mn ~ Mn_Dweight + CE_BMI + CE_AGE + EN_FORMERSMOKER + passivesmoke + road50 + THC_CUMULATIVE1 + workstat + STATEFP, data = datalog)
mod11<- lm(Pb ~ Pb_Dweight + CE_BMI + CE_AGE + EN_FORMERSMOKER + passivesmoke + road50 + THC_CUMULATIVE1 + workstat + STATEFP, data = datalog)
mod12<- lm(Se ~ Se_Dweight + CE_BMI + CE_AGE + EN_FORMERSMOKER + passivesmoke + road50 + THC_CUMULATIVE1 + workstat + STATEFP, data = datalog)
mod13<- lm(As ~ As_EMweight + CE_BMI + CE_AGE + EN_FORMERSMOKER + passivesmoke + road50 + THC_CUMULATIVE1 + workstat + STATEFP, data = datalog)
mod14<- lm(Cr ~ Cr_EMweight + CE_BMI + CE_AGE + EN_FORMERSMOKER + passivesmoke + road50 + THC_CUMULATIVE1 + workstat + STATEFP, data = datalog)
mod15<- lm(Hg ~ Hg_EMweight + CE_BMI + CE_AGE + EN_FORMERSMOKER + passivesmoke + road50 + THC_CUMULATIVE1 + workstat + STATEFP, data = datalog)
mod16<- lm(Mn ~ Mn_EMweight + CE_BMI + CE_AGE + EN_FORMERSMOKER + passivesmoke + road50 + THC_CUMULATIVE1 + workstat + STATEFP, data = datalog)
mod17<- lm(Pb ~ Pb_EMweight + CE_BMI + CE_AGE + EN_FORMERSMOKER + passivesmoke + road50 + THC_CUMULATIVE1 + workstat + STATEFP, data = datalog)
mod18<- lm(Se ~ Se_EMweight + CE_BMI + CE_AGE + EN_FORMERSMOKER + passivesmoke + road50 + THC_CUMULATIVE1 + workstat + STATEFP, data = datalog)
mod19<- lm(As ~ As_EMISSIONS + CE_BMI + CE_AGE + EN_FORMERSMOKER + passivesmoke + road50 + THC_CUMULATIVE1 + workstat + STATEFP, data = datalog)
mod20<- lm(Cr ~ Cr_EMISSIONS + CE_BMI + CE_AGE + EN_FORMERSMOKER + passivesmoke + road50 + THC_CUMULATIVE1 + workstat + STATEFP, data = datalog)
mod21<- lm(Hg ~ Hg_EMISSIONS + CE_BMI + CE_AGE + EN_FORMERSMOKER + passivesmoke + road50 + THC_CUMULATIVE1 + workstat + STATEFP, data = datalog)
mod22<- lm(Mn ~ Mn_EMISSIONS + CE_BMI + CE_AGE + EN_FORMERSMOKER + passivesmoke + road50 + THC_CUMULATIVE1 + workstat + STATEFP, data = datalog)
mod23<- lm(Pb ~ Pb_EMISSIONS + CE_BMI + CE_AGE + EN_FORMERSMOKER + passivesmoke + road50 + THC_CUMULATIVE1 + workstat + STATEFP, data = datalog)
mod24<- lm(Se ~ Se_EMISSIONS + CE_BMI + CE_AGE + EN_FORMERSMOKER + passivesmoke + road50 + THC_CUMULATIVE1 + workstat + STATEFP, data = datalog)

mod1 <-broom::tidy(mod1) %>% filter(term != "road50") %>% filter(term != "road501") %>% filter(term != "CE_BMI")%>% filter(term != "CE_AGE")%>%filter(term != "EN_FORMERSMOKER1")%>%filter(term != "passivesmoke1")%>%filter(term != "THC_CUMULATIVE1")%>%filter(term != "workstat1")%>%filter(term != "STATEFP12")%>%filter(term != "STATEFP22")%>%filter(term != "STATEFP28")%>%filter(term != "workstat")
mod2 <-broom::tidy(mod2) %>% filter(term != "road50") %>% filter(term != "road501") %>% filter(term != "CE_BMI")%>% filter(term != "CE_AGE")%>%filter(term != "EN_FORMERSMOKER1")%>%filter(term != "passivesmoke1")%>%filter(term != "THC_CUMULATIVE1")%>%filter(term != "workstat1")%>%filter(term != "STATEFP12")%>%filter(term != "STATEFP22")%>%filter(term != "STATEFP28")%>%filter(term != "workstat")
mod3 <-broom::tidy(mod3) %>% filter(term != "road50") %>% filter(term != "road501") %>% filter(term != "CE_BMI")%>% filter(term != "CE_AGE")%>%filter(term != "EN_FORMERSMOKER1")%>%filter(term != "passivesmoke1")%>%filter(term != "THC_CUMULATIVE1")%>%filter(term != "workstat1")%>%filter(term != "STATEFP12")%>%filter(term != "STATEFP22")%>%filter(term != "STATEFP28")%>%filter(term != "workstat")
mod4 <-broom::tidy(mod4) %>% filter(term != "road50") %>% filter(term != "road501") %>% filter(term != "CE_BMI")%>% filter(term != "CE_AGE")%>%filter(term != "EN_FORMERSMOKER1")%>%filter(term != "passivesmoke1")%>%filter(term != "THC_CUMULATIVE1")%>%filter(term != "workstat1")%>%filter(term != "STATEFP12")%>%filter(term != "STATEFP22")%>%filter(term != "STATEFP28")%>%filter(term != "workstat")
mod5 <-broom::tidy(mod5) %>% filter(term != "road50") %>% filter(term != "road501") %>% filter(term != "CE_BMI")%>% filter(term != "CE_AGE")%>%filter(term != "EN_FORMERSMOKER1")%>%filter(term != "passivesmoke1")%>%filter(term != "THC_CUMULATIVE1")%>%filter(term != "workstat1")%>%filter(term != "STATEFP12")%>%filter(term != "STATEFP22")%>%filter(term != "STATEFP28")%>%filter(term != "workstat")
mod6 <-broom::tidy(mod6) %>% filter(term != "road50") %>% filter(term != "road501") %>% filter(term != "CE_BMI")%>% filter(term != "CE_AGE")%>%filter(term != "EN_FORMERSMOKER1")%>%filter(term != "passivesmoke1")%>%filter(term != "THC_CUMULATIVE1")%>%filter(term != "workstat1")%>%filter(term != "STATEFP12")%>%filter(term != "STATEFP22")%>%filter(term != "STATEFP28")%>%filter(term != "workstat")
mod7 <-broom::tidy(mod7) %>% filter(term != "road50") %>% filter(term != "road501") %>% filter(term != "CE_BMI")%>% filter(term != "CE_AGE")%>%filter(term != "EN_FORMERSMOKER1")%>%filter(term != "passivesmoke1")%>%filter(term != "THC_CUMULATIVE1")%>%filter(term != "workstat1")%>%filter(term != "STATEFP12")%>%filter(term != "STATEFP22")%>%filter(term != "STATEFP28")%>%filter(term != "workstat")
mod8 <-broom::tidy(mod8) %>% filter(term != "road50") %>% filter(term != "road501") %>% filter(term != "CE_BMI")%>% filter(term != "CE_AGE")%>%filter(term != "EN_FORMERSMOKER1")%>%filter(term != "passivesmoke1")%>%filter(term != "THC_CUMULATIVE1")%>%filter(term != "workstat1")%>%filter(term != "STATEFP12")%>%filter(term != "STATEFP22")%>%filter(term != "STATEFP28")%>%filter(term != "workstat")
mod9 <-broom::tidy(mod9) %>% filter(term != "road50") %>% filter(term != "road501") %>% filter(term != "CE_BMI")%>% filter(term != "CE_AGE")%>%filter(term != "EN_FORMERSMOKER1")%>%filter(term != "passivesmoke1")%>%filter(term != "THC_CUMULATIVE1")%>%filter(term != "workstat1")%>%filter(term != "STATEFP12")%>%filter(term != "STATEFP22")%>%filter(term != "STATEFP28")%>%filter(term != "workstat")
mod10 <-broom::tidy(mod10) %>% filter(term != "road50") %>% filter(term != "road501") %>% filter(term != "CE_BMI")%>% filter(term != "CE_AGE")%>%filter(term != "EN_FORMERSMOKER1")%>%filter(term != "passivesmoke1")%>%filter(term != "THC_CUMULATIVE1")%>%filter(term != "workstat1")%>%filter(term != "STATEFP12")%>%filter(term != "STATEFP22")%>%filter(term != "STATEFP28")%>%filter(term != "workstat")
mod11 <-broom::tidy(mod11) %>% filter(term != "road50") %>% filter(term != "road501") %>% filter(term != "CE_BMI")%>% filter(term != "CE_AGE")%>%filter(term != "EN_FORMERSMOKER1")%>%filter(term != "passivesmoke1")%>%filter(term != "THC_CUMULATIVE1")%>%filter(term != "workstat1")%>%filter(term != "STATEFP12")%>%filter(term != "STATEFP22")%>%filter(term != "STATEFP28")%>%filter(term != "workstat")
mod12 <-broom::tidy(mod12) %>% filter(term != "road50") %>% filter(term != "road501") %>% filter(term != "CE_BMI")%>% filter(term != "CE_AGE")%>%filter(term != "EN_FORMERSMOKER1")%>%filter(term != "passivesmoke1")%>%filter(term != "THC_CUMULATIVE1")%>%filter(term != "workstat1")%>%filter(term != "STATEFP12")%>%filter(term != "STATEFP22")%>%filter(term != "STATEFP28")%>%filter(term != "workstat")
mod13 <-broom::tidy(mod13) %>% filter(term != "road50") %>% filter(term != "road501") %>% filter(term != "CE_BMI")%>% filter(term != "CE_AGE")%>%filter(term != "EN_FORMERSMOKER1")%>%filter(term != "passivesmoke1")%>%filter(term != "THC_CUMULATIVE1")%>%filter(term != "workstat1")%>%filter(term != "STATEFP12")%>%filter(term != "STATEFP22")%>%filter(term != "STATEFP28")%>%filter(term != "workstat")
mod14 <-broom::tidy(mod14) %>% filter(term != "road50") %>% filter(term != "road501") %>% filter(term != "CE_BMI")%>% filter(term != "CE_AGE")%>%filter(term != "EN_FORMERSMOKER1")%>%filter(term != "passivesmoke1")%>%filter(term != "THC_CUMULATIVE1")%>%filter(term != "workstat1")%>%filter(term != "STATEFP12")%>%filter(term != "STATEFP22")%>%filter(term != "STATEFP28")%>%filter(term != "workstat")
mod15 <-broom::tidy(mod15) %>% filter(term != "road50") %>% filter(term != "road501") %>% filter(term != "CE_BMI")%>% filter(term != "CE_AGE")%>%filter(term != "EN_FORMERSMOKER1")%>%filter(term != "passivesmoke1")%>%filter(term != "THC_CUMULATIVE1")%>%filter(term != "workstat1")%>%filter(term != "STATEFP12")%>%filter(term != "STATEFP22")%>%filter(term != "STATEFP28")%>%filter(term != "workstat")
mod16 <-broom::tidy(mod16) %>% filter(term != "road50") %>% filter(term != "road501") %>% filter(term != "CE_BMI")%>% filter(term != "CE_AGE")%>%filter(term != "EN_FORMERSMOKER1")%>%filter(term != "passivesmoke1")%>%filter(term != "THC_CUMULATIVE1")%>%filter(term != "workstat1")%>%filter(term != "STATEFP12")%>%filter(term != "STATEFP22")%>%filter(term != "STATEFP28")%>%filter(term != "workstat")
mod17 <-broom::tidy(mod17) %>% filter(term != "road50") %>% filter(term != "road501") %>% filter(term != "CE_BMI")%>% filter(term != "CE_AGE")%>%filter(term != "EN_FORMERSMOKER1")%>%filter(term != "passivesmoke1")%>%filter(term != "THC_CUMULATIVE1")%>%filter(term != "workstat1")%>%filter(term != "STATEFP12")%>%filter(term != "STATEFP22")%>%filter(term != "STATEFP28")%>%filter(term != "workstat")
mod18 <-broom::tidy(mod18) %>% filter(term != "road50") %>% filter(term != "road501") %>% filter(term != "CE_BMI")%>% filter(term != "CE_AGE")%>%filter(term != "EN_FORMERSMOKER1")%>%filter(term != "passivesmoke1")%>%filter(term != "THC_CUMULATIVE1")%>%filter(term != "workstat1")%>%filter(term != "STATEFP12")%>%filter(term != "STATEFP22")%>%filter(term != "STATEFP28")%>%filter(term != "workstat")
mod19 <-broom::tidy(mod19) %>% filter(term != "road50") %>% filter(term != "road501") %>% filter(term != "CE_BMI")%>% filter(term != "CE_AGE")%>%filter(term != "EN_FORMERSMOKER1")%>%filter(term != "passivesmoke1")%>%filter(term != "THC_CUMULATIVE1")%>%filter(term != "workstat1")%>%filter(term != "STATEFP12")%>%filter(term != "STATEFP22")%>%filter(term != "STATEFP28")%>%filter(term != "workstat")
mod20 <-broom::tidy(mod20) %>% filter(term != "road50") %>% filter(term != "road501") %>% filter(term != "CE_BMI")%>% filter(term != "CE_AGE")%>%filter(term != "EN_FORMERSMOKER1")%>%filter(term != "passivesmoke1")%>%filter(term != "THC_CUMULATIVE1")%>%filter(term != "workstat1")%>%filter(term != "STATEFP12")%>%filter(term != "STATEFP22")%>%filter(term != "STATEFP28")%>%filter(term != "workstat")
mod21 <-broom::tidy(mod21) %>% filter(term != "road50") %>% filter(term != "road501") %>% filter(term != "CE_BMI")%>% filter(term != "CE_AGE")%>%filter(term != "EN_FORMERSMOKER1")%>%filter(term != "passivesmoke1")%>%filter(term != "THC_CUMULATIVE1")%>%filter(term != "workstat1")%>%filter(term != "STATEFP12")%>%filter(term != "STATEFP22")%>%filter(term != "STATEFP28")%>%filter(term != "workstat")
mod22 <-broom::tidy(mod22) %>% filter(term != "road50") %>% filter(term != "road501") %>% filter(term != "CE_BMI")%>% filter(term != "CE_AGE")%>%filter(term != "EN_FORMERSMOKER1")%>%filter(term != "passivesmoke1")%>%filter(term != "THC_CUMULATIVE1")%>%filter(term != "workstat1")%>%filter(term != "STATEFP12")%>%filter(term != "STATEFP22")%>%filter(term != "STATEFP28")%>%filter(term != "workstat")
mod23 <-broom::tidy(mod23) %>% filter(term != "road50") %>% filter(term != "road501") %>% filter(term != "CE_BMI")%>% filter(term != "CE_AGE")%>%filter(term != "EN_FORMERSMOKER1")%>%filter(term != "passivesmoke1")%>%filter(term != "THC_CUMULATIVE1")%>%filter(term != "workstat1")%>%filter(term != "STATEFP12")%>%filter(term != "STATEFP22")%>%filter(term != "STATEFP28")%>%filter(term != "workstat")
mod24 <-broom::tidy(mod24) %>% filter(term != "road50") %>% filter(term != "road501") %>% filter(term != "CE_BMI")%>% filter(term != "CE_AGE")%>%filter(term != "EN_FORMERSMOKER1")%>%filter(term != "passivesmoke1")%>%filter(term != "THC_CUMULATIVE1")%>%filter(term != "workstat1")%>%filter(term != "STATEFP12")%>%filter(term != "STATEFP22")%>%filter(term != "STATEFP28")%>%filter(term != "workstat")
summary(mod11)

all_models <- combine(mod1, mod2, mod3, mod4, mod5, mod6)
colnames(all_models)[6] ="model"
plotdist<- dwplot(mod5, dodge_size = 0.5, dot_args = list(aes(colour = model),size = 2), whisker_args = list(aes(colour=model), size = 1))  %>% relabel_predictors(c("AsDistance" = "As", "CrDistance" = "Cr", "HgDistance" = "Hg", "MnDistance" = "Mn", "PbDistance" = "Pb", "SeDistance" = "Se"))
plotdist
plotdist <- plotdist+ ggtitle("dist all") + geom_vline(xintercept = 0, colour = "grey60", linetype = 2) + theme(text = element_text(size = 12)) + 
  labs(x = bquote('Coefficent Estimate with 95% CIs')) + theme(axis.text=element_text(size=12)) + theme(axis.title.x = element_text(margin = margin(t = 12)))+
  scale_x_continuous(breaks = log(pretty(exp(all_models$estimate),n=30)), labels = pretty(exp(all_models$estimate),n=30))
plotdist

# All distance plot
all_models <- combine(mod1, mod2, mod3, mod4, mod5, mod6)
colnames(all_models)[6] ="model"
plotdist<- dwplot(all_models, dodge_size = 0.5, dot_args = list(aes(colour = model),size = 2), whisker_args = list(aes(colour=model), size = 1))  %>% relabel_predictors(c("AsDistance" = "As", "CrDistance" = "Cr", "HgDistance" = "Hg", "MnDistance" = "Mn", "PbDistance" = "Pb", "SeDistance" = "Se"))
plotdist
plotdist <- plotdist+ ggtitle("dist all") + geom_vline(xintercept = 0, colour = "grey60", linetype = 2) + theme(text = element_text(size = 12)) + 
  labs(x = bquote('Coefficent Estimate with 95% CIs')) + theme(axis.text=element_text(size=12)) + theme(axis.title.x = element_text(margin = margin(t = 12)))+
  scale_x_continuous(breaks = log10(pretty(10^(all_models$estimate),n=40)), labels = pretty(10^(all_models$estimate),n=40))+ theme_bw() + theme(panel.border = element_rect(fill=NA, colour = "black", size=1))
plotdist

# All IDW distance plot
all_models <- combine(mod7, mod8, mod9, mod10, mod11, mod12)
colnames(all_models)[6] ="model"
plotdist<- dwplot(all_models, dodge_size = 0.5, dot_args = list(aes(colour = model),size = 2), whisker_args = list(aes(colour=model), size = 1))  %>% relabel_predictors(c("As_Dweight" = "As", "Cr_Dweight" = "Cr", "Hg_Dweight" = "Hg", "Mn_Dweight" = "Mn", "Pb_Dweight" = "Pb", "Se_Dweight" = "Se"))
plotdist
plotdist <- plotdist+ ggtitle("idw d all") + geom_vline(xintercept = 0, colour = "grey60", linetype = 2) + theme(text = element_text(size = 12)) + 
  labs(x = bquote('Coefficent Estimate with 95% CIs')) + theme(axis.text=element_text(size=12)) +
 scale_x_continuous(breaks = log10(pretty(10^(all_models$estimate),n=4)), labels = (pretty(10^(all_models$estimate),n=4)))+ theme_bw() + theme(panel.border = element_rect(fill=NA, colour = "black", size=1))
plotdist

# All IDW EM plot
all_models <- combine(mod13, mod14, mod15, mod16, mod17, mod18)
colnames(all_models)[6] ="model"
plotdist<- dwplot(all_models, dodge_size = 0.5, dot_args = list(aes(colour = model),size = 2), whisker_args = list(aes(colour=model), size = 1))  %>% relabel_predictors(c("As_EMweight" = "As", "Cr_EMweight" = "Cr", "Hg_EMweight" = "Hg", "Mn_EMweight" = "Mn", "Pb_EMweight" = "Pb", "Se_EMweight" = "Se"))
plotdist
plotdist <- plotdist+ ggtitle("idw em all") + geom_vline(xintercept = 0, colour = "grey60", linetype = 2) + theme(text = element_text(size = 12)) + 
  labs(x = bquote('Coefficent Estimate with 95% CIs')) + theme(axis.text=element_text(size=12)) + theme(axis.title.x = element_text(margin = margin(t = 12))) +
  scale_x_continuous(limits=c(-0.025, 0.005), breaks = log10(pretty(10^(all_models$estimate),n=100)), labels = (pretty(10^(all_models$estimate),n=100)))+ theme_bw() + theme(panel.border = element_rect(fill=NA, colour = "black", size=1))
plotdist

# All EM plot
all_models <- combine(mod19, mod20, mod21, mod22, mod23, mod24)
colnames(all_models)[6] ="model"
plotdist<- dwplot(all_models, dodge_size = 0.5, dot_args = list(aes(colour = model),size = 2), whisker_args = list(aes(colour=model), size = 1))  %>% relabel_predictors(c("As_EMISSIONS" = "As", "Cr_EMISSIONS" = "Cr", "Hg_EMISSIONS" = "Hg", "Mn_EMISSIONS" = "Mn", "Pb_EMISSIONS" = "Pb", "Se_EMISSIONS" = "Se"))
plotdist
plotdist <- plotdist+ ggtitle("em all") + geom_vline(xintercept = 0, colour = "grey60", linetype = 2) + theme(text = element_text(size = 12)) + 
  labs(x = bquote('Coefficent Estimate with 95% CIs')) + theme(axis.text=element_text(size=12)) + theme(axis.title.x = element_text(margin = margin(t = 12))) +
  scale_x_continuous(breaks = log(pretty(exp(all_models$estimate),n=250)), labels = (pretty(exp(all_models$estimate),n=250)))
plotdist

## Add SES/neighborhood variable dot whisker plot Pb
datalog<-datalog %>% mutate(struct5 = medianyearstructurebuilt/5)
datalog<-datalog %>% mutate(edu5 = CE_C1/5)
datalog<-datalog %>% mutate(perbelowpov10 = percentbelowpovertylevel/10)

moda<- lm(Pb ~ PbDistance + CE_C1 + CE_BMI + CE_AGE + EN_FORMERSMOKER + passivesmoke + road50 + THC_CUMULATIVE1 + workstat + STATEFP, data = datalog)
modb<- lm(Pb ~ PbDistance + percentbelowpovertylevel + CE_BMI + CE_AGE + EN_FORMERSMOKER + passivesmoke + road50 + THC_CUMULATIVE1 + workstat + STATEFP, data = datalog)
modc<- lm(Pb ~ PbDistance + medianyearstructurebuilt + CE_BMI + CE_AGE + EN_FORMERSMOKER + passivesmoke + road50 + THC_CUMULATIVE1 + workstat + STATEFP, data = datalog)
modd<- lm(Pb ~ Pb_Dweight + CE_C1 + CE_BMI + CE_AGE + EN_FORMERSMOKER + passivesmoke + road50 + THC_CUMULATIVE1 + workstat + STATEFP, data = datalog)
mode<- lm(Pb ~ Pb_Dweight + percentbelowpovertylevel + CE_BMI + CE_AGE + EN_FORMERSMOKER + passivesmoke + road50 + THC_CUMULATIVE1 + workstat + STATEFP, data = datalog)
modf<- lm(Pb ~ Pb_Dweight + medianyearstructurebuilt + CE_BMI + CE_AGE + EN_FORMERSMOKER + passivesmoke + road50 + THC_CUMULATIVE1 + workstat + STATEFP, data = datalog)

moda<- lm(Pb ~ PbDistance + EN_TOTINCOME + CE_BMI + CE_AGE + EN_FORMERSMOKER + passivesmoke + road50 + THC_CUMULATIVE1 + workstat + STATEFP, data = datalog)
summary(moda)
(10^(coef(moda))-1)*100
(10^(confint(modd))-1)*100

moda <-broom::tidy(moda) %>% filter(term != "road50") %>% filter(term != "road501") %>% filter(term != "CE_BMI")%>% filter(term != "CE_AGE")%>%filter(term != "EN_FORMERSMOKER1")%>%filter(term != "passivesmoke1")%>%filter(term != "THC_CUMULATIVE1")%>%filter(term != "workstat1")%>%filter(term != "STATEFP12")%>%filter(term != "STATEFP22")%>%filter(term != "STATEFP28")%>%filter(term != "workstat")
modb <-broom::tidy(modb) %>% filter(term != "road50") %>% filter(term != "road501") %>% filter(term != "CE_BMI")%>% filter(term != "CE_AGE")%>%filter(term != "EN_FORMERSMOKER1")%>%filter(term != "passivesmoke1")%>%filter(term != "THC_CUMULATIVE1")%>%filter(term != "workstat1")%>%filter(term != "STATEFP12")%>%filter(term != "STATEFP22")%>%filter(term != "STATEFP28")%>%filter(term != "workstat")
modc <-broom::tidy(modc) %>% filter(term != "road50") %>% filter(term != "road501") %>% filter(term != "CE_BMI")%>% filter(term != "CE_AGE")%>%filter(term != "EN_FORMERSMOKER1")%>%filter(term != "passivesmoke1")%>%filter(term != "THC_CUMULATIVE1")%>%filter(term != "workstat1")%>%filter(term != "STATEFP12")%>%filter(term != "STATEFP22")%>%filter(term != "STATEFP28")%>%filter(term != "workstat")
modd <-broom::tidy(modd) %>% filter(term != "road50") %>% filter(term != "road501") %>% filter(term != "CE_BMI")%>% filter(term != "CE_AGE")%>%filter(term != "EN_FORMERSMOKER1")%>%filter(term != "passivesmoke1")%>%filter(term != "THC_CUMULATIVE1")%>%filter(term != "workstat1")%>%filter(term != "STATEFP12")%>%filter(term != "STATEFP22")%>%filter(term != "STATEFP28")%>%filter(term != "workstat")
mode <-broom::tidy(mode) %>% filter(term != "road50") %>% filter(term != "road501") %>% filter(term != "CE_BMI")%>% filter(term != "CE_AGE")%>%filter(term != "EN_FORMERSMOKER1")%>%filter(term != "passivesmoke1")%>%filter(term != "THC_CUMULATIVE1")%>%filter(term != "workstat1")%>%filter(term != "STATEFP12")%>%filter(term != "STATEFP22")%>%filter(term != "STATEFP28")%>%filter(term != "workstat")
modf <-broom::tidy(modf) %>% filter(term != "road50") %>% filter(term != "road501") %>% filter(term != "CE_BMI")%>% filter(term != "CE_AGE")%>%filter(term != "EN_FORMERSMOKER1")%>%filter(term != "passivesmoke1")%>%filter(term != "THC_CUMULATIVE1")%>%filter(term != "workstat1")%>%filter(term != "STATEFP12")%>%filter(term != "STATEFP22")%>%filter(term != "STATEFP28")%>%filter(term != "workstat")

all_models <- combine(moda, modb, modc)
all_idw<-combine(modd, mode, modf)
colnames(all_models)[6] ="model"
colnames(all_idw)[6] ="model"
library(scales)

plotPb<- dwplot(all_models, dodge_size = 0.5, dot_args = list(aes(colour = model),size = 2), whisker_args = list(aes(colour=model), size = 1)) + 
  xlim(-0.075, 0.025)+ geom_vline(xintercept = 0, colour = "grey60", linetype = 2)+ theme_bw() +theme(text = element_text(size = 12)) 
plotPb <-plotPb +scale_x_continuous(breaks = log10(pretty(10^(moda$estimate),n=10)), labels = (pretty(10^(moda$estimate),n=10)))
plotPb

plotPb<- dwplot(all_idw, dodge_size = 0.5, dot_args = list(aes(colour = model),size = 2), whisker_args = list(aes(colour=model), size = 1)) + 
  xlim(-0.075, 0.025)+ geom_vline(xintercept = 0, colour = "grey60", linetype = 2)+ theme_bw() +theme(text = element_text(size = 12)) 
plotPb <-plotPb +scale_x_continuous(breaks = log10(pretty(10^(modd$estimate),n=10)), labels = (pretty(10^(modd$estimate),n=10)))
plotPb

# plotPb<- dwplot(moda, dodge_size = 0.5, dot_args = list(aes(colour = model),size = 2), whisker_args = list(aes(colour=model), size = 1)) + 
#   xlim(-0.075, 0.025)+ geom_vline(xintercept = 0, colour = "grey60", linetype = 2)+ theme_bw() +theme(text = element_text(size = 12)) 
# plotPb <-plotPb +scale_x_continuous(breaks = log(pretty(exp(moda$estimate),n=5)), labels = (pretty(exp(moda$estimate),n=5)))
# plotPb
# 
# plotPb<- dwplot(modb, dodge_size = 0.5, dot_args = list(aes(colour = model),size = 2), whisker_args = list(aes(colour=model), size = 1)) + 
#   xlim(-0.075, 0.025)+ geom_vline(xintercept = 0, colour = "grey60", linetype = 2)+ theme_bw() +theme(text = element_text(size = 12)) 
# plotPb <-plotPb +scale_x_continuous(breaks = log(pretty(exp(modb$estimate),n=5)), labels = (pretty(exp(modb$estimate),n=5)))
# plotPb

## Test race in models
blacksubgroup<-filter(datalog, race =="Black")
whitesubgroup<-filter(datalog, race =="White")

mod<- lm(Pb ~ Pb_Dweight + race + CE_BMI + CE_AGE + EN_FORMERSMOKER + passivesmoke + road50 + THC_CUMULATIVE1 + workstat + STATEFP, data = datalog)
summary(mod)
mod1<- lm(Pb ~ Pb_Dweight + CE_BMI + CE_AGE + EN_FORMERSMOKER + passivesmoke + road50 + THC_CUMULATIVE1 + workstat + STATEFP, data = whitesubgroup)
summary(mod1)
mod2<- lm(Pb ~ Pb_Dweight+ CE_BMI + CE_AGE + EN_FORMERSMOKER + passivesmoke + road50 + THC_CUMULATIVE1 + workstat + STATEFP, data = blacksubgroup)
summary(mod2)


mod1<- lm(Pb ~ PbDistance + CE_BMI + CE_AGE + EN_FORMERSMOKER + passivesmoke + road50 + THC_CUMULATIVE1 + workstat + STATEFP, data = whitesubgroup)
summary(mod1)
mod2<- lm(Pb ~ PbDistance+ CE_BMI + CE_AGE + EN_FORMERSMOKER + passivesmoke + road50 + THC_CUMULATIVE1 + workstat + STATEFP, data = blacksubgroup)
summary(mod2)

## Mercury -------------------------------------
## Add SES/neighborhood variable dot whisker plot Hg
datalog<-datalog %>% mutate(struct5 = medianyearstructurebuilt/5)
datalog<-datalog %>% mutate(edu5 = CE_C1/5)
datalog<-datalog %>% mutate(perbelowpov10 = percentbelowpovertylevel/10)

moda<- lm(Hg ~ Hg_EMweight + CE_C1 + CE_BMI + CE_AGE + EN_FORMERSMOKER + passivesmoke + road50 + THC_CUMULATIVE1 + workstat + STATEFP, data = datalog)
modb<- lm(Hg ~ Hg_EMweight + percentbelowpovertylevel + CE_BMI + CE_AGE + EN_FORMERSMOKER + passivesmoke + road50 + THC_CUMULATIVE1 + workstat + STATEFP, data = datalog)
modc<- lm(Hg ~ Hg_EMweight + medianyearstructurebuilt  + CE_BMI + CE_AGE + EN_FORMERSMOKER + passivesmoke + road50 + THC_CUMULATIVE1 + workstat + STATEFP, data = datalog)
modd<- lm(Hg ~ Hg_Dweight + CE_C1 + CE_BMI + CE_AGE + EN_FORMERSMOKER + passivesmoke + road50 + THC_CUMULATIVE1 + workstat + STATEFP, data = datalog)
mode<- lm(Hg ~ Hg_Dweight + percentbelowpovertylevel + CE_BMI + CE_AGE + EN_FORMERSMOKER + passivesmoke + road50 + THC_CUMULATIVE1 + workstat + STATEFP, data = datalog)
modf<- lm(Hg ~ Hg_Dweight + medianyearstructurebuilt + CE_BMI + CE_AGE + EN_FORMERSMOKER + passivesmoke + road50 + THC_CUMULATIVE1 + workstat + STATEFP, data = datalog)
library(car)
vif(modf)

(10^(coef(modc))-1)*100
(10^(confint(modc))-1)*100

moda <-broom::tidy(moda) %>% filter(term != "road50") %>% filter(term != "road501") %>% filter(term != "CE_BMI")%>% filter(term != "CE_AGE")%>%filter(term != "EN_FORMERSMOKER1")%>%filter(term != "passivesmoke1")%>%filter(term != "THC_CUMULATIVE1")%>%filter(term != "workstat1")%>%filter(term != "STATEFP12")%>%filter(term != "STATEFP22")%>%filter(term != "STATEFP28")%>%filter(term != "workstat")
modb <-broom::tidy(modb) %>% filter(term != "road50") %>% filter(term != "road501") %>% filter(term != "CE_BMI")%>% filter(term != "CE_AGE")%>%filter(term != "EN_FORMERSMOKER1")%>%filter(term != "passivesmoke1")%>%filter(term != "THC_CUMULATIVE1")%>%filter(term != "workstat1")%>%filter(term != "STATEFP12")%>%filter(term != "STATEFP22")%>%filter(term != "STATEFP28")%>%filter(term != "workstat")
modc <-broom::tidy(modc) %>% filter(term != "road50") %>% filter(term != "road501") %>% filter(term != "CE_BMI")%>% filter(term != "CE_AGE")%>%filter(term != "EN_FORMERSMOKER1")%>%filter(term != "passivesmoke1")%>%filter(term != "THC_CUMULATIVE1")%>%filter(term != "workstat1")%>%filter(term != "STATEFP12")%>%filter(term != "STATEFP22")%>%filter(term != "STATEFP28")%>%filter(term != "workstat")
modd <-broom::tidy(modd) %>% filter(term != "road50") %>% filter(term != "road501") %>% filter(term != "CE_BMI")%>% filter(term != "CE_AGE")%>%filter(term != "EN_FORMERSMOKER1")%>%filter(term != "passivesmoke1")%>%filter(term != "THC_CUMULATIVE1")%>%filter(term != "workstat1")%>%filter(term != "STATEFP12")%>%filter(term != "STATEFP22")%>%filter(term != "STATEFP28")%>%filter(term != "workstat")
mode <-broom::tidy(mode) %>% filter(term != "road50") %>% filter(term != "road501") %>% filter(term != "CE_BMI")%>% filter(term != "CE_AGE")%>%filter(term != "EN_FORMERSMOKER1")%>%filter(term != "passivesmoke1")%>%filter(term != "THC_CUMULATIVE1")%>%filter(term != "workstat1")%>%filter(term != "STATEFP12")%>%filter(term != "STATEFP22")%>%filter(term != "STATEFP28")%>%filter(term != "workstat")
modf <-broom::tidy(modf) %>% filter(term != "road50") %>% filter(term != "road501") %>% filter(term != "CE_BMI")%>% filter(term != "CE_AGE")%>%filter(term != "EN_FORMERSMOKER1")%>%filter(term != "passivesmoke1")%>%filter(term != "THC_CUMULATIVE1")%>%filter(term != "workstat1")%>%filter(term != "STATEFP12")%>%filter(term != "STATEFP22")%>%filter(term != "STATEFP28")%>%filter(term != "workstat")

all_em <- combine(moda, modb, modc)
all_d<-combine(modd, mode, modf)
colnames(all_em)[6] ="model"
colnames(all_d)[6] ="model"
library(scales)

plotHg<- dwplot(all_d, dodge_size = 0.5, dot_args = list(aes(colour = model),size = 2), whisker_args = list(aes(colour=model), size = 1)) + 
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2)+ theme_bw() +theme(text = element_text(size = 12)) 
plotHg
plotHg <-plotHg +scale_x_continuous(breaks = log10(pretty(10^(modd$estimate),n=5)), labels = (pretty(10^(modd$estimate),n=5)))
plotHg

plotHg<- dwplot(all_em, dodge_size = 0.5, dot_args = list(aes(colour = model),size = 2), whisker_args = list(aes(colour=model), size = 1)) + 
 geom_vline(xintercept = 0, colour = "grey60", linetype = 2)+ theme_bw() +theme(text = element_text(size = 12)) 
plotHg <-plotHg +scale_x_continuous(breaks = log10(pretty(10^(moda$estimate),n=20)), labels = (pretty(10^(moda$estimate),n=20)))
plotHg

## dot whisker split by race Pb IDW
library(dotwhisker)
library(dplyr)
library(gdata)
blacksubgroup<-filter(datalog, race =="Black")
whitesubgroup<-filter(datalog, race =="White")

mod1<- lm(Hg ~ Hg_Dweight + CE_BMI + CE_AGE + EN_FORMERSMOKER + passivesmoke + road50 + THC_CUMULATIVE1 + workstat + STATEFP, data = datalog)
mod2<- lm(Hg ~ Hg_Dweight + CE_BMI + CE_AGE + EN_FORMERSMOKER + passivesmoke + road50 + THC_CUMULATIVE1 + workstat + STATEFP, data = whitesubgroup)
mod3<- lm(Hg ~ Hg_Dweight + CE_BMI + CE_AGE + EN_FORMERSMOKER + passivesmoke + road50 + THC_CUMULATIVE1 + workstat + STATEFP, data = blacksubgroup)

All <-broom::tidy(mod1) %>% filter(term != "road50") %>% filter(term != "road501") %>% filter(term != "CE_BMI")%>% filter(term != "CE_AGE")%>%filter(term != "EN_FORMERSMOKER1")%>%filter(term != "passivesmoke1")%>%filter(term != "THC_CUMULATIVE1")%>%filter(term != "workstat1")%>%filter(term != "STATEFP12")%>%filter(term != "STATEFP22")%>%filter(term != "STATEFP28")%>%filter(term != "workstat")
White <-broom::tidy(mod2) %>% filter(term != "road50") %>% filter(term != "road501") %>% filter(term != "CE_BMI")%>% filter(term != "CE_AGE")%>%filter(term != "EN_FORMERSMOKER1")%>%filter(term != "passivesmoke1")%>%filter(term != "THC_CUMULATIVE1")%>%filter(term != "workstat1")%>%filter(term != "STATEFP12")%>%filter(term != "STATEFP22")%>%filter(term != "STATEFP28")%>%filter(term != "workstat")
Black <-broom::tidy(mod3) %>% filter(term != "road50") %>% filter(term != "road501") %>% filter(term != "CE_BMI")%>% filter(term != "CE_AGE")%>%filter(term != "EN_FORMERSMOKER1")%>%filter(term != "passivesmoke1")%>%filter(term != "THC_CUMULATIVE1")%>%filter(term != "workstat1")%>%filter(term != "STATEFP12")%>%filter(term != "STATEFP22")%>%filter(term != "STATEFP28")%>%filter(term != "workstat")

all_models <- combine(All, White, Black)
colnames(all_models)[6] ="model"

plotRace_As<- dwplot(all_models, dodge_size = 0.5, dot_args = list(aes(colour = model),size = 2), whisker_args = list(aes(colour=model), size = 1)) +
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2)+ theme_bw() +theme(text = element_text(size = 12)) + xlim(-1, 0.6)
plotRace_As
plotRace_As1 <- plotRace_As + scale_x_continuous(limits = c(-1, 0.6), breaks = log10(pretty(10^(All$estimate),n=2)), labels = (pretty(10^(All$estimate),n=2)))
plotRace_As1

## dot whisker split by race Pb km
blacksubgroup<-filter(datalog, race =="Black")
whitesubgroup<-filter(datalog, race =="White")

mod1<- lm(Pb ~ PbDistance + CE_BMI + CE_AGE + EN_FORMERSMOKER + passivesmoke + road50 + THC_CUMULATIVE1 + workstat + STATEFP, data = datalog)
mod2<- lm(Pb ~ PbDistance + CE_BMI + CE_AGE + EN_FORMERSMOKER + passivesmoke + road50 + THC_CUMULATIVE1 + workstat + STATEFP, data = whitesubgroup)
mod3<- lm(Pb ~ PbDistance + CE_BMI + CE_AGE + EN_FORMERSMOKER + passivesmoke + road50 + THC_CUMULATIVE1 + workstat + STATEFP, data = blacksubgroup)

All <-broom::tidy(mod1) %>% filter(term != "road50") %>% filter(term != "road501") %>% filter(term != "CE_BMI")%>% filter(term != "CE_AGE")%>%filter(term != "EN_FORMERSMOKER1")%>%filter(term != "passivesmoke1")%>%filter(term != "THC_CUMULATIVE1")%>%filter(term != "workstat1")%>%filter(term != "STATEFP12")%>%filter(term != "STATEFP22")%>%filter(term != "STATEFP28")%>%filter(term != "workstat")
White <-broom::tidy(mod2) %>% filter(term != "road50") %>% filter(term != "road501") %>% filter(term != "CE_BMI")%>% filter(term != "CE_AGE")%>%filter(term != "EN_FORMERSMOKER1")%>%filter(term != "passivesmoke1")%>%filter(term != "THC_CUMULATIVE1")%>%filter(term != "workstat1")%>%filter(term != "STATEFP12")%>%filter(term != "STATEFP22")%>%filter(term != "STATEFP28")%>%filter(term != "workstat")
Black <-broom::tidy(mod3) %>% filter(term != "road50") %>% filter(term != "road501") %>% filter(term != "CE_BMI")%>% filter(term != "CE_AGE")%>%filter(term != "EN_FORMERSMOKER1")%>%filter(term != "passivesmoke1")%>%filter(term != "THC_CUMULATIVE1")%>%filter(term != "workstat1")%>%filter(term != "STATEFP12")%>%filter(term != "STATEFP22")%>%filter(term != "STATEFP28")%>%filter(term != "workstat")

all_models <- combine(All, White, Black)
colnames(all_models)[6] ="model"

plotRace_As<- dwplot(all_models, dodge_size = 0.5, dot_args = list(aes(colour = model),size = 2), whisker_args = list(aes(colour=model), size = 1)) +
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2)+ theme_bw() +theme(text = element_text(size = 12)) 
plotRace_As
plotRace_As1 <- plotRace_As + scale_x_continuous(limits = c(-0.08, 0.01), breaks = log10(pretty(10^(Black$estimate),n=20)), labels = (pretty(10^(Black$estimate),n=20)))
plotRace_As1 

## dot whisker split by race Hg
blacksubgroup<-filter(datalog, race =="Black")
whitesubgroup<-filter(datalog, race =="White")

mod1<- lm(Hg ~ Hg_EMweight + CE_BMI + CE_AGE + EN_FORMERSMOKER + passivesmoke + road50 + THC_CUMULATIVE1 + workstat + STATEFP, data = datalog)
mod2<- lm(Hg ~ Hg_EMweight + CE_BMI + CE_AGE + EN_FORMERSMOKER + passivesmoke + road50 + THC_CUMULATIVE1 + workstat + STATEFP, data = whitesubgroup)
mod3<- lm(Hg ~ Hg_EMweight + CE_BMI + CE_AGE + EN_FORMERSMOKER + passivesmoke + road50 + THC_CUMULATIVE1 + workstat + STATEFP, data = blacksubgroup)

All <-broom::tidy(mod1) %>% filter(term != "road50") %>% filter(term != "road501") %>% filter(term != "CE_BMI")%>% filter(term != "CE_AGE")%>%filter(term != "EN_FORMERSMOKER1")%>%filter(term != "passivesmoke1")%>%filter(term != "THC_CUMULATIVE1")%>%filter(term != "workstat1")%>%filter(term != "STATEFP12")%>%filter(term != "STATEFP22")%>%filter(term != "STATEFP28")%>%filter(term != "workstat")
White <-broom::tidy(mod2) %>% filter(term != "road50") %>% filter(term != "road501") %>% filter(term != "CE_BMI")%>% filter(term != "CE_AGE")%>%filter(term != "EN_FORMERSMOKER1")%>%filter(term != "passivesmoke1")%>%filter(term != "THC_CUMULATIVE1")%>%filter(term != "workstat1")%>%filter(term != "STATEFP12")%>%filter(term != "STATEFP22")%>%filter(term != "STATEFP28")%>%filter(term != "workstat")
Black <-broom::tidy(mod3) %>% filter(term != "road50") %>% filter(term != "road501") %>% filter(term != "CE_BMI")%>% filter(term != "CE_AGE")%>%filter(term != "EN_FORMERSMOKER1")%>%filter(term != "passivesmoke1")%>%filter(term != "THC_CUMULATIVE1")%>%filter(term != "workstat1")%>%filter(term != "STATEFP12")%>%filter(term != "STATEFP22")%>%filter(term != "STATEFP28")%>%filter(term != "workstat")

all_models <- combine(All, White, Black)
colnames(all_models)[6] ="model"

plotRace_As<- dwplot(all_models, dodge_size = 0.5, dot_args = list(aes(colour = model),size = 2), whisker_args = list(aes(colour=model), size = 1)) +
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2)+ theme_bw() +theme(text = element_text(size = 12)) 
plotRace_As
plotRace_As1 <- plotRace_As + scale_x_continuous(breaks = log10(pretty(10^(all_models$estimate),n=60)), labels = (pretty(10^(all_models$estimate),n=60)))
plotRace_As1 



## dot whisker split by race Pb additional adjustment for SES
blacksubgroup<-filter(datalog, race =="Black")
whitesubgroup<-filter(datalog, race =="White")

mod1<- lm(Hg ~ Hg_Dweight + income + CE_BMI + CE_AGE + EN_FORMERSMOKER + passivesmoke + road50 + THC_CUMULATIVE1 + workstat + STATEFP, data = datalog)
mod2<- lm(Hg ~ Hg_Dweight + income + CE_BMI + CE_AGE + EN_FORMERSMOKER + passivesmoke + road50 + THC_CUMULATIVE1 + workstat + STATEFP, data = whitesubgroup)
mod3<- lm(Hg ~ Hg_Dweight + income + CE_BMI + CE_AGE + EN_FORMERSMOKER + passivesmoke + road50 + THC_CUMULATIVE1 + workstat + STATEFP, data = blacksubgroup)

All <-broom::tidy(mod1) %>% filter(term != "road50") %>% filter(term != "road501") %>% filter(term != "CE_BMI")%>% filter(term != "CE_AGE")%>%filter(term != "EN_FORMERSMOKER1")%>%filter(term != "passivesmoke1")%>%filter(term != "THC_CUMULATIVE1")%>%filter(term != "workstat1")%>%filter(term != "STATEFP12")%>%filter(term != "STATEFP22")%>%filter(term != "STATEFP28")%>%filter(term != "workstat")
White <-broom::tidy(mod2) %>% filter(term != "road50") %>% filter(term != "road501") %>% filter(term != "CE_BMI")%>% filter(term != "CE_AGE")%>%filter(term != "EN_FORMERSMOKER1")%>%filter(term != "passivesmoke1")%>%filter(term != "THC_CUMULATIVE1")%>%filter(term != "workstat1")%>%filter(term != "STATEFP12")%>%filter(term != "STATEFP22")%>%filter(term != "STATEFP28")%>%filter(term != "workstat")
Black <-broom::tidy(mod3) %>% filter(term != "road50") %>% filter(term != "road501") %>% filter(term != "CE_BMI")%>% filter(term != "CE_AGE")%>%filter(term != "EN_FORMERSMOKER1")%>%filter(term != "passivesmoke1")%>%filter(term != "THC_CUMULATIVE1")%>%filter(term != "workstat1")%>%filter(term != "STATEFP12")%>%filter(term != "STATEFP22")%>%filter(term != "STATEFP28")%>%filter(term != "workstat")

all_models <- combine(All, White, Black)
colnames(all_models)[6] ="model"

plotRace_As<- dwplot(all_models, dodge_size = 0.5, dot_args = list(aes(colour = model),size = 2), whisker_args = list(aes(colour=model), size = 1)) +
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2)+ theme_bw() +theme(text = element_text(size = 12)) + xlim(-1.2, 0.6)
plotRace_As
plotRace_As1 <- plotRace_As + scale_x_continuous(limits = c(-1.0, 0.6), breaks = log(pretty(exp(all_models$estimate),n=4)), labels = (pretty(exp(all_models$estimate),n=4)))
plotRace_As1 


## for paper words
mod<- lm(Pb ~ PbDistance + CE_C1 + CE_BMI + CE_AGE + EN_FORMERSMOKER + passivesmoke + road50 + THC_CUMULATIVE1 + workstat + STATEFP, data = datalog)
summary(mod)
(10^(coef(mod))-1)*100
(10^(confint(mod))-1)*100

mod<- lm(Hg ~ Hg_Dweight + CE_C1 + CE_BMI + CE_AGE + EN_FORMERSMOKER + passivesmoke + road50 + THC_CUMULATIVE1 + workstat + STATEFP, data = datalog)
summary(mod)
(10^(coef(mod))-1)*100
(10^(confint(mod))-1)*100

#race subset
blacksubgroup<-filter(datalog, race =="Black")
whitesubgroup<-filter(datalog, race =="White")

mod<- lm(Hg ~ Hg_Dweight + CE_BMI + CE_AGE + EN_FORMERSMOKER + passivesmoke + road50 + THC_CUMULATIVE1 + workstat + STATEFP, data = datalog)
summary(mod)
(10^(coef(mod))-1)*100
(10^(confint(mod))-1)*100

mod<- lm(Pb ~ Pb_Dweight + CE_BMI + CE_AGE + EN_FORMERSMOKER + passivesmoke + road50 + THC_CUMULATIVE1 + workstat + STATEFP, data = whitesubgroup)
summary(mod)
(10^(coef(mod))-1)*100
(10^(confint(mod))-1)*100

mod<- lm(Pb ~ Pb_Dweight + CE_BMI + CE_AGE + EN_FORMERSMOKER + passivesmoke + road50 + THC_CUMULATIVE1 + workstat + STATEFP, data = blacksubgroup)
summary(mod)
(10^(coef(mod))-1)*100
(10^(confint(mod))-1)*100

mod1 <- lm(Mn ~ CE_AGE + THC_CUMULATIVE1 + CE_C1 + EN_FORMERSMOKER + passivesmoke + race + CE_BMI, data = datalog)
mod2 <- lm(Se ~ CE_AGE + THC_CUMULATIVE1 + CE_C1 + EN_FORMERSMOKER + passivesmoke + race + CE_BMI, data = datalog)
mod3 <- lm(As ~ CE_AGE + THC_CUMULATIVE1 + CE_C1 + EN_FORMERSMOKER + passivesmoke + race + CE_BMI, data = datalog)
mod4 <- lm(Cr ~ CE_AGE + THC_CUMULATIVE1 + CE_C1 + EN_FORMERSMOKER + passivesmoke + race + CE_BMI, data = datalog)
mod5 <- lm(Hg ~ CE_AGE + THC_CUMULATIVE1 + CE_C1 + EN_FORMERSMOKER + passivesmoke + race + CE_BMI, data = datalog)
mod6 <- lm(Pb ~ CE_AGE + THC_CUMULATIVE1 + CE_C1 + EN_FORMERSMOKER + passivesmoke + race + CE_BMI, data = datalog)
(exp(coef(mod2))-1)*100
(exp(confint(mod2))-1)*100

### subgroup boxplot analyses -------------------------------------------------------------------------
datalog <- datalog %>% mutate(Agegroup = case_when(CE_AGE >= 60 & CE_AGE <=69 ~ '60 - 69',
                                             CE_AGE >= 40 & CE_AGE<= 59 ~ '40 - 59',
                                             CE_AGE >= 20 & CE_AGE <= 39 ~ '20 - 39')) # end function

datalog$Agegroup <- relevel(factor(datalog$Agegroup), ref = "20 - 39")
datalog$EDU <- relevel(factor(datalog$EDU), ref = "College or more")
datalog$income <- relevel(factor(datalog$income), ref = ">$50,000")
datalog$quartileTHC <- factor(datalog$quartileTHC)
datalog$race <- relevel(factor(datalog$race), ref = "White")
datalograce_ <- datalog %>% filter(race != "Other" & race != "NA")
datalograce <- datalograce_ %>% filter(income != "NA")
## income and education by race
datalograce$income <- factor(datalograce$income , levels=c("<$20,000", "$20,000 - $49,999", ">$50,000"), labels=c("<$20K", "$20 - $49K", ">$50K"))
datalograce$race <- factor(datalograce$race , levels=c("White", "Black"), labels=c("White (n=190)", "Black (n=191)"))
ggplot(datalograce, aes(x=income, y=CE_C1)) + geom_boxplot(aes(fill=race)) + geom_jitter(size=0.05, alpha=0.9) + theme(panel.border = element_rect(fill=NA, colour = "black", size=1)) + theme_bw()
ggplot(datalograce, aes(x=income, y= PbDistance)) + geom_boxplot(aes(fill=race)) + geom_jitter(size=0.1, alpha=0.9) + labs(y = bquote('Distance from nearest Pb emissions site (km)'), x = bquote('Income'))+ theme(panel.border = element_rect(fill=NA, colour = "black", size=1)) + theme_bw()+theme(text = element_text(size = 12)) + scale_y_continuous(limits = c(0, 61),breaks=c(0, 20, 40, 60)) 
ggplot(datalograce, aes(x=STATEFP, y=percentbelowpovertylevel)) + geom_boxplot(aes(fill=race)) + geom_jitter(size=0.1, alpha=0.9)+ theme(panel.border = element_rect(fill=NA, colour = "black", size=1)) + theme_bw()
ggplot(datalograce, aes(x=income, y=percentbelowpovertylevel)) + geom_boxplot(aes(fill=race)) + geom_jitter(size=0.05, alpha=0.9)+ theme(panel.border = element_rect(fill=NA, colour = "black", size=1)) + theme_bw()
ggplot(datalograce, aes(x=EDU, y=percentbelowpovertylevel)) + geom_boxplot(aes(fill=race)) + geom_jitter(size=0.05, alpha=0.9)+ theme(panel.border = element_rect(fill=NA, colour = "black", size=1)) + theme_bw()


inc <- blacksubgroup %>%
  group_by(income) %>%
  summarise(Count = n())
inc


## R1 revision comment - check if time to toenail collection confounds results
## time worked until toenail collection
datalog$EXAM_COMP_DATE = format(as.Date(datalog$EXAM_COMP_DATE, origin="1960-01-01"),"%Y-%m-%d")
datalog <- mutate(datalog, CEDate = format(as.Date(datalog$EXAM_COMP_DATE, origin="1960-01-01"),"%Y-%m-%d"))
datalog$EN_STARTDATE2 = format(as.Date(datalog$EN_STARTDATE2, origin="1960-01-01"),"%Y-%m-%d")
datalog$EN_ENDDATE2 = format(as.Date(datalog$EN_ENDDATE2, origin="1960-01-01"),"%Y-%m-%d")
datalog <- mutate(datalog, StartDate = format(as.Date(datalog$EN_STARTDATE2, origin="1960-01-01"),"%Y-%m-%d"))
datalog <- mutate(datalog, EndDate = format(as.Date(datalog$EN_ENDDATE2, origin="1960-01-01"),"%Y-%m-%d"))
datalog$EN_STARTDATE2
datalog$EN_ENDDATE2

datalog <- datalog %>% mutate(timeworked = difftime(EndDate, StartDate , units = c("days")))
mean(datalog$timeworked)
median(datalog$timeworked)
range(datalog$timeworked)

datalog <- datalog %>% mutate(toenailsincework = difftime(CEDate ,EndDate , units = c("days")))
mean(datalog$toenailsincework)
median(datalog$toenailsincework)
range(datalog$toenailsincework)

## models with timesincework
mod12<- lm(Pb ~ Pb_Dweight + toenailsincework + CE_BMI + CE_AGE + EN_FORMERSMOKER + passivesmoke + road50 + THC_CUMULATIVE1 + workstat + STATEFP, data = datalog)
summary(mod12)
summary(mod11)
