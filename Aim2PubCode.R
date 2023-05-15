setwd("/Users/joycelin/Desktop/Gulf/Aim2/GuLF_2")
library(tidyverse)
library(dplyr)

data <- read_csv("CE_NEI_IDW.csv")

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
data<- data %>% mutate(income = case_when(EN_TOTINCOME >= 5 ~ '>$50,000',
                                          EN_TOTINCOME > 1 & EN_TOTINCOME<= 4 ~ '$20,000 - $49,999',
                                          EN_TOTINCOME ==1 ~ '<$20,000')) # end function
data$income <- relevel(as.factor(data$income), ref = ">$50,000")

datlong<-pivot_longer(data, 6:23, names_to = "Metal", values_to = "Concentration")

data<- data %>% mutate(BMI = case_when(CE_BMI <= 24.9 ~ 'healthy',
                                       CE_BMI >=25 & CE_BMI <=30 ~ 'overweight',
                                       CE_BMI >=30 ~ 'obese')) # end function
data$BMI <- factor(data$BMI)

data<- data %>% mutate(PbDistancekm = PbDistance*100,
                       AsDistancekm = AsDistance*100,
                       MnDistancekm = MnDistance*100,
                       HgDistancekm = HgDistance*100,
                       SeDistancekm = SeDistance*100,
                       CrDistancekm = CrDistance*100)

datalog <- data %>% mutate(across(c(Mg, Al, Ca, Cr, Mn, Fe, Ni, Cu, Zn, As, Se, Hg, Pb), log10))
#datalog <- datalog[-c(414:416, 394, 364), ] # for old dataset "CE_blockgroupNEI_tertile.csv"

min(datalog$AsDistancekm)

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
mod1 <- lm(Mn ~ CE_AGE + THC_CUMULATIVE1 + CE_C1 + EN_FORMERSMOKER + passivesmoke + race + CE_BMI, data = datalog)
mod2 <- lm(Se ~ CE_AGE + THC_CUMULATIVE1 + CE_C1 + EN_FORMERSMOKER + passivesmoke + race + CE_BMI, data = datalog)
mod3 <- lm(As ~ CE_AGE + THC_CUMULATIVE1 + CE_C1 + EN_FORMERSMOKER + passivesmoke + race + CE_BMI, data = datalog)
mod4 <- lm(Cr ~ CE_AGE + THC_CUMULATIVE1 + CE_C1 + EN_FORMERSMOKER + passivesmoke + race + CE_BMI, data = datalog)
mod5 <- lm(Hg ~ CE_AGE + THC_CUMULATIVE1 + CE_C1 + EN_FORMERSMOKER + passivesmoke + race + CE_BMI, data = datalog)
mod6 <- lm(Pb ~ CE_AGE + THC_CUMULATIVE1 + CE_C1 + EN_FORMERSMOKER + passivesmoke + race + CE_BMI, data = datalog)

Mn <-broom::tidy(mod1) %>% filter(term != "CE_AGE") %>% filter(term != "Batch")%>% filter(term != "Batch1.5") %>% filter(term != "Batch2") %>% filter(term != "Batch3")%>% filter(term != "Batch4")%>% filter(term != "Batch5")%>% filter(term != "Batch6")%>% filter(term != "Batch6.5")%>% filter(term != "Batch7")%>% filter(term != "THC_CUMULATIVE1") %>% filter(term != "EN_FORMERSMOKER") %>% filter(term != "CE_C1") %>% filter(term != "EN_FORMERSMOKER1") %>% filter(term != "passivesmoke1")%>% filter(term != "passivesmoke")%>% filter(term != "raceOther")%>% filter(term != "CE_BMI")
Se <-broom::tidy(mod2) %>% filter(term != "CE_AGE") %>% filter(term != "Batch")%>% filter(term != "Batch1.5") %>% filter(term != "Batch2") %>% filter(term != "Batch3")%>% filter(term != "Batch4")%>% filter(term != "Batch5")%>% filter(term != "Batch6")%>% filter(term != "Batch6.5")%>% filter(term != "Batch7")%>% filter(term != "THC_CUMULATIVE1") %>% filter(term != "EN_FORMERSMOKER") %>% filter(term != "CE_C1") %>% filter(term != "EN_FORMERSMOKER1") %>% filter(term != "passivesmoke1")%>% filter(term != "passivesmoke")%>% filter(term != "raceOther")%>% filter(term != "CE_BMI")
As <-broom::tidy(mod3) %>% filter(term != "CE_AGE") %>% filter(term != "Batch")%>% filter(term != "Batch1.5") %>% filter(term != "Batch2") %>% filter(term != "Batch3")%>% filter(term != "Batch4")%>% filter(term != "Batch5")%>% filter(term != "Batch6")%>% filter(term != "Batch6.5")%>% filter(term != "Batch7")%>% filter(term != "THC_CUMULATIVE1") %>% filter(term != "EN_FORMERSMOKER") %>% filter(term != "CE_C1") %>% filter(term != "EN_FORMERSMOKER1") %>% filter(term != "passivesmoke1")%>% filter(term != "passivesmoke")%>% filter(term != "raceOther")%>% filter(term != "CE_BMI")
Cr <-broom::tidy(mod4) %>% filter(term != "CE_AGE") %>% filter(term != "Batch")%>% filter(term != "Batch1.5") %>% filter(term != "Batch2") %>% filter(term != "Batch3")%>% filter(term != "Batch4")%>% filter(term != "Batch5")%>% filter(term != "Batch6")%>% filter(term != "Batch6.5")%>% filter(term != "Batch7")%>% filter(term != "THC_CUMULATIVE1") %>% filter(term != "EN_FORMERSMOKER") %>% filter(term != "CE_C1") %>% filter(term != "EN_FORMERSMOKER1") %>% filter(term != "passivesmoke1")%>% filter(term != "passivesmoke")%>% filter(term != "raceOther")%>% filter(term != "CE_BMI")
Hg <-broom::tidy(mod5) %>% filter(term != "CE_AGE") %>% filter(term != "Batch")%>% filter(term != "Batch1.5") %>% filter(term != "Batch2") %>% filter(term != "Batch3")%>% filter(term != "Batch4")%>% filter(term != "Batch5")%>% filter(term != "Batch6")%>% filter(term != "Batch6.5")%>% filter(term != "Batch7")%>% filter(term != "THC_CUMULATIVE1") %>% filter(term != "EN_FORMERSMOKER") %>% filter(term != "CE_C1") %>% filter(term != "EN_FORMERSMOKER1") %>% filter(term != "passivesmoke1")%>% filter(term != "passivesmoke")%>% filter(term != "raceOther")%>% filter(term != "CE_BMI")
Pb <-broom::tidy(mod6) %>% filter(term != "CE_AGE") %>% filter(term != "Batch")%>% filter(term != "Batch1.5") %>% filter(term != "Batch2") %>% filter(term != "Batch3")%>% filter(term != "Batch4")%>% filter(term != "Batch5")%>% filter(term != "Batch6")%>% filter(term != "Batch6.5")%>% filter(term != "Batch7")%>% filter(term != "THC_CUMULATIVE1") %>% filter(term != "EN_FORMERSMOKER") %>% filter(term != "CE_C1") %>% filter(term != "EN_FORMERSMOKER1") %>% filter(term != "passivesmoke1")%>% filter(term != "passivesmoke")%>% filter(term != "raceOther")%>% filter(term != "CE_BMI")

summary(mod1)
(exp(coef(mod3))-1)*100
library(dotwhisker)
library(dplyr)
library(gdata)
essential_models <- combine(Se, Mn, Pb, Hg, Cr, As)
essential_models <- mutate(estimate)
colnames(essential_models)[6] ="model"
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#0072B2", "#D55E00", "#CC79A7")


plotRace_essential<- dwplot(essential_models, dodge_size = 1, dot_args = list(aes(color = model),size = 2), whisker_args = list(aes(colour=model), size = 1)) %>% relabel_predictors(c("raceBlack" = "Black"))
plotRace_essential
plotRace_essential1 <- plotRace_essential + geom_vline(xintercept = 0, colour = "grey60", linetype = 2) + ggtitle("NEI Metals") + 
  scale_color_manual(values=cbPalette) + theme(text = element_text(size = 12)) +  labs(x = bquote('Percent difference compared to White')) + 
  theme(axis.text=element_text(size=12)) + theme(axis.title.x = element_text(margin = margin(t = 12))) +  
  scale_x_continuous(breaks = log(pretty(exp(essential_models$estimate),n=15)), labels = pretty(exp(essential_models$estimate),n=15))
plotRace_essential1 
ggsave("metals.png", plotRace_essential1, bg='transparent')
png("essential.png", width = 9.9, height = 5.4, units = 'in', res = 300) 
plotRace_essential1 
dev.off()


##corrlation matrix of covariates
datacor <- data[, c("CE_AGE", "THC_CUMULATIVE1", "EN_RACE2", "EN_EDU", "Pb5_SUMEMISSIONS", "medianyearstructurebuilt", "percentbelowpovertylevel", "medianhouseholdincome", "CE_C1", "EN_TOTINCOME", "PbDistance", "Pb5_pointcount")]
library(Hmisc)
cormat = rcorr(as.matrix(datacor))
cormat


