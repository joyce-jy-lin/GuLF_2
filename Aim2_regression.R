setwd("/Users/joycelin/Desktop/Gulf/Aim2/GuLF_2")
library(tidyverse)
library(dplyr)
library(Hmisc)
data <- read_csv("CE_NEI_TRI_IDW.csv")

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

datlong<-pivot_longer(data, 7:24, names_to = "Metal", values_to = "Concentration")
datlongmain<-datlong %>% filter(Metal %notin% low_metals) 

data<- data %>% mutate(BMI = case_when(CE_BMI <= 24.9 ~ 'healthy',
                                       CE_BMI >=25 & CE_BMI <=30 ~ 'overweight',
                                       CE_BMI >=30 ~ 'obese')) # end function

data$BMI <- factor(data$BMI)

data<- data %>% mutate(workstat = case_when(workstat == 1 ~ 1,
                                          workstat > 1 ~ 0)) # end function
data$workstat <- as.factor(data$workstat)


data<- data %>% mutate(PbDistance = PbDistance*100,
                       CrDistance = CrDistance*100,
                       AsDistance = AsDistance*100,
                       MnDistance = MnDistance*100,
                       SeDistance = SeDistance*100,
                       HgDistance = HgDistance*100) # end function


data_log <- data %>% mutate(across(c(Mg, Al, Ca, Cr, Mn, Fe, Ni, Cu, Zn, As, Se, Hg, Pb), log10))

blacksubgroup<-filter(data_log, race =="Black")
whitesubgroup<-filter(data_log, race =="White")

# Testing each covariate in univariate models
m1<- lm(Pb ~ PbDistance, data = data_log) 
summary(m1)

m2<- lm(Pb ~ Pb_Dweight, data = data_log) 
summary(m2)

m1<- lm(Hg ~ HgDistance, data = data_log) 
summary(m1)

m2<- lm(Hg ~ Hg_EMweight, data = data_log) 
summary(m2)


m1<- lm(Zn ~ TRI_Zn_Dweight, data = data_log) 
summary(m1)

m2<- lm(Hg ~ Hg_EMweight, data = data_log) 
summary(m2)

# Test negative control using Cr data
#subset for when pb EMweight. = 0
Pb_no <- subset(data_log, data_log$Pb_EMweight == 0)

m2<- lm(Pb ~ Pb_Dweight, data = Pb_no) 
summary(m2)

## short cut way using map() in purrr
library(dplyr)
library(purrr)

dataSe <- data_log[, c("Se", "race", "CE_AGE", "THC_CUMULATIVE1", "TRI_Se_EMweight", "TRI_Se_Dweight", "Se_EMweight", "Se_Dweight", "SeDistance","Se_EMISSIONS", "Se10_SUMEMISSIONS", "Se5_SUMEMISSIONS", "Se3_SUMEMISSIONS", "Se10_pointcount","Se5_pointcount", "Se3_pointcount","medianyearstructurebuilt", "percentbelowpovertylevel", "medianhouseholdincome", "Totalpop", "percenthispanicllatino", "percentblack", "percentamericanindian", "percentasian", "lessthanhighschool", "owneroccupied", "CE_C1", "EN_TOTINCOME", "EN_FORMERSMOKER", "passivesmoke", "STATEFP", "road50", "road100", "road200", "CE_BMI")]
d<-dataSe %>% 
  map(~lm(Se ~ .x, data = dataSe)) %>% 
  map(summary)
d


dataCu <- data_log[, c("Cu", "race", "CE_AGE", "THC_CUMULATIVE1", "TRI_Cu_EMweight", "TRI_Cu_Dweight","medianyearstructurebuilt", "percentbelowpovertylevel", "medianhouseholdincome", "Totalpop", "percenthispanicllatino", "percentblack", "percentamericanindian", "percentasian", "lessthanhighschool", "owneroccupied", "CE_C1", "EN_TOTINCOME", "EN_FORMERSMOKER", "passivesmoke", "STATEFP", "road50", "road100", "road200", "CE_BMI")]
d<-dataCu %>% 
  map(~lm(Cu ~ .x, data = dataCu)) %>% 
  map(summary)
d

## plot individual univariate models, dot whisker
library(dotwhisker)
library(dplyr)
library(gdata)

mod1<- lm(Pb ~ PbDistance + CE_BMI + CE_AGE + EN_FORMERSMOKER + passivesmoke + road50 + THC_CUMULATIVE1 + workstat + STATEFP, data = data_log)
mod2<- lm(Pb ~ Pb_EMISSIONS + CE_BMI + CE_AGE + EN_FORMERSMOKER + passivesmoke + road50 + THC_CUMULATIVE1 + workstat + STATEFP, data = data_log)
mod3<- lm(Pb ~ Pb_Dweight + CE_BMI + CE_AGE + EN_FORMERSMOKER + passivesmoke + road50 + THC_CUMULATIVE1 + workstat + STATEFP, data = data_log)
mod4<- lm(Pb ~ Pb_EMweight + CE_BMI + CE_AGE + EN_FORMERSMOKER + passivesmoke + road50 + THC_CUMULATIVE1 + workstat + STATEFP, data = data_log)
mod5<- lm(Pb ~ Pb10_pointcount + CE_BMI + CE_AGE + EN_FORMERSMOKER + passivesmoke + road50 + THC_CUMULATIVE1 + workstat + STATEFP, data = data_log)
mod6<- lm(Pb ~ Pb10_SUMEMISSIONS + CE_BMI + CE_AGE + EN_FORMERSMOKER + passivesmoke + road50 + THC_CUMULATIVE1 + workstat + STATEFP, data = data_log)
mod7<- lm(Pb ~ Pb5_pointcount + CE_BMI + CE_AGE + EN_FORMERSMOKER + passivesmoke + road50 + THC_CUMULATIVE1 + workstat + STATEFP, data = data_log)
mod8<- lm(Pb ~ Pb5_SUMEMISSIONS + CE_BMI + CE_AGE + EN_FORMERSMOKER + passivesmoke + road50 + THC_CUMULATIVE1 + workstat + STATEFP, data = data_log)
mod9<- lm(Pb ~ Pb3_pointcount + CE_BMI + CE_AGE + EN_FORMERSMOKER + passivesmoke + road50 + THC_CUMULATIVE1 + workstat + STATEFP, data = data_log)
mod10<- lm(Pb ~ Pb3_SUMEMISSIONS + CE_BMI + CE_AGE + EN_FORMERSMOKER + passivesmoke + road50 + THC_CUMULATIVE1 + workstat + STATEFP, data = data_log)
mod11<- lm(Pb ~ TRI_Pb_Dweight + CE_BMI + CE_AGE + EN_FORMERSMOKER + passivesmoke + road50 + THC_CUMULATIVE1 + workstat + STATEFP, data = data_log)
mod12<- lm(Pb ~ TRI_Pb_EMweight + CE_BMI + CE_AGE + EN_FORMERSMOKER + passivesmoke + road50 + THC_CUMULATIVE1 + workstat + STATEFP, data = data_log)

mod1<- lm(As ~ AsDistance + CE_BMI + CE_AGE + EN_FORMERSMOKER + passivesmoke + road50 + THC_CUMULATIVE1 + workstat + STATEFP, data = data_log)


# for plot showing all covariates
Distance <-broom::tidy(mod1) %>% filter(term != "road501")%>% filter(term != "road50")%>% filter(term != "STATEFP12")%>% filter(term != "STATEFP22")%>% filter(term != "STATEFP28")%>% filter(term != "THC_CUMULATIVE1")%>% filter(term != "workstat1")%>% filter(term != "passivesmoke1")%>% filter(term != "EN_FORMERSMOKER1")
Emissions <-broom::tidy(mod2)%>% filter(term != "road501")%>% filter(term != "road50")
Count <-broom::tidy(mod3)%>% filter(term != "road501")%>% filter(term != "road50")

plotRace_Pb<- dwplot(Distance, dodge_size = 0.5, dot_args = list(aes(colour = model),size = 2), whisker_args = list(aes(colour=model), size = 1)) %>% relabel_predictors(c("raceBlack" = "Black", "PbQuartile2" = "Pb Q2", "PbQuartile3" = "Pb Q3", "PbQuartile4" = "Pb Q4"))
plotRace_Pb
plotRace_Pb1 <- plotRace_Pb + geom_vline(xintercept = 0, colour = "grey60", linetype = 2)  + theme(text = element_text(size = 12)) + labs(x = bquote('Coefficent Estimate with 95% CIs')) + theme(axis.text=element_text(size=12)) + theme(axis.title.x = element_text(margin = margin(t = 12))) 
plotRace_Pb1

## For plots comparing different Pb models
Pb_Dweight <-broom::tidy(mod3) %>% filter(term != "road50") %>% filter(term != "road501") %>% filter(term != "CE_BMI")%>% filter(term != "CE_AGE")%>%filter(term != "EN_FORMERSMOKER1")%>%filter(term != "passivesmoke1")%>%filter(term != "THC_CUMULATIVE1")%>%filter(term != "workstat1")%>%filter(term != "STATEFP12")%>%filter(term != "STATEFP22")%>%filter(term != "STATEFP28")
Pb_EMweight <-broom::tidy(mod4)%>% filter(term != "road50")  %>% filter(term != "road501")%>% filter(term != "CE_BMI")%>% filter(term != "CE_AGE")%>%filter(term != "EN_FORMERSMOKER1")%>%filter(term != "passivesmoke1")%>%filter(term != "THC_CUMULATIVE1")%>%filter(term != "workstat1")%>%filter(term != "STATEFP12")%>%filter(term != "STATEFP22")%>%filter(term != "STATEFP28")
PbDistance <-broom::tidy(mod1) %>% filter(term != "road50") %>% filter(term != "road501") %>% filter(term != "CE_BMI")%>% filter(term != "CE_AGE")%>%filter(term != "EN_FORMERSMOKER1")%>%filter(term != "passivesmoke1")%>%filter(term != "THC_CUMULATIVE1")%>%filter(term != "workstat1")%>%filter(term != "STATEFP12")%>%filter(term != "STATEFP22")%>%filter(term != "STATEFP28")
Pb_EMISSIONS <-broom::tidy(mod2)%>% filter(term != "road50")  %>% filter(term != "road501")%>% filter(term != "CE_BMI")%>% filter(term != "CE_AGE")%>%filter(term != "EN_FORMERSMOKER1")%>%filter(term != "passivesmoke1")%>%filter(term != "THC_CUMULATIVE1")%>%filter(term != "workstat1")%>%filter(term != "STATEFP12")%>%filter(term != "STATEFP22")%>%filter(term != "STATEFP28")
Pb10_pointcount <-broom::tidy(mod5)%>% filter(term != "road50") %>% filter(term != "road501") %>% filter(term != "CE_BMI")%>% filter(term != "CE_AGE")%>%filter(term != "EN_FORMERSMOKER1")%>%filter(term != "passivesmoke1")%>%filter(term != "THC_CUMULATIVE1")%>%filter(term != "workstat1")%>%filter(term != "STATEFP12")%>%filter(term != "STATEFP22")%>%filter(term != "STATEFP28")
Pb10_SUMEMISSIONS <-broom::tidy(mod6)%>% filter(term != "road50")  %>% filter(term != "road501")%>% filter(term != "CE_BMI")%>% filter(term != "CE_AGE")%>%filter(term != "EN_FORMERSMOKER1")%>%filter(term != "passivesmoke1")%>%filter(term != "THC_CUMULATIVE1")%>%filter(term != "workstat1")%>%filter(term != "STATEFP12")%>%filter(term != "STATEFP22")%>%filter(term != "STATEFP28")
Pb5_pointcount <-broom::tidy(mod7)%>% filter(term != "road50")  %>% filter(term != "road501")%>% filter(term != "CE_BMI")%>% filter(term != "CE_AGE")%>%filter(term != "EN_FORMERSMOKER1")%>%filter(term != "passivesmoke1")%>%filter(term != "THC_CUMULATIVE1")%>%filter(term != "workstat1")%>%filter(term != "STATEFP12")%>%filter(term != "STATEFP22")%>%filter(term != "STATEFP28")
Pb5_SUMEMISSIONS <-broom::tidy(mod8)%>% filter(term != "road50")  %>% filter(term != "road501")%>% filter(term != "CE_BMI")%>% filter(term != "CE_AGE")%>%filter(term != "EN_FORMERSMOKER1")%>%filter(term != "passivesmoke1")%>%filter(term != "THC_CUMULATIVE1")%>%filter(term != "workstat1")%>%filter(term != "STATEFP12")%>%filter(term != "STATEFP22")%>%filter(term != "STATEFP28")
Pb3_pointcount <-broom::tidy(mod9)%>% filter(term != "road50") %>% filter(term != "road501") %>% filter(term != "CE_BMI")%>% filter(term != "CE_AGE")%>%filter(term != "EN_FORMERSMOKER1")%>%filter(term != "passivesmoke1")%>%filter(term != "THC_CUMULATIVE1")%>%filter(term != "workstat1")%>%filter(term != "STATEFP12")%>%filter(term != "STATEFP22")%>%filter(term != "STATEFP28")
Pb3_SUMEMISSIONS <-broom::tidy(mod10)%>% filter(term != "road50")  %>% filter(term != "road501")%>% filter(term != "CE_BMI")%>% filter(term != "CE_AGE")%>%filter(term != "EN_FORMERSMOKER1")%>%filter(term != "passivesmoke1")%>%filter(term != "THC_CUMULATIVE1")%>%filter(term != "workstat1")%>%filter(term != "STATEFP12")%>%filter(term != "STATEFP22")%>%filter(term != "STATEFP28")
PbTRI_Dweight <-broom::tidy(mod11)%>% filter(term != "road50")  %>% filter(term != "road501")%>% filter(term != "CE_BMI")%>% filter(term != "CE_AGE")%>%filter(term != "EN_FORMERSMOKER1")%>%filter(term != "passivesmoke1")%>%filter(term != "THC_CUMULATIVE1")%>%filter(term != "workstat1")%>%filter(term != "STATEFP12")%>%filter(term != "STATEFP22")%>%filter(term != "STATEFP28")
PbTRI_EMweight <-broom::tidy(mod12)%>% filter(term != "road50")  %>% filter(term != "road501")%>% filter(term != "CE_BMI")%>% filter(term != "CE_AGE")%>%filter(term != "EN_FORMERSMOKER1")%>%filter(term != "passivesmoke1")%>%filter(term != "THC_CUMULATIVE1")%>%filter(term != "workstat1")%>%filter(term != "STATEFP12")%>%filter(term != "STATEFP22")%>%filter(term != "STATEFP28")

# All plot
all_models <- combine(PbDistance,Pb_EMISSIONS,Pb_Dweight, Pb_EMweight, Pb10_pointcount, Pb5_pointcount, Pb3_pointcount,Pb10_SUMEMISSIONS, Pb5_SUMEMISSIONS,Pb3_SUMEMISSIONS)
plotPb<- dwplot(all_models, dodge_size = 0.5, dot_args = list(aes(colour = model),size = 2), whisker_args = list(aes(colour=model), size = 1)) 
plotPb
plotPb <- plotPb+ ggtitle("Pb all") + geom_vline(xintercept = 0, colour = "grey60", linetype = 2) + theme(text = element_text(size = 12)) + 
  labs(x = bquote('Coefficent Estimate with 95% CIs')) + theme(axis.text=element_text(size=12)) + theme(axis.title.x = element_text(margin = margin(t = 12)))+
  scale_x_continuous(breaks = log(pretty(exp(all_models$estimate),n=15)), labels = pretty(exp(all_models$estimate),n=15))
plotPb

# All distance plot
all_models <- combine(PbDistance,Pb_Dweight, Pb10_pointcount, Pb5_pointcount, Pb3_pointcount)
plotPb<- dwplot(all_models, dodge_size = 0.5, dot_args = list(aes(colour = model),size = 2), whisker_args = list(aes(colour=model), size = 1)) 
plotPb
plotPb <- plotPb+ ggtitle("Pb distance") + geom_vline(xintercept = 0, colour = "grey60", linetype = 2) + theme(text = element_text(size = 12)) + 
  labs(x = bquote('Coefficent Estimate with 95% CIs')) + theme(axis.text=element_text(size=12)) + theme(axis.title.x = element_text(margin = margin(t = 12)))+
  scale_x_continuous(breaks = log(pretty(exp(all_models$estimate),n=20)), labels = pretty(exp(all_models$estimate),n=20))
plotPb

# All emissions plot
all_models <- combine(Pb_EMISSIONS,Pb_EMweight,Pb10_SUMEMISSIONS, Pb5_SUMEMISSIONS,Pb3_SUMEMISSIONS)
plotPb<- dwplot(all_models, dodge_size = 0.5, dot_args = list(aes(colour = model),size = 2), whisker_args = list(aes(colour=model), size = 1)) 
plotPb
plotPb <- plotPb+ ggtitle("Pb emissions") + geom_vline(xintercept = 0, colour = "grey60", linetype = 2) + theme(text = element_text(size = 12)) + 
  labs(x = bquote('Coefficent Estimate with 95% CIs')) + theme(axis.text=element_text(size=12)) + theme(axis.title.x = element_text(margin = margin(t = 12))) 
plotPb

#IDW plot
all_models <- combine(Pb_Dweight,Pb_EMweight)
plotPb<- dwplot(all_models, dodge_size = 0.5, dot_args = list(aes(colour = model),size = 2), whisker_args = list(aes(colour=model), size = 1)) %>% relabel_predictors(c("Pb10_pointcount" = "Pb 10km", "Pb5_pointcount" = "Pb 5km", "Pb3_pointcount" = "Pb 3km"))
plotPb
plotPb <- plotPb+ ggtitle("Pb IDW") + geom_vline(xintercept = 0, colour = "grey60", linetype = 2) + theme(text = element_text(size = 12)) + labs(x = bquote('Coefficent Estimate with 95% CIs')) + theme(axis.text=element_text(size=12)) + theme(axis.title.x = element_text(margin = margin(t = 12))) 
plotPb

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

## Stratified analysis by race
mod1<- lm(Pb ~ PbDistance + CE_BMI + CE_AGE + EN_FORMERSMOKER + passivesmoke + road50 + THC_CUMULATIVE1 + workstat + STATEFP, data = data_log)
mod2<- lm(Pb ~ PbDistance + CE_BMI + CE_AGE + EN_FORMERSMOKER + passivesmoke + road50 + THC_CUMULATIVE1 + workstat + STATEFP, data = whitesubgroup)
mod3<- lm(Pb ~ PbDistance + CE_BMI + CE_AGE + EN_FORMERSMOKER + passivesmoke + road50 + THC_CUMULATIVE1 + workstat + STATEFP, data = blacksubgroup)

All <-broom::tidy(mod1) %>% filter(term != "road501")%>% filter(term != "road50")%>% filter(term != "STATEFP12")%>% filter(term != "STATEFP22")%>% filter(term != "STATEFP28")%>% filter(term != "passivesmoke1")%>% filter(term != "EN_FORMERSMOKER1")%>% filter(term != "workstat1")
White <-broom::tidy(mod2)%>% filter(term != "road501")%>% filter(term != "road50")%>% filter(term != "STATEFP12")%>% filter(term != "STATEFP22")%>% filter(term != "STATEFP28")%>% filter(term != "passivesmoke1")%>% filter(term != "EN_FORMERSMOKER1")%>% filter(term != "workstat1")
Black <-broom::tidy(mod3)%>% filter(term != "road501")%>% filter(term != "road50")%>% filter(term != "STATEFP12")%>% filter(term != "STATEFP22")%>% filter(term != "STATEFP28")%>% filter(term != "passivesmoke1")%>% filter(term != "EN_FORMERSMOKER1")%>% filter(term != "workstat1")

all_models <- combine(All, White, Black)
colnames(all_models)[6] ="model"

plotPb<- dwplot(all_models, dodge_size = 0.5, dot_args = list(aes(colour = model),size = 2), whisker_args = list(aes(colour=model), size = 1)) %>% relabel_predictors(c("Pb10_pointcount" = "Pb 10km", "Pb5_pointcount" = "Pb 5km", "Pb3_pointcount" = "Pb 3km"))
plotPb
plotPb <- plotPb+ ggtitle("Pb Distance (km)") + geom_vline(xintercept = 0, colour = "grey60", linetype = 2) + theme(text = element_text(size = 12)) + labs(x = bquote('Coefficent Estimate with 95% CIs')) + theme(axis.text=element_text(size=12)) + theme(axis.title.x = element_text(margin = margin(t = 12))) 
plotPb


## TEsting significance for different metals with community variables
mod1<- lm(Pb ~ PbDistance + percentbelowpovertylevel+ CE_BMI + CE_AGE + EN_FORMERSMOKER + passivesmoke + road50 + THC_CUMULATIVE1 + workstat + STATEFP, data = data_log)
mod2<- lm(Mn ~ Mn_EMISSIONS + CE_BMI + CE_AGE + EN_FORMERSMOKER + passivesmoke + road50 + THC_CUMULATIVE1 + workstat + STATEFP, data = data_log)
mod3<- lm(Mn ~ Mn10_pointcount + CE_BMI + CE_AGE + EN_FORMERSMOKER + passivesmoke + road50 + THC_CUMULATIVE1 + workstat + STATEFP, data = data_log)
summary(mod3)

Distance <-broom::tidy(mod1) %>% filter(term != "road501")%>% filter(term != "road50")%>% filter(term != "STATEFP12")%>% filter(term != "STATEFP22")%>% filter(term != "STATEFP28")%>% filter(term != "workstat1")%>% filter(term != "passivesmoke1")%>% filter(term != "EN_FORMERSMOKER1")
Emissions <-broom::tidy(mod2)%>% filter(term != "road501")%>% filter(term != "road50")%>% filter(term != "STATEFP12")%>% filter(term != "STATEFP22")%>% filter(term != "STATEFP28")%>% filter(term != "workstat1")%>% filter(term != "passivesmoke1")%>% filter(term != "EN_FORMERSMOKER1")
Count <-broom::tidy(mod3)%>% filter(term != "road501")%>% filter(term != "road50")%>% filter(term != "STATEFP12")%>% filter(term != "STATEFP22")%>% filter(term != "STATEFP28")%>% filter(term != "workstat1")%>% filter(term != "passivesmoke1")%>% filter(term != "EN_FORMERSMOKER1")

plotRace_Pb<- dwplot(Distance, dodge_size = 0.5, dot_args = list(aes(colour = model),size = 2), whisker_args = list(aes(colour=model), size = 1)) %>% relabel_predictors(c("raceBlack" = "Black", "PbQuartile2" = "Pb Q2", "PbQuartile3" = "Pb Q3", "PbQuartile4" = "Pb Q4"))
plotRace_Pb
plotRace_Pb1 <- plotRace_Pb + geom_vline(xintercept = 0, colour = "grey60", linetype = 2)  + theme(text = element_text(size = 12)) + labs(x = bquote('Coefficent Estimate with 95% CIs')) + theme(axis.text=element_text(size=12)) + theme(axis.title.x = element_text(margin = margin(t = 12))) 
plotRace_Pb1


##
## Add SES/neighborhood variable dot whisker plot
mod1<- lm(Pb ~ PbDistance + CE_C1 + CE_BMI + CE_AGE + EN_FORMERSMOKER + passivesmoke + road50 + THC_CUMULATIVE1 + workstat + STATEFP, data = datalog)
mod2<- lm(Pb ~ PbDistance + percentbelowpovertylevel + CE_BMI + CE_AGE + EN_FORMERSMOKER + passivesmoke + road50 + THC_CUMULATIVE1 + workstat + STATEFP, data = datalog)
mod3<- lm(Pb ~ PbDistance + medianyearstructurebuilt + CE_BMI + CE_AGE + EN_FORMERSMOKER + passivesmoke + road50 + THC_CUMULATIVE1 + workstat + STATEFP, data = datalog)
mod4<- lm(Pb ~ Pb_Dweight + CE_C1 + CE_BMI + CE_AGE + EN_FORMERSMOKER + passivesmoke + road50 + THC_CUMULATIVE1 + workstat + STATEFP, data = datalog)
mod5<- lm(Pb ~ Pb_Dweight + percentbelowpovertylevel + CE_BMI + CE_AGE + EN_FORMERSMOKER + passivesmoke + road50 + THC_CUMULATIVE1 + workstat + STATEFP, data = datalog)
mod6<- lm(Pb ~ Pb_Dweight + medianyearstructurebuilt + CE_BMI + CE_AGE + EN_FORMERSMOKER + passivesmoke + road50 + THC_CUMULATIVE1 + workstat + STATEFP, data = datalog)

mod1 <-broom::tidy(mod1) %>% filter(term != "road50") %>% filter(term != "road501") %>% filter(term != "CE_BMI")%>% filter(term != "CE_AGE")%>%filter(term != "EN_FORMERSMOKER1")%>%filter(term != "passivesmoke1")%>%filter(term != "THC_CUMULATIVE1")%>%filter(term != "workstat1")%>%filter(term != "STATEFP12")%>%filter(term != "STATEFP22")%>%filter(term != "STATEFP28")
mod2 <-broom::tidy(mod2) %>% filter(term != "road50") %>% filter(term != "road501") %>% filter(term != "CE_BMI")%>% filter(term != "CE_AGE")%>%filter(term != "EN_FORMERSMOKER1")%>%filter(term != "passivesmoke1")%>%filter(term != "THC_CUMULATIVE1")%>%filter(term != "workstat1")%>%filter(term != "STATEFP12")%>%filter(term != "STATEFP22")%>%filter(term != "STATEFP28")
mod3 <-broom::tidy(mod3) %>% filter(term != "road50") %>% filter(term != "road501") %>% filter(term != "CE_BMI")%>% filter(term != "CE_AGE")%>%filter(term != "EN_FORMERSMOKER1")%>%filter(term != "passivesmoke1")%>%filter(term != "THC_CUMULATIVE1")%>%filter(term != "workstat1")%>%filter(term != "STATEFP12")%>%filter(term != "STATEFP22")%>%filter(term != "STATEFP28")
mod4<-broom::tidy(mod4) %>% filter(term != "road50") %>% filter(term != "road501") %>% filter(term != "CE_BMI")%>% filter(term != "CE_AGE")%>%filter(term != "EN_FORMERSMOKER1")%>%filter(term != "passivesmoke1")%>%filter(term != "THC_CUMULATIVE1")%>%filter(term != "workstat1")%>%filter(term != "STATEFP12")%>%filter(term != "STATEFP22")%>%filter(term != "STATEFP28")
mod5 <-broom::tidy(mod5) %>% filter(term != "road50") %>% filter(term != "road501") %>% filter(term != "CE_BMI")%>% filter(term != "CE_AGE")%>%filter(term != "EN_FORMERSMOKER1")%>%filter(term != "passivesmoke1")%>%filter(term != "THC_CUMULATIVE1")%>%filter(term != "workstat1")%>%filter(term != "STATEFP12")%>%filter(term != "STATEFP22")%>%filter(term != "STATEFP28")
mod6 <-broom::tidy(mod6) %>% filter(term != "road50") %>% filter(term != "road501") %>% filter(term != "CE_BMI")%>% filter(term != "CE_AGE")%>%filter(term != "EN_FORMERSMOKER1")%>%filter(term != "passivesmoke1")%>%filter(term != "THC_CUMULATIVE1")%>%filter(term != "workstat1")%>%filter(term != "STATEFP12")%>%filter(term != "STATEFP22")%>%filter(term != "STATEFP28")






