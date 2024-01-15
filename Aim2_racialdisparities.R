setwd("/Users/joycelin/Desktop/Gulf/Aim2/GuLF_2")
library(tidyverse)
library(dplyr)
data <- read_csv("CE_blockgroupNEI_tertile.csv")
low_metals <- c("Cd", "Co", "Mo" ,"V", "Sb") # metals below 60% detect
`%notin%` <- Negate(`%in%`)
 
data$CdBinary <- as.factor(data$CdBinary) 
data$EDU<- as.factor(data$EDU)
data$CE_housetype<- as.factor(data$CE_housetype)
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



datalog <- data %>% mutate(across(c(Mg, Al, Ca, Cr, Mn, Fe, Ni, Cu, Zn, As, Se, Hg, Pb), log10))
#data_log$race <- relevel(factor(data_log$race), ref = "White")
#datalograce <- data_log %>% filter(race != "Other")
#datalog <- datalog[-c(414:416, 394, 364), ] # for old dataset "CE_blockgroupNEI_tertile.csv"


#hispanic ethnicity check


##plot metal concentration disparity by race
# start plot for essential metals-------------------------------------------------
mod1 <- lm(Ca ~ CE_AGE + THC_CUMULATIVE1 + CE_C1 + EN_FORMERSMOKER + passivesmoke + race + CE_BMI, data = datalog)
mod2 <- lm(Cu ~ CE_AGE + THC_CUMULATIVE1 + CE_C1 + EN_FORMERSMOKER + passivesmoke + race + CE_BMI, data = datalog)
mod3 <- lm(Fe ~ CE_AGE + THC_CUMULATIVE1 + CE_C1 + EN_FORMERSMOKER + passivesmoke + race + CE_BMI, data = datalog)
mod4 <- lm(Mg ~ CE_AGE + THC_CUMULATIVE1 + CE_C1 + EN_FORMERSMOKER + passivesmoke + race + CE_BMI, data = datalog)
mod5 <- lm(Mn ~ CE_AGE + THC_CUMULATIVE1 + CE_C1 + EN_FORMERSMOKER + passivesmoke + race + CE_BMI, data = datalog)
mod6 <- lm(Se ~ CE_AGE + THC_CUMULATIVE1 + CE_C1 + EN_FORMERSMOKER + passivesmoke + race + CE_BMI, data = datalog)
mod7 <- lm(Zn ~ CE_AGE + THC_CUMULATIVE1 + CE_C1 + EN_FORMERSMOKER + passivesmoke + race + CE_BMI, data = datalog)

Ca <-broom::tidy(mod1) %>% filter(term != "CE_AGE") %>% filter(term != "Batch")%>% filter(term != "Batch1.5") %>% filter(term != "Batch2") %>% filter(term != "Batch3")%>% filter(term != "Batch4")%>% filter(term != "Batch5")%>% filter(term != "Batch6")%>% filter(term != "Batch6.5")%>% filter(term != "Batch7")%>% filter(term != "THC_CUMULATIVE1") %>% filter(term != "EN_FORMERSMOKER") %>% filter(term != "CE_C1") %>% filter(term != "EN_FORMERSMOKER1") %>% filter(term != "passivesmoke1")%>% filter(term != "passivesmoke")%>% filter(term != "raceOther")%>% filter(term != "CE_BMI")
Cu <-broom::tidy(mod2) %>% filter(term != "CE_AGE") %>% filter(term != "Batch")%>% filter(term != "Batch1.5") %>% filter(term != "Batch2") %>% filter(term != "Batch3")%>% filter(term != "Batch4")%>% filter(term != "Batch5")%>% filter(term != "Batch6")%>% filter(term != "Batch6.5")%>% filter(term != "Batch7")%>% filter(term != "THC_CUMULATIVE1") %>% filter(term != "EN_FORMERSMOKER") %>% filter(term != "CE_C1") %>% filter(term != "EN_FORMERSMOKER1") %>% filter(term != "passivesmoke1")%>% filter(term != "passivesmoke")%>% filter(term != "raceOther")%>% filter(term != "CE_BMI")
Fe <-broom::tidy(mod3) %>% filter(term != "CE_AGE") %>% filter(term != "Batch")%>% filter(term != "Batch1.5") %>% filter(term != "Batch2") %>% filter(term != "Batch3")%>% filter(term != "Batch4")%>% filter(term != "Batch5")%>% filter(term != "Batch6")%>% filter(term != "Batch6.5")%>% filter(term != "Batch7")%>% filter(term != "THC_CUMULATIVE1") %>% filter(term != "EN_FORMERSMOKER") %>% filter(term != "CE_C1") %>% filter(term != "EN_FORMERSMOKER1") %>% filter(term != "passivesmoke1")%>% filter(term != "passivesmoke")%>% filter(term != "raceOther")%>% filter(term != "CE_BMI")
Mg <-broom::tidy(mod4) %>% filter(term != "CE_AGE") %>% filter(term != "Batch")%>% filter(term != "Batch1.5") %>% filter(term != "Batch2") %>% filter(term != "Batch3")%>% filter(term != "Batch4")%>% filter(term != "Batch5")%>% filter(term != "Batch6")%>% filter(term != "Batch6.5")%>% filter(term != "Batch7")%>% filter(term != "THC_CUMULATIVE1") %>% filter(term != "EN_FORMERSMOKER") %>% filter(term != "CE_C1") %>% filter(term != "EN_FORMERSMOKER1") %>% filter(term != "passivesmoke1")%>% filter(term != "passivesmoke")%>% filter(term != "raceOther")%>% filter(term != "CE_BMI")
Mn <-broom::tidy(mod5) %>% filter(term != "CE_AGE") %>% filter(term != "Batch")%>% filter(term != "Batch1.5") %>% filter(term != "Batch2") %>% filter(term != "Batch3")%>% filter(term != "Batch4")%>% filter(term != "Batch5")%>% filter(term != "Batch6")%>% filter(term != "Batch6.5")%>% filter(term != "Batch7")%>% filter(term != "THC_CUMULATIVE1") %>% filter(term != "EN_FORMERSMOKER") %>% filter(term != "CE_C1") %>% filter(term != "EN_FORMERSMOKER1") %>% filter(term != "passivesmoke1")%>% filter(term != "passivesmoke")%>% filter(term != "raceOther")%>% filter(term != "CE_BMI")
Se <-broom::tidy(mod6) %>% filter(term != "CE_AGE") %>% filter(term != "Batch")%>% filter(term != "Batch1.5") %>% filter(term != "Batch2") %>% filter(term != "Batch3")%>% filter(term != "Batch4")%>% filter(term != "Batch5")%>% filter(term != "Batch6")%>% filter(term != "Batch6.5")%>% filter(term != "Batch7")%>% filter(term != "THC_CUMULATIVE1") %>% filter(term != "EN_FORMERSMOKER") %>% filter(term != "CE_C1") %>% filter(term != "EN_FORMERSMOKER1") %>% filter(term != "passivesmoke1")%>% filter(term != "passivesmoke")%>% filter(term != "raceOther")%>% filter(term != "CE_BMI")
Zn <-broom::tidy(mod7) %>% filter(term != "CE_AGE") %>% filter(term != "Batch")%>% filter(term != "Batch1.5") %>% filter(term != "Batch2") %>% filter(term != "Batch3")%>% filter(term != "Batch4")%>% filter(term != "Batch5")%>% filter(term != "Batch6")%>% filter(term != "Batch6.5")%>% filter(term != "Batch7")%>% filter(term != "THC_CUMULATIVE1") %>% filter(term != "EN_FORMERSMOKER") %>% filter(term != "CE_C1") %>% filter(term != "EN_FORMERSMOKER1") %>% filter(term != "passivesmoke1")%>% filter(term != "passivesmoke")%>% filter(term != "raceOther")%>% filter(term != "CE_BMI")

library(dotwhisker)
library(dplyr)
library(gdata)
essential_models <- combine(Ca, Cu, Fe, Mg, Mn, Se, Zn)
colnames(essential_models)[6] ="model"

plotRace_essential<- dwplot(essential_models, dodge_size = 1, dot_args = list(aes(colour = model),size = 5), whisker_args = list(aes(colour=model), size = 3)) %>% relabel_predictors(c("raceBlack" = "Black"))
plotRace_essential
plotRace_essential1 <- plotRace_essential + theme_bw(base_size = 4) + geom_vline(xintercept = 0, colour = "grey60", linetype = 2) + ggtitle("Essential Metals") + scale_color_brewer(palette="Set1") + theme(text = element_text(size = 16)) + labs(x = bquote('Coefficent Estimate with 95% CIs')) + theme(axis.text=element_text(size=17)) + theme(axis.title.x = element_text(margin = margin(t = 15))) + theme(panel.border = element_rect(fill=NA, colour = "black", size=1))  
plotRace_essential1 
ggsave("metals.png", plotRace_essential1, bg='transparent')
png("essential.png", width = 9.9, height = 5.4, units = 'in', res = 300) 
plotRace_essential1 
dev.off()


## start plot for toxic metals
mod8 <- lm(Al ~ CE_AGE + THC_CUMULATIVE1 + CE_C1 + EN_FORMERSMOKER + passivesmoke + race + CE_BMI, data = datalog)
mod9 <- lm(As ~ CE_AGE + THC_CUMULATIVE1 + CE_C1 + EN_FORMERSMOKER + passivesmoke + race + CE_BMI, data = datalog)
mod10 <- lm(Cr ~ CE_AGE + THC_CUMULATIVE1 + CE_C1 + EN_FORMERSMOKER + passivesmoke + race + CE_BMI, data = datalog)
mod11 <- lm(Hg ~ CE_AGE + THC_CUMULATIVE1 + CE_C1 + EN_FORMERSMOKER + passivesmoke + race + CE_BMI, data = datalog)
mod12 <- lm(Ni ~ CE_AGE + THC_CUMULATIVE1 + CE_C1 + EN_FORMERSMOKER + passivesmoke + race + CE_BMI, data = datalog)
mod13 <- lm(Pb ~ CE_AGE + THC_CUMULATIVE1 + CE_C1 + EN_FORMERSMOKER + passivesmoke + race + CE_BMI, data = datalog)

Al <-broom::tidy(mod8) %>% filter(term != "CE_AGE") %>% filter(term != "THC_CUMULATIVE1") %>% filter(term != "EN_FORMERSMOKER") %>% filter(term != "CE_C1") %>% filter(term != "EN_FORMERSMOKER1") %>% filter(term != "passivesmoke1")%>% filter(term != "passivesmoke")%>% filter(term != "raceOther")%>% filter(term != "CE_BMI")
As <-broom::tidy(mod9) %>% filter(term != "CE_AGE") %>% filter(term != "THC_CUMULATIVE1") %>% filter(term != "EN_FORMERSMOKER") %>% filter(term != "CE_C1") %>% filter(term != "EN_FORMERSMOKER1") %>% filter(term != "passivesmoke1")%>% filter(term != "passivesmoke")%>% filter(term != "raceOther")%>% filter(term != "CE_BMI")
Cr <-broom::tidy(mod10)%>% filter(term != "CE_AGE") %>% filter(term != "THC_CUMULATIVE1") %>% filter(term != "EN_FORMERSMOKER") %>% filter(term != "CE_C1") %>% filter(term != "EN_FORMERSMOKER1") %>% filter(term != "passivesmoke1")%>% filter(term != "passivesmoke")%>% filter(term != "raceOther")%>% filter(term != "CE_BMI")
Hg <-broom::tidy(mod11) %>% filter(term != "CE_AGE") %>% filter(term != "THC_CUMULATIVE1") %>% filter(term != "EN_FORMERSMOKER") %>% filter(term != "CE_C1") %>% filter(term != "EN_FORMERSMOKER1") %>% filter(term != "passivesmoke1")%>% filter(term != "passivesmoke")%>% filter(term != "raceOther")%>% filter(term != "CE_BMI")
Ni <-broom::tidy(mod12)%>% filter(term != "CE_AGE") %>% filter(term != "THC_CUMULATIVE1") %>% filter(term != "EN_FORMERSMOKER") %>% filter(term != "CE_C1") %>% filter(term != "EN_FORMERSMOKER1") %>% filter(term != "passivesmoke1")%>% filter(term != "passivesmoke")%>% filter(term != "raceOther")%>% filter(term != "CE_BMI")
Pb <-broom::tidy(mod13) %>% filter(term != "CE_AGE") %>% filter(term != "THC_CUMULATIVE1") %>% filter(term != "EN_FORMERSMOKER") %>% filter(term != "CE_C1") %>% filter(term != "EN_FORMERSMOKER1") %>% filter(term != "passivesmoke1")%>% filter(term != "passivesmoke")%>% filter(term != "raceOther")%>% filter(term != "CE_BMI")

toxic_models <- combine(Al, As, Cr, Hg, Pb)
colnames(toxic_models)[6] ="model"

plotRace_toxic<- dwplot(toxic_models, dodge_size = 1, dot_args = list(aes(colour = model),size = 5), whisker_args = list(aes(colour=model), size = 3)) %>% relabel_predictors(c("raceBlack" = "Black", "raceOther" = "Other"))
plotRace_toxic1 <- plotRace_toxic + theme_bw(base_size = 4) + 
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2) + ggtitle("Toxic Metals") + scale_color_brewer(palette="Set2") + theme(text = element_text(size = 16)) + labs(x = bquote('Coefficent Estimate with 95% CIs')) + theme(axis.text=element_text(size=17)) + theme(axis.title.x = element_text(margin = margin(t = 15))) + theme(panel.border = element_rect(fill=NA, colour = "black", 
                                                                                                                                                                                                                                                                                                                                                                       size=1))  
plotRace_toxic1 
ggsave("toxicmetals.png", plotRace_toxic1, bg='transparent')
png("toxic.png", width = 9.9, height = 5.4, units = 'in', res = 300) 
plotRace_toxic1 
dev.off()


library(patchwork)
figure<- plotRace_essential1 + plotRace_toxic1
figure
png("figure.png", width = 10, height = 4, units = 'in', res = 300) 
figure
dev.off()


### check for disparity by income cat
# start plot for essential metals-------------------------------------------------
mod1 <- lm(Ca ~ CE_AGE + THC_CUMULATIVE1 + income + EN_FORMERSMOKER + passivesmoke + CE_BMI, data = datalog)
mod2 <- lm(Cu ~ CE_AGE + THC_CUMULATIVE1 + income + EN_FORMERSMOKER + passivesmoke + CE_BMI, data = datalog)
mod3 <- lm(Fe ~ CE_AGE + THC_CUMULATIVE1 + income + EN_FORMERSMOKER + passivesmoke + CE_BMI, data = datalog)
mod4 <- lm(Mg ~ CE_AGE + THC_CUMULATIVE1 + income + EN_FORMERSMOKER + passivesmoke + CE_BMI, data = datalog)
mod5 <- lm(Mn ~ CE_AGE + THC_CUMULATIVE1 + income + EN_FORMERSMOKER + passivesmoke + CE_BMI, data = datalog)
mod6 <- lm(Se ~ CE_AGE + THC_CUMULATIVE1 + income + EN_FORMERSMOKER + passivesmoke + CE_BMI, data = datalog)
mod7 <- lm(Zn ~ CE_AGE + THC_CUMULATIVE1 + income + EN_FORMERSMOKER + passivesmoke + CE_BMI, data = datalog)

Ca <-broom::tidy(mod1) %>% filter(term != "CE_AGE") %>% filter(term != "THC_CUMULATIVE1") %>% filter(term != "EN_FORMERSMOKER") %>% filter(term != "CE_C1") %>% filter(term != "EN_FORMERSMOKER1") %>% filter(term != "passivesmoke1")%>% filter(term != "passivesmoke")%>% filter(term != "raceOther")%>% filter(term != "CE_BMI")%>% filter(term != "Black")
Cu <-broom::tidy(mod2) %>% filter(term != "CE_AGE") %>% filter(term != "THC_CUMULATIVE1") %>% filter(term != "EN_FORMERSMOKER") %>% filter(term != "CE_C1") %>% filter(term != "EN_FORMERSMOKER1") %>% filter(term != "passivesmoke1")%>% filter(term != "passivesmoke")%>% filter(term != "raceOther")%>% filter(term != "CE_BMI")%>% filter(term != "Black")
Fe <-broom::tidy(mod3) %>% filter(term != "CE_AGE") %>% filter(term != "THC_CUMULATIVE1") %>% filter(term != "EN_FORMERSMOKER") %>% filter(term != "CE_C1") %>% filter(term != "EN_FORMERSMOKER1") %>% filter(term != "passivesmoke1")%>% filter(term != "passivesmoke")%>% filter(term != "raceOther")%>% filter(term != "CE_BMI")%>% filter(term != "Black")
Mg <-broom::tidy(mod4) %>% filter(term != "CE_AGE") %>% filter(term != "THC_CUMULATIVE1") %>% filter(term != "EN_FORMERSMOKER") %>% filter(term != "CE_C1") %>% filter(term != "EN_FORMERSMOKER1") %>% filter(term != "passivesmoke1")%>% filter(term != "passivesmoke")%>% filter(term != "raceOther")%>% filter(term != "CE_BMI")%>% filter(term != "Black")
Mn <-broom::tidy(mod5) %>% filter(term != "CE_AGE") %>% filter(term != "THC_CUMULATIVE1") %>% filter(term != "EN_FORMERSMOKER") %>% filter(term != "CE_C1") %>% filter(term != "EN_FORMERSMOKER1") %>% filter(term != "passivesmoke1")%>% filter(term != "passivesmoke")%>% filter(term != "raceOther")%>% filter(term != "CE_BMI")%>% filter(term != "Black")
Se <-broom::tidy(mod6) %>% filter(term != "CE_AGE") %>% filter(term != "THC_CUMULATIVE1") %>% filter(term != "EN_FORMERSMOKER") %>% filter(term != "CE_C1") %>% filter(term != "EN_FORMERSMOKER1") %>% filter(term != "passivesmoke1")%>% filter(term != "passivesmoke")%>% filter(term != "raceOther")%>% filter(term != "CE_BMI")%>% filter(term != "Black")
Zn <-broom::tidy(mod7) %>% filter(term != "CE_AGE") %>% filter(term != "THC_CUMULATIVE1") %>% filter(term != "EN_FORMERSMOKER") %>% filter(term != "CE_C1") %>% filter(term != "EN_FORMERSMOKER1") %>% filter(term != "passivesmoke1")%>% filter(term != "passivesmoke")%>% filter(term != "raceOther")%>% filter(term != "CE_BMI")%>% filter(term != "Black")

library(dotwhisker)
library(dplyr)
library(gdata)
essential_models <- combine(Ca, Cu, Fe, Mg, Mn, Se, Zn)
colnames(essential_models)[6] ="model"

plotRace_essential<- dwplot(essential_models, dodge_size = 1, dot_args = list(aes(colour = model),size = 5), whisker_args = list(aes(colour=model), size = 3)) %>% relabel_predictors(c("income<$20,000" = "<$20,000"))%>% relabel_predictors(c("income$20,000 - $49,999" = "$20,000 - $49,999"))
plotRace_essential
plotRace_essential1 <- plotRace_essential + theme_bw(base_size = 4) + geom_vline(xintercept = 0, colour = "grey60", linetype = 2) + ggtitle("Essential Metals") + scale_color_brewer(palette="Set2") + theme(text = element_text(size = 16)) + labs(x = bquote('Coefficent Estimate with 95% CIs')) + theme(axis.text=element_text(size=17)) + theme(axis.title.x = element_text(margin = margin(t = 15))) + theme(panel.border = element_rect(fill=NA, colour = "black", size=1))  
plotRace_essential1 
ggsave("metals.png", plotRace_essential1, bg='transparent')
png("essential.png", width = 9.9, height = 5.4, units = 'in', res = 300) 
plotRace_essential1 
dev.off()


## start plot for toxic metals
mod8 <- lm(Al ~ CE_AGE + THC_CUMULATIVE1 + income + EN_FORMERSMOKER + passivesmoke + CE_BMI, data = datalog)
mod9 <- lm(As ~ CE_AGE + THC_CUMULATIVE1 + income + EN_FORMERSMOKER + passivesmoke + CE_BMI, data = datalog)
mod10 <- lm(Cr ~ CE_AGE + THC_CUMULATIVE1 + income + EN_FORMERSMOKER + passivesmoke + CE_BMI, data = datalog)
mod11 <- lm(Hg ~ CE_AGE + THC_CUMULATIVE1 + income + EN_FORMERSMOKER + passivesmoke + CE_BMI, data = datalog)
mod12 <- lm(Ni ~ CE_AGE + THC_CUMULATIVE1 + income + EN_FORMERSMOKER + passivesmoke + CE_BMI, data = datalog)
mod13 <- lm(Pb ~ CE_AGE + THC_CUMULATIVE1 + income + EN_FORMERSMOKER + passivesmoke + CE_BMI, data = datalog)

Al <-broom::tidy(mod8) %>% filter(term != "CE_AGE") %>% filter(term != "THC_CUMULATIVE1") %>% filter(term != "EN_FORMERSMOKER") %>% filter(term != "CE_C1") %>% filter(term != "EN_FORMERSMOKER1") %>% filter(term != "passivesmoke1")%>% filter(term != "passivesmoke")%>% filter(term != "raceOther")%>% filter(term != "CE_BMI")
As <-broom::tidy(mod9) %>% filter(term != "CE_AGE") %>% filter(term != "THC_CUMULATIVE1") %>% filter(term != "EN_FORMERSMOKER") %>% filter(term != "CE_C1") %>% filter(term != "EN_FORMERSMOKER1") %>% filter(term != "passivesmoke1")%>% filter(term != "passivesmoke")%>% filter(term != "raceOther")%>% filter(term != "CE_BMI")
Cr <-broom::tidy(mod10)%>% filter(term != "CE_AGE") %>% filter(term != "THC_CUMULATIVE1") %>% filter(term != "EN_FORMERSMOKER") %>% filter(term != "CE_C1") %>% filter(term != "EN_FORMERSMOKER1") %>% filter(term != "passivesmoke1")%>% filter(term != "passivesmoke")%>% filter(term != "raceOther")%>% filter(term != "CE_BMI")
Hg <-broom::tidy(mod11) %>% filter(term != "CE_AGE") %>% filter(term != "THC_CUMULATIVE1") %>% filter(term != "EN_FORMERSMOKER") %>% filter(term != "CE_C1") %>% filter(term != "EN_FORMERSMOKER1") %>% filter(term != "passivesmoke1")%>% filter(term != "passivesmoke")%>% filter(term != "raceOther")%>% filter(term != "CE_BMI")
Ni <-broom::tidy(mod12)%>% filter(term != "CE_AGE") %>% filter(term != "THC_CUMULATIVE1") %>% filter(term != "EN_FORMERSMOKER") %>% filter(term != "CE_C1") %>% filter(term != "EN_FORMERSMOKER1") %>% filter(term != "passivesmoke1")%>% filter(term != "passivesmoke")%>% filter(term != "raceOther")%>% filter(term != "CE_BMI")
Pb <-broom::tidy(mod13) %>% filter(term != "CE_AGE") %>% filter(term != "THC_CUMULATIVE1") %>% filter(term != "EN_FORMERSMOKER") %>% filter(term != "CE_C1") %>% filter(term != "EN_FORMERSMOKER1") %>% filter(term != "passivesmoke1")%>% filter(term != "passivesmoke")%>% filter(term != "raceOther")%>% filter(term != "CE_BMI")

toxic_models <- combine(Al, As, Cr, Hg, Ni, Pb)
colnames(toxic_models)[6] ="model"

plotRace_toxic<- dwplot(toxic_models, dodge_size = 1, dot_args = list(aes(colour = model),size = 5), whisker_args = list(aes(colour=model), size = 3)) %>% relabel_predictors(c("raceBlack" = "Black", "raceOther" = "Other"))
plotRace_toxic1 <- plotRace_toxic + theme_bw(base_size = 4) + 
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2) + ggtitle("Toxic Metals") + scale_color_brewer(palette="Set2") + theme(text = element_text(size = 16)) + labs(x = bquote('Coefficent Estimate with 95% CIs')) + theme(axis.text=element_text(size=17)) + theme(axis.title.x = element_text(margin = margin(t = 15))) + theme(panel.border = element_rect(fill=NA, colour = "black", 
                                                                                                                                                                                                                                                                                                                                                                       size=1))  
plotRace_toxic1 
ggsave("toxicmetals.png", plotRace_toxic1, bg='transparent')
png("toxic.png", width = 9.9, height = 5.4, units = 'in', res = 300) 
plotRace_toxic1 
dev.off()


library(patchwork)
figure<- plotRace_essential1 + plotRace_toxic1
figure
png("figure.png", width = 10, height = 4, units = 'in', res = 300) 
figure
dev.off()



### check for disparity by edu cat
# start plot for essential metals-------------------------------------------------
mod1 <- lm(Ca ~ CE_AGE + THC_CUMULATIVE1 + EDU + EN_FORMERSMOKER + passivesmoke + CE_BMI, data = datalog)
mod2 <- lm(Cu ~ CE_AGE + THC_CUMULATIVE1 + EDU + EN_FORMERSMOKER + passivesmoke + CE_BMI, data = datalog)
mod3 <- lm(Fe ~ CE_AGE + THC_CUMULATIVE1 + EDU + EN_FORMERSMOKER + passivesmoke + CE_BMI, data = datalog)
mod4 <- lm(Mg ~ CE_AGE + THC_CUMULATIVE1 + EDU + EN_FORMERSMOKER + passivesmoke + CE_BMI, data = datalog)
mod5 <- lm(Mn ~ CE_AGE + THC_CUMULATIVE1 + EDU + EN_FORMERSMOKER + passivesmoke + CE_BMI, data = datalog)
mod6 <- lm(Se ~ CE_AGE + THC_CUMULATIVE1 + EDU + EN_FORMERSMOKER + passivesmoke + CE_BMI, data = datalog)
mod7 <- lm(Zn ~ CE_AGE + THC_CUMULATIVE1 + EDU + EN_FORMERSMOKER + passivesmoke + CE_BMI, data = datalog)

Ca <-broom::tidy(mod1) %>% filter(term != "CE_AGE") %>% filter(term != "THC_CUMULATIVE1") %>% filter(term != "EN_FORMERSMOKER") %>% filter(term != "CE_C1") %>% filter(term != "EN_FORMERSMOKER1") %>% filter(term != "passivesmoke1")%>% filter(term != "passivesmoke")%>% filter(term != "raceOther")%>% filter(term != "CE_BMI")%>% filter(term != "Black")
Cu <-broom::tidy(mod2) %>% filter(term != "CE_AGE") %>% filter(term != "THC_CUMULATIVE1") %>% filter(term != "EN_FORMERSMOKER") %>% filter(term != "CE_C1") %>% filter(term != "EN_FORMERSMOKER1") %>% filter(term != "passivesmoke1")%>% filter(term != "passivesmoke")%>% filter(term != "raceOther")%>% filter(term != "CE_BMI")%>% filter(term != "Black")
Fe <-broom::tidy(mod3) %>% filter(term != "CE_AGE") %>% filter(term != "THC_CUMULATIVE1") %>% filter(term != "EN_FORMERSMOKER") %>% filter(term != "CE_C1") %>% filter(term != "EN_FORMERSMOKER1") %>% filter(term != "passivesmoke1")%>% filter(term != "passivesmoke")%>% filter(term != "raceOther")%>% filter(term != "CE_BMI")%>% filter(term != "Black")
Mg <-broom::tidy(mod4) %>% filter(term != "CE_AGE") %>% filter(term != "THC_CUMULATIVE1") %>% filter(term != "EN_FORMERSMOKER") %>% filter(term != "CE_C1") %>% filter(term != "EN_FORMERSMOKER1") %>% filter(term != "passivesmoke1")%>% filter(term != "passivesmoke")%>% filter(term != "raceOther")%>% filter(term != "CE_BMI")%>% filter(term != "Black")
Mn <-broom::tidy(mod5) %>% filter(term != "CE_AGE") %>% filter(term != "THC_CUMULATIVE1") %>% filter(term != "EN_FORMERSMOKER") %>% filter(term != "CE_C1") %>% filter(term != "EN_FORMERSMOKER1") %>% filter(term != "passivesmoke1")%>% filter(term != "passivesmoke")%>% filter(term != "raceOther")%>% filter(term != "CE_BMI")%>% filter(term != "Black")
Se <-broom::tidy(mod6) %>% filter(term != "CE_AGE") %>% filter(term != "THC_CUMULATIVE1") %>% filter(term != "EN_FORMERSMOKER") %>% filter(term != "CE_C1") %>% filter(term != "EN_FORMERSMOKER1") %>% filter(term != "passivesmoke1")%>% filter(term != "passivesmoke")%>% filter(term != "raceOther")%>% filter(term != "CE_BMI")%>% filter(term != "Black")
Zn <-broom::tidy(mod7) %>% filter(term != "CE_AGE") %>% filter(term != "THC_CUMULATIVE1") %>% filter(term != "EN_FORMERSMOKER") %>% filter(term != "CE_C1") %>% filter(term != "EN_FORMERSMOKER1") %>% filter(term != "passivesmoke1")%>% filter(term != "passivesmoke")%>% filter(term != "raceOther")%>% filter(term != "CE_BMI")%>% filter(term != "Black")

library(dotwhisker)
library(dplyr)
library(gdata)
essential_models <- combine(Ca, Cu, Fe, Mg, Mn, Se, Zn)
colnames(essential_models)[6] ="model"

plotRace_essential<- dwplot(essential_models, dodge_size = 1, dot_args = list(aes(colour = model),size = 5), whisker_args = list(aes(colour=model), size = 3)) %>% relabel_predictors(c("income<$20,000" = "<$20,000"))%>% relabel_predictors(c("income$20,000 - $49,999" = "$20,000 - $49,999"))
plotRace_essential
plotRace_essential1 <- plotRace_essential + theme_bw(base_size = 4) + geom_vline(xintercept = 0, colour = "grey60", linetype = 2) + ggtitle("Essential Metals") + scale_color_brewer(palette="Set2") + theme(text = element_text(size = 14)) + labs(x = bquote('Coefficent Estimate with 95% CIs')) + theme(axis.text=element_text(size=14)) + theme(axis.title.x = element_text(margin = margin(t = 15))) + theme(panel.border = element_rect(fill=NA, colour = "black", size=1))  
plotRace_essential1 
ggsave("metals.png", plotRace_essential1, bg='transparent')
png("essential.png", width = 9.9, height = 5.4, units = 'in', res = 300) 
plotRace_essential1 
dev.off()


## start plot for toxic metals
mod8 <- lm(Al ~ CE_AGE + THC_CUMULATIVE1 + EDU + EN_FORMERSMOKER + passivesmoke + CE_BMI, data = datalog)
mod9 <- lm(As ~ CE_AGE + THC_CUMULATIVE1 + EDU + EN_FORMERSMOKER + passivesmoke + CE_BMI, data = datalog)
mod10 <- lm(Cr ~ CE_AGE + THC_CUMULATIVE1 + EDU + EN_FORMERSMOKER + passivesmoke + CE_BMI, data = datalog)
mod11 <- lm(Hg ~ CE_AGE + THC_CUMULATIVE1 + EDU + EN_FORMERSMOKER + passivesmoke + CE_BMI, data = datalog)
mod12 <- lm(Ni ~ CE_AGE + THC_CUMULATIVE1 + EDU + EN_FORMERSMOKER + passivesmoke + CE_BMI, data = datalog)
mod13 <- lm(Pb ~ CE_AGE + THC_CUMULATIVE1 + EDU + EN_FORMERSMOKER + passivesmoke + CE_BMI, data = datalog)

Al <-broom::tidy(mod8) %>% filter(term != "CE_AGE") %>% filter(term != "THC_CUMULATIVE1") %>% filter(term != "EN_FORMERSMOKER") %>% filter(term != "CE_C1") %>% filter(term != "EN_FORMERSMOKER1") %>% filter(term != "passivesmoke1")%>% filter(term != "passivesmoke")%>% filter(term != "raceOther")%>% filter(term != "CE_BMI")
As <-broom::tidy(mod9) %>% filter(term != "CE_AGE") %>% filter(term != "THC_CUMULATIVE1") %>% filter(term != "EN_FORMERSMOKER") %>% filter(term != "CE_C1") %>% filter(term != "EN_FORMERSMOKER1") %>% filter(term != "passivesmoke1")%>% filter(term != "passivesmoke")%>% filter(term != "raceOther")%>% filter(term != "CE_BMI")
Cr <-broom::tidy(mod10)%>% filter(term != "CE_AGE") %>% filter(term != "THC_CUMULATIVE1") %>% filter(term != "EN_FORMERSMOKER") %>% filter(term != "CE_C1") %>% filter(term != "EN_FORMERSMOKER1") %>% filter(term != "passivesmoke1")%>% filter(term != "passivesmoke")%>% filter(term != "raceOther")%>% filter(term != "CE_BMI")
Hg <-broom::tidy(mod11) %>% filter(term != "CE_AGE") %>% filter(term != "THC_CUMULATIVE1") %>% filter(term != "EN_FORMERSMOKER") %>% filter(term != "CE_C1") %>% filter(term != "EN_FORMERSMOKER1") %>% filter(term != "passivesmoke1")%>% filter(term != "passivesmoke")%>% filter(term != "raceOther")%>% filter(term != "CE_BMI")
Ni <-broom::tidy(mod12)%>% filter(term != "CE_AGE") %>% filter(term != "THC_CUMULATIVE1") %>% filter(term != "EN_FORMERSMOKER") %>% filter(term != "CE_C1") %>% filter(term != "EN_FORMERSMOKER1") %>% filter(term != "passivesmoke1")%>% filter(term != "passivesmoke")%>% filter(term != "raceOther")%>% filter(term != "CE_BMI")
Pb <-broom::tidy(mod13) %>% filter(term != "CE_AGE") %>% filter(term != "THC_CUMULATIVE1") %>% filter(term != "EN_FORMERSMOKER") %>% filter(term != "CE_C1") %>% filter(term != "EN_FORMERSMOKER1") %>% filter(term != "passivesmoke1")%>% filter(term != "passivesmoke")%>% filter(term != "raceOther")%>% filter(term != "CE_BMI")

toxic_models <- combine(Al, As, Cr, Hg, Ni, Pb)
colnames(toxic_models)[6] ="model"

plotRace_toxic<- dwplot(toxic_models, dodge_size = 1, dot_args = list(aes(colour = model),size = 5), whisker_args = list(aes(colour=model), size = 3)) %>% relabel_predictors(c("raceBlack" = "Black", "raceOther" = "Other"))
plotRace_toxic1 <- plotRace_toxic + theme_bw(base_size = 4) + 
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2) + ggtitle("Toxic Metals") + scale_color_brewer(palette="Set2") + theme(text = element_text(size = 14)) + labs(x = bquote('Coefficent Estimate with 95% CIs')) + theme(axis.text=element_text(size=14)) + theme(axis.title.x = element_text(margin = margin(t = 15))) + theme(panel.border = element_rect(fill=NA, colour = "black", 
                                                                                                                                                                                                                                                                                                                                                                       size=1))  
plotRace_toxic1 
ggsave("toxicmetals.png", plotRace_toxic1, bg='transparent')
png("toxic.png", width = 9.9, height = 5.4, units = 'in', res = 300) 
plotRace_toxic1 
dev.off()

## assess correlations between race and income and education
cor(data$EN_TOTINCOME, data$EN_EDU, method = c("pearson"), use = "complete.obs")

## scatterplot of metals and covariates
ggplot(data=data, aes(x=CE_AGE, y=log10(Pb))) + geom_point() + geom_smooth(method=loess)
ggplot(data=data, aes(x=THC_CUMULATIVE1, y=log10(Pb))) + geom_point() + geom_smooth(method=loess)
ggplot(data=data, aes(x=Pb10_SUMEMISSIONS, y=log10(Pb))) + geom_point() + geom_smooth(method=loess)
ggplot(data=data, aes(x=medianyearstructurebuilt, y=log10(Pb))) + geom_point() + geom_smooth(method=lm)
ggplot(data=data, aes(x=percentbelowpovertylevel, y=log10(Pb))) + geom_point() + geom_smooth(method=lm)
ggplot(data=data, aes(x=medianhouseholdincome, y=log10(Pb))) + geom_point() + geom_smooth(method=lm)
ggplot(data=data, aes(x=CE_C1, y=log10(Pb))) + geom_point() + geom_smooth(method=lm)
ggplot(data=data, aes(x=PbDistance, y=log10(Pb))) + geom_point() + geom_smooth(method=lm)

##corrlation matrix of covariates
datacor <- data[, c("CE_AGE", "THC_CUMULATIVE1", "EN_RACE2", "EN_EDU", "Pb5_SUMEMISSIONS", "medianyearstructurebuilt", "percentbelowpovertylevel", "medianhouseholdincome", "CE_C1", "EN_TOTINCOME", "PbDistance", "Pb5_pointcount")]
library(Hmisc)
cormat = rcorr(as.matrix(datacor))
cormat


## Pb best model maximizing R2 ---------------------------------------------------------------
mod1 <- lm(log10(Pb) ~ PbDistance + CE_C1, data = data) # maybe shouldnt be controlling for education/income for env analysis
summary(mod1)
confint(mod1)
(exp(coef(mod1))-1)*100
library(car)
vif(mod1)

m1<-lm(log10(Pb) ~ CE_AGE, data = data)
summary(m1)
## Mn best model maximizing R2 ---------------------------------------------------------------
mod2 <- lm(log10(Mn) ~ CE_AGE + MnDistance + road200 + EN_FORMERSMOKER + CE_BMI + percentbelowpovertylevel + passivesmoke + EN_EDU + STATEFP + THC_CUMULATIVE1, data = data) # maybe shouldnt be controlling for education/income for env analysis
summary(mod2)
confint(mod2)
(exp(coef(mod2))-1)*100

## As best model maximizing R2 ---------------------------------------------------------------
mod3 <- lm(log10(As) ~ CE_AGE + As_idw + EN_FORMERSMOKER + CE_BMI + percentbelowpovertylevel + passivesmoke + EN_EDU + STATEFP + THC_CUMULATIVE1, data = data) # maybe shouldnt be controlling for education/income for env analysis
summary(mod3)
confint(mod3)
(exp(coef(mod3))-1)*100

## Hg best model maximizing R2 ---------------------------------------------------------------
mod4 <- lm(log10(Hg) ~ CE_AGE + Hg_idw + EN_FORMERSMOKER + CE_BMI + percentbelowpovertylevel + passivesmoke + EN_EDU + STATEFP + THC_CUMULATIVE1, data = data) # maybe shouldnt be controlling for education/income for env analysis
summary(mod4)
confint(mod4)
(exp(coef(mod4))-1)*100

## Se best model maximizing R2 ---------------------------------------------------------------
mod5 <- lm(log10(Se) ~  CE_AGE + Se_idw + EN_FORMERSMOKER + CE_BMI + percentbelowpovertylevel + passivesmoke + EN_EDU + STATEFP + THC_CUMULATIVE1, data = data) # maybe shouldnt be controlling for education/income for env analysis
summary(mod5)
confint(mod5)
(exp(coef(mod5))-1)*100

#without metal exposure variables ISEE Abstract
mod1 <- lm(log10(Pb) ~ Batch + CE_AGE +EN_FORMERSMOKER + passivesmoke + STATEFP, data = data) 
summary(mod1)
confint(mod1)
(exp(coef(mod1))-1)*100

data<-data %>% mutate(THC1_30 = THC_CUMULATIVE1/30)
data<-data %>% mutate(Age5 = CE_AGE/5)

mod1 <- lm(log10(Zn) ~ Mass + Batch + Age5 + race + EN_FORMERSMOKER + THC1_30 + EDU  + road500 + STATEFP, data = data)
summary(mod1)
confint(mod1)
(exp(coef(mod1))-1)*100

## neighborhood level risk factors (added individually into model) ------------------------------------------------
data<-data %>% mutate(peremploy10 = percentunemployed/10)
data<-data %>% mutate(perblack10 = percentblack/10)
data<-data %>% mutate(perhis10 = percenthispanicllatino/10)
data<-data %>% mutate(struct5 = medianyearstructurebuilt/5)
data<-data %>% mutate(perbelowpov10 = percentbelowpovertylevel/10)
data<-data %>% mutate(medincome10 = medianhouseholdincome/10000)

mod2 <- lm(log10(Pb) ~ Age5 + race + EN_FORMERSMOKER + THC1_30 + EDU + road500 + STATEFP+ Pb10_pointcount + perbelowpov10, data = data)
summary(mod2)
confint(mod2)
(exp(coef(mod2))-1)*100

## Race/income interaction terms--------------------------------------------------- check global f test for interaction
#boxplot of income by race
fig3<-ggplot(data = datlongmain, aes(x=race, y=EN_TOTINCOME))+geom_boxplot(aes(fill=race),  outlier.size = 0.7)
fig3

mod2 <- lm(Pb ~ CE_AGE + CE_BMI + race*PbDistance + workstat + road50 + EN_FORMERSMOKER + passivesmoke + THC_CUMULATIVE1+ STATEFP, data = datalog)
summary(mod2)
confint(mod2)
(exp(coef(mod2))-1)*100


## subgroup boxplot analyses -------------------------------------------------------------------------
data <- data %>% mutate(Agegroup = case_when(CE_AGE >= 60 & CE_AGE <=69 ~ '60 - 69',
                                             CE_AGE >= 40 & CE_AGE<= 59 ~ '40 - 59',
                                             CE_AGE >= 20 & CE_AGE <= 39 ~ '20 - 39')) # end function

data$Agegroup <- relevel(factor(data$Agegroup), ref = "20 - 39")
data$EDU <- relevel(factor(data$EDU), ref = "College or more")
data$income <- relevel(factor(data$income), ref = ">$50,000")
data$quartileTHC <- factor(data$quartileTHC)
data$race <- relevel(factor(data$race), ref = "White")
datarace_ <- data %>% filter(race != "Other" & race != "NA")
datarace <- datarace_ %>% filter(income != "NA")

## income and education by race
datarace$income <- factor(datarace$income , levels=c("<$20,000", "$20,000 - $49,999", ">$50,000"), labels=c("<$20K", "$20 - $49K", ">$50K"))
datarace$race <- factor(datarace$race , levels=c("White", "Black"), labels=c("White (n=190)", "Black (n=191)"))
ggplot(datarace, aes(x=income, y=CE_C1)) + geom_boxplot(aes(fill=race)) + geom_jitter(size=0.05, alpha=0.9)
ggplot(datarace, aes(x=STATEFP, y=percentbelowpovertylevel)) + geom_boxplot(aes(fill=race)) + geom_jitter(size=0.05, alpha=0.9)
ggplot(datarace, aes(x=STATEFP, y=log(As))) + geom_boxplot(aes(fill=race)) + geom_jitter(size=0.05, alpha=0.9)
ggplot(datarace, aes(x=STATEFP, y=EN_TOTINCOME)) + geom_boxplot(aes(fill=STATEFP)) + geom_jitter(size=0.05, alpha=0.9)

ggplot(datarace, aes(x=race, y=EN_TOTINCOME)) + geom_boxplot(aes(fill=race))+ geom_jitter(size=0.05, alpha=0.9)
ggplot(datarace, aes(x=race, y=CE_C1)) + geom_boxplot(aes(fill=race))+ geom_jitter(size=0.05, alpha=0.9)
ggplot(datarace, aes(x=income, y=AsDistance)) + geom_boxplot(aes(fill=race)) + geom_jitter(size=0.2, alpha=0.9) + labs(y = bquote('Distance from As Emissions Site (km)'), x = bquote('Income'))
ggplot(datarace, aes(x=income, y=PbDistance)) + geom_boxplot(aes(fill=race)) + geom_jitter(size=0.2, alpha=0.9) + labs(y = bquote('Distance from Pb Emissions Site (km)'), x = bquote('Income'))
ggplot(datarace, aes(x=STATEFP, y=log10(Pb))) + geom_boxplot(aes(fill=race)) + geom_jitter(size=0.2, alpha=0.9)
ggplot(datarace, aes(x=(income), y=log10(Pb))) + geom_boxplot(aes(fill=race))+ geom_jitter(size=0.2, alpha=0.9)

## metal conc by distance, income, and race ---------------------------------------
ggplot(datarace, aes(x=(PbDistance), y=log10(Pb), color = race)) + geom_point(size=1, aes(shape = income, size=3)) + geom_smooth(method=lm) + labs(y = bquote('Pb Concentration'~(log10(µg/g))), x = bquote('Distance from Emissions Site (km)'))
ggplot(datarace, aes(x=(AsDistance), y=log10(As), color = race)) + geom_point(size=1, aes(shape = income, size=3)) + geom_smooth(method=lm) + labs(y = bquote('As Concentration'~(log10(µg/g))), x = bquote('Distance from Emissions Site (km)'))
ggplot(datarace, aes(x=(CrDistance), y=log10(Cr), color = race)) + geom_point(size=1, aes(shape = income, size=3)) + geom_smooth(method=lm) + labs(y = bquote('Cr Concentration'~(log10(µg/g))), x = bquote('Distance from Emissions Site (km)'))
ggplot(datarace, aes(x=(MnDistance), y=log10(Mn), color = race)) + geom_point(size=1, aes(shape = income, size=3)) + geom_smooth(method=lm) + labs(y = bquote('Mn Concentration'~(log10(µg/g))), x = bquote('Distance from Emissions Site (km)'))
ggplot(datarace, aes(x=(HgDistance), y=log10(Hg), color = race)) + geom_point(size=1, aes(shape = income, size=3)) + geom_smooth(method=lm) + labs(y = bquote('Hg Concentration'~(log10(µg/g))), x = bquote('Distance from Emissions Site (km)'))


datadis <- datarace %>% mutate(PbDistance_ = case_when(PbDistance >=5 & PbDistance<= 10 ~ '5-10 km',
                                                       PbDistance < 5 ~ '<5 km',
                                                       (is.na(PbDistance)) ~ '>10 km')) # end function

datadis <- datadis %>% mutate(AsDistance_ = case_when(AsDistance >= 5 & AsDistance <=10 ~ '5-10 km',
                                                       AsDistance < 5 ~ '<5 km',
                                                       (is.na(AsDistance)) ~ '>10 km')) # end function

datadis <- datadis %>% mutate(MnDistance_ = case_when(MnDistance > 5 & MnDistance <=10 ~ '5-10 km',
                                                      MnDistance >3 & MnDistance<= 5 ~ '3-5 km',
                                                      MnDistance < 3 ~ '<3 km',
                                                      (is.na(MnDistance)) ~ '>10 km')) # end function

datadis <- datadis %>% mutate(CrDistance_ = case_when(CrDistance > 5 & CrDistance <=10 ~ '5-10 km',
                                                      CrDistance >3 & CrDistance<= 5 ~ '3-5 km',
                                                      CrDistance < 3 ~ '<3 km',
                                                      (is.na(CrDistance)) ~ '>10 km')) # end function

datadis <- datadis %>% mutate(HgDistance_ = case_when(HgDistance > 5 & HgDistance <=10 ~ '5-10 km',
                                                      HgDistance >3 & HgDistance<= 5 ~ '3-5 km',
                                                      HgDistance < 3 ~ '<3 km',
                                                      (is.na(HgDistance)) ~ '>10 km')) # end function


datadis$PbDistance_ <- factor(datadis$PbDistance_ , levels=c("<5 km", "5-10 km", ">10 km"), labels=c("<5 km", "5-10 km", ">10 km"))
datadis$AsDistance_ <- factor(datadis$AsDistance_ , levels=c("<5 km", "5-10 km", ">10 km"), labels=c("<5 km", "5-10 km", ">10 km"))
datadis$MnDistance_ <- factor(datadis$MnDistance_ , levels=c("<3 km", "3-5 km", "5-10 km", ">10 km"), labels=c("<3 km", "3-5 km", "5-10 km", ">10 km"))
datadis$CrDistance_ <- factor(datadis$CrDistance_ , levels=c("<3 km", "3-5 km", "5-10 km", ">10 km"), labels=c("<3 km", "3-5 km", "5-10 km", ">10 km"))
datadis$HgDistance_ <- factor(datadis$HgDistance_ , levels=c("<3 km", "3-5 km", "5-10 km", ">10 km"), labels=c("<3 km", "3-5 km", "5-10 km", ">10 km"))

ggplot(datadis, aes(x=income, y=log10(Pb))) + geom_boxplot(outlier.size=1, aes(fill=race)) + facet_wrap(~PbDistance_, nrow = 1)+ theme_bw() + theme(axis.text.x = element_text(angle =45, hjust=1)) + labs(y = bquote('Pb Concentration'~(log10(µg/g)))) + geom_jitter(size=0.2, alpha=0.9)
ggplot(datadis, aes(x=income, y=log10(As))) + geom_boxplot(outlier.size=1, aes(fill=race)) + facet_wrap(~AsDistance_, nrow = 1)+ theme_bw() + theme(axis.text.x = element_text(angle =45, hjust=1)) + labs(y = bquote('As Concentration'~(log10(µg/g)))) + geom_jitter(size=0.2, alpha=0.9)
ggplot(datadis, aes(x=(income), y=log10(Mn))) + geom_boxplot(aes(fill=race)) + facet_wrap(~MnDistance_, nrow = 1)+ theme_bw() +theme(axis.text.x = element_text(angle =45, hjust=1)) + labs(y = bquote('Mn Concentration'~(log10(µg/g))))
ggplot(datadis, aes(x=(income), y=log10(Cr))) + geom_boxplot(aes(fill=race)) + facet_wrap(~CrDistance_, nrow = 1)+ theme_bw() +theme(axis.text.x = element_text(angle =45, hjust=1)) + labs(y = bquote('Cr Concentration'~(log10(µg/g))))
ggplot(datadis, aes(x=(income), y=log10(Hg))) + geom_boxplot(aes(fill=race)) + facet_wrap(~HgDistance_, nrow = 1)+ theme_bw() +theme(axis.text.x = element_text(angle =45, hjust=1)) + labs(y = bquote('Hg Concentration'~(log10(µg/g))))

#median age at CE by race 
datarace %>%
  group_by(race) %>%
  summarise(medage = median(CE_AGE),
            SDage = sd(CE_AGE))

#median age at CE total
median(datarace$CE_AGE,na.rm=T )
sd(datarace$CE_AGE,na.rm=T )



####################-----------------------------------------------------------------------------

blacksubgroup<-filter(data, race =="Black")
whitesubgroup<-filter(data, race =="White")

median(blacksubgroup$SeDistance, na.rm=T)
median(whitesubgroup$SeDistance, na.rm=T)

median(blacksubgroup$EN_TOTINCOME, na.rm=T)
median(whitesubgroup$EN_TOTINCOME, na.rm=T)

pbNEI <- blacksubgroup %>%
  group_by(Pb10_pointcount) %>%
  summarise(Count = n())
pbNEI

pbNEI <- whitesubgroup %>%
  group_by(Pb10_pointcount) %>%
  summarise(Count = n())
pbNEI

# black subgroup
mod1 <- lm(log10(Pb) ~ Batch + CE_AGE + EN_FORMERSMOKER  + passivesmoke + CE_C1 + road500 + Pb3_pointcount + STATEFP, data = blacksubgroup)
summary(mod1)
confint(mod1)
(exp(coef(mod1))-1)*100

#white subgroup
mod1 <- lm(log10(Pb) ~ Batch + CE_AGE + EN_FORMERSMOKER  + passivesmoke + CE_C1 + road500 + Pb3_pointcount + STATEFP, data = whitesubgroup)
summary(mod1)
confint(mod1)
(exp(coef(mod1))-1)*100

## all participants not separated by race -- minimally adjusted model
mod1 <- lm(log10(As) ~ Mass + Batch + CE_AGE + EN_FORMERSMOKER + THC_CUMULATIVE1 + road500 + As3_SUMEMISSIONS + STATEFP, data = data) # maybe shouldnt be controlling for education/income for env analysis
summary(mod1)
confint(mod1)
(exp(coef(mod1))-1)*100


# work status
wwork <- datalog %>%
  group_by(workstat) %>%
  summarise(Count = n())
wwork

bwork <- blacksubgroup %>%
  group_by(workstat) %>%
  summarise(Count = n())
bwork

wwork <- whitesubgroup %>%
  group_by(workstat) %>%
  summarise(Count = n())
wwork

## Cum THC
library(EnvStats)
median(data$THC_CUMULATIVE1, na.rm=T)
geoSD(data$THC_CUMULATIVE1, na.rm=T)

median(blacksubgroup$THC_CUMULATIVE1, na.rm=T)
geoSD(blacksubgroup$THC_CUMULATIVE1, na.rm=T)


##Age
median(data$CE_AGE, na.rm=T)
geoSD(data$CE_AGE, na.rm=T)

median(whitesubgroup$CE_AGE, na.rm=T)
geoSD(whitesubgroup$CE_AGE, na.rm=T)

# residence in lower income neighborhoods by race
wwork <- whitesubgroup %>%
  group_by(percentbelowpovertylevel) %>%
  summarise(median = median(percentbelowpovertylevel))
wwork

