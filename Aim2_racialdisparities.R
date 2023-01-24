library(tidyverse)
library(dplyr)
data <- read_csv("CE_blockgroupNEI_tertile.csv")
low_metals <- c("Cd", "Co", "Mo" ,"V", "Sb") # metals below 60% detect
`%notin%` <- Negate(`%in%`)
 
data$CdBinary <- as.factor(data$CdBinary) 
data$EDU<- as.factor(data$EDU)
data$EN_FORMERSMOKER<- as.factor(data$EN_FORMERSMOKER)
data$CoBinary <- as.factor(data$CoBinary)
data$MoBinary <- as.factor(data$MoBinary)
data$SbTertile <- as.factor(data$SbTertile)
data$VTertile <- as.factor(data$VTertile)
data$race <- as.factor(data$race)
data$road500 <- as.factor(data$road500)
data$STATEFP <- as.factor(data$STATEFP)
data$road1000 <- as.factor(data$road1000)
data$HV_L2B_NUM <- as.factor(data$HV_L2B_NUM)
data$passivesmoke <- as.factor(data$passivesmoke)
data$race <- relevel(as.factor(data$race), ref = "White")
data<- data %>% mutate(income = case_when(EN_TOTINCOME >= 5 ~ '>$50,000',
                                                  EN_TOTINCOME > 1 & EN_TOTINCOME<= 4 ~ '$20,000 - $49,999',
                                                  EN_TOTINCOME ==1 ~ '<$20,000')) # end function

data$income <- relevel(as.factor(data$income), ref = ">$50,000")

datlong<-pivot_longer(data, 6:23, names_to = "Metal", values_to = "Concentration")
datlongmain<-datlong %>% filter(Metal %notin% low_metals) 

data_log <- data %>% mutate(across(c(Mg, Al, Ca, Cr, Mn, Fe, Ni, Cu, Zn, As, Se, Hg, Pb), log10))
data_log$race <- relevel(factor(data_log$race), ref = "White")
datalograce <- data_log %>% filter(race != "Other")

##plot metal concentration disparity by race
# start plot for essential metals-------------------------------------------------
mod1 <- lm(Ca ~ Batch + CE_AGE + THC_CUMULATIVE1 + EN_EDU + EN_FORMERSMOKER + passivesmoke + race, data = datalograce)
mod2 <- lm(Cu ~ Batch + CE_AGE + THC_CUMULATIVE1 + EN_EDU + EN_FORMERSMOKER + passivesmoke + race, data = datalograce)
mod3 <- lm(Fe ~ Batch + CE_AGE + THC_CUMULATIVE1 + EN_EDU + EN_FORMERSMOKER + passivesmoke + race, data = datalograce)
mod4 <- lm(Mg ~ Batch + CE_AGE + THC_CUMULATIVE1 + EN_EDU + EN_FORMERSMOKER + passivesmoke + race, data = datalograce)
mod5 <- lm(Mn ~ Batch + CE_AGE + THC_CUMULATIVE1 + EN_EDU + EN_FORMERSMOKER + passivesmoke + race, data = datalograce)
mod6 <- lm(Se ~ Batch + CE_AGE + THC_CUMULATIVE1 + EN_EDU + EN_FORMERSMOKER + passivesmoke + race, data = datalograce)
mod7 <- lm(Zn ~ Batch + CE_AGE + THC_CUMULATIVE1 + EN_EDU + EN_FORMERSMOKER + passivesmoke + race, data = datalograce)

Ca <-broom::tidy(mod1) %>% filter(term != "CE_AGE") %>% filter(term != "Batch") %>% filter(term != "THC_CUMULATIVE1") %>% filter(term != "EN_FORMERSMOKER") %>% filter(term != "EN_EDU") %>% filter(term != "EN_FORMERSMOKER1") %>% filter(term != "passivesmoke1")%>% filter(term != "passivesmoke")%>% filter(term != "raceOther")
Cu <-broom::tidy(mod2) %>% filter(term != "CE_AGE") %>% filter(term != "Batch") %>% filter(term != "THC_CUMULATIVE1") %>% filter(term != "EN_FORMERSMOKER") %>% filter(term != "EN_EDU") %>% filter(term != "EN_FORMERSMOKER1") %>% filter(term != "passivesmoke1")%>% filter(term != "passivesmoke")%>% filter(term != "raceOther")
Fe <-broom::tidy(mod3) %>% filter(term != "CE_AGE") %>% filter(term != "Batch") %>% filter(term != "THC_CUMULATIVE1") %>% filter(term != "EN_FORMERSMOKER") %>% filter(term != "EN_EDU") %>% filter(term != "EN_FORMERSMOKER1") %>% filter(term != "passivesmoke1")%>% filter(term != "passivesmoke")%>% filter(term != "raceOther")
Mg <-broom::tidy(mod4) %>% filter(term != "CE_AGE") %>% filter(term != "Batch") %>% filter(term != "THC_CUMULATIVE1") %>% filter(term != "EN_FORMERSMOKER") %>% filter(term != "EN_EDU") %>% filter(term != "EN_FORMERSMOKER1") %>% filter(term != "passivesmoke1")%>% filter(term != "passivesmoke")%>% filter(term != "raceOther")
Mn <-broom::tidy(mod5) %>% filter(term != "CE_AGE") %>% filter(term != "Batch") %>% filter(term != "THC_CUMULATIVE1") %>% filter(term != "EN_FORMERSMOKER") %>% filter(term != "EN_EDU") %>% filter(term != "EN_FORMERSMOKER1") %>% filter(term != "passivesmoke1")%>% filter(term != "passivesmoke")%>% filter(term != "raceOther")
Se <-broom::tidy(mod6) %>% filter(term != "CE_AGE") %>% filter(term != "Batch") %>% filter(term != "THC_CUMULATIVE1") %>% filter(term != "EN_FORMERSMOKER") %>% filter(term != "EN_EDU") %>% filter(term != "EN_FORMERSMOKER1") %>% filter(term != "passivesmoke1")%>% filter(term != "passivesmoke")%>% filter(term != "raceOther")
Zn <-broom::tidy(mod7) %>% filter(term != "CE_AGE") %>% filter(term != "Batch") %>% filter(term != "THC_CUMULATIVE1") %>% filter(term != "EN_FORMERSMOKER") %>% filter(term != "EN_EDU") %>% filter(term != "EN_FORMERSMOKER1") %>% filter(term != "passivesmoke1")%>% filter(term != "passivesmoke")%>% filter(term != "raceOther")

library(dotwhisker)
library(dplyr)
library(gdata)
essential_models <- combine(Ca, Cu, Fe, Mg, Mn, Se, Zn)
colnames(essential_models)[6] ="model"

plotRace_essential<- dwplot(essential_models, dodge_size = 1, dot_args = list(aes(colour = model),size = 5), whisker_args = list(aes(colour=model), size = 3)) %>% relabel_predictors(c("raceBlack" = "Black"))
plotRace_essential
plotRace_essential1 <- plotRace_essential + theme_bw(base_size = 4) + geom_vline(xintercept = 0, colour = "grey60", linetype = 2) + ggtitle("Essential Metals") + scale_color_brewer(palette="Set2") + theme(text = element_text(size = 16)) + labs(x = bquote('Coefficent Estimate with 95% CIs')) + theme(axis.text=element_text(size=17)) + theme(axis.title.x = element_text(margin = margin(t = 15))) + theme(panel.border = element_rect(fill=NA, colour = "black", size=1))  
plotRace_essential1 
ggsave("metals.png", plotRace_essential1, bg='transparent')
png("essential.png", width = 9.9, height = 5.4, units = 'in', res = 300) 
plotRace_essential1 
dev.off()


## start plot for toxic metals
mod8 <- lm(Al ~ Batch + CE_AGE + THC_CUMULATIVE1 + EN_EDU + EN_FORMERSMOKER + passivesmoke + race, data = datalograce)
mod9 <- lm(As ~ Batch + CE_AGE + THC_CUMULATIVE1 + EN_EDU + EN_FORMERSMOKER + passivesmoke + race, data = datalograce)
mod10 <- lm(Cr ~ Batch + CE_AGE + THC_CUMULATIVE1 + EN_EDU + EN_FORMERSMOKER + passivesmoke + race, data = datalograce)
mod11 <- lm(Hg ~ Batch + CE_AGE + THC_CUMULATIVE1 + EN_EDU + EN_FORMERSMOKER + passivesmoke + race, data = datalograce)
mod12 <- lm(Ni ~ Batch + CE_AGE + THC_CUMULATIVE1 + EN_EDU + EN_FORMERSMOKER + passivesmoke + race, data = datalograce)
mod13 <- lm(Pb ~ Batch + CE_AGE + THC_CUMULATIVE1 + EN_EDU + EN_FORMERSMOKER + passivesmoke + race, data = datalograce)

Al <-broom::tidy(mod8) %>% filter(term != "CE_AGE") %>% filter(term != "Batch") %>% filter(term != "passivesmoke1") %>% filter(term != "THC_CUMULATIVE1") %>% filter(term != "EN_FORMERSMOKER") %>% filter(term != "EN_EDU")  %>% filter(term != "EN_FORMERSMOKER1")%>% filter(term != "passivesmoke")
As <-broom::tidy(mod9) %>% filter(term != "CE_AGE") %>% filter(term != "Batch") %>% filter(term != "passivesmoke1") %>% filter(term != "THC_CUMULATIVE1") %>% filter(term != "EN_FORMERSMOKER") %>% filter(term != "EN_EDU")  %>% filter(term != "EN_FORMERSMOKER1")%>% filter(term != "passivesmoke")
Cr <-broom::tidy(mod10) %>% filter(term != "CE_AGE") %>% filter(term != "Batch") %>% filter(term != "passivesmoke1") %>% filter(term != "THC_CUMULATIVE1") %>% filter(term != "EN_FORMERSMOKER") %>% filter(term != "EN_EDU")  %>% filter(term != "EN_FORMERSMOKER1")%>% filter(term != "passivesmoke")
Hg <-broom::tidy(mod11) %>% filter(term != "CE_AGE") %>% filter(term != "Batch") %>% filter(term != "passivesmoke1") %>% filter(term != "THC_CUMULATIVE1") %>% filter(term != "EN_FORMERSMOKER") %>% filter(term != "EN_EDU")  %>% filter(term != "EN_FORMERSMOKER1")%>% filter(term != "passivesmoke")
Ni <-broom::tidy(mod12) %>% filter(term != "CE_AGE") %>% filter(term != "Batch") %>% filter(term != "passivesmoke1") %>% filter(term != "THC_CUMULATIVE1") %>% filter(term != "EN_FORMERSMOKER") %>% filter(term != "EN_EDU")  %>% filter(term != "EN_FORMERSMOKER1")%>% filter(term != "passivesmoke")
Pb <-broom::tidy(mod13) %>% filter(term != "CE_AGE") %>% filter(term != "Batch") %>% filter(term != "passivesmoke1") %>% filter(term != "THC_CUMULATIVE1") %>% filter(term != "EN_FORMERSMOKER") %>% filter(term != "EN_EDU")  %>% filter(term != "EN_FORMERSMOKER1")%>% filter(term != "passivesmoke")

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


## individual risk factors -----------------------------------------------------------------------Se10_pointcount + Se10_SUMEMISSIONS
mod1 <- lm(log10(Pb) ~ Batch + CE_AGE + race + EN_FORMERSMOKER + passivesmoke + road500 + Pb3_SUMEMISSIONS + STATEFP, data = data) # maybe shouldnt be controlling for education/income for env analysis
summary(mod1)
confint(mod1)
(exp(coef(mod1))-1)*100

#without metal exposure variables ISEE Abstract
mod1 <- lm(log10(Pb) ~ Batch + race + CE_AGE +EN_FORMERSMOKER + passivesmoke + STATEFP + THC_CUMULATIVE1, data = data) 
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

mod2 <- lm(log10(Pb) ~ Batch + Age5 + race + EN_FORMERSMOKER + THC1_30 + EDU + road500 + STATEFP+ Pb10_pointcount + perbelowpov10, data = data)
summary(mod2)
confint(mod2)
(exp(coef(mod2))-1)*100

## Race/income interaction terms---------------------------------------------------
#boxplot of income by race
fig3<-ggplot(data = datlongmain, aes(x=race, y=EN_TOTINCOME))+geom_boxplot(aes(fill=race),  outlier.size = 0.7)
fig3

mod2 <- lm(log10(Pb) ~ Mass + Batch + Age5 + race + EDU + race*EDU + EN_FORMERSMOKER + THC1_30 + CE_housetype + road500 + STATEFP+ Pb10_pointcount + peremploy10, data = data)
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
ggplot(datarace, aes(x=income, y=EN_EDU)) + geom_boxplot(aes(fill=race)) + geom_jitter(size=0.05, alpha=0.9)
ggplot(datarace, aes(x=race, y=EN_TOTINCOME)) + geom_boxplot(aes(fill=race))+ geom_jitter(size=0.05, alpha=0.9)
ggplot(datarace, aes(x=race, y=EN_EDU)) + geom_boxplot(aes(fill=race))+ geom_jitter(size=0.05, alpha=0.9)
ggplot(datarace, aes(x=income, y=AsDistance)) + geom_boxplot(aes(fill=race)) + geom_jitter(size=0.2, alpha=0.9) + labs(y = bquote('Distance from As Emissions Site (km)'), x = bquote('Income'))
ggplot(datarace, aes(x=STATEFP, y=log10(Pb))) + geom_boxplot(aes(fill=race)) + geom_jitter(size=0.2, alpha=0.9)

## metal conc by distance, income, and race ---------------------------------------
ggplot(datarace, aes(x=(income), y=log10(Pb))) + geom_boxplot(aes(fill=race))+ geom_jitter(size=0.2, alpha=0.9)
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

pbNEI <- blacksubgroup %>%
  group_by(Pb10_pointcount) %>%
  summarise(Count = n())
pbNEI

pbNEI <- whitesubgroup %>%
  group_by(Pb10_pointcount) %>%
  summarise(Count = n())
pbNEI

# black subgroup
mod1 <- lm(log10(Pb) ~ Batch + CE_AGE + EN_FORMERSMOKER  + passivesmoke + EN_EDU + road500 + Pb3_pointcount + STATEFP, data = blacksubgroup)
summary(mod1)
confint(mod1)
(exp(coef(mod1))-1)*100

#white subgroup
mod1 <- lm(log10(Pb) ~ Batch + CE_AGE + EN_FORMERSMOKER  + passivesmoke + EN_EDU + road500 + Pb3_pointcount + STATEFP, data = whitesubgroup)
summary(mod1)
confint(mod1)
(exp(coef(mod1))-1)*100

## all participants not separated by race -- minimally adjusted model
mod1 <- lm(log10(As) ~ Mass + Batch + CE_AGE + EN_FORMERSMOKER + THC_CUMULATIVE1 + road500 + As3_SUMEMISSIONS + STATEFP, data = data) # maybe shouldnt be controlling for education/income for env analysis
summary(mod1)
confint(mod1)
(exp(coef(mod1))-1)*100


