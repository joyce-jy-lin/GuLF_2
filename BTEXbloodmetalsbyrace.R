##blood conc by race in BTEX substudy
setwd("/Users/joycelin/Desktop/Gulf/Aim2")
library(tidyverse)
library(readxl)
library(Hmisc) 
library(gtsummary)
library(summarytools)
library(ggplot2)

# load data
data<- read.csv("btexblood.csv")
data_log <- data %>% mutate(across(c(BX_BLD_BPB_NUM, BX_BLD_THG_NUM, BX_BLD_BMN_NUM, BX_BLD_BSE_NUM), log10))


data_log<- data_log %>% mutate(income = case_when(EN_TOTINCOME >= 5 ~ '>$50,000',
                                          EN_TOTINCOME > 1 & EN_TOTINCOME<= 4 ~ '$20,000 - $49,999',
                                          EN_TOTINCOME ==1 ~ '<$20,000')) # end function

data_log$income <- relevel(as.factor(data_log$income), ref = ">$50,000")


data_log <- data_log %>% mutate(EDU = case_when(EN_EDU >= 18 ~ 'College or more',
                                                EN_EDU >= 15 & EN_EDU <=17 ~ 'Some College',
                                                EN_EDU >= 13 & EN_EDU<= 14 ~ 'Highschool',
                                                EN_EDU <=12 ~ 'Less than Highschool')) # end function

data_log$EDU <- relevel(factor(data_log$EDU), ref = "College or more")

data_log <- data_log %>% mutate(race = case_when(EN_RACE2 >=3 ~ 'Other', 
                                                 EN_RACE2 == 2 ~ 'Black',
                                                 EN_RACE2 == 1 ~ 'White')) # end function

data_log$race <- relevel(factor(data_log$race), ref = "White")

## blood metal conc by race------------------------------------
#boxplot of blood metal conc by race
data_log$BX_BLD_BPB_NUM<- as.numeric(data_log$BX_BLD_BPB_NUM)
data_log$BX_BLD_THG_NUM<- as.numeric(data_log$BX_BLD_THG_NUM)
data_log$BX_BLD_BMN_NUM<- as.numeric(data_log$BX_BLD_BMN_NUM)
data_log$BX_BLD_BSE_NUM<- as.numeric(data_log$BX_BLD_BSE_NUM)


##descriptive for blood metals - total n = 84 (47 white, 37 black)
data_logbtex <- subset(data_log, !is.na(BX_BLD_BPB_NUM))
data_logbtex <- subset(data_logbtex, !is.na(BX_BLD_THG_NUM))
data_logbtex <- subset(data_logbtex, !is.na(BX_BLD_BMN_NUM))
data_logbtex <- subset(data_logbtex, !is.na(BX_BLD_BSE_NUM))
data_logbtex %>% group_by(race) %>% dplyr:: summarize(n=n(),
                                                      min = min(BX_BLD_BPB_NUM), 
                                                      med = median(BX_BLD_BPB_NUM),
                                                      max = max(BX_BLD_BPB_NUM))

data_logbtex<- data_logbtex %>% filter(race != "Other" & race != "NA" )

##boxplots
data_logbtex$race <- factor(data_logbtex$race , levels=c("White", "Black"), labels=c("White (n=538)", "Black (n=444)"))
fig3<-ggplot(data = data_logbtex, aes(x=race, y=BX_BLD_BPB_NUM)) + geom_boxplot(aes(fill=race)) + labs(y = bquote('Blood Pb Concentration (log10(µg/dL))'), x = bquote('Race'))
fig3

fig2<-ggplot(data = data_logbtex, aes(x=race, y=BX_BLD_BMN_NUM)) + geom_boxplot(aes(fill=race)) + labs(y = bquote('Blood Mn Concentration (log10(µg/dL))'), x = bquote('Race'))
fig2

fig1<-ggplot(data = data_logbtex, aes(x=race, y=BX_BLD_THG_NUM)) + geom_boxplot(aes(fill=race)) + labs(y = bquote('Blood Hg Concentration (log10(µg/dL))'), x = bquote('Race'))
fig1

fig4<-ggplot(data = data_logbtex, aes(x=race, y=BX_BLD_BSE_NUM)) + geom_boxplot(aes(fill=race)) + labs(y = bquote('Blood Se Concentration (log10(µg/dL))'), x = bquote('Race'))
fig4

## dot whisker plot ----------------------------------------------
mod5 <- lm(BX_BLD_BMN_NUM ~ EN_AGE + EN_EDU + EN_FORMERSMOKER + EN_CURRENTSMOKER+ race, data = data_logbtex)
mod6 <- lm(BX_BLD_BSE_NUM ~ EN_AGE + EN_EDU + EN_FORMERSMOKER + EN_CURRENTSMOKER+ race, data = data_logbtex)
mod11 <- lm(BX_BLD_THG_NUM ~ EN_AGE + EN_EDU + EN_FORMERSMOKER+ EN_CURRENTSMOKER + race, data = data_logbtex)
mod13 <- lm(BX_BLD_BPB_NUM ~ EN_AGE + EN_EDU + EN_FORMERSMOKER + EN_CURRENTSMOKER+ race, data = data_logbtex)

Mn <-broom::tidy(mod5) %>% filter(term != "EN_AGE")%>% filter(term != "EN_FORMERSMOKER") %>% filter(term != "EN_EDU") %>% filter(term != "EN_FORMERSMOKER1") %>% filter(term != "passivesmoke1")%>% filter(term != "passivesmoke")%>% filter(term != "raceOther")%>% filter(term != "EN_CURRENTSMOKER")
Se <-broom::tidy(mod6) %>% filter(term != "EN_AGE")%>% filter(term != "EN_FORMERSMOKER") %>% filter(term != "EN_EDU") %>% filter(term != "EN_FORMERSMOKER1") %>% filter(term != "passivesmoke1")%>% filter(term != "passivesmoke")%>% filter(term != "raceOther")%>% filter(term != "EN_CURRENTSMOKER")
Hg <-broom::tidy(mod11) %>% filter(term != "EN_AGE") %>% filter(term != "EN_FORMERSMOKER") %>% filter(term != "EN_EDU") %>% filter(term != "EN_FORMERSMOKER1") %>% filter(term != "passivesmoke1")%>% filter(term != "passivesmoke")%>% filter(term != "raceOther")%>% filter(term != "EN_CURRENTSMOKER")
Pb <-broom::tidy(mod13) %>% filter(term != "EN_AGE") %>% filter(term != "EN_FORMERSMOKER") %>% filter(term != "EN_EDU") %>% filter(term != "EN_FORMERSMOKER1") %>% filter(term != "passivesmoke1")%>% filter(term != "passivesmoke")%>% filter(term != "raceOther")%>% filter(term != "EN_CURRENTSMOKER")

library(dotwhisker)
library(dplyr)
library(gdata)
essential_models <- combine(Mn, Se, Hg, Pb)
colnames(essential_models)[6] ="model"

cbPalette <- c("#009E73","#0072B2","#E69F00","#56B4E9")

plotRace_essential<- dwplot(essential_models, dodge_size = 1, dot_args = list(aes(colour = model),size = 2), whisker_args = list(aes(colour=model), size = 1)) %>% relabel_predictors(c("raceBlack" = "Black"))%>% relabel_predictors(c("income$20,000 - $49,999" = "$20,000 - $49,999"))
plotRace_essential
plotRace_essential1 <- plotRace_essential + geom_vline(xintercept = 0, colour = "grey60", linetype = 2) + ggtitle("GuLF BTEX Blood Metals") + theme(text = element_text(size = 12)) + 
  labs(x = bquote('Percent difference Black vs White')) + theme(axis.text=element_text(size=12)) + theme(axis.title.x = element_text(margin = margin(t = 12)))+
  scale_color_manual(values=cbPalette) +scale_x_continuous(breaks = log10(pretty(10^(essential_models$estimate),n=2000)), labels = pretty(10^(essential_models$estimate),n=2000))+ theme_bw() + theme(panel.border = element_rect(fill=NA, colour = "black", size=1))
plotRace_essential1 



## Comparing btex to nhanes -----------------------------------------------------------------------------------------
BxNh<- read.csv("btexnhanes.csv")

col <- c("0"= "#3333FF", "1" = "#65FF65")

Pbplot <- BxNh %>% ggplot(aes(x = log10(Pb), color = Study, fill = Study)) +
  geom_histogram(alpha=0.3, position="identity", bins = 50) + theme_minimal() +
  scale_color_manual(name = "", labels = c("NHANES (n=602)", "GuLF BTEX (n=1060)"), values = col) + 
  scale_fill_manual(name = "", labels = c("NHANES (n=602)", "GuLF BTEX (n=1060)"), values=col)+ 
  labs(x =expression( "Concentration" ~ mu*g/g), y = "Count", title = "Pb") + theme(legend.position="bottom")
Pbplot

#organize data for boxplot
BxNh<- read.csv("btexnhanes.csv")
BxNh <- BxNh[ which(BxNh$Gender=='1'
                         & BxNh$Age <=70), ]
write.csv(BxNh,file='BTEXNHANESCOMP.csv')
BxNh<- read.csv("BTEXNHANESCOMP.csv")

BxNh %>%
  group_by(Studyname) %>%
  count()

BxNh[BxNh == "GuLF BTEX (n=1060)"] <- "GuLF BTEX (n=723)"
BxNh[BxNh == "NHANES (n=602)"] <- "NHANES (n=239)"

BxNh$Race <- factor(BxNh$Race,
                       levels = c('White','Black'),ordered = TRUE)

Pbbox<-ggplot(data = BxNh, aes(x=Studyname, y=log10(Pb)), fill=Race) + geom_boxplot(aes(fill=Race)) + scale_fill_manual(values=c("#F8766D", "#00BFC4")) + 
  labs(y = bquote('Pb Concentration (log10(µg/dL))'), x = bquote('Race'))+ theme(text = element_text(size = 12))+ 
  theme(text = element_text(size = 12))+
  geom_jitter(color="black", size=0.05, alpha=0.9)+ theme_bw()
Pbbox

Mnbox<-ggplot(data = BxNh, aes(x=Studyname, y=log10(Mn)), fill=Race) + geom_boxplot(aes(fill=Race)) + scale_fill_manual(values=c("#F8766D", "#00BFC4")) + 
  labs(y = bquote('Mn Concentration (log10(µg/dL))'), x = bquote('Race'))+ theme(text = element_text(size = 12))+ 
  theme(text = element_text(size = 12))+ 
  geom_jitter(color="black", size=0.05, alpha=0.9)+ theme_bw()
Mnbox

Sebox<-ggplot(data = BxNh, aes(x=Studyname, y=log10(Se)), fill=Race) + geom_boxplot(aes(fill=Race)) + scale_fill_manual(values=c("#F8766D", "#00BFC4")) + 
  labs(y = bquote('Se Concentration (log10(µg/dL))'), x = bquote('Race'))+ theme(text = element_text(size = 12))+ 
  theme(text = element_text(size = 12))+ 
  geom_jitter(color="black", size=0.05, alpha=0.9)+ theme_bw()
Sebox

Hgbox<-ggplot(data = BxNh, aes(x=Studyname, y=log10(Hg)), fill=Race) + geom_boxplot(aes(fill=Race)) + scale_fill_manual(values=c("#F8766D", "#00BFC4")) + 
  labs(y = bquote('Hg Concentration (log10(µg/dL))'), x = bquote('Race'))+ theme(text = element_text(size = 12))+ 
  theme(text = element_text(size = 12))+ 
  geom_jitter(color="black", size=0.05, alpha=0.9)+ theme_bw()
Hgbox

