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
data<- data %>% mutate(income = case_when(EN_TOTINCOME >= 5 ~ '>$50,000',
                                          EN_TOTINCOME > 1 & EN_TOTINCOME<= 4 ~ '$20,000 - $49,999',
                                          EN_TOTINCOME ==1 ~ '<$20,000')) # end function

data$income <- relevel(as.factor(data$income), ref = ">$50,000")


data <- data %>% mutate(EDU = case_when(EN_EDU >= 18 ~ 'College or more',
                                                EN_EDU >= 15 & EN_EDU <=17 ~ 'Some College',
                                                EN_EDU >= 13 & EN_EDU<= 14 ~ 'Highschool',
                                                EN_EDU <=12 ~ 'Less than Highschool')) # end function

data$EDU <- relevel(factor(data$EDU), ref = "College or more")

data <- data %>% mutate(race = case_when(EN_RACE2 >=3 ~ 'Other', 
                                                 EN_RACE2 == 2 ~ 'Black',
                                                 EN_RACE2 == 1 ~ 'White')) # end function

data$race <- relevel(factor(data$race), ref = "White")

data_log <- data %>% mutate(across(c(BX_BLD_BPB_NUM, BX_BLD_THG_NUM, BX_BLD_BMN_NUM, BX_BLD_BSE_NUM), log10))

## blood metal conc by race------------------------------------
#boxplot of blood metal conc by race
data_log$BX_BLD_BPB_NUM<- as.numeric(data_log$BX_BLD_BPB_NUM)
data_log$BX_BLD_THG_NUM<- as.numeric(data_log$BX_BLD_THG_NUM)
data_log$BX_BLD_BMN_NUM<- as.numeric(data_log$BX_BLD_BMN_NUM)
data_log$BX_BLD_BSE_NUM<- as.numeric(data_log$BX_BLD_BSE_NUM)

data$BX_BLD_BPB_NUM<- as.numeric(data$BX_BLD_BPB_NUM)
data$BX_BLD_THG_NUM<- as.numeric(data$BX_BLD_THG_NUM)
data$BX_BLD_BMN_NUM<- as.numeric(data$BX_BLD_BMN_NUM)
data$BX_BLD_BSE_NUM<- as.numeric(data$BX_BLD_BSE_NUM)


##descriptive for blood metals - total n = 84 (47 white, 37 black)
databtex <- subset(data, !is.na(BX_BLD_BPB_NUM))
databtex <- subset(databtex, !is.na(BX_BLD_THG_NUM))
databtex <- subset(databtex, !is.na(BX_BLD_BMN_NUM))
databtex <- subset(databtex, !is.na(BX_BLD_BSE_NUM))


databtex %>% dplyr::summarize(n=n(),
                            quant10 = quantile(BX_BLD_BPB_NUM, probs = 0.10),        
                            quant25 = quantile(BX_BLD_BPB_NUM, probs = 0.25), 
                            quant50 = quantile(BX_BLD_BPB_NUM, probs = 0.5),
                            quant75 = quantile(BX_BLD_BPB_NUM, probs = 0.75),
                            quant95 = quantile(BX_BLD_BPB_NUM, probs = 0.95))

databtex %>% group_by(race) %>% dplyr::summarize(n=n(),
                                              quant10 = quantile(BX_BLD_BPB_NUM, probs = 0.10),        
                                              quant25 = quantile(BX_BLD_BPB_NUM, probs = 0.25), 
                                                      quant50 = quantile(BX_BLD_BPB_NUM, probs = 0.5),
                                                      quant75 = quantile(BX_BLD_BPB_NUM, probs = 0.75),
                                                      quant95 = quantile(BX_BLD_BPB_NUM, probs = 0.95))

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
