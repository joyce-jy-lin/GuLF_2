setwd("/Users/joycelin/Desktop/Gulf/Aim2/GuLF_2")
library(tidyverse)
library(dplyr)
library(readxl)
data <- read_csv("CE_blockgroupNEI_tertile.csv")
low_metals <- c("Cd", "Co", "Mo" ,"V", "Sb") # metals below 60% detect
`%notin%` <- Negate(`%in%`)

data$CdBinary <- as.factor(data$CdBinary) 
data$EDU<- as.factor(data$EDU)
data$CE_housetype<- as.factor(data$CE_housetype)
data$Batch<- as.factor(data$Batch)
data$EN_FORMERSMOKER<- as.factor(data$EN_FORMERSMOKER)
data$quartileTHC <- as.factor(data$quartileTHC)
data$CoBinary <- as.factor(data$CoBinary)
data$MoBinary <- as.factor(data$MoBinary)
data$SbTertile <- as.factor(data$SbTertile)
data$VTertile <- as.factor(data$VTertile)
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

data$race <- as.numeric(data$race)

data<- data %>% mutate(As_idw = As10_SUMEMISSIONS/AsDistance,
                       Cr_idw = Cr10_SUMEMISSIONS/CrDistance,
                       Pb_idw = Pb10_SUMEMISSIONS/PbDistance,
                       Mn_idw = Mn10_SUMEMISSIONS/MnDistance,
                       Se_idw = Se10_SUMEMISSIONS/SeDistance,
                       Hg_idw = Hg10_SUMEMISSIONS/HgDistance) # end function

data<- data %>% mutate(workstat = case_when(workstat == 1 ~ 'Working now',
                                       workstat >=2 & workstat <8 ~ 'Not working',
                                       workstat ==8 ~ 'Other',
                                       )) # end function


data_log <- data %>% mutate(across(c(Mg, Al, Ca, Cr, Mn, Fe, Ni, Cu, Zn, As, Se, Hg, Pb), log10))

## ENET

datapb <- data_log[, c("Pb", "race", "CE_AGE", "THC_CUMULATIVE1", "PbDistance", "Pb10_SUMEMISSIONS", "Pb5_SUMEMISSIONS", "Pb3_SUMEMISSIONS", "Pb10_pointcount","Pb5_pointcount", "Pb3_pointcount","medianyearstructurebuilt", "percentbelowpovertylevel", "medianhouseholdincome", "Totalpop", "percenthispanicllatino", "percentblack", "percentamericanindian", "percentasian", "lessthanhighschool", "owneroccupied", "CE_C1", "EN_TOTINCOME", "EN_FORMERSMOKER", "passivesmoke", "STATEFP", "road50", "road100", "road200", "CE_BMI")]
datapb <-datapb %>% na.omit()

library(caret)
library(glmnet)
library(psych)

#check correlation across continuous covariates
cor(datapb[c(-29,-28,-27,-26, -25, -24,-23,-2)])

# partition data
set.seed(123)
training.samples <- datapb$Pb %>%
  createDataPartition(p = 0.8, list = FALSE)
train  <- datapb[training.samples, ]
test <- datapb[-training.samples, ]

x<- model.matrix(Pb ~ ., train)[,-1]
y<-train$Pb

#Custom control parameters
custom <-trainControl(method = "repeatedcv",
                      number = 10,
                      repeats = 5,
                      verboseIter = T)
#linear model
set.seed(123)
lm<-train(Pb~.,
               datapb,
               method = "lm",
               trControl = custom)
lm$results
lm
summary(lm)

#ridge regression
set.seed(123)
ridge<-train(Pb~.,
             datapb,
             method='glmnet',
             tuneGrid = expand.grid(alpha=0,
             lambda = seq(0.0001, 1, length=5)),
             trControl=custom)

#plot results
plot(ridge)
ridge
plot(ridge$finalModel, xvar = "lambda", lable = T)
plot(ridge$finalModel, xvar = 'dev', label=T)
plot(varImp(ridge, scale=F))

#Lasso regression
set.seed(123)
lasso<-train(Pb~.,
             datapb,
             method = 'glmnet',
             tuneGrid = expand.grid(alpha=1,
                                   lambda = seq(0.000001, 0.2, length=5)),
                                   trControl=custom)
plot(lasso)
lasso
plot(lasso$finalModel, xvar = "lambda", lable = T)
plot(lasso$finalModel, xvar = 'dev', label=T)
plot(varImp(lasso, scale=F))

#Elastic net
set.seed(123)
en<-train(Pb~.,
          datapb,
          method='glmnet',
          tuneGrid = expand.grid(alpha = seq(0,1, length=10),
                                 lambda=seq(0.0001, 1, length=5)),
          trControl=custom)

plot(en)
en
plot(en$finalModel, xvar = "lambda", lable = T)
plot(en$finalModel, xvar = 'dev', label=T)
plot(varImp(en, scale=F))
coef(enet, s=en$bestTune$lambda)

# compare models
model_list <- list(LinearModel = lm, Ridge = ridge, Lasso = lasso, ElasticNet=en)
res <-resamples(model_list)
summary(res)
xyplot(res, metric = 'RMSE')

#Best model
best<-en$bestTune
coef(best, s = en$bestTune$lambda)
