setwd("/Users/joycelin/Desktop/Gulf/Aim2")

library(tidyverse)
library(dplyr)
library(readxl)

data <- read_csv("CE_blockgroupNEI_tertile.csv")
low_metals <- c("Cd", "Co", "Mo" ,"V", "Sb") # metals below 60% detect
`%notin%` <- Negate(`%in%`)
data <- mutate_all(data, function(x) as.numeric(as.character(x)))
data_log <- data %>% mutate(across(c(Mg, Al, Ca, Cr, Mn, Fe, Ni, Cu, Zn, As, Se, Hg, Pb), log10))


# PCA for toenail metals --------------------------------------------------
dftoe <- data_log[, 6:23]
dftoe <- subset(dftoe, select = -c(Cd, Co, V, Mo, Sb, Al, Ni))
dftoe <- na.omit(dftoe)

toepc <- prcomp(dftoe, center = TRUE, scale. = TRUE)
summary(toepc)
toepc

# biplot
biplot(toepc, scale = 0)
eigenvals <- toepc$sdev^2
eigenvals # 2 PCs based off eigenvalues >1

## promax rotation
library(psych)
toepc.pro <-principal(dftoe, nfactors=2, rotate = "promax")# not orthogonal, allows correlation
toepc.pro

## varimax rotation
toepc.var <-principal(dftoe, nfactors=2, rotate = "varimax") #orthogonal, no correlation
toepc.var


# Determine Number of Factors to Extract and plot for PCA
library(nFactors)
library(FactoMineR)
ev <- eigen(cor(dftoe)) # get eigenvalues
ap <- parallel(subject=nrow(dftoe),var=ncol(dftoe),
               rep=100,cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS)

##visualize graph
result <- PCA(dftoe)
# Principal components regression using 3 PCs after varimax rotation----------------
install.packages("pls")
library(pls)


#fit PCR model
model <- pcr(hp~mpg+disp+drat+wt+qsec, data=mtcars, scale=TRUE, validation="CV")

## Exploratory factor analysis --------------------------------------
install.packages("REdaS")
library(REdaS)
install.packages("GPArotation") 
library(GPArotation)
library(psych)
 
data <- read_csv("CE_blockgroupNEI_tertile.csv")
data <- mutate_all(data, function(x) as.numeric(as.character(x)))

dftoe <- data_log[, 6:23]
dftoe <- subset(dftoe, select = -c(Cd, Co, V, Mo, Sb, Al, Ni))
dftoe <- na.omit(dftoe)

bart_spher(dftoe) # test of spherecity want significant
KMO(dftoe) # want about >0.7

fa(dftoe, nfactors=3, rotate = "oblimin") # not orthogonal, factors are still correlated
fa(dftoe, nfactors=2, rotate = "varimax") # orthogonal, not correlated

M1 <- fa(dftoe, nfactors = 2, rotate = "varimax")
fa.diagram(M1, main="dftoe")



