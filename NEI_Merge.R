#Merge datasets for NEI IDW
setwd("/Users/joycelin/Desktop/Gulf/Aim2/GuLF_2")
library(tidyverse)
library(dplyr)
library(readxl)
library(Hmisc) 

Pbdat<- read_excel("Pb30dat.xlsx")
Pb <- read_excel("PbNEI.xlsx")
Pb_merge <- merge(Pbdat, Pb) 
Pb_merge <- Pb_merge %>% 
  rename("RECORD" = "IN_FID")
ID <- read_excel("CEparticipants_bg_TableToExcel_1.xlsx")
Pb_merge1 <- merge(Pb_merge, ID) 
Pb_merge1 = select(Pb_merge1, -c(OBJECTID, RECORD))
write.csv(Pb_merge1, file = "Pbmerge.csv")

Asdat<- read_excel("As30dat.xlsx")
As <- read_excel("AsNEI.xlsx")
As_merge <- merge(Asdat, As) 
As_merge <- As_merge %>% 
  rename("RECORD" = "IN_FID")
ID <- read_excel("CEparticipants_bg_TableToExcel_1.xlsx")
As_merge1 <- merge(As_merge, ID) 
As_merge1 = select(As_merge1, -c(RECORD))
write.csv(As_merge1, file = "Asmerge.csv")

Mndat<- read_excel("Mn30dat.xlsx")
Mn <- read_excel("MnNEI.xlsx")
Mn_merge <- merge(Mndat, Mn) 
Mn_merge <- Mn_merge %>% 
  rename("RECORD" = "IN_FID")
ID <- read_excel("CEparticipants_bg_TableToExcel_1.xlsx")
Mn_merge1 <- merge(Mn_merge, ID) 
Mn_merge1 = select(Mn_merge1, -c(OBJECTID, RECORD))
write.csv(Mn_merge1, file = "Mnmerge.csv")

Sedat<- read_excel("Se30dat.xlsx")
Se <- read_excel("SeNEI.xlsx")
Se_merge <- merge(Sedat, Se) 
Se_merge <- Se_merge %>% 
  rename("RECORD" = "IN_FID")
ID <- read_excel("CEparticipants_bg_TableToExcel_1.xlsx")
Se_merge1 <- merge(Se_merge, ID) 
Se_merge1 = select(Se_merge1, -c(OBJECTID, RECORD))
write.csv(Se_merge1, file = "Semerge.csv")

Crdat<- read_excel("Cr30dat.xlsx")
Cr <- read_excel("CrNEI.xlsx")
Cr_merge <- merge(Crdat, Cr) 
Cr_merge <- Cr_merge %>% 
  rename("RECORD" = "IN_FID")
ID <- read_excel("CEparticipants_bg_TableToExcel_1.xlsx")
Cr_merge1 <- merge(Cr_merge, ID) 
Cr_merge1 = select(Cr_merge1, -c(OBJECTID, RECORD))
write.csv(Cr_merge1, file = "Crmerge.csv")

Hgdat<- read_excel("Hg30dat.xlsx")
Hg <- read_excel("HgNEI.xlsx")
Hg_merge <- merge(Hgdat, Hg) 
Hg_merge <- Hg_merge %>% 
  rename("RECORD" = "IN_FID")
ID <- read_excel("CEparticipants_bg_TableToExcel_1.xlsx")
Hg_merge1 <- merge(Hg_merge, ID) 
Hg_merge1 = select(Hg_merge1, -c(OBJECTID, RECORD))
write.csv(Hg_merge1, file = "Hgmerge.csv")

Cddat<- read_excel("Cd30dat.xlsx")
Cd <- read_excel("CdNEI.xlsx")
Cd_merge <- merge(Cddat, Cd) 
Cd_merge <- Cd_merge %>% 
  rename("RECORD" = "IN_FID")
ID <- read_excel("CEparticipants_bg_TableToExcel_1.xlsx")
Cd_merge1 <- merge(Cd_merge, ID) 
Cd_merge1 = select(Cd_merge1, -c(OBJECTID, RECORD))
write.csv(Cd_merge1, file = "Cdmerge.csv")

##Load datasets for calculations ---------------------------
setwd("/Users/joycelin/Desktop/Gulf/Aim2/GuLF_2")
library(dplyr)
Pb <- read_excel("Pbmerge.xlsx")
Pb<-Pb %>% mutate(NEAR_DIST = NEAR_DIST*100)
#Emissions weight
Pb <- Pb %>% mutate(weight = (Pb_EMISSIONS/NEAR_DIST))  
# distance weight
Pb<- Pb %>% mutate(Dweight = 1/NEAR_DIST)
#transmute
Pb_IDW<-Pb %>%
  group_by(ID) %>% 
  transmute(EMweight=sum(weight), Dweight=sum(Dweight))

Pb_IDW<- unique(Pb_IDW)

As <- read_excel("Asmerge.xlsx")
As<-As %>% mutate(NEAR_DIST = NEAR_DIST*100)
As <- As %>% mutate(weight = (USER_EMISSIONS/NEAR_DIST))  
As<- As %>% mutate(Dweight = 1/NEAR_DIST)
#transmute
As_IDW<-As %>%
  group_by(ID) %>% 
  transmute(EMweight=sum(weight), Dweight=sum(Dweight))
As_IDW<- unique(As_IDW)

Mn <- read_excel("Mnmerge.xlsx")
Mn<-Mn %>% mutate(NEAR_DIST = NEAR_DIST*100)
Mn <- Mn %>% mutate(weight = (Mn_EMISSIONS/NEAR_DIST))  
Mn<- Mn %>% mutate(Dweight = 1/NEAR_DIST)
#transmute
Mn_IDW<-Mn %>%
  group_by(ID) %>% 
  transmute(EMweight=sum(weight), Dweight=sum(Dweight))
Mn_IDW<- unique(Mn_IDW)

Hg <- read_excel("Hgmerge.xlsx")
Hg<-Hg %>% mutate(NEAR_DIST = NEAR_DIST*100)
Hg <- Hg %>% mutate(weight = (Hg_EMISSIONS/NEAR_DIST)) 
Hg<- Hg %>% mutate(Dweight = 1/NEAR_DIST)
#transmute
Hg_IDW<-Hg %>%
  group_by(ID) %>% 
  transmute(EMweight=sum(weight), Dweight=sum(Dweight))
Hg_IDW<- unique(Hg_IDW)

Cr <- read_excel("Crmerge.xlsx")
Cr<-Cr %>% mutate(NEAR_DIST = NEAR_DIST*100)
Cr <- Cr %>% mutate(weight = (Cr_EMISSIONS/NEAR_DIST))  
Cr<- Cr %>% mutate(Dweight = 1/NEAR_DIST)
#transmute
Cr_IDW<-Cr %>%
  group_by(ID) %>% 
  transmute(EMweight=sum(weight), Dweight=sum(Dweight))
Cr_IDW<- unique(Cr_IDW)

Se <- read_excel("Semerge.xlsx")
Se<-Se %>% mutate(NEAR_DIST = NEAR_DIST*100)
Se <- Se %>% mutate(weight = (Se_EMISSIONS/NEAR_DIST))  
Se<- Se %>% mutate(Dweight = 1/NEAR_DIST)
#transmute
Se_IDW<-Se %>%
  group_by(ID) %>% 
  transmute(EMweight=sum(weight), Dweight=sum(Dweight))
Se_IDW<- unique(Se_IDW)

Cd <- read_excel("Cdmerge.xlsx")
Cd<-Cd %>% mutate(NEAR_DIST = NEAR_DIST*100)
Cd <- Cd %>% mutate(weight = (Cd_EMISSIONS/NEAR_DIST))  
Cd<- Cd %>% mutate(Dweight = 1/NEAR_DIST)
#transmute
Cd_IDW<-Cd %>%
  group_by(ID) %>% 
  transmute(EMweight=sum(weight), Dweight=sum(Dweight))
Cd_IDW<- unique(Cd_IDW)

data <- read_excel("CE_NEI.xlsx")
data<- merge(data, Pb_IDW, all=T)
data <- data %>% 
  rename("Pb_EMweight" = "EMweight",
         "Pb_Dweight" = "Dweight")
data<- merge(data, As_IDW, all=T)
data <- data %>% 
  rename("As_EMweight" = "EMweight",
         "As_Dweight" = "Dweight")
data<- merge(data, Mn_IDW, all=T)
data <- data %>% 
  rename("Mn_EMweight" = "EMweight",
         "Mn_Dweight" = "Dweight")
data<- merge(data, Hg_IDW, all=T)
data <- data %>% 
  rename("Hg_EMweight" = "EMweight",
         "Hg_Dweight" = "Dweight")
data<- merge(data, Cr_IDW, all=T)
data <- data %>% 
  rename("Cr_EMweight" = "EMweight",
         "Cr_Dweight" = "Dweight")
data<- merge(data, Cd_IDW, all=T)
data <- data %>% 
  rename("Cd_EMweight" = "EMweight",
         "Cd_Dweight" = "Dweight")
data<- merge(data, Se_IDW, all=T)
data <- data %>% 
  rename("Se_EMweight" = "EMweight",
         "Se_Dweight" = "Dweight")

write.csv(data, file = "CE_NEI_IDW.csv")

####### Add TRI data --------------
setwd("/Users/joycelin/Desktop/Gulf/Aim2/GuLF_2")
library(tidyverse)
library(dplyr)
library(readxl)
data<- read_csv("CE_NEI_IDW.csv")

Pbdat<- read_csv("CE_TRI_Pb.csv")
Pb <- read_excel("TRI_Pb.xlsx")
Pb_merge <- merge(Pbdat, Pb) 
Pb_merge <- Pb_merge %>% 
  rename("RECORD" = "IN_FID")
ID <- read_excel("CEparticipants_bg_TableToExcel_1.xlsx")
Pb_merge1 <- merge(Pb_merge, ID) 
Pb_merge1 = select(Pb_merge1, -c(OBJECTID, RECORD))
write.csv(Pb_merge1, file = "PbTRImerge.csv")

Asdat<- read_csv("CE_TRI_As.csv")
As <- read_excel("TRI_As.xlsx")
As_merge <- merge(Asdat, As) 
As_merge <- As_merge %>% 
  rename("RECORD" = "IN_FID")
ID <- read_excel("CEparticipants_bg_TableToExcel_1.xlsx")
As_merge1 <- merge(As_merge, ID) 
As_merge1 = select(As_merge1, -c(OBJECTID, RECORD))
write.csv(As_merge1, file = "AsTRImerge.csv")

Crdat<- read_csv("CE_TRI_Cr.csv")
Cr <- read_excel("TRI_Cr.xlsx")
Cr_merge <- merge(Crdat, Cr) 
Cr_merge <- Cr_merge %>% 
  rename("RECORD" = "IN_FID")
ID <- read_excel("CEparticipants_bg_TableToExcel_1.xlsx")
Cr_merge1 <- merge(Cr_merge, ID) 
Cr_merge1 = select(Cr_merge1, -c(OBJECTID, RECORD))
write.csv(Cr_merge1, file = "CrTRImerge.csv")

Cudat<- read_csv("CE_TRI_Cu.csv")
Cu <- read_excel("TRI_Cu.xlsx")
Cu_merge <- merge(Cudat, Cu) 
Cu_merge <- Cu_merge %>% 
  rename("RECORD" = "IN_FID")
ID <- read_excel("CEparticipants_bg_TableToExcel_1.xlsx")
Cu_merge1 <- merge(Cu_merge, ID) 
Cu_merge1 = select(Cu_merge1, -c(OBJECTID, RECORD))
write.csv(Cu_merge1, file = "CuTRImerge.csv")

Hgdat<- read_csv("CE_TRI_Hg.csv")
Hg <- read_excel("TRI_Hg.xlsx")
Hg_merge <- merge(Hgdat, Hg) 
Hg_merge <- Hg_merge %>% 
  rename("RECORD" = "IN_FID")
ID <- read_excel("CEparticipants_bg_TableToExcel_1.xlsx")
Hg_merge1 <- merge(Hg_merge, ID) 
Hg_merge1 = select(Hg_merge1, -c(OBJECTID, RECORD))
write.csv(Hg_merge1, file = "HgTRImerge.csv")

Mndat<- read_csv("CE_TRI_Mn.csv")
Mn <- read_excel("TRI_Mn.xlsx")
Mn_merge <- merge(Mndat, Mn) 
Mn_merge <- Mn_merge %>% 
  rename("RECORD" = "IN_FID")
ID <- read_excel("CEparticipants_bg_TableToExcel_1.xlsx")
Mn_merge1 <- merge(Mn_merge, ID) 
Mn_merge1 = select(Mn_merge1, -c(OBJECTID, RECORD))
write.csv(Mn_merge1, file = "MnTRImerge.csv")

Sedat<- read_csv("CE_TRI_Se.csv")
Se <- read_excel("TRI_Se.xlsx")
Se_merge <- merge(Sedat, Se) 
Se_merge <- Se_merge %>% 
  rename("RECORD" = "IN_FID")
ID <- read_excel("CEparticipants_bg_TableToExcel_1.xlsx")
Se_merge1 <- merge(Se_merge, ID) 
Se_merge1 = select(Se_merge1, -c(OBJECTID, RECORD))
write.csv(Se_merge1, file = "SeTRImerge.csv")

Zndat<- read_csv("CE_TRI_Zn.csv")
Zn <- read_excel("TRI_Zn.xlsx")
Zn_merge <- merge(Zndat, Zn) 
Zn_merge <- Zn_merge %>% 
  rename("RECORD" = "IN_FID")
ID <- read_excel("CEparticipants_bg_TableToExcel_1.xlsx")
Zn_merge1 <- merge(Zn_merge, ID) 
Zn_merge1 = select(Zn_merge1, -c(OBJECTID, RECORD))
write.csv(Zn_merge1, file = "ZnTRImerge.csv")

##Load datasets for calculations ---------------------------
setwd("/Users/joycelin/Desktop/Gulf/Aim2/GuLF_2")
library(dplyr)
Pb <- read_csv("PbTRImerge.csv")
Pb<-Pb %>% mutate(NEAR_DIST = NEAR_DIST*100)
#Emissions weight
Pb <- Pb %>% mutate(weight = (TOTAL_RELEASES/NEAR_DIST))  
# distance weight
Pb<- Pb %>% mutate(Dweight = 1/NEAR_DIST)
#transmute
Pb_IDW<-Pb %>%
  group_by(ID) %>% 
  transmute(EMweight=sum(weight), Dweight=sum(Dweight))

Pb_IDW<- unique(Pb_IDW)

As <- read_csv("AsTRImerge.csv")
As<-As %>% mutate(NEAR_DIST = NEAR_DIST*100)
As <- As %>% mutate(weight = (TOTAL_RELEASES/NEAR_DIST))  
As<- As %>% mutate(Dweight = 1/NEAR_DIST)
#transmute
As_IDW<-As %>%
  group_by(ID) %>% 
  transmute(EMweight=sum(weight), Dweight=sum(Dweight))
As_IDW<- unique(As_IDW)

Mn <- read_csv("MnTRImerge.csv")
Mn<-Mn %>% mutate(NEAR_DIST = NEAR_DIST*100)
Mn <- Mn %>% mutate(weight = (TOTAL_RELEASES/NEAR_DIST))  
Mn<- Mn %>% mutate(Dweight = 1/NEAR_DIST)
#transmute
Mn_IDW<-Mn %>%
  group_by(ID) %>% 
  transmute(EMweight=sum(weight), Dweight=sum(Dweight))
Mn_IDW<- unique(Mn_IDW)

Hg <- read_csv("HgTRImerge.csv")
Hg<-Hg %>% mutate(NEAR_DIST = NEAR_DIST*100)
Hg <- Hg %>% mutate(weight = (TOTAL_RELEASES/NEAR_DIST)) 
Hg<- Hg %>% mutate(Dweight = 1/NEAR_DIST)
#transmute
Hg_IDW<-Hg %>%
  group_by(ID) %>% 
  transmute(EMweight=sum(weight), Dweight=sum(Dweight))
Hg_IDW<- unique(Hg_IDW)

Cr <- read_csv("CrTRImerge.csv")
Cr<-Cr %>% mutate(NEAR_DIST = NEAR_DIST*100)
Cr <- Cr %>% mutate(weight = (TOTAL_RELEASES/NEAR_DIST))  
Cr<- Cr %>% mutate(Dweight = 1/NEAR_DIST)
#transmute
Cr_IDW<-Cr %>%
  group_by(ID) %>% 
  transmute(EMweight=sum(weight), Dweight=sum(Dweight))
Cr_IDW<- unique(Cr_IDW)

Se <- read_csv("SeTRImerge.csv")
Se<-Se %>% mutate(NEAR_DIST = NEAR_DIST*100)
Se <- Se %>% mutate(weight = (TOTAL_RELEASES/NEAR_DIST))  
Se<- Se %>% mutate(Dweight = 1/NEAR_DIST)
#transmute
Se_IDW<-Se %>%
  group_by(ID) %>% 
  transmute(EMweight=sum(weight), Dweight=sum(Dweight))
Se_IDW<- unique(Se_IDW)

Cu <- read_csv("CuTRImerge.csv")
Cu<-Cu %>% mutate(NEAR_DIST = NEAR_DIST*100)
Cu <- Cu %>% mutate(weight = (TOTAL_RELEASES/NEAR_DIST))  
Cu<- Cu %>% mutate(Dweight = 1/NEAR_DIST)
#transmute
Cu_IDW<-Cu %>%
  group_by(ID) %>% 
  transmute(EMweight=sum(weight), Dweight=sum(Dweight))
Cu_IDW<- unique(Cu_IDW)

Zn <- read_csv("ZnTRImerge.csv")
Zn<-Zn %>% mutate(NEAR_DIST = NEAR_DIST*100)
Zn <- Zn %>% mutate(weight = (TOTAL_RELEASES/NEAR_DIST))  
Zn<- Zn %>% mutate(Dweight = 1/NEAR_DIST)
#transmute
Zn_IDW<-Zn %>%
  group_by(ID) %>% 
  transmute(EMweight=sum(weight), Dweight=sum(Dweight))
Zn_IDW<- unique(Zn_IDW)


data <- read_csv("CE_NEI_IDW.csv")
data<- merge(data, Pb_IDW, all=T)
data <- data %>% 
  rename("TRI_Pb_EMweight" = "EMweight",
         "TRI_Pb_Dweight" = "Dweight")
data<- merge(data, As_IDW, all=T)
data <- data %>% 
  rename("TRIAs_EMweight" = "EMweight",
         "TRIAs_Dweight" = "Dweight")
data<- merge(data, Mn_IDW, all=T)
data <- data %>% 
  rename("TRI_Mn_EMweight" = "EMweight",
         "TRI_Mn_Dweight" = "Dweight")
data<- merge(data, Hg_IDW, all=T)
data <- data %>% 
  rename("TRI_Hg_EMweight" = "EMweight",
         "TRI_Hg_Dweight" = "Dweight")
data<- merge(data, Cr_IDW, all=T)
data <- data %>% 
  rename("TRI_Cr_EMweight" = "EMweight",
         "TRI_Cr_Dweight" = "Dweight")
data<- merge(data, Cu_IDW, all=T)
data <- data %>% 
  rename("TRI_Cu_EMweight" = "EMweight",
         "TRI_Cu_Dweight" = "Dweight")
data<- merge(data, Se_IDW, all=T)
data <- data %>% 
  rename("TRI_Se_EMweight" = "EMweight",
         "TRI_Se_Dweight" = "Dweight")
data<- merge(data, Zn_IDW, all=T)
data <- data %>% 
  rename("TRI_Zn_EMweight" = "EMweight",
         "TRI_Zn_Dweight" = "Dweight")

write.csv(data, file = "CE_NEI_TRI_IDW.csv")


