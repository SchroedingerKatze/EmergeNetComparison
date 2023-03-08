#1# Read data
## BY
BY_Hosp<-read.csv("E:/Work/Bavaria/2. BY_Comparison/2.2. BY_Data/2.2.1. BY_Basic Data/BY_Hosp_C.csv", header=T)
BY_Hosp$A_DAT<-as.Date(as.character(BY_Hosp$A_DAT))
BY_Hosp$D_DAT<-as.Date(as.character(BY_Hosp$D_DAT))
BY_Hosp<-BY_Hosp[order(BY_Hosp$P_ID, BY_Hosp$A_DAT, BY_Hosp$D_DAT),]
## LS
LS_Hosp<-read.csv("E:/Work/Lower Saxony/2. LS_Comparison/2.2. LS_Data/2.2.1. LS_Basic Data/LS_Hosp_C.csv", header=T)
LS_Hosp$A_DAT<-as.Date(as.character(LS_Hosp$A_DAT))
LS_Hosp$D_DAT<-as.Date(as.character(LS_Hosp$D_DAT))
LS_Hosp<-LS_Hosp[order(LS_Hosp$P_ID, LS_Hosp$A_DAT, LS_Hosp$D_DAT),]
## ST
ST_Hosp<-read.csv("E:/Work/Saxony and Thuringia/2. ST_Comparison/2.2. ST_Data/2.2.1. ST_Basic Data/ST_Hosp_C.csv", header=T)
ST_Hosp$A_DAT<-as.Date(as.character(ST_Hosp$A_DAT))
ST_Hosp$D_DAT<-as.Date(as.character(ST_Hosp$D_DAT))
ST_Hosp<-ST_Hosp[order(ST_Hosp$P_ID, ST_Hosp$A_DAT, ST_Hosp$D_DAT),]

#2# Data information
## Time between two successive hospital stays
#### BY
BY_bool<-BY_Hosp$P_ID[1:(dim(BY_Hosp)[1]-1)]==BY_Hosp$P_ID[2:dim(BY_Hosp)[1]]
BY_Trans<-data.frame(BY_Hosp$D_DAT[c(BY_bool,F)], BY_Hosp$A_DAT[c(F,BY_bool)])
BY_LosBetTwoStays<-as.numeric(BY_Trans[,2]-BY_Trans[,1])
###### Mean
mean(BY_LosBetTwoStays)
###### IQR
IQR(BY_LosBetTwoStays)
#### LS
LS_bool<-LS_Hosp$P_ID[1:(dim(LS_Hosp)[1]-1)]==LS_Hosp$P_ID[2:dim(LS_Hosp)[1]]
LS_Trans<-data.frame(LS_Hosp$D_DAT[c(LS_bool,F)], LS_Hosp$A_DAT[c(F,LS_bool)])
LS_LosBetTwoStays<-as.numeric(LS_Trans[,2]-LS_Trans[,1])
###### Mean
mean(LS_LosBetTwoStays)
###### IQR
IQR(LS_LosBetTwoStays)
#### ST
ST_bool<-ST_Hosp$P_ID[1:(dim(ST_Hosp)[1]-1)]==ST_Hosp$P_ID[2:dim(ST_Hosp)[1]]
ST_Trans<-data.frame(ST_Hosp$D_DAT[c(ST_bool,F)], ST_Hosp$A_DAT[c(F,ST_bool)])
ST_LosBetTwoStays<-as.numeric(ST_Trans[,2]-ST_Trans[,1])
###### Mean
mean(ST_LosBetTwoStays)
###### IQR
IQR(ST_LosBetTwoStays)

## Number of hospitalizations per patient
#### BY
BY_Hosp_Freq<-as.numeric(table(BY_Hosp$P_ID))
###### Mean
mean(BY_Hosp_Freq)
IQR(BY_Hosp_Freq)
#### LS
LS_Hosp_Freq<-as.numeric(table(LS_Hosp$P_ID))
mean(LS_Hosp_Freq)
IQR(LS_Hosp_Freq)
#### ST
ST_Hosp_Freq<-as.numeric(table(ST_Hosp$P_ID))
mean(ST_Hosp_Freq)
IQR(ST_Hosp_Freq)

## BY Number of People
BY_P_Resp<-unique(data.frame(P_ID = BY_Hosp$P_ID, Resp = BY_Hosp$Resp))
sum(as.numeric(BY_P_Resp$Resp))
## Time span
min(BY_Hosp$A_DAT)
max(BY_Hosp$D_DAT)
## Nr hospitals
max(BY_Hosp$H_ID)

## LS Number of people
LS_P_Resp<-unique(data.frame(P_ID = LS_Hosp$P_ID, Resp = LS_Hosp$Resp))
sum(as.numeric(LS_P_Resp$Resp))
## Time span
min(LS_Hosp$A_DAT)
max(LS_Hosp$D_DAT)
## Nr hospitals
max(LS_Hosp$H_ID)
## ST Number of People
ST_P_Resp<-unique(data.frame(P_ID = ST_Hosp$P_ID, Resp = ST_Hosp$Resp))
sum(as.numeric(ST_P_Resp$Resp))
## Time span
min(ST_Hosp$A_DAT)
max(ST_Hosp$D_DAT)
## Nr hospitals
max(ST_Hosp$H_ID)