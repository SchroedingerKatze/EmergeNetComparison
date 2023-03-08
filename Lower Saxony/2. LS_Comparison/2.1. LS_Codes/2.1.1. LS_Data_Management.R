#1# Data Preparing
setwd("E:/Work/Lower Saxony/2. LS_Comparison/2.2. LS_Data/2.2.1. LS_Basic Data")
LS_Hosp<-read.csv("LS_Hosp_R.csv", header = T)
LS_Hosp$A_DAT<-as.Date(LS_Hosp$A_DAT)
LS_Hosp$D_DAT<-as.Date(LS_Hosp$D_DAT)
LS_Hosp<-LS_Hosp[order(LS_Hosp$P_ID, LS_Hosp$A_DAT, LS_Hosp$D_DAT),]
## Weekdays for the admission and discharge dates
LS_Hosp$A_WD<-as.numeric(format(LS_Hosp$A_DAT, format = "%u"))
LS_Hosp$D_WD<-as.numeric(format(LS_Hosp$D_DAT, format = "%u"))
## Upscaling Information Here we just use one upscaling
load("LS_FS_Population_P_IDresp_unique_Incompleteness_0.RData")
load("LS_FS_Population_P_IDresp_freq_Incompleteness_0.RData")

#2# Patient transfers between same hospital and less than 1 day inbetween we set them as one records
bool<-(LS_Hosp$P_ID[1:(dim(LS_Hosp)[1]-1)]==LS_Hosp$P_ID[2:dim(LS_Hosp)[1]])&((LS_Hosp$D_DAT[1:(dim(LS_Hosp)[1]-1)]+1)==LS_Hosp$A_DAT[2:dim(LS_Hosp)[1]])&(LS_Hosp$H_ID[1:(dim(LS_Hosp)[1]-1)]==LS_Hosp$H_ID[2:dim(LS_Hosp)[1]])
## Separate LS_Hosp to two parts: 1.Without the one day transfers between same hospital; 2. Only the one day transfers between same hospital.
LS_Hosp1<-LS_Hosp[!(c(F,bool)|c(bool,F)),]
LS_Hosp2<-LS_Hosp[c(F,bool)|c(bool,F),]
## Select the records in LS_Hosp2, which can be combined together
#### 0 means the start of the combined data
LS_Hosp2$bool<-as.numeric(c(F,(LS_Hosp2$P_ID[1:(dim(LS_Hosp2)[1]-1)]==LS_Hosp2$P_ID[2:dim(LS_Hosp2)[1]])&((LS_Hosp2$D_DAT[1:(dim(LS_Hosp2)[1]-1)]+1)==LS_Hosp2$A_DAT[2:dim(LS_Hosp2)[1]])&(LS_Hosp2$H_ID[1:(dim(LS_Hosp2)[1]-1)]==LS_Hosp2$H_ID[2:dim(LS_Hosp2)[1]])))
#### Delete 1 between two 1s, whichs mean the records can be deleted
bool_d<-(LS_Hosp2$bool==1)&(c(LS_Hosp2$bool[1:(dim(LS_Hosp2)[1]-1)]==LS_Hosp2$bool[2:dim(LS_Hosp2)[1]],F))
LS_Hosp2<-LS_Hosp2[!bool_d,]
#### Make new data with combined records of LS_Hosp2
LS_Hosp2<-data.frame(LS_Hosp2[!as.logical(LS_Hosp2$bool), c(1,2,4:8)], LS_Hosp2[as.logical(LS_Hosp2$bool), c(3,9)])
LS_Hosp2<-LS_Hosp2[,c(1,2,8,3:7,9)]
## Combine the rest data and cleaned data together
LS_Hosp<-rbind(LS_Hosp1, LS_Hosp2)
LS_Hosp<-LS_Hosp[order(LS_Hosp$P_ID, LS_Hosp$A_DAT, LS_Hosp$D_DAT),]

#2# For each individual in AOK data, a resampling time is given.
LS_Hosp$Resp<-factor(LS_Hosp$P_ID, levels = LS_FS_P_IDresp_unique[[1]], labels = LS_FS_P_IDresp_freq[[1]])

#5# Save created Hosp dataset
setwd("E:/Work/Lower Saxony/2. LS_Comparison/2.2. LS_Data/2.2.1. LS_Basic Data")
write.csv(LS_Hosp, file = "LS_Hosp_C.csv", row.names = F)