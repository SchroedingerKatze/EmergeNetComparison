#1# Data Preparing
setwd("E:/Work/Saxony and Thuringia/2. ST_Comparison/2.2. ST_Data/2.2.1. ST_Basic Data")
ST_Hosp<-read.csv("ST_Hosp_R.csv", header = T)
ST_Hosp$A_DAT<-as.Date(ST_Hosp$A_DAT)
ST_Hosp$D_DAT<-as.Date(ST_Hosp$D_DAT)
ST_Hosp<-ST_Hosp[order(ST_Hosp$P_ID, ST_Hosp$A_DAT, ST_Hosp$D_DAT),]
## Weekdays for the admission and discharge dates
ST_Hosp$A_WD<-as.numeric(format(ST_Hosp$A_DAT, format = "%u"))
ST_Hosp$D_WD<-as.numeric(format(ST_Hosp$D_DAT, format = "%u"))
## Upscaling Information Here we just use one upscaling
load("ST_FS_Population_P_IDresp_unique_Incompleteness_0.RData")
load("ST_FS_Population_P_IDresp_freq_Incompleteness_0.RData")

#2# Patient transfers between same hospital and less than 1 day inbetween we set them as one records
bool<-(ST_Hosp$P_ID[1:(dim(ST_Hosp)[1]-1)]==ST_Hosp$P_ID[2:dim(ST_Hosp)[1]])&((ST_Hosp$D_DAT[1:(dim(ST_Hosp)[1]-1)]+1)==ST_Hosp$A_DAT[2:dim(ST_Hosp)[1]])&(ST_Hosp$H_ID[1:(dim(ST_Hosp)[1]-1)]==ST_Hosp$H_ID[2:dim(ST_Hosp)[1]])
## Separate ST_Hosp to two parts: 1.Without the one day transfers between same hospital; 2. Only the one day transfers between same hospital.
ST_Hosp1<-ST_Hosp[!(c(F,bool)|c(bool,F)),]
ST_Hosp2<-ST_Hosp[c(F,bool)|c(bool,F),]
## Select the records in ST_Hosp2, which can be combined together
#### 0 means the start of the combined data
ST_Hosp2$bool<-as.numeric(c(F,(ST_Hosp2$P_ID[1:(dim(ST_Hosp2)[1]-1)]==ST_Hosp2$P_ID[2:dim(ST_Hosp2)[1]])&((ST_Hosp2$D_DAT[1:(dim(ST_Hosp2)[1]-1)]+1)==ST_Hosp2$A_DAT[2:dim(ST_Hosp2)[1]])&(ST_Hosp2$H_ID[1:(dim(ST_Hosp2)[1]-1)]==ST_Hosp2$H_ID[2:dim(ST_Hosp2)[1]])))
#### Delete 1 between two 1s, whichs mean the records can be deleted
bool_d<-(ST_Hosp2$bool==1)&(c(ST_Hosp2$bool[1:(dim(ST_Hosp2)[1]-1)]==ST_Hosp2$bool[2:dim(ST_Hosp2)[1]],F))
ST_Hosp2<-ST_Hosp2[!bool_d,]
#### Make new data with combined records of ST_Hosp2
ST_Hosp2<-data.frame(ST_Hosp2[!as.logical(ST_Hosp2$bool), c(1,2,4:8)], ST_Hosp2[as.logical(ST_Hosp2$bool), c(3,9)])
ST_Hosp2<-ST_Hosp2[,c(1,2,8,3:7,9)]
## Combine the rest data and cleaned data together
ST_Hosp<-rbind(ST_Hosp1, ST_Hosp2)
ST_Hosp<-ST_Hosp[order(ST_Hosp$P_ID, ST_Hosp$A_DAT, ST_Hosp$D_DAT),]

#2# For each individual in AOK data, a resampling time is given.
ST_Hosp$Resp<-factor(ST_Hosp$P_ID, levels = ST_FS_P_IDresp_unique[[1]], labels = ST_FS_P_IDresp_freq[[1]])

#5# Save created Hosp dataset
setwd("E:/Work/Saxony and Thuringia/2. ST_Comparison/2.2. ST_Data/2.2.1. ST_Basic Data")
write.csv(ST_Hosp, file = "ST_Hosp_C.csv", row.names = F)