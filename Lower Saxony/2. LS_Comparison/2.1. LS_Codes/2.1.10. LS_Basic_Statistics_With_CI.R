#1# Data Preparing
library(Rmisc)
setwd("D:/Work/Lower Saxony/2. LS_Comparison/2.2. LS_Data/2.2.1. LS_Basic Data")
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

#2# For each individual in AOK data, a resampling time is given. Here we will do 100 times of upscaling.
## Number of people (100 upscaling)
LS_Num_P<-NULL
## Number of occupied beds per hospital per day (100 upscaling)
LS_Num_B<-NULL
## Average LOS (100 upscaling)
LS_LOS<-NULL
## Average time between two hospitalizations
LS_DayBet<-NULL
## Average number of hospitalizations per patient
LS_Num_Hosp<-NULL

for (i in 1:100)
{
  LS_Hosp_ith_seed<-LS_Hosp
  LS_Hosp_ith_seed$Resp<-factor(LS_Hosp_ith_seed$P_ID, levels = LS_FS_P_IDresp_unique[[i]], labels = LS_FS_P_IDresp_freq[[i]])
  LS_Hosp_ith_seed$LOS<-as.numeric(LS_Hosp_ith_seed$D_DAT-LS_Hosp_ith_seed$A_DAT+1)
  Trans_bool<-(LS_Hosp_ith_seed$P_ID[1:(dim(LS_Hosp_ith_seed)[1]-1)] == LS_Hosp_ith_seed$P_ID[2:dim(LS_Hosp_ith_seed)[1]])
  LS_Trans_ith_seed<-data.frame(LS_Hosp_ith_seed[c(Trans_bool,F),],LS_Hosp_ith_seed[c(F,Trans_bool),])
  LS_Trans_ith_seed$DayBet<-LS_Trans_ith_seed$A_DAT.1-LS_Trans_ith_seed$D_DAT
  LS_ID_ith_seed<-unique(data.frame(P_ID = as.numeric(LS_Hosp_ith_seed$P_ID), Resp = as.numeric(LS_Hosp_ith_seed$Resp)))
  
  ## Number of patients
  LS_Num_P<-c(LS_Num_P,sum(LS_ID_ith_seed$Resp))
  ## Number of occupied beds per hospital per day
  LS_Num_B<-c(LS_Num_B,sum(as.numeric(LS_Hosp_ith_seed$Resp)*as.numeric(LS_Hosp_ith_seed$LOS))/length(unique(LS_Hosp_ith_seed$H_ID))/(as.numeric(max(LS_Hosp_ith_seed$D_DAT)-min(LS_Hosp_ith_seed$A_DAT))+1))
  ## Average LOS
  LS_LOS<-c(LS_LOS,sum(as.numeric(LS_Hosp_ith_seed$LOS)*as.numeric(LS_Hosp_ith_seed$Resp))/sum(as.numeric(LS_Hosp_ith_seed$Resp)))
  ## Average time between two hospitalizations
  LS_DayBet<-c(LS_DayBet,sum(as.numeric(LS_Trans_ith_seed$A_DAT.1-LS_Trans_ith_seed$D_DAT)*as.numeric(LS_Trans_ith_seed$Resp))/sum(as.numeric(LS_Trans_ith_seed$Resp)))
  ## Average number of hospitalizations per patient
  LS_Num_Hosp<-c(LS_Num_Hosp,sum(as.numeric(LS_Hosp_ith_seed$Resp))/sum(as.numeric(LS_ID_ith_seed$Resp)))
}

#3# Calculate the Confidence intervals for 100 upscaling
## Number of people (100 upscaling) CI
CI(LS_Num_P, 0.95)
## Number of occupied beds per hospital per day (100 upscaling) CI
CI(LS_Num_B, 0.95)
## Average LOS (100 upscaling) CI
CI(LS_LOS, 0.95)
## Average time between two hospitalizations CI
CI(LS_DayBet, 0.95)
## Average number of hospitalizations per patient CI
CI(LS_Num_Hosp, 0.95)