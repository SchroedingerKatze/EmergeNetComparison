#1# Data Preparing
library(Rmisc)
setwd("D:/Work/Bavaria/2. BY_Comparison/2.2. BY_Data/2.2.1. BY_Basic Data")
BY_Hosp<-read.csv("BY_Hosp_R.csv", header = T)
BY_Hosp$A_DAT<-as.Date(BY_Hosp$A_DAT)
BY_Hosp$D_DAT<-as.Date(BY_Hosp$D_DAT)
BY_Hosp<-BY_Hosp[order(BY_Hosp$P_ID, BY_Hosp$A_DAT, BY_Hosp$D_DAT),]
## Weekdays for the admission and discharge dates
BY_Hosp$A_WD<-as.numeric(format(BY_Hosp$A_DAT, format = "%u"))
BY_Hosp$D_WD<-as.numeric(format(BY_Hosp$D_DAT, format = "%u"))
## Upscaling Information Here we just use one upscaling
load("BY_FS_Population_P_IDresp_unique_Incompleteness_0.RData")
load("BY_FS_Population_P_IDresp_freq_Incompleteness_0.RData")

#2# Patient transfers between same hospital and less than 1 day inbetween we set them as one records
bool<-(BY_Hosp$P_ID[1:(dim(BY_Hosp)[1]-1)]==BY_Hosp$P_ID[2:dim(BY_Hosp)[1]])&((BY_Hosp$D_DAT[1:(dim(BY_Hosp)[1]-1)]+1)==BY_Hosp$A_DAT[2:dim(BY_Hosp)[1]])&(BY_Hosp$H_ID[1:(dim(BY_Hosp)[1]-1)]==BY_Hosp$H_ID[2:dim(BY_Hosp)[1]])
## Separate BY_Hosp to two parts: 1.Without the one day transfers between same hospital; 2. Only the one day transfers between same hospital.
BY_Hosp1<-BY_Hosp[!(c(F,bool)|c(bool,F)),]
BY_Hosp2<-BY_Hosp[c(F,bool)|c(bool,F),]
## Select the records in BY_Hosp2, which can be combined together
#### 0 means the start of the combined data
BY_Hosp2$bool<-as.numeric(c(F,(BY_Hosp2$P_ID[1:(dim(BY_Hosp2)[1]-1)]==BY_Hosp2$P_ID[2:dim(BY_Hosp2)[1]])&((BY_Hosp2$D_DAT[1:(dim(BY_Hosp2)[1]-1)]+1)==BY_Hosp2$A_DAT[2:dim(BY_Hosp2)[1]])&(BY_Hosp2$H_ID[1:(dim(BY_Hosp2)[1]-1)]==BY_Hosp2$H_ID[2:dim(BY_Hosp2)[1]])))
#### Delete 1 between two 1s, whichs mean the records can be deleted
bool_d<-(BY_Hosp2$bool==1)&(c(BY_Hosp2$bool[1:(dim(BY_Hosp2)[1]-1)]==BY_Hosp2$bool[2:dim(BY_Hosp2)[1]],F))
BY_Hosp2<-BY_Hosp2[!bool_d,]
#### Make new data with combined records of BY_Hosp2
BY_Hosp2<-data.frame(BY_Hosp2[!as.logical(BY_Hosp2$bool), c(1,2,4:8)], BY_Hosp2[as.logical(BY_Hosp2$bool), c(3,9)])
BY_Hosp2<-BY_Hosp2[,c(1,2,8,3:7,9)]
## Combine the rest data and cleaned data together
BY_Hosp<-rbind(BY_Hosp1, BY_Hosp2)
BY_Hosp<-BY_Hosp[order(BY_Hosp$P_ID, BY_Hosp$A_DAT, BY_Hosp$D_DAT),]

#2# For each individual in AOK data, a resampling time is given. Here we will do 100 times of upscaling.
## Number of people (100 upscaling)
BY_Num_P<-NULL
## Number of occupied beds per hospital per day (100 upscaling)
BY_Num_B<-NULL
## Average LOS (100 upscaling)
BY_LOS<-NULL
## Average time between two hospitalizations
BY_DayBet<-NULL
## Average number of hospitalizations per patient
BY_Num_Hosp<-NULL

for (i in 1:100)
{
  BY_Hosp_ith_seed<-BY_Hosp
  BY_Hosp_ith_seed$Resp<-factor(BY_Hosp_ith_seed$P_ID, levels = BY_FS_P_IDresp_unique[[i]], labels = BY_FS_P_IDresp_freq[[i]])
  BY_Hosp_ith_seed$LOS<-as.numeric(BY_Hosp_ith_seed$D_DAT-BY_Hosp_ith_seed$A_DAT+1)
  Trans_bool<-(BY_Hosp_ith_seed$P_ID[1:(dim(BY_Hosp_ith_seed)[1]-1)] == BY_Hosp_ith_seed$P_ID[2:dim(BY_Hosp_ith_seed)[1]])
  BY_Trans_ith_seed<-data.frame(BY_Hosp_ith_seed[c(Trans_bool,F),],BY_Hosp_ith_seed[c(F,Trans_bool),])
  BY_Trans_ith_seed$DayBet<-BY_Trans_ith_seed$A_DAT.1-BY_Trans_ith_seed$D_DAT
  BY_ID_ith_seed<-unique(data.frame(P_ID = as.numeric(BY_Hosp_ith_seed$P_ID), Resp = as.numeric(BY_Hosp_ith_seed$Resp)))
  
  ## Number of patients
  BY_Num_P<-c(BY_Num_P,sum(BY_ID_ith_seed$Resp))
  ## Number of occupied beds per hospital per day
  BY_Num_B<-c(BY_Num_B,sum(as.numeric(BY_Hosp_ith_seed$Resp)*as.numeric(BY_Hosp_ith_seed$LOS))/length(unique(BY_Hosp_ith_seed$H_ID))/(as.numeric(max(BY_Hosp_ith_seed$D_DAT)-min(BY_Hosp_ith_seed$A_DAT))+1))
  ## Average LOS
  BY_LOS<-c(BY_LOS,sum(as.numeric(BY_Hosp_ith_seed$LOS)*as.numeric(BY_Hosp_ith_seed$Resp))/sum(as.numeric(BY_Hosp_ith_seed$Resp)))
  ## Average time between two hospitalizations
  BY_DayBet<-c(BY_DayBet,sum(as.numeric(BY_Trans_ith_seed$A_DAT.1-BY_Trans_ith_seed$D_DAT)*as.numeric(BY_Trans_ith_seed$Resp))/sum(as.numeric(BY_Trans_ith_seed$Resp)))
  ## Average number of hospitalizations per patient
  BY_Num_Hosp<-c(BY_Num_Hosp,sum(as.numeric(BY_Hosp_ith_seed$Resp))/sum(as.numeric(BY_ID_ith_seed$Resp)))
}

#3# Calculate the Confidence intervals for 100 upscaling
## Number of people (100 upscaling) CI
CI(BY_Num_P, 0.95)
## Number of occupied beds per hospital per day (100 upscaling) CI
CI(BY_Num_B, 0.95)
## Average LOS (100 upscaling) CI
CI(BY_LOS, 0.95)
## Average time between two hospitalizations CI
CI(BY_DayBet, 0.95)
## Average number of hospitalizations per patient CI
CI(BY_Num_Hosp, 0.95)