#1# Data Preparing
library(Rmisc)
setwd("D:/Work/Saxony and Thuringia/2. ST_Comparison/2.2. ST_Data/2.2.1. ST_Basic Data")
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

#2# For each individual in AOK data, a resampling time is given. Here we will do 100 times of upscaling.
## Number of people (100 upscaling)
ST_Num_P<-NULL
## Number of occupied beds per hospital per day (100 upscaling)
ST_Num_B<-NULL
## Average LOS (100 upscaling)
ST_LOS<-NULL
## Average time between two hospitalizations
ST_DayBet<-NULL
## Average number of hospitalizations per patient
ST_Num_Hosp<-NULL

for (i in 1:100)
{
  ST_Hosp_ith_seed<-ST_Hosp
  ST_Hosp_ith_seed$Resp<-factor(ST_Hosp_ith_seed$P_ID, levels = ST_FS_P_IDresp_unique[[i]], labels = ST_FS_P_IDresp_freq[[i]])
  ST_Hosp_ith_seed$LOS<-as.numeric(ST_Hosp_ith_seed$D_DAT-ST_Hosp_ith_seed$A_DAT+1)
  Trans_bool<-(ST_Hosp_ith_seed$P_ID[1:(dim(ST_Hosp_ith_seed)[1]-1)] == ST_Hosp_ith_seed$P_ID[2:dim(ST_Hosp_ith_seed)[1]])
  ST_Trans_ith_seed<-data.frame(ST_Hosp_ith_seed[c(Trans_bool,F),],ST_Hosp_ith_seed[c(F,Trans_bool),])
  ST_Trans_ith_seed$DayBet<-ST_Trans_ith_seed$A_DAT.1-ST_Trans_ith_seed$D_DAT
  ST_ID_ith_seed<-unique(data.frame(P_ID = as.numeric(ST_Hosp_ith_seed$P_ID), Resp = as.numeric(ST_Hosp_ith_seed$Resp)))
  
  ## Number of patients
  ST_Num_P<-c(ST_Num_P,sum(ST_ID_ith_seed$Resp))
  ## Number of occupied beds per hospital per day
  ST_Num_B<-c(ST_Num_B,sum(as.numeric(ST_Hosp_ith_seed$Resp)*as.numeric(ST_Hosp_ith_seed$LOS))/length(unique(ST_Hosp_ith_seed$H_ID))/(as.numeric(max(ST_Hosp_ith_seed$D_DAT)-min(ST_Hosp_ith_seed$A_DAT))+1))
  ## Average LOS
  ST_LOS<-c(ST_LOS,sum(as.numeric(ST_Hosp_ith_seed$LOS)*as.numeric(ST_Hosp_ith_seed$Resp))/sum(as.numeric(ST_Hosp_ith_seed$Resp)))
  ## Average time between two hospitalizations
  ST_DayBet<-c(ST_DayBet,sum(as.numeric(ST_Trans_ith_seed$A_DAT.1-ST_Trans_ith_seed$D_DAT)*as.numeric(ST_Trans_ith_seed$Resp))/sum(as.numeric(ST_Trans_ith_seed$Resp)))
  ## Average number of hospitalizations per patient
  ST_Num_Hosp<-c(ST_Num_Hosp,sum(as.numeric(ST_Hosp_ith_seed$Resp))/sum(as.numeric(ST_ID_ith_seed$Resp)))
}

#3# Calculate the Confidence intervals for 100 upscaling
## Number of people (100 upscaling) CI
CI(ST_Num_P, 0.95)
## Number of occupied beds per hospital per day (100 upscaling) CI
CI(ST_Num_B, 0.95)
## Average LOS (100 upscaling) CI
CI(ST_LOS, 0.95)
## Average time between two hospitalizations CI
CI(ST_DayBet, 0.95)
## Average number of hospitalizations per patient CI
CI(ST_Num_Hosp, 0.95)