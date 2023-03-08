#1# Read data
setwd("E:/Work/Lower Saxony/2. LS_Comparison/2.2. LS_Data/2.2.1. LS_Basic Data")
LS_Hosp<-read.csv("LS_Hosp_C.csv", header = T)
LS_Hosp$A_DAT<-as.Date(as.character(LS_Hosp$A_DAT))
LS_Hosp$D_DAT<-as.Date(as.character(LS_Hosp$D_DAT))
LS_Hosp<-LS_Hosp[order(LS_Hosp$P_ID, LS_Hosp$A_DAT, LS_Hosp$D_DAT),]

#2# Generate the data of patient stays (1:n_H are hospitals and (n_H+1):(2*n_H) are communities)
## Number of hospitals
n_H<-length(unique(LS_Hosp$H_ID))
## Creat day format of the date
LS_Hosp$A_Day<-as.numeric(LS_Hosp$A_DAT-min(c(LS_Hosp$A_DAT, LS_Hosp$D_DAT)))+1
LS_Hosp$D_Day<-as.numeric(LS_Hosp$D_DAT-min(c(LS_Hosp$A_DAT, LS_Hosp$D_DAT)))+1
## Minimum of the Day and Maximum of the Day
Day_min<-min(c(LS_Hosp$A_Day, LS_Hosp$D_Day))
Day_max<-max(c(LS_Hosp$A_Day, LS_Hosp$D_Day))
## Select the useful variables
LS_Hosp<-LS_Hosp[,c(1,5,11,12,8:10)]

#3# Datasets for generating the patient locations with different type of hospital stays
## Check the transfers between hospitals of each patient
bool<-LS_Hosp$P_ID[1:(dim(LS_Hosp)[1]-1)]==LS_Hosp$P_ID[2:dim(LS_Hosp)[1]]
LS_Hosp$bool<-as.numeric(c(F,bool))
## Dataframe selcting the patients which have only one hospitalization records, for them we need to generate the start and end location
bool1<-!(c(bool,F)|c(F,bool))
LS_Transfer_1<-LS_Hosp[bool1,]
## Transfers of patients between two hospital stays: from hospital to community and to hospital
LS_Transfer_2<-data.frame(LS_Hosp[c(bool,F),], LS_Hosp[c(F,bool),])
LS_Transfer_2<-LS_Transfer_2[,-c(9,15,16)]
## Transfers of pateints with multiple hospitalization records: the start and end locations of all hospital stays
bool2<-((!c(F,bool))|(!c(bool,F)))&(!(LS_Hosp$P_ID %in% LS_Transfer_1$P_ID)) #Except the patients with only one hospitalization record.#
LS_Transfer_3<-LS_Hosp[bool2,] # The column named "bool" indicates the location we newly generate is start or end: 0 is start, 1 is end.#
## Final community locations
LS_C_ID<-data.frame(P_ID = LS_Hosp$P_ID[LS_Hosp$bool==0], C_ID = LS_Hosp$H_ID[LS_Hosp$bool==0]+n_H)

#4# Establish the patient locations (In the following locations, if the value is NA, then means no corresponding locations)
## LS_Transfer_1
#### Calculate the number of days between the admission date and the first date
LS_Transfer_1$D_bet.f<-LS_Transfer_1$A_Day-Day_min
#### Calculate the number of days between the discharge date and the last date
LS_Transfer_1$D_bet.l<-Day_max-LS_Transfer_1$D_Day
#### Start location
###### Start from community
LS_Transfer_1$s[LS_Transfer_1$D_bet.f!=0]<-LS_Transfer_1$H_ID[LS_Transfer_1$D_bet.f!=0]+n_H
#### End location
###### End in community
LS_Transfer_1$e[LS_Transfer_1$D_bet.l!=0]<-LS_Transfer_1$H_ID[LS_Transfer_1$D_bet.l!=0]+n_H
## LS_Transfer_2
#### Caculate the day between two hospital stays
LS_Transfer_2$Day_bet<-LS_Transfer_2$A_Day.1-LS_Transfer_2$D_Day-1
#### The community stay between each hospital transfers
###### If there is a community stay between two hospital stays, we assign community ID.
LS_Transfer_2$C_ID[LS_Transfer_2$Day_bet!=0]<-LS_Transfer_2$H_ID.1[LS_Transfer_2$Day_bet!=0]+n_H
## LS_Transfer_3 (0: Start location, 1: End location)
#### Calculate the number of days between the first admission date and the first date
LS_Transfer_3$D_bet[LS_Transfer_3$bool==0]<-LS_Transfer_3$A_Day[LS_Transfer_3$bool==0]-Day_min
#### Calculate the number of days between the last discharge date and the last date
LS_Transfer_3$D_bet[LS_Transfer_3$bool==1]<-Day_max-LS_Transfer_3$D_Day[LS_Transfer_3$bool==1]
#### Locations
###### Start location
LS_Transfer_3$C_ID[(LS_Transfer_3$bool==0)&(LS_Transfer_3$D_bet!=0)]<-LS_Transfer_3$H_ID[(LS_Transfer_3$bool==0)&(LS_Transfer_3$D_bet!=0)]+n_H
###### End location
LS_Transfer_3$C_ID[(LS_Transfer_3$bool==1)&(LS_Transfer_3$D_bet!=0)]<-LS_Transfer_3$H_ID[(LS_Transfer_3$bool==1)&(LS_Transfer_3$D_bet!=0)]+n_H

#5# Create the patient location data outside the original dataset
## LS_Transfer_1
LS_Location_1<-rbind(data.frame(P_ID=LS_Transfer_1$P_ID, A_Day=Day_min, D_Day=LS_Transfer_1$A_Day-1, L_ID=LS_Transfer_1$s), data.frame(P_ID=LS_Transfer_1$P_ID, A_Day=LS_Transfer_1$D_Day+1, D_Day=Day_max, L_ID=LS_Transfer_1$e))
LS_Location_1<-LS_Location_1[!is.na(LS_Location_1$L_ID),]
## LS_Transfer_2
LS_Location_2<-data.frame(P_ID=LS_Transfer_2$P_ID, A_Day=LS_Transfer_2$D_Day+1, D_Day=LS_Transfer_2$A_Day.1-1, L_ID=LS_Transfer_2$C_ID)
LS_Location_2<-LS_Location_2[!is.na(LS_Location_2$L_ID),]
## LS_Transfer_3
LS_Location_3<-rbind(data.frame(P_ID=LS_Transfer_3$P_ID[LS_Transfer_3$bool==0], A_Day=1, D_Day=LS_Transfer_3$A_Day[LS_Transfer_3$bool==0]-1, L_ID=LS_Transfer_3$C_ID[LS_Transfer_3$bool==0]), data.frame(P_ID=LS_Transfer_3$P_ID[LS_Transfer_3$bool==1], A_Day=LS_Transfer_3$D_Day[LS_Transfer_3$bool==1]+1, D_Day= Day_max, L_ID=LS_Transfer_3$C_ID[LS_Transfer_3$bool==1]))
LS_Location_3<-LS_Location_3[!is.na(LS_Location_3$L_ID),]
## Combine them with the original data
LS_Location<-rbind(data.frame(P_ID=LS_Hosp$P_ID, A_Day=LS_Hosp$A_Day, D_Day=LS_Hosp$D_Day, L_ID=LS_Hosp$H_ID), LS_Location_1, LS_Location_2, LS_Location_3)
## Order the LS_Location
LS_Location<-LS_Location[order(LS_Location$P_ID, LS_Location$A_Day, LS_Location$D_Day),]

#6# Resampling time
## Read up-scaling data construction data lists
load("LS_FS_Population_P_IDresp_unique_Incompleteness_0.RData")
LS_FS_P_IDresp_unique<-LS_FS_P_IDresp_unique[[1]]
load("LS_FS_Population_P_IDresp_freq_Incompleteness_0.RData")
LS_FS_P_IDresp_freq<-LS_FS_P_IDresp_freq[[1]]
## Assign the value to the LS_Location
LS_Location$Resp<-as.numeric(as.character(factor(LS_Location$P_ID, levels = LS_FS_P_IDresp_unique, labels = LS_FS_P_IDresp_freq)))

#7# Assign the week days to the location dataframe
## The week day of the first day of the dataset
WD.0<-unique(LS_Hosp$A_WD[LS_Hosp$A_Day==1])
## Week day function
Fun_WD<-function(day) (day-1+WD.0)%%7  
## Compute the week day!
LS_Location$A_WD<-Fun_WD(LS_Location$A_Day)
LS_Location$D_WD<-Fun_WD(LS_Location$D_Day)

#8# Number of patients in different locations each day
## Daily stay matrix (rows: hospitals and communities, columns: days)
LS_Dstay<-matrix(0, nrow = max(LS_Location$L_ID), ncol = max(c(LS_Location$A_Day, LS_Location$D_Day)))
## Assign patients stays to this matrix
for (i in 1:dim(LS_Location)[1]) {
  LS_Dstay[LS_Location$L_ID[i], LS_Location$A_Day[i]:LS_Location$D_Day[i]]<-LS_Dstay[LS_Location$L_ID[i], LS_Location$A_Day[i]:LS_Location$D_Day[i]]+LS_Location$Resp[i]
}

#9# Save dataframe
setwd("E:/Work/Lower Saxony/2. LS_Comparison/2.2. LS_Data/2.2.3. LS_Location Data")
## Daily Locations
write.csv(LS_Location, file = "LS_Location.csv", row.names = F)
## Unchosable community end locations
write.csv(LS_C_ID, file = "LS_C_ID.csv", row.names = F)
## Daily number of patients in the hospitals and communities
write.table(LS_Dstay, file = "LS_Daily_N_Stay.txt")
