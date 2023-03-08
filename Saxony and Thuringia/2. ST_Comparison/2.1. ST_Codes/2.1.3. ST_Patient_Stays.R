#1# Read data
setwd("E:/Work/Saxony and Thuringia/2. ST_Comparison/2.2. ST_Data/2.2.1. ST_Basic Data")
ST_Hosp<-read.csv("ST_Hosp_C.csv", header = T)
ST_Hosp$A_DAT<-as.Date(as.character(ST_Hosp$A_DAT))
ST_Hosp$D_DAT<-as.Date(as.character(ST_Hosp$D_DAT))
ST_Hosp<-ST_Hosp[order(ST_Hosp$P_ID, ST_Hosp$A_DAT, ST_Hosp$D_DAT),]

#2# Generate the data of patient stays (1:n_H are hospitals and (n_H+1):(2*n_H) are communities)
## Number of hospitals
n_H<-length(unique(ST_Hosp$H_ID))
## Creat day format of the date
ST_Hosp$A_Day<-as.numeric(ST_Hosp$A_DAT-min(c(ST_Hosp$A_DAT, ST_Hosp$D_DAT)))+1
ST_Hosp$D_Day<-as.numeric(ST_Hosp$D_DAT-min(c(ST_Hosp$A_DAT, ST_Hosp$D_DAT)))+1
## Minimum of the Day and Maximum of the Day
Day_min<-min(c(ST_Hosp$A_Day, ST_Hosp$D_Day))
Day_max<-max(c(ST_Hosp$A_Day, ST_Hosp$D_Day))
## Select the useful variables
ST_Hosp<-ST_Hosp[,c(1,5,11,12,8:10)]

#3# Datasets for generating the patient locations with different type of hospital stays
## Check the transfers between hospitals of each patient
bool<-ST_Hosp$P_ID[1:(dim(ST_Hosp)[1]-1)]==ST_Hosp$P_ID[2:dim(ST_Hosp)[1]]
ST_Hosp$bool<-as.numeric(c(F,bool))
## Dataframe selcting the patients which have only one hospitalization records, for them we need to generate the start and end location
bool1<-!(c(bool,F)|c(F,bool))
ST_Transfer_1<-ST_Hosp[bool1,]
## Transfers of patients between two hospital stays: from hospital to community and to hospital
ST_Transfer_2<-data.frame(ST_Hosp[c(bool,F),], ST_Hosp[c(F,bool),])
ST_Transfer_2<-ST_Transfer_2[,-c(9,15,16)]
## Transfers of pateints with multiple hospitalization records: the start and end locations of all hospital stays
bool2<-((!c(F,bool))|(!c(bool,F)))&(!(ST_Hosp$P_ID %in% ST_Transfer_1$P_ID)) #Except the patients with only one hospitalization record.#
ST_Transfer_3<-ST_Hosp[bool2,] # The column named "bool" indicates the location we newly generate is start or end: 0 is start, 1 is end.#
## Final community locations
ST_C_ID<-data.frame(P_ID = ST_Hosp$P_ID[ST_Hosp$bool==0], C_ID = ST_Hosp$H_ID[ST_Hosp$bool==0]+n_H)

#4# Establish the patient locations (In the following locations, if the value is NA, then means no corresponding locations)
## ST_Transfer_1
#### Calculate the number of days between the admission date and the first date
ST_Transfer_1$D_bet.f<-ST_Transfer_1$A_Day-Day_min
#### Calculate the number of days between the discharge date and the last date
ST_Transfer_1$D_bet.l<-Day_max-ST_Transfer_1$D_Day
#### Start location
###### Start from community
ST_Transfer_1$s[ST_Transfer_1$D_bet.f!=0]<-ST_Transfer_1$H_ID[ST_Transfer_1$D_bet.f!=0]+n_H
#### End location
###### End in community
ST_Transfer_1$e[ST_Transfer_1$D_bet.l!=0]<-ST_Transfer_1$H_ID[ST_Transfer_1$D_bet.l!=0]+n_H
## ST_Transfer_2
#### Caculate the day between two hospital stays
ST_Transfer_2$Day_bet<-ST_Transfer_2$A_Day.1-ST_Transfer_2$D_Day-1
#### The community stay between each hospital transfers
###### If there is a community stay between two hospital stays, we assign community ID.
ST_Transfer_2$C_ID[ST_Transfer_2$Day_bet!=0]<-ST_Transfer_2$H_ID.1[ST_Transfer_2$Day_bet!=0]+n_H
## ST_Transfer_3 (0: Start location, 1: End location)
#### Calculate the number of days between the first admission date and the first date
ST_Transfer_3$D_bet[ST_Transfer_3$bool==0]<-ST_Transfer_3$A_Day[ST_Transfer_3$bool==0]-Day_min
#### Calculate the number of days between the last discharge date and the last date
ST_Transfer_3$D_bet[ST_Transfer_3$bool==1]<-Day_max-ST_Transfer_3$D_Day[ST_Transfer_3$bool==1]
#### Locations
###### Start location
ST_Transfer_3$C_ID[(ST_Transfer_3$bool==0)&(ST_Transfer_3$D_bet!=0)]<-ST_Transfer_3$H_ID[(ST_Transfer_3$bool==0)&(ST_Transfer_3$D_bet!=0)]+n_H
###### End location
ST_Transfer_3$C_ID[(ST_Transfer_3$bool==1)&(ST_Transfer_3$D_bet!=0)]<-ST_Transfer_3$H_ID[(ST_Transfer_3$bool==1)&(ST_Transfer_3$D_bet!=0)]+n_H

#5# Create the patient location data outside the original dataset
## ST_Transfer_1
ST_Location_1<-rbind(data.frame(P_ID=ST_Transfer_1$P_ID, A_Day=Day_min, D_Day=ST_Transfer_1$A_Day-1, L_ID=ST_Transfer_1$s), data.frame(P_ID=ST_Transfer_1$P_ID, A_Day=ST_Transfer_1$D_Day+1, D_Day=Day_max, L_ID=ST_Transfer_1$e))
ST_Location_1<-ST_Location_1[!is.na(ST_Location_1$L_ID),]
## ST_Transfer_2
ST_Location_2<-data.frame(P_ID=ST_Transfer_2$P_ID, A_Day=ST_Transfer_2$D_Day+1, D_Day=ST_Transfer_2$A_Day.1-1, L_ID=ST_Transfer_2$C_ID)
ST_Location_2<-ST_Location_2[!is.na(ST_Location_2$L_ID),]
## ST_Transfer_3
ST_Location_3<-rbind(data.frame(P_ID=ST_Transfer_3$P_ID[ST_Transfer_3$bool==0], A_Day=1, D_Day=ST_Transfer_3$A_Day[ST_Transfer_3$bool==0]-1, L_ID=ST_Transfer_3$C_ID[ST_Transfer_3$bool==0]), data.frame(P_ID=ST_Transfer_3$P_ID[ST_Transfer_3$bool==1], A_Day=ST_Transfer_3$D_Day[ST_Transfer_3$bool==1]+1, D_Day= Day_max, L_ID=ST_Transfer_3$C_ID[ST_Transfer_3$bool==1]))
ST_Location_3<-ST_Location_3[!is.na(ST_Location_3$L_ID),]
## Combine them with the original data
ST_Location<-rbind(data.frame(P_ID=ST_Hosp$P_ID, A_Day=ST_Hosp$A_Day, D_Day=ST_Hosp$D_Day, L_ID=ST_Hosp$H_ID), ST_Location_1, ST_Location_2, ST_Location_3)
## Order the ST_Location
ST_Location<-ST_Location[order(ST_Location$P_ID, ST_Location$A_Day, ST_Location$D_Day),]

#6# Resampling time
## Read up-scaling data construction data lists
load("ST_FS_Population_P_IDresp_unique_Incompleteness_0.RData")
ST_FS_P_IDresp_unique<-ST_FS_P_IDresp_unique[[1]]
load("ST_FS_Population_P_IDresp_freq_Incompleteness_0.RData")
ST_FS_P_IDresp_freq<-ST_FS_P_IDresp_freq[[1]]
## Assign the value to the ST_Location
ST_Location$Resp<-as.numeric(as.character(factor(ST_Location$P_ID, levels = ST_FS_P_IDresp_unique, labels = ST_FS_P_IDresp_freq)))

#7# Assign the week days to the location dataframe
## The week day of the first day of the dataset
WD.0<-unique(ST_Hosp$A_WD[ST_Hosp$A_Day==1])
## Week day function
Fun_WD<-function(day) (day-1+WD.0)%%7  
## Compute the week day!
ST_Location$A_WD<-Fun_WD(ST_Location$A_Day)
ST_Location$D_WD<-Fun_WD(ST_Location$D_Day)

#8# Number of patients in different locations each day
## Daily stay matrix (rows: hospitals and communities, columns: days)
ST_Dstay<-matrix(0, nrow = max(ST_Location$L_ID), ncol = max(c(ST_Location$A_Day, ST_Location$D_Day)))
## Assign patients stays to this matrix
for (i in 1:dim(ST_Location)[1]) {
  ST_Dstay[ST_Location$L_ID[i], ST_Location$A_Day[i]:ST_Location$D_Day[i]]<-ST_Dstay[ST_Location$L_ID[i], ST_Location$A_Day[i]:ST_Location$D_Day[i]]+ST_Location$Resp[i]
}

#9# Save dataframe
setwd("E:/Work/Saxony and Thuringia/2. ST_Comparison/2.2. ST_Data/2.2.3. ST_Location Data")
## Daily Locations
write.csv(ST_Location, file = "ST_Location.csv", row.names = F)
## Unchosable community end locations
write.csv(ST_C_ID, file = "ST_C_ID.csv", row.names = F)
## Daily number of patients in the hospitals and communities
write.table(ST_Dstay, file = "ST_Daily_N_Stay.txt")
