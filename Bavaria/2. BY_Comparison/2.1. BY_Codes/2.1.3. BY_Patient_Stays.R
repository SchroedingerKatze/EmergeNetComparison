#1# Read data
setwd("E:/Work/Bavaria/2. BY_Comparison/2.2. BY_Data/2.2.1. BY_Basic Data")
BY_Hosp<-read.csv("BY_Hosp_C.csv", header = T)
BY_Hosp$A_DAT<-as.Date(as.character(BY_Hosp$A_DAT))
BY_Hosp$D_DAT<-as.Date(as.character(BY_Hosp$D_DAT))
BY_Hosp<-BY_Hosp[order(BY_Hosp$P_ID, BY_Hosp$A_DAT, BY_Hosp$D_DAT),]

#2# Generate the data of patient stays (1:n_H are hospitals and (n_H+1):(2*n_H) are communities)
## Number of hospitals
n_H<-length(unique(BY_Hosp$H_ID))
## Creat day format of the date
BY_Hosp$A_Day<-as.numeric(BY_Hosp$A_DAT-min(c(BY_Hosp$A_DAT, BY_Hosp$D_DAT)))+1
BY_Hosp$D_Day<-as.numeric(BY_Hosp$D_DAT-min(c(BY_Hosp$A_DAT, BY_Hosp$D_DAT)))+1
## Minimum of the Day and Maximum of the Day
Day_min<-min(c(BY_Hosp$A_Day, BY_Hosp$D_Day))
Day_max<-max(c(BY_Hosp$A_Day, BY_Hosp$D_Day))
## Select the useful variables
BY_Hosp<-BY_Hosp[,c(1,5,11,12,8:10)]

#3# Datasets for generating the patient locations with different type of hospital stays
## Check the transfers between hospitals of each patient
bool<-BY_Hosp$P_ID[1:(dim(BY_Hosp)[1]-1)]==BY_Hosp$P_ID[2:dim(BY_Hosp)[1]]
BY_Hosp$bool<-as.numeric(c(F,bool))
## Dataframe selcting the patients which have only one hospitalization records, for them we need to generate the start and end location
bool1<-!(c(bool,F)|c(F,bool))
BY_Transfer_1<-BY_Hosp[bool1,]
## Transfers of patients between two hospital stays: from hospital to community and to hospital
BY_Transfer_2<-data.frame(BY_Hosp[c(bool,F),], BY_Hosp[c(F,bool),])
BY_Transfer_2<-BY_Transfer_2[,-c(9,15,16)]
## Transfers of pateints with multiple hospitalization records: the start and end locations of all hospital stays
bool2<-((!c(F,bool))|(!c(bool,F)))&(!(BY_Hosp$P_ID %in% BY_Transfer_1$P_ID)) #Except the patients with only one hospitalization record.#
BY_Transfer_3<-BY_Hosp[bool2,] # The column named "bool" indicates the location we newly generate is start or end: 0 is start, 1 is end.#
## Final community locations
BY_C_ID<-data.frame(P_ID = BY_Hosp$P_ID[BY_Hosp$bool==0], C_ID = BY_Hosp$H_ID[BY_Hosp$bool==0]+n_H)

#4# Establish the patient locations (In the following locations, if the value is NA, then means no corresponding locations)
## BY_Transfer_1
#### Calculate the number of days between the admission date and the first date
BY_Transfer_1$D_bet.f<-BY_Transfer_1$A_Day-Day_min
#### Calculate the number of days between the discharge date and the last date
BY_Transfer_1$D_bet.l<-Day_max-BY_Transfer_1$D_Day
#### Start location
###### Start from community
BY_Transfer_1$s[BY_Transfer_1$D_bet.f!=0]<-BY_Transfer_1$H_ID[BY_Transfer_1$D_bet.f!=0]+n_H
#### End location
###### End in community
BY_Transfer_1$e[BY_Transfer_1$D_bet.l!=0]<-BY_Transfer_1$H_ID[BY_Transfer_1$D_bet.l!=0]+n_H
## BY_Transfer_2
#### Caculate the day between two hospital stays
BY_Transfer_2$Day_bet<-BY_Transfer_2$A_Day.1-BY_Transfer_2$D_Day-1
#### The community stay between each hospital transfers
###### If there is a community stay between two hospital stays, we assign community ID.
BY_Transfer_2$C_ID[BY_Transfer_2$Day_bet!=0]<-BY_Transfer_2$H_ID.1[BY_Transfer_2$Day_bet!=0]+n_H
## BY_Transfer_3 (0: Start location, 1: End location)
#### Calculate the number of days between the first admission date and the first date
BY_Transfer_3$D_bet[BY_Transfer_3$bool==0]<-BY_Transfer_3$A_Day[BY_Transfer_3$bool==0]-Day_min
#### Calculate the number of days between the last discharge date and the last date
BY_Transfer_3$D_bet[BY_Transfer_3$bool==1]<-Day_max-BY_Transfer_3$D_Day[BY_Transfer_3$bool==1]
#### Locations
###### Start location
BY_Transfer_3$C_ID[(BY_Transfer_3$bool==0)&(BY_Transfer_3$D_bet!=0)]<-BY_Transfer_3$H_ID[(BY_Transfer_3$bool==0)&(BY_Transfer_3$D_bet!=0)]+n_H
###### End location
BY_Transfer_3$C_ID[(BY_Transfer_3$bool==1)&(BY_Transfer_3$D_bet!=0)]<-BY_Transfer_3$H_ID[(BY_Transfer_3$bool==1)&(BY_Transfer_3$D_bet!=0)]+n_H

#5# Create the patient location data outside the original dataset
## BY_Transfer_1
BY_Location_1<-rbind(data.frame(P_ID=BY_Transfer_1$P_ID, A_Day=Day_min, D_Day=BY_Transfer_1$A_Day-1, L_ID=BY_Transfer_1$s), data.frame(P_ID=BY_Transfer_1$P_ID, A_Day=BY_Transfer_1$D_Day+1, D_Day=Day_max, L_ID=BY_Transfer_1$e))
BY_Location_1<-BY_Location_1[!is.na(BY_Location_1$L_ID),]
## BY_Transfer_2
BY_Location_2<-data.frame(P_ID=BY_Transfer_2$P_ID, A_Day=BY_Transfer_2$D_Day+1, D_Day=BY_Transfer_2$A_Day.1-1, L_ID=BY_Transfer_2$C_ID)
BY_Location_2<-BY_Location_2[!is.na(BY_Location_2$L_ID),]
## BY_Transfer_3
BY_Location_3<-rbind(data.frame(P_ID=BY_Transfer_3$P_ID[BY_Transfer_3$bool==0], A_Day=1, D_Day=BY_Transfer_3$A_Day[BY_Transfer_3$bool==0]-1, L_ID=BY_Transfer_3$C_ID[BY_Transfer_3$bool==0]), data.frame(P_ID=BY_Transfer_3$P_ID[BY_Transfer_3$bool==1], A_Day=BY_Transfer_3$D_Day[BY_Transfer_3$bool==1]+1, D_Day= Day_max, L_ID=BY_Transfer_3$C_ID[BY_Transfer_3$bool==1]))
BY_Location_3<-BY_Location_3[!is.na(BY_Location_3$L_ID),]
## Combine them with the original data
BY_Location<-rbind(data.frame(P_ID=BY_Hosp$P_ID, A_Day=BY_Hosp$A_Day, D_Day=BY_Hosp$D_Day, L_ID=BY_Hosp$H_ID), BY_Location_1, BY_Location_2, BY_Location_3)
## Order the BY_Location
BY_Location<-BY_Location[order(BY_Location$P_ID, BY_Location$A_Day, BY_Location$D_Day),]

#6# Resampling time
## Read up-scaling data construction data lists
load("BY_FS_Population_P_IDresp_unique_Incompleteness_0.RData")
BY_FS_P_IDresp_unique<-BY_FS_P_IDresp_unique[[1]]
load("BY_FS_Population_P_IDresp_freq_Incompleteness_0.RData")
BY_FS_P_IDresp_freq<-BY_FS_P_IDresp_freq[[1]]
## Assign the value to the BY_Location
BY_Location$Resp<-as.numeric(as.character(factor(BY_Location$P_ID, levels = BY_FS_P_IDresp_unique, labels = BY_FS_P_IDresp_freq)))

#7# Assign the week days to the location dataframe
## The week day of the first day of the dataset
WD.0<-unique(BY_Hosp$A_WD[BY_Hosp$A_Day==1])
## Week day function
Fun_WD<-function(day) (day-1+WD.0)%%7  
## Compute the week day!
BY_Location$A_WD<-Fun_WD(BY_Location$A_Day)
BY_Location$D_WD<-Fun_WD(BY_Location$D_Day)

#8# Number of patients in different locations each day
## Daily stay matrix (rows: hospitals and communities, columns: days)
BY_Dstay<-matrix(0, nrow = max(BY_Location$L_ID), ncol = max(c(BY_Location$A_Day, BY_Location$D_Day)))
## Assign patients stays to this matrix
for (i in 1:dim(BY_Location)[1]) {
  BY_Dstay[BY_Location$L_ID[i], BY_Location$A_Day[i]:BY_Location$D_Day[i]]<-BY_Dstay[BY_Location$L_ID[i], BY_Location$A_Day[i]:BY_Location$D_Day[i]]+BY_Location$Resp[i]
}

#9# Save dataframe
setwd("F:/Work/Bavaria/2. BY_Comparison/2.2. BY_Data/2.2.3. BY_Location Data")
## Daily Locations
write.csv(BY_Location, file = "BY_Location.csv", row.names = F)
## Unchosable community end locations
write.csv(BY_C_ID, file = "BY_C_ID.csv", row.names = F)
## Daily number of patients in the hospitals and communities
write.table(BY_Dstay, file = "BY_Daily_N_Stay.txt")
