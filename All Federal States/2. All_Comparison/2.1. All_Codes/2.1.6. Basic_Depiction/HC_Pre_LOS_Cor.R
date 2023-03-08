#1# Read prevalence
## Bavaria
#### Average prevalence
setwd("E:/Work/Bavaria/2. BY_Comparison/2.2. BY_Data/2.2.8. BY_Hospital_Community_Based_Model Data/2.2.8.2. BY_Modelling_Results")
BY_I<-as.matrix(read.table("BY_I_1_1_0.txt"))
BY_N<-as.matrix(read.table("BY_N_1_1_0.txt"))
BY_Pre<-BY_I/BY_N
BY_Pre_Avg<-rowMeans(BY_Pre[,(dim(BY_Pre)[2]-6):dim(BY_Pre)[2]])
#### LOS
setwd("E:/Work/Bavaria/2. BY_Comparison/2.2. BY_Data/2.2.4. BY_Location Data")
BY_Location<-read.csv("BY_Location.csv")
BY_Location$LOS<-BY_Location$D_Day-BY_Location$A_Day + 1
BY_L_LOS<-data.frame(L_ID = 1:nrow(BY_I), Nr_Hos = NA, LOS_Sum = NA)
for (i in BY_L_LOS$L_ID) {
  BY_L_LOS$Nr_Hos[BY_L_LOS$L_ID == i]<-sum(BY_Location$Resp[BY_Location$L_ID == i])
  BY_L_LOS$LOS_Sum[BY_L_LOS$L_ID == i]<-sum(BY_Location$LOS[BY_Location$L_ID == i]*BY_Location$Resp[BY_Location$L_ID == i])
}
BY_L_LOS$LOS_Avg<-BY_L_LOS$LOS_Sum/BY_L_LOS$Nr_Hos
###### LOS and IQR
mean(BY_Location$LOS[BY_Location$L_ID <= (max(BY_Location$L_ID)/2)])
IQR(BY_Location$LOS[BY_Location$L_ID <= (max(BY_Location$L_ID)/2)])
#### Correlation
BY_H_LOS_Cor<-cor(BY_Pre_Avg[1:(length(BY_Pre_Avg)/2)], BY_L_LOS$LOS_Avg[BY_L_LOS$L_ID %in% 1:(length(BY_Pre_Avg)/2)])
BY_C_LOS_Cor<-cor(BY_Pre_Avg[(length(BY_Pre_Avg)/2+1):length(BY_Pre_Avg)], BY_L_LOS$LOS_Avg[BY_L_LOS$L_ID %in% (length(BY_Pre_Avg)/2+1):length(BY_Pre_Avg)])

## Lower Saxony
#### Average prevalence
setwd("E:/Work/Lower Saxony/2. LS_Comparison/2.2. LS_Data/2.2.8. LS_Hospital_Community_Based_Model Data/2.2.8.2. LS_Modelling_Results")
LS_I<-as.matrix(read.table("LS_I_1_1_0.txt"))
LS_N<-as.matrix(read.table("LS_N_1_1_0.txt"))
LS_Pre<-LS_I/LS_N
LS_Pre_Avg<-rowMeans(LS_Pre[,(dim(LS_Pre)[2]-6):dim(LS_Pre)[2]])
#### LOS
setwd("E:/Work/Lower Saxony/2. LS_Comparison/2.2. LS_Data/2.2.4. LS_Location Data")
LS_Location<-read.csv("LS_Location.csv")
LS_Location$LOS<-LS_Location$D_Day-LS_Location$A_Day + 1
LS_L_LOS<-data.frame(L_ID = 1:nrow(LS_I), Nr_Hos = NA, LOS_Sum = NA)
for (i in LS_L_LOS$L_ID) {
  LS_L_LOS$Nr_Hos[LS_L_LOS$L_ID == i]<-sum(LS_Location$Resp[LS_Location$L_ID == i])
  LS_L_LOS$LOS_Sum[LS_L_LOS$L_ID == i]<-sum(LS_Location$LOS[LS_Location$L_ID == i]*LS_Location$Resp[LS_Location$L_ID == i])
}
LS_L_LOS$LOS_Avg<-LS_L_LOS$LOS_Sum/LS_L_LOS$Nr_Hos
###### LOS and IQR
mean(LS_Location$LOS[LS_Location$L_ID <= (max(LS_Location$L_ID)/2)])
IQR(LS_Location$LOS[LS_Location$L_ID <= (max(LS_Location$L_ID)/2)])
#### Correlation
LS_H_LOS_Cor<-cor(LS_Pre_Avg[1:(length(LS_Pre_Avg)/2)], LS_L_LOS$LOS_Avg[LS_L_LOS$L_ID %in% 1:(length(LS_Pre_Avg)/2)])
LS_C_LOS_Cor<-cor(LS_Pre_Avg[(length(LS_Pre_Avg)/2+1):length(LS_Pre_Avg)], LS_L_LOS$LOS_Avg[LS_L_LOS$L_ID %in% (length(LS_Pre_Avg)/2+1):length(LS_Pre_Avg)])

## Saxony and Thuringia
#### Average prevalence
setwd("E:/Work/Saxony and Thuringia/2. ST_Comparison/2.2. ST_Data/2.2.8. ST_Hospital_Community_Based_Model Data/2.2.8.2. ST_Modelling_Results")
ST_I<-as.matrix(read.table("ST_I_1_1_0.txt"))
ST_N<-as.matrix(read.table("ST_N_1_1_0.txt"))
ST_Pre<-ST_I/ST_N
ST_Pre_Avg<-rowMeans(ST_Pre[,(dim(ST_Pre)[2]-6):dim(ST_Pre)[2]])
#### LOS
setwd("E:/Work/Saxony and Thuringia/2. ST_Comparison/2.2. ST_Data/2.2.4. ST_Location Data")
ST_Location<-read.csv("ST_Location.csv")
ST_Location$LOS<-ST_Location$D_Day-ST_Location$A_Day + 1
ST_L_LOS<-data.frame(L_ID = 1:nrow(ST_I), Nr_Hos = NA, LOS_Sum = NA)
for (i in ST_L_LOS$L_ID) {
  ST_L_LOS$Nr_Hos[ST_L_LOS$L_ID == i]<-sum(ST_Location$Resp[ST_Location$L_ID == i])
  ST_L_LOS$LOS_Sum[ST_L_LOS$L_ID == i]<-sum(ST_Location$LOS[ST_Location$L_ID == i]*ST_Location$Resp[ST_Location$L_ID == i])
}
ST_L_LOS$LOS_Avg<-ST_L_LOS$LOS_Sum/ST_L_LOS$Nr_Hos
###### LOS and IQR
mean(ST_Location$LOS[ST_Location$L_ID <= (max(ST_Location$L_ID)/2)])
IQR(ST_Location$LOS[ST_Location$L_ID <= (max(ST_Location$L_ID)/2)])
#### Correlation
ST_H_LOS_Cor<-cor(ST_Pre_Avg[1:(length(ST_Pre_Avg)/2)], ST_L_LOS$LOS_Avg[ST_L_LOS$L_ID %in% 1:(length(ST_Pre_Avg)/2)])
ST_C_LOS_Cor<-cor(ST_Pre_Avg[(length(ST_Pre_Avg)/2+1):length(ST_Pre_Avg)], ST_L_LOS$LOS_Avg[ST_L_LOS$L_ID %in% (length(ST_Pre_Avg)/2+1):length(ST_Pre_Avg)])