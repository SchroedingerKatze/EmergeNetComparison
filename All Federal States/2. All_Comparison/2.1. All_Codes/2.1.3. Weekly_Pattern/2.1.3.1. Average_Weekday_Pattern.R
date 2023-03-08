#1# Read data
## Bavaria
BY_WPattern<-read.csv("E:/Work/Bavaria/2. BY_Comparison/2.2. BY_Data/2.2.9. BY_Weekly_Pattern Data/BY_Week_Transfer_Prop.csv", header=T)
## Lower Saxony
LS_WPattern<-read.csv("E:/Work/Lower Saxony/2. LS_Comparison/2.2. LS_Data/2.2.9. LS_Weekly_Pattern Data/LS_Week_Transfer_Prop.csv", header=T)
## Saxony and Thuringia
ST_WPattern<-read.csv("E:/Work/Saxony and Thuringia/2. ST_Comparison/2.2. ST_Data/2.2.9. ST_Weekly_Pattern Data/ST_Week_Transfer_Prop.csv", header=T)

#2# Generate dataframe for plot
Admission_WPattern<-data.frame(Proportion=rowMeans(cbind(BY_WPattern$Admission, LS_WPattern$Admission, ST_WPattern$Admission)), Type="Admission", Weekday=BY_WPattern$Weekday)
Discharge_WPattern<-data.frame(Proportion=rowMeans(cbind(BY_WPattern$Discharge, LS_WPattern$Discharge, ST_WPattern$Discharge)), Type="Discharge", Weekday=BY_WPattern$Weekday)