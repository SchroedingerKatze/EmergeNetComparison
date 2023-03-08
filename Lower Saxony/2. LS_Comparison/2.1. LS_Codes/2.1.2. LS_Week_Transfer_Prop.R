#1# Read Data
LS_Hosp<-read.csv("E:/Work/Lower Saxony/2. LS_Comparison/2.2. LS_Data/2.2.1. LS_Basic Data/LS_Hosp_C.csv", header = T)

#2# Function for calculating weekly patterns
Fun_WP<-function(wd){
  A_Freq<-sum(LS_Hosp$Resp[LS_Hosp$A_WD == wd])
  D_Freq<-sum(LS_Hosp$Resp[LS_Hosp$D_WD == wd])
  WP<-c(A_Freq, D_Freq)
  names(WP)<-c('Admission', 'Discharge')
  return(WP)
}
## Weekly pattern
WP<-mapply(Fun_WP, 1:7)
WP<-unlist(WP)
## Proportional weekly pattern
WP<-WP/rowSums(WP)
WP<-as.data.frame(t(WP))
WP$Weekday<-c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday', 'Sunday')

#3# Write df
setwd("E:/Work/Lower Saxony/2. LS_Comparison/2.2. LS_Data/2.2.2. LS_Weekly_Pattern Data")
write.csv(WP, file = "LS_Week_Transfer_Prop.csv", row.names = F)
