#1# Read Data
BY_Hosp<-read.csv("E:/Work/Bavaria/2. BY_Comparison/2.2. BY_Data/2.2.1. BY_Basic Data/BY_Hosp_C.csv", header = T)

#2# Function for calculating weekly patterns
Fun_WP<-function(wd){
  A_Freq<-sum(BY_Hosp$Resp[BY_Hosp$A_WD == wd])
  D_Freq<-sum(BY_Hosp$Resp[BY_Hosp$D_WD == wd])
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
setwd("E:/Work/Bavaria/2. BY_Comparison/2.2. BY_Data/2.2.2. BY_Weekly_Pattern Data")
write.csv(WP, file = "BY_Week_Transfer_Prop.csv", row.names = F)
