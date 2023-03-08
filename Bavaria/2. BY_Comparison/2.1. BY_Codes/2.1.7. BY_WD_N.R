# Function for calculating the total number of patients (N) on Monday (eigenvector) and other weekdays.
Fun_WD_N<-function(FS,s_FS){
  
  #1# Read transfer probability matrix on each weekday
  setwd(paste0("E:/Work/",FS,"/2. ",s_FS,"_Comparison/2.2. ",s_FS,"_Data/2.2.6. ",s_FS,"_Transfer_Probability Data"))
  TransMat_Mul<-as.matrix(read.table(paste0(s_FS,"_Trans_Pr0.txt")))
  
  #2# Multiplying all the matrices of different weekdays
  for (WD in 1:6) {
    TransMat<-as.matrix(read.table(paste0(s_FS,"_Trans_Pr",WD,".txt")))
    TransMat_Mul<-TransMat_Mul%*%TransMat
  }
  
  #3# N on different weekdays 
  ## Starting with N on Monday (eigenvector), then combine other weekdays in one matrix. Each column is N on each weekdays.
  N<-t(t(abs(as.numeric(eigen(t(TransMat_Mul))$vectors[,1]))*1000000000))
  for (WD in 1:6) {
    TransMat<-as.matrix(read.table(paste0(s_FS,"_Trans_Pr",WD,".txt")))
    N<-cbind(N,t(t(N[,WD])%*%TransMat))
  }
  colnames(N)<-c("Sun", "Mon","Tue","Wed","Thu","Fri","Sat")
  return(N)
}

#4# Results
## Bavaria
BY_WD_N<-Fun_WD_N("Bavaria","BY")
setwd("E:/Work/Bavaria/2. BY_Comparison/2.2. BY_Data/2.2.8. BY_Hospital_Community_Based_Model Data/2.2.8.1. BY_Modelling_Preparation")
write.table(BY_WD_N, "BY_WD_N.txt", col.names = T, row.names = F)
## Lower Saxony
LS_WD_N<-Fun_WD_N("Lower Saxony","LS")
setwd("E:/Work/Lower Saxony/2. LS_Comparison/2.2. LS_Data/2.2.8. LS_Hospital_Community_Based_Model Data/2.2.8.1. LS_Modelling_Preparation")
write.table(LS_WD_N, "LS_WD_N.txt", col.names = T, row.names = F)
## Saxony and Thuringia
ST_WD_N<-Fun_WD_N("Saxony and Thuringia","ST")
setwd("E:/Work/Saxony and Thuringia/2. ST_Comparison/2.2. ST_Data/2.2.8. ST_Hospital_Community_Based_Model Data/2.2.8.1. ST_Modelling_Preparation")
write.table(ST_WD_N, "ST_WD_N.txt", col.names = T, row.names = F)
