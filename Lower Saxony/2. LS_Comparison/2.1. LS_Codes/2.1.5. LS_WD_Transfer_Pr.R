#1# Read data
setwd("E:/Work/Lower Saxony/2. LS_Comparison/2.2. LS_Data/2.2.4. LS_Location Data")
## Location
LS_Location<-read.csv("LS_Location.csv", header = T)
LS_Location<-LS_Location[order(LS_Location$P_ID, LS_Location$A_Day, LS_Location$D_Day),]
## Number of persons in different places
LS_Dstay<-as.matrix(read.table("LS_Daily_N_Stay.txt"))
## Assign the weekdays as the column names of the Dstay matrix
#### The corresponding weekday of the first day
W0<-unique(LS_Location$A_WD[LS_Location$A_Day==1])
#### Column names as weekdays (0:Sunday, others: 1-6)
dimnames(LS_Dstay)[[2]]<-(1:dim(LS_Dstay)[2]-1+W0)%%7

#2# Patient transfers
## Create the transfer data
bool_t<-LS_Location$P_ID[1:(dim(LS_Location)[1]-1)]==LS_Location$P_ID[2:dim(LS_Location)[1]]
LS_Transfer<-data.frame(LS_Location[c(bool_t,F),],LS_Location[c(F,bool_t),])
## Patient transfers on weekdays
LS_WTransfer<-LS_Transfer[,c(4,11,5,13)]
names(LS_WTransfer)<-c("L_From", "L_To", "Resp", "WD")
LS_WTransfer<-LS_WTransfer[order(LS_WTransfer$WD),]

#3# Create the averaged transfer matrix for each weekday
## Function
Fun_Trans_Matrix<-function(WD){
  LS_WTransfer_WD<-LS_WTransfer[LS_WTransfer$WD==WD,]
  Trans_Mat<-matrix(0, nrow = max(LS_Location$L_ID), ncol = max(LS_Location$L_ID))
  for(i in 1:dim(LS_WTransfer_WD)[1]){
    Trans_Mat[LS_WTransfer_WD$L_From[i], LS_WTransfer_WD$L_To[i]]<-Trans_Mat[LS_WTransfer_WD$L_From[i], LS_WTransfer_WD$L_To[i]]+LS_WTransfer_WD$Resp[i]
  }
  #### Average the transfer matrix: divide the number of this weekday in the data
  return(Trans_Mat/(length(dimnames(LS_Dstay)[[2]][dimnames(LS_Dstay)[[2]]==as.character(WD)])))
}
## Apply this function to every weekday from Sun to Sat(Data output is a list with 7 elements)
LS_Trans_Mat<-lapply(0:6, Fun_Trans_Matrix)

#4# Compute the average number of patients in each location each weekday
## Function
Fun_NPL_WD<-function(WD){
  LS_Dstay_WD<-LS_Dstay[,which(colnames(LS_Dstay)==as.character(WD))]
  LS_Dstay_WD<-rowMeans(LS_Dstay_WD)
  return(LS_Dstay_WD)
}
## Apply this function to every weekday (Data output is a list with 7 elements)
LS_Wstay<-lapply(c(6,0:5), Fun_NPL_WD) # The order follows the transfer probability calculation: transfer on day k should divide over number of people on day k-1 #

#5# Compute the transfer probability matrix
LS_Trans_Pr_Mat<-mapply(function(Mat, V) Mat/V, LS_Trans_Mat, LS_Wstay, SIMPLIFY = FALSE)
## Calculate the probability of patient staying in the same hospital in the diagonal positions
for (i in 1:length(LS_Trans_Pr_Mat)) {
  LS_Trans_Pr_i<-LS_Trans_Pr_Mat[[i]]
  diag(LS_Trans_Pr_i)<-1-rowSums(LS_Trans_Pr_i)
  LS_Trans_Pr_Mat[[i]]<-LS_Trans_Pr_i
}

#6# Save data
## Hospital stays per weekday
setwd("E:/Work/Lower Saxony/2. LS_Comparison/2.2. LS_Data/2.2.15. LS_Week_HSize")
LS_Wstay_Mat<-matrix(unlist(LS_Wstay), nrow = 7, byrow = T)
dimnames(LS_Wstay_Mat)<-list(c("Sat", "Sun", "Mon", "Tue", "Wed", "Thu", "Fri"), 1:dim(LS_Wstay_Mat)[2])
write.table(LS_Wstay_Mat, "LS_Wstay_Mat.txt")
## Transfer probability
setwd("E:/Work/Lower Saxony/2. LS_Comparison/2.2. LS_Data/2.2.6. LS_Transfer_Probability Data")
WD<-0
while (WD<=6) {
  write.table(LS_Trans_Pr_Mat[[WD+1]], paste0("LS_Trans_Pr", WD, ".txt"))
  WD<-WD+1
}