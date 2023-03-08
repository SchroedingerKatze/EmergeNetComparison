#1# Read data
setwd("E:/Work/Saxony and Thuringia/2. ST_Comparison/2.2. ST_Data/2.2.4. ST_Location Data")
## Location
ST_Location<-read.csv("ST_Location.csv", header = T)
ST_Location<-ST_Location[order(ST_Location$P_ID, ST_Location$A_Day, ST_Location$D_Day),]
## Number of persons in different places
ST_Dstay<-as.matrix(read.table("ST_Daily_N_Stay.txt"))
## Assign the weekdays as the column names of the Dstay matrix
#### The corresponding weekday of the first day
W0<-unique(ST_Location$A_WD[ST_Location$A_Day==1])
#### Column names as weekdays (0:Sunday, others: 1-6)
dimnames(ST_Dstay)[[2]]<-(1:dim(ST_Dstay)[2]-1+W0)%%7

#2# Patient transfers
## Create the transfer data
bool_t<-ST_Location$P_ID[1:(dim(ST_Location)[1]-1)]==ST_Location$P_ID[2:dim(ST_Location)[1]]
ST_Transfer<-data.frame(ST_Location[c(bool_t,F),],ST_Location[c(F,bool_t),])
## Patient transfers on weekdays
ST_WTransfer<-ST_Transfer[,c(4,11,5,13)]
names(ST_WTransfer)<-c("L_From", "L_To", "Resp", "WD")
ST_WTransfer<-ST_WTransfer[order(ST_WTransfer$WD),]

#3# Create the averaged transfer matrix for each weekday
## Function
Fun_Trans_Matrix<-function(WD){
  ST_WTransfer_WD<-ST_WTransfer[ST_WTransfer$WD==WD,]
  Trans_Mat<-matrix(0, nrow = max(ST_Location$L_ID), ncol = max(ST_Location$L_ID))
  for(i in 1:dim(ST_WTransfer_WD)[1]){
    Trans_Mat[ST_WTransfer_WD$L_From[i], ST_WTransfer_WD$L_To[i]]<-Trans_Mat[ST_WTransfer_WD$L_From[i], ST_WTransfer_WD$L_To[i]]+ST_WTransfer_WD$Resp[i]
  }
  #### Average the transfer matrix: divide the number of this weekday in the data
  return(Trans_Mat/(length(dimnames(ST_Dstay)[[2]][dimnames(ST_Dstay)[[2]]==as.character(WD)])))
}
## Apply this function to every weekday from Sun to Sat(Data output is a list with 7 elements)
ST_Trans_Mat<-lapply(0:6, Fun_Trans_Matrix)

#4# Compute the average number of patients in each location each weekday
## Function
Fun_NPL_WD<-function(WD){
  ST_Dstay_WD<-ST_Dstay[,which(colnames(ST_Dstay)==as.character(WD))]
  ST_Dstay_WD<-rowMeans(ST_Dstay_WD)
  return(ST_Dstay_WD)
}
## Apply this function to every weekday (Data output is a list with 7 elements)
ST_Wstay<-lapply(c(6,0:5), Fun_NPL_WD) # The order follows the transfer probability calculation: transfer on day k should divide over number of people on day k-1 #

#5# Compute the transfer probability matrix
ST_Trans_Pr_Mat<-mapply(function(Mat, V) Mat/V, ST_Trans_Mat, ST_Wstay, SIMPLIFY = FALSE)
## Calculate the probability of patient staying in the same hospital in the diagonal positions
for (i in 1:length(ST_Trans_Pr_Mat)) {
  ST_Trans_Pr_i<-ST_Trans_Pr_Mat[[i]]
  diag(ST_Trans_Pr_i)<-1-rowSums(ST_Trans_Pr_i)
  ST_Trans_Pr_Mat[[i]]<-ST_Trans_Pr_i
}

#6# Save data
## Hospital stays per weekday
setwd("E:/Work/Saxony and Thuringia/2. ST_Comparison/2.2. ST_Data/2.2.15. ST_Week_HSize")
ST_Wstay_Mat<-matrix(unlist(ST_Wstay), nrow = 7, byrow = T)
dimnames(ST_Wstay_Mat)<-list(c("Sat", "Sun", "Mon", "Tue", "Wed", "Thu", "Fri"), 1:dim(ST_Wstay_Mat)[2])
write.table(ST_Wstay_Mat, "ST_Wstay_Mat.txt")
## Transfer probability
setwd("E:/Work/Saxony and Thuringia/2. ST_Comparison/2.2. ST_Data/2.2.6. ST_Transfer_Probability Data")
WD<-0
while (WD<=6) {
  write.table(ST_Trans_Pr_Mat[[WD+1]], paste0("ST_Trans_Pr", WD, ".txt"))
  WD<-WD+1
}