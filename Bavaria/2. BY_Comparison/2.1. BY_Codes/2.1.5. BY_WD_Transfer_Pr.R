#1# Read data
setwd("E:/Work/Bavaria/2. BY_Comparison/2.2. BY_Data/2.2.4. BY_Location Data")
## Location
BY_Location<-read.csv("BY_Location.csv", header = T)
BY_Location<-BY_Location[order(BY_Location$P_ID, BY_Location$A_Day, BY_Location$D_Day),]
## Number of persons in different places
BY_Dstay<-as.matrix(read.table("BY_Daily_N_Stay.txt"))
## Assign the weekdays as the column names of the Dstay matrix
#### The corresponding weekday of the first day
W0<-unique(BY_Location$A_WD[BY_Location$A_Day==1])
#### Column names as weekdays (0:Sunday, others: 1-6)
dimnames(BY_Dstay)[[2]]<-(1:dim(BY_Dstay)[2]-1+W0)%%7

#2# Patient transfers
## Create the transfer data
bool_t<-BY_Location$P_ID[1:(dim(BY_Location)[1]-1)]==BY_Location$P_ID[2:dim(BY_Location)[1]]
BY_Transfer<-data.frame(BY_Location[c(bool_t,F),],BY_Location[c(F,bool_t),])
## Patient transfers on weekdays
BY_WTransfer<-BY_Transfer[,c(4,11,5,13)]
names(BY_WTransfer)<-c("L_From", "L_To", "Resp", "WD")
BY_WTransfer<-BY_WTransfer[order(BY_WTransfer$WD),]

#3# Create the averaged transfer matrix for each weekday
## Function
Fun_Trans_Matrix<-function(WD){
  BY_WTransfer_WD<-BY_WTransfer[BY_WTransfer$WD==WD,]
  Trans_Mat<-matrix(0, nrow = max(BY_Location$L_ID), ncol = max(BY_Location$L_ID))
  for(i in 1:dim(BY_WTransfer_WD)[1]){
    Trans_Mat[BY_WTransfer_WD$L_From[i], BY_WTransfer_WD$L_To[i]]<-Trans_Mat[BY_WTransfer_WD$L_From[i], BY_WTransfer_WD$L_To[i]]+BY_WTransfer_WD$Resp[i]
  }
  #### Average the transfer matrix: divide the number of this weekday in the data
  return(Trans_Mat/(length(dimnames(BY_Dstay)[[2]][dimnames(BY_Dstay)[[2]]==as.character(WD)])))
}
## Apply this function to every weekday from Sun to Sat(Data output is a list with 7 elements)
BY_Trans_Mat<-lapply(0:6, Fun_Trans_Matrix)

#4# Compute the average number of patients in each location each weekday
## Function
Fun_NPL_WD<-function(WD){
  BY_Dstay_WD<-BY_Dstay[,which(colnames(BY_Dstay)==as.character(WD))]
  BY_Dstay_WD<-rowMeans(BY_Dstay_WD)
  return(BY_Dstay_WD)
}
## Apply this function to every weekday (Data output is a list with 7 elements)
BY_Wstay<-lapply(c(6,0:5), Fun_NPL_WD) # The order follows the transfer probability calculation: transfer on day k should divide over number of people on day k-1 #

#5# Compute the transfer probability matrix
BY_Trans_Pr_Mat<-mapply(function(Mat, V) Mat/V, BY_Trans_Mat, BY_Wstay, SIMPLIFY = FALSE)
## Calculate the probability of patient staying in the same hospital in the diagonal positions
for (i in 1:length(BY_Trans_Pr_Mat)) {
  BY_Trans_Pr_i<-BY_Trans_Pr_Mat[[i]]
  diag(BY_Trans_Pr_i)<-1-rowSums(BY_Trans_Pr_i)
  BY_Trans_Pr_Mat[[i]]<-BY_Trans_Pr_i
}

#6# Save data
## Hospital stays per weekday
setwd("E:/Work/Bavaria/2. BY_Comparison/2.2. BY_Data/2.2.15. BY_Week_HSize")
BY_Wstay_Mat<-matrix(unlist(BY_Wstay), nrow = 7, byrow = T)
dimnames(BY_Wstay_Mat)<-list(c("Sat", "Sun", "Mon", "Tue", "Wed", "Thu", "Fri"), 1:dim(BY_Wstay_Mat)[2])
write.table(BY_Wstay_Mat, "BY_Wstay_Mat.txt")
## Transfer probability
setwd("E:/Work/Bavaria/2. BY_Comparison/2.2. BY_Data/2.2.6. BY_Transfer_Probability Data")
WD<-0
while (WD<=6) {
  write.table(BY_Trans_Pr_Mat[[WD+1]], paste0("BY_Trans_Pr", WD, ".txt"))
  WD<-WD+1
}