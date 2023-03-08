#1# Read data
## Hospitalization data
setwd("E:/Work/Lower Saxony/2. LS_Comparison/2.2. LS_Data/2.2.1. LS_Basic Data")
LS_Hosp<-read.csv("LS_Hosp_C.csv", header = T)
## No. of patient stays each day
setwd("E:/Work/Lower Saxony/2. LS_Comparison/2.2. LS_Data/2.2.4. LS_Location Data")
LS_Dstay<-as.matrix(read.table("LS_Daily_N_Stay.txt"))

#2# Hospital size
LS_Hsize<-data.frame(H_ID=unique(LS_Hosp$H_ID), N_HBed=as.numeric(as.character(factor(unique(LS_Hosp$H_ID), levels = 1:max(LS_Hosp$H_ID), labels = rowMeans(LS_Dstay[1:max(LS_Hosp$H_ID),])))))
LS_Hsize<-LS_Hsize[order(LS_Hsize$N_HBed, decreasing = T),]
LS_Hsize$Rank<-1:dim(LS_Hsize)[1]

#3# Define the size of hospitals (Large: Beds>=800; medium: Beds in [400,799]; small: Beds<=399)
## Size classification function for the hospitals
Fun_size<-function(x){
  if (x>=800) {
    return("L")
  }else if (x<=399) {
    return("S")
  }else{
    return("M")
  }
}
## Hospital sizes
LS_Hsize$H_S<-mapply(Fun_size,LS_Hsize$N_HBed)

#4# Save hospital size categories
setwd("E:/Work/Lower Saxony/2. LS_Comparison/2.2. LS_Data/2.2.5. LS_Hospital_Size Data")
write.csv(LS_Hsize, "LS_Hospital_Size.csv", row.names = F)