#1# Read data
## Hospitalization data
setwd("F:/Work/Bavaria/2. BY_Comparison/2.2. BY_Data/2.2.1. BY_Basic Data")
BY_Hosp<-read.csv("BY_Hosp_C.csv", header = T)
## No. of patient stays each day
setwd("F:/Work/Bavaria/2. BY_Comparison/2.2. BY_Data/2.2.4. BY_Location Data")
BY_Dstay<-as.matrix(read.table("BY_Daily_N_Stay.txt"))

#2# Hospital size
BY_Hsize<-data.frame(H_ID=unique(BY_Hosp$H_ID), N_HBed=as.numeric(as.character(factor(unique(BY_Hosp$H_ID), levels = 1:max(BY_Hosp$H_ID), labels = rowMeans(BY_Dstay[1:max(BY_Hosp$H_ID),])))))
BY_Hsize<-BY_Hsize[order(BY_Hsize$N_HBed, decreasing = T),]
BY_Hsize$Rank<-1:dim(BY_Hsize)[1]

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
BY_Hsize$H_S<-mapply(Fun_size,BY_Hsize$N_HBed)

#4# Save hospital size categories
setwd("F:/Work/Bavaria/2. BY_Comparison/2.2. BY_Data/2.2.5. BY_Hospital_Size Data")
write.csv(BY_Hsize, "BY_Hospital_Size.csv", row.names = F)