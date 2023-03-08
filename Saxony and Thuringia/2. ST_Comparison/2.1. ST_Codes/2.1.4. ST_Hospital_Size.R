#1# Read data
## Hospitalization data
setwd("E:/Work/Saxony and Thuringia/2. ST_Comparison/2.2. ST_Data/2.2.1. ST_Basic Data")
ST_Hosp<-read.csv("ST_Hosp_C.csv", header = T)
## No. of patient stays each day
setwd("E:/Work/Saxony and Thuringia/2. ST_Comparison/2.2. ST_Data/2.2.4. ST_Location Data")
ST_Dstay<-as.matrix(read.table("ST_Daily_N_Stay.txt"))

#2# Hospital size
ST_Hsize<-data.frame(H_ID=unique(ST_Hosp$H_ID), N_HBed=as.numeric(as.character(factor(unique(ST_Hosp$H_ID), levels = 1:max(ST_Hosp$H_ID), labels = rowMeans(ST_Dstay[1:max(ST_Hosp$H_ID),])))))
ST_Hsize<-ST_Hsize[order(ST_Hsize$N_HBed, decreasing = T),]
ST_Hsize$Rank<-1:dim(ST_Hsize)[1]

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
ST_Hsize$H_S<-mapply(Fun_size,ST_Hsize$N_HBed)

#4# Save hospital size categories
setwd("E:/Work/Saxony and Thuringia/2. ST_Comparison/2.2. ST_Data/2.2.5. ST_Hospital_Size Data")
write.csv(ST_Hsize, "ST_Hospital_Size.csv", row.names = F)