#1# Read data
BY_Hsize<-read.csv("F:/Work/Bavaria/2. BY_Comparison/2.2. BY_Data/2.2.5. BY_Hospital_Size Data/BY_Hospital_Size.csv", header=T)
LS_Hsize<-read.csv("F:/Work/Lower Saxony/2. LS_Comparison/2.2. LS_Data/2.2.5. LS_Hospital_Size Data/LS_Hospital_Size.csv", header=T)
ST_Hsize<-read.csv("F:/Work/Saxony and Thuringia/2. ST_Comparison/2.2. ST_Data/2.2.5. ST_Hospital_Size Data/ST_Hospital_Size.csv", header=T)

#2# Data description
## Mean size of different level hospitals
Fun_Mean<-function(Data, Size){
  return(mean(Data$N_HBed[Data$H_S==Size]))
}
## Average size of different level hospital in all federal states
Hsize_Total<-c(mean(BY_Hsize$N_HBed),mean(LS_Hsize$N_HBed),mean(ST_Hsize$N_HBed))
Hsize_L<-mapply(Fun_Mean, list(BY_Hsize,LS_Hsize,ST_Hsize),"L")
Hsize_M<-mapply(Fun_Mean, list(BY_Hsize,LS_Hsize,ST_Hsize),"M")
Hsize_S<-mapply(Fun_Mean, list(BY_Hsize,LS_Hsize,ST_Hsize),"S")
## Interquantile range of hospital beds
IQR(BY_Hsize$N_HBed)
IQR(LS_Hsize$N_HBed)
IQR(ST_Hsize$N_HBed)
## Mean size of hospitals
Hospital_size<-data.frame(Hsize_Total=Hsize_Total, L=Hsize_L, M=Hsize_M, S=Hsize_S)
rownames(Hospital_size)<-c("Bavaria", "Lower Saxony", "Saxony and Thuringia")