#1# Read data
## Bavaria
setwd("E:/Work/Bavaria/2. BY_Comparison/2.2. BY_Data/2.2.8. BY_Hospital_Community_Based_Model Data/2.2.8.2. BY_Modelling_Results")
#### Daily number of infectious and total patients in different locations
BY_I_C_0<-as.matrix(read.table("BY_I_1_1_0.txt"))
BY_N_C_0<-as.matrix(read.table("BY_N_1_1_0.txt"))
#### Hospital sizes
BY_HSize<-read.csv("E:/Work/Bavaria/2. BY_Comparison/2.2. BY_Data/2.2.5. BY_Hospital_Size Data/BY_Hospital_Size.csv", header = T)
BY_HSize<-BY_HSize[order(BY_HSize$H_S),]
## Lower Saxony 
setwd("E:/Work/Lower Saxony/2. LS_Comparison/2.2. LS_Data/2.2.8. LS_Hospital_Community_Based_Model Data/2.2.8.2. LS_Modelling_Results")
#### Daily number of infectious and total patients in different locations
LS_I_C_0<-as.matrix(read.table("LS_I_1_1_0.txt"))
LS_N_C_0<-as.matrix(read.table("LS_N_1_1_0.txt"))
#### Hospital sizes
LS_HSize<-read.csv("E:/Work/Lower Saxony/2. LS_Comparison/2.2. LS_Data/2.2.5. LS_Hospital_Size Data/LS_Hospital_Size.csv", header = T)
LS_HSize<-LS_HSize[order(LS_HSize$H_S),]
## Saxony and Thuringia
setwd("E:/Work/Saxony and Thuringia/2. ST_Comparison/2.2. ST_Data/2.2.8. ST_Hospital_Community_Based_Model Data/2.2.8.2. ST_Modelling_Results")
#### Daily number of infectious and total patients in different locations
ST_I_C_0<-as.matrix(read.table("ST_I_1_1_0.txt"))
ST_N_C_0<-as.matrix(read.table("ST_N_1_1_0.txt"))
#### Hospital sizes
ST_HSize<-read.csv("E:/Work/Saxony and Thuringia/2. ST_Comparison/2.2. ST_Data/2.2.5. ST_Hospital_Size Data/ST_Hospital_Size.csv", header = T)
ST_HSize<-ST_HSize[order(ST_HSize$H_S),]

#2# Prevalence function
Fun_Mat_pr<-function(I,N,n_day){
  Pr<-I/N
  Pr[is.nan(Pr)]<-0
  ## Select days
  Pr<-Pr[,1:n_day]
}
BY_Pr<-Fun_Mat_pr(BY_I_C_0,BY_N_C_0,14600)
LS_Pr<-Fun_Mat_pr(LS_I_C_0,LS_N_C_0,14600)
ST_Pr<-Fun_Mat_pr(ST_I_C_0,ST_N_C_0,14600)

#3# Calculate Weekly prevalence
Weekly_Avg_Pr<-function(Mat_pr){
  Mat<-matrix(0, nrow = nrow(Mat_pr), ncol = ncol(Mat_pr))
  for (i in 1:2085) {
    Mat[,i]<-rowMeans(Mat_pr[, ((i-1)*7+1):(i*7)])
  }
  return(Mat)
}

#4# Calculate final states
Final_State<-function(Mat_Weekly_Avg_Pr){
  F_state<-NULL
  for (i in 1:dim(Mat_Weekly_Avg_Pr)[1]) {
    j=365
    while (abs((Mat_Weekly_Avg_Pr[i,j]-Mat_Weekly_Avg_Pr[i,2085])/Mat_Weekly_Avg_Pr[i,2085])>0.01) {
      j=j+1
    }
    F_state<-c(F_state, j)
  }
  return(F_state)
}

#5# Final states
BY_Final_State<-Final_State(Weekly_Avg_Pr(BY_Pr))
LS_Final_State<-Final_State(Weekly_Avg_Pr(LS_Pr))
ST_Final_State<-Final_State(Weekly_Avg_Pr(ST_Pr))