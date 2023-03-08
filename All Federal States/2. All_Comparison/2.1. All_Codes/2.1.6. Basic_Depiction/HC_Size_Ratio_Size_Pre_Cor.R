#1# Hospital size:Community size
## BY HC Ratio
setwd("E:/Work/Bavaria/2. BY_Comparison/2.2. BY_Data/2.2.8. BY_Hospital_Community_Based_Model Data/2.2.8.1. BY_Modelling_Results")
BY_N<-as.matrix(read.table("BY_N_C_0.txt"))
BY_H_Size<-colSums(BY_N[1:(dim(BY_N)[1]/2),])[dim(BY_N)[2]]
BY_C_Size<-colSums(BY_N[((dim(BY_N)[1]/2)+1):dim(BY_N)[1],])[dim(BY_N)[2]]
BY_HC_Ratio<-BY_H_Size/BY_C_Size

## LS HC Ratio
setwd("E:/Work/Lower Saxony/2. LS_Comparison/2.2. LS_Data/2.2.8. LS_Hospital_Community_Based_Model Data/2.2.8.1. LS_Modelling_Results")
LS_N<-as.matrix(read.table("LS_N_C_0.txt"))
LS_H_Size<-colSums(LS_N[1:(dim(LS_N)[1]/2),])[dim(LS_N)[2]]
LS_C_Size<-colSums(LS_N[((dim(LS_N)[1]/2)+1):dim(LS_N)[1],])[dim(LS_N)[2]]
LS_HC_Ratio<-LS_H_Size/LS_C_Size

## ST HC Ratio
setwd("E:/Work/Saxony and Thuringia/2. ST_Comparison/2.2. ST_Data/2.2.8. ST_Hospital_Community_Based_Model Data/2.2.8.1. ST_Modelling_Results")
ST_N<-as.matrix(read.table("ST_N_C_0.txt"))
ST_H_Size<-colSums(ST_N[1:(dim(ST_N)[1]/2),])[dim(ST_N)[2]]
ST_C_Size<-colSums(ST_N[((dim(ST_N)[1]/2)+1):dim(ST_N)[1],])[dim(ST_N)[2]]
ST_HC_Ratio<-ST_H_Size/ST_C_Size

#2# Correlation between hospital size and final prevalence
## BY Pre and Size correlation
BY_Pre<-as.matrix(read.table("E:/Work/Bavaria/2. BY_Comparison/2.2. BY_Data/2.2.8. BY_Hospital_Community_Based_Model Data/2.2.8.2. BY_Final_State/BY_WF_Pre_C_0.txt"))
BY_cor<-cor(as.numeric(BY_Pre[,dim(BY_Pre)[2]])[1:(dim(BY_Pre)[1]/2)], as.numeric(BY_N[,dim(BY_N)[2]])[1:(dim(BY_N)[1]/2)])
LS_Pre<-as.matrix(read.table("E:/Work/Lower Saxony/2. LS_Comparison/2.2. LS_Data/2.2.8. LS_Hospital_Community_Based_Model Data/2.2.8.2. LS_Final_State/LS_WF_Pre_C_0.txt"))
LS_cor<-cor(as.numeric(LS_Pre[,dim(LS_Pre)[2]])[1:(dim(LS_Pre)[1]/2)], as.numeric(LS_N[,dim(LS_N)[2]])[1:(dim(LS_N)[1]/2)])
ST_Pre<-as.matrix(read.table("E:/Work/Saxony and Thuringia/2. ST_Comparison/2.2. ST_Data/2.2.8. ST_Hospital_Community_Based_Model Data/2.2.8.2. ST_Final_State/ST_WF_Pre_C_0.txt"))
ST_cor<-cor(as.numeric(ST_Pre[,dim(ST_Pre)[2]])[1:(dim(ST_Pre)[1]/2)], as.numeric(ST_N[,dim(ST_N)[2]])[1:(dim(ST_N)[1]/2)])

#3# Save data
r<-data.frame(FS = c('Bavaria', 'Lower Saxony', 'Saxony and Thuringia'), H_C_Ratio = c(BY_HC_Ratio, LS_HC_Ratio, ST_HC_Ratio), Cor = c(BY_cor, LS_cor, ST_cor))