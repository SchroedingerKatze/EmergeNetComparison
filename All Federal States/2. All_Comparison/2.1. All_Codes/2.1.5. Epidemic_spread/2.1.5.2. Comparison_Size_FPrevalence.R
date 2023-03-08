#1# Read data
library(ggplot2)
library(matrixStats)
## Prevalence
#### Bavaria
BY_Pr<-as.matrix(read.table("E:/Work/Bavaria/2. BY_Comparison/2.2. BY_Data/2.2.8. BY_Hospital_Community_Based_Model Data/2.2.8.1. BY_Modelling_Results/BY_I_C_0.txt"))/as.matrix(read.table("E:/Work/Bavaria/2. BY_Comparison/2.2. BY_Data/2.2.8. BY_Hospital_Community_Based_Model Data/2.2.8.1. BY_Modelling_Results/BY_N_C_0.txt"))
#### Lower Saxony
LS_Pr<-as.matrix(read.table("E:/Work/Lower Saxony/2. LS_Comparison/2.2. LS_Data/2.2.8. LS_Hospital_Community_Based_Model Data/2.2.8.1. LS_Modelling_Results/LS_I_C_0.txt"))/as.matrix(read.table("E:/Work/Lower Saxony/2. LS_Comparison/2.2. LS_Data/2.2.8. LS_Hospital_Community_Based_Model Data/2.2.8.1. LS_Modelling_Results/LS_N_C_0.txt"))
#### Saxony and Thuringia
ST_Pr<-as.matrix(read.table("E:/Work/Saxony and Thuringia/2. ST_Comparison/2.2. ST_Data/2.2.8. ST_Hospital_Community_Based_Model Data/2.2.8.1. ST_Modelling_Results/ST_I_C_0.txt"))/as.matrix(read.table("E:/Work/Saxony and Thuringia/2. ST_Comparison/2.2. ST_Data/2.2.8. ST_Hospital_Community_Based_Model Data/2.2.8.1. ST_Modelling_Results/ST_N_C_0.txt"))
## Classification of hospital sizes
#### Bavaria
BY_Size<-read.csv("E:/Work/Bavaria/2. BY_Comparison/2.2. BY_Data/2.2.5. BY_Hospital_Size Data/BY_Hospital_Size.csv", header = T)
#### Lower Saxony
LS_Size<-read.csv("E:/Work/Lower Saxony/2. LS_Comparison/2.2. LS_Data/2.2.5. LS_Hospital_Size Data/LS_Hospital_Size.csv", header = T)
#### Saxony and Thuringia
ST_Size<-read.csv("E:/Work/Saxony and Thuringia/2. ST_Comparison/2.2. ST_Data/2.2.5. ST_Hospital_Size Data/ST_Hospital_Size.csv", header = T)

#2# The Final weekly prevalence for different hospital groups
## Function of average final prevalence and the boarder
Fun_FPr<-function(Pr, Size, FS){
  Pr<-Pr[,dim(Pr)[2]:(dim(Pr)[2]-6)]
  FPr<-matrix(c(rowMeans(Pr), rowMins(Pr), rowMaxs(Pr)), ncol = 3)
  ## Hospital final prevalence
  H_FPr<-data.frame(Size=c("L", "M", "S"), FS=FS, Mean_FPr=0, FPr.lower=0, FPr.upper=0)
  ## Community final prevalence
  C_FPr<-data.frame(Size=c("L", "M", "S"), FS=FS, Mean_FPr=0, FPr.lower=0, FPr.upper=0)
  for (s in c("L", "M", "S")) {
    H_FPr[H_FPr$Size==s,3:5]<-colMeans(FPr[Size$H_ID[Size$H_S==s],])
    C_FPr[C_FPr$Size==s,3:5]<-colMeans(FPr[Size$H_ID[Size$H_S==s]+max(Size$H_ID),])
  }
  FPr<-list(H_FPr, C_FPr)
  names(FPr)<-c("Hospital", "Community")
  return(FPr)
}
## Apply the function
BY_FPr<-Fun_FPr(BY_Pr, BY_Size, "Bavaria")
LS_FPr<-Fun_FPr(LS_Pr, LS_Size, "Lower Saxony")
ST_FPr<-Fun_FPr(ST_Pr, ST_Size, "Saxony and Thuringia")

#3# Data for plot
## Hospital FPr
H_FPr<-rbind(BY_FPr[[1]], LS_FPr[[1]], ST_FPr[[1]])
## Community FPr
C_FPr<-rbind(BY_FPr[[2]], LS_FPr[[2]], ST_FPr[[2]])

#4# Plot
## Hospital
H_p<-ggplot(H_FPr, aes(x=H_FPr$Size, y=H_FPr$Mean_FPr, color = H_FPr$FS)) + 
  geom_point(position = position_dodge(width=0.3)) +
  geom_errorbar(aes(ymin = H_FPr$FPr.lower, ymax = H_FPr$FPr.upper), width=0.3, size=1, position = position_dodge(width=0.3))+
  xlab("Hospital sizes") +
  scale_y_continuous(name="Final hospital prevalence", limits =c(0,0.04), breaks=seq(0,0.04,0.01), expand = c(0.01,0))+
  theme_bw() + scale_color_discrete(name = "Federal states") + theme(legend.position = "bottom")
## Community
C_p<-ggplot(C_FPr, aes(x=C_FPr$Size, y=C_FPr$Mean_FPr, color = C_FPr$FS)) + 
  geom_point(position = position_dodge(width=0.3)) +
  geom_errorbar(aes(ymin = C_FPr$FPr.lower, ymax = C_FPr$FPr.upper), width=0.3, size=1, position = position_dodge(width=0.3))+
  xlab("Hospital sizes") +
  scale_y_continuous(name="Final prevalence in community nodes", limits =c(0,0.04), breaks=seq(0,0.04,0.01), expand = c(0.01,0))+
  theme_bw() + scale_color_discrete(name = "Federal states") + theme(legend.position = "bottom")

#5# Save graphs
setwd("E:/Second Paper/Graphs/Hospital and community size final prevalence")
## png
png(filename = "Hospital_FPr.png", units = "in", width = 8.5, height = 4, res = 1500)
H_p
dev.off()
png(filename = "Community_FPr.png", units = "in", width = 8.5, height = 4, res = 1500)
C_p
dev.off()
## pdf
pdf(file = "FPr.pdf", width = 8.5, height = 4)
H_p
C_p
dev.off()