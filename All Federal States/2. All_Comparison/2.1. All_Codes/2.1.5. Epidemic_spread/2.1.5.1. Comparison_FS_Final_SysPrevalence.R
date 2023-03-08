#1# Read data
library(ggplot2)
library(matrixStats)
## Bavaria
setwd("E:/Work/Bavaria/2. BY_Comparison/2.2. BY_Data/2.2.8. BY_Hospital_Community_Based_Model Data/2.2.8.4. BY_FP_TProp")
BY_HFP<-as.matrix(read.table("Hospital_Final_Prevalence.txt"))
BY_CFP<-as.matrix(read.table("Community_Final_Prevalence.txt"))
## Lower Saxony
setwd("E:/Work/Lower Saxony/2. LS_Comparison/2.2. LS_Data/2.2.8. LS_Hospital_Community_Based_Model Data/2.2.8.4. LS_FP_TProp")
LS_HFP<-as.matrix(read.table("Hospital_Final_Prevalence.txt"))
LS_CFP<-as.matrix(read.table("Community_Final_Prevalence.txt"))
## Saxony and Thuringia
setwd("E:/Work/Saxony and Thuringia/2. ST_Comparison/2.2. ST_Data/2.2.8. ST_Hospital_Community_Based_Model Data/2.2.8.4. ST_FP_TProp")
ST_HFP<-as.matrix(read.table("Hospital_Final_Prevalence.txt"))
ST_CFP<-as.matrix(read.table("Community_Final_Prevalence.txt"))

#2# Data construction
## Function of mean max min
Fun_Statistic<-function(Mat, FS){
  return(data.frame(TProp=seq(0,0.3,0.05), Pr_Mean=colMeans(Mat), Pr.lower=colMins(Mat), Pr.upper=colMaxs(Mat), FS=FS))
}
## HFP
HFP<-rbind(Fun_Statistic(BY_HFP, "Bavaria"), Fun_Statistic(LS_HFP, "Lower Saxony"), Fun_Statistic(ST_HFP, "Saxony and Thuringia"))
## CFP
CFP<-rbind(Fun_Statistic(BY_CFP, "Bavaria"), Fun_Statistic(LS_CFP, "Lower Saxony"), Fun_Statistic(ST_CFP, "Saxony and Thuringia"))

#3# Plot
## Hospital
H_p<-ggplot(HFP, aes(x=HFP$TProp, y=HFP$Pr_Mean, color = HFP$FS)) + 
   geom_point(position = position_dodge(0.03)) +
   geom_errorbar(aes(ymin = HFP$Pr.lower, ymax = HFP$Pr.upper), width = .02, size=1, position = position_dodge(0.03))+
   scale_x_continuous(name=expression(theta), breaks=seq(0,0.3,0.05))+
   scale_y_continuous(name="Final hospital prevalence", limits=c(0,0.8), breaks=seq(0,0.8,0.05), expand = c(0,0.05))+
   theme_bw() + scale_color_discrete(name = "Federal states") + theme(legend.position = "bottom")
## Community
C_p<-ggplot(CFP, aes(x=CFP$TProp, y=CFP$Pr_Mean, color = CFP$FS)) + 
  geom_point(position = position_dodge(0.03)) +
  geom_errorbar(aes(ymin = CFP$Pr.lower, ymax = CFP$Pr.upper), width = .03, size=1, position = position_dodge(0.03))+
  scale_x_continuous(name=expression(theta), breaks=seq(0,0.3,0.05))+
  scale_y_continuous(name="Final prevalence in the community nodes", limits=c(0,0.8), breaks=seq(0,0.8,0.05), expand = c(0,0.05))+
  theme_bw() + scale_color_discrete(name = "Federal states") + theme(legend.position = "bottom")

#4# Save graphs
setwd("E:/Second Paper/Graphs/Comparison of FS Final prevalence")
## png
png(filename = "Comparison_FS_SysHPrevalence.png", width = 8.5, height = 4, units = "in", res = 1500)
H_p
dev.off()
png(filename = "Comparison_FS_SysCPrevalence.png", width = 8.5, height = 4, units = "in", res = 1500)
C_p
dev.off()
## pdf
pdf("Comparison_FS_SysPrevalence.pdf", width = 9.9, height = 5)
H_p
C_p
dev.off()
