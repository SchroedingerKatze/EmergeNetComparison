#1# Read data
## LOS Bavaria
BY_LOS_Distr<-read.csv("E:/Work/Bavaria/2. BY_Comparison/2.2. BY_Data/2.2.11. BY_Statistics Data/BY_LOS_Distribution.csv", header = T)
BY_LOS_Distr$FS<-"Bavaria"
BY_x<-c(30, sum(BY_LOS_Distr$Prop[BY_LOS_Distr$x>29]), "Bavaria")
## LOS Lower Saxony
LS_LOS_Distr<-read.csv("E:/Work/Lower Saxony/2. LS_Comparison/2.2. LS_Data/2.2.11. LS_Statistics Data/LS_LOS_Distribution.csv", header = T)
LS_LOS_Distr$FS<-"Lower Saxony"
LS_x<-c(30, sum(LS_LOS_Distr$Prop[LS_LOS_Distr$x>29]), "Lower Saxony")
## LOS Saxony and Thuringia
ST_LOS_Distr<-read.csv("E:/Work/Saxony and Thuringia/2. ST_Comparison/2.2. ST_Data/2.2.11. ST_Statistics Data/ST_LOS_Distribution.csv", header = T)
ST_LOS_Distr$FS<-"Saxony and Thuringia"
ST_x<-c(30, sum(ST_LOS_Distr$Prop[ST_LOS_Distr$x>29]), "Saxony and Thuringia")

#2# Dataframe for plot
LOS_Distr<-rbind(BY_LOS_Distr[,c(1,3,4)], LS_LOS_Distr[,c(1,3,4)], ST_LOS_Distr[,c(1,3,4)])
LOS_Distr<-LOS_Distr[LOS_Distr$x<=29,]
LOS_Distr<-rbind(LOS_Distr, BY_x, LS_x, ST_x)
LOS_Distr$x<-as.numeric(LOS_Distr$x)
LOS_Distr<-LOS_Distr[order(LOS_Distr$FS, LOS_Distr$x),]
LOS_Distr$Prop<-as.numeric(LOS_Distr$Prop)

#3# Plot
library(ggplot2)
p <- ggplot(data = LOS_Distr, aes(x = LOS_Distr$x, y = LOS_Distr$Prop, group=LOS_Distr$FS, color = LOS_Distr$FS, shape = LOS_Distr$FS))+geom_point(size = 2)+geom_line(linetype="dashed", size=1, alpha = 0.8)+scale_y_continuous(breaks = seq(0,0.14,0.02), limits=c(0, 0.14), expand = c(0.01,0))+scale_x_continuous(breaks = 1:30, limits=c(1,30), expand = c(0.01,0.01), labels = c(1:29, ">29"))
p<-p + theme(axis.text=element_text(size=12), axis.title=element_text(size=14), axis.text.x=element_text(angle = 90, hjust = 1))+theme_bw()+labs(color = "Federal States", shape = "Federal States")+xlab("Length of hospital stay (days)")+ylab("Density\n")+theme(legend.position = "bottom")

#4# Save graph
setwd("E:/Second Paper/Graphs/Figures/S9 Distribution of length of hospital stay")
## pdf
pdf("LOS_Distr.pdf", width = 9, height = 3.7)
p
dev.off()