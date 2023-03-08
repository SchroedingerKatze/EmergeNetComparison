#1# Read data
#Transfer times Bavaria
BY_Hos_Time<-read.csv("E:/Work/Bavaria/2. BY_Comparison/2.2. BY_Data/2.2.11. BY_Statistics Data/BY_Hospitalization_Time_Distribution.csv", header = T)
BY_Hos_Prop<-BY_Hos_Time[BY_Hos_Time$Nr_Htime %in% 1:6,c(1,3)]
BY_Hos_Prop<-rbind(BY_Hos_Prop,c(">6",1-sum(BY_Hos_Prop$Prop)))
BY_Hos_Prop$FS<-"Bavaria"
#Transfer times Lower Saxony
LS_Hos_Time<-read.csv("E:/Work/Lower Saxony/2. LS_Comparison/2.2. LS_Data/2.2.11. LS_Statistics Data/LS_Hospitalization_Time_Distribution.csv", header = T)
LS_Hos_Prop<-LS_Hos_Time[LS_Hos_Time$Nr_Htime %in% 1:6,c(1,3)]
LS_Hos_Prop<-rbind(LS_Hos_Prop,c(">6",1-sum(LS_Hos_Prop$Prop)))
LS_Hos_Prop$FS<-"Lower Saxony"
#Transfer times Saxony and Thuringia
ST_Hos_Time<-read.csv("E:/Work/Saxony and Thuringia/2. ST_Comparison/2.2. ST_Data/2.2.11. ST_Statistics Data/ST_Hospitalization_Time_Distribution.csv", header = T)
ST_Hos_Prop<-ST_Hos_Time[ST_Hos_Time$Nr_Htime %in% 1:6,c(1,3)]
ST_Hos_Prop<-rbind(ST_Hos_Prop,c(">6",1-sum(ST_Hos_Prop$Prop)))
ST_Hos_Prop$FS<-"Saxony and Thuringia"

#2# Dataframe for plot
Hos_Prop<-rbind(BY_Hos_Prop,LS_Hos_Prop,ST_Hos_Prop)
Hos_Prop$Nr_Htime<-factor(Hos_Prop$Nr_Htime,levels = c(1:6,">6"))
Hos_Prop$Prop<-as.numeric(Hos_Prop$Prop)

#3# Plot
library(ggplot2)
p<-ggplot(data=Hos_Prop, aes(x=Nr_Htime, y=Prop, fill=FS))+geom_bar(stat="identity", color="black", position=position_dodge())+theme_minimal()
p<-p+scale_y_continuous(breaks = seq(0,0.5,0.05),limits = c(0,0.5))+scale_fill_brewer(palette="Blues")+labs(fill="Federal States")+xlab("Number of hospitalizations")+ylab("Density\n")
p<-p+theme(axis.text=element_text(size=12), axis.title=element_text(size=14), axis.text.x=element_text(hjust = 1))+theme(legend.position="bottom")

#4# Save graph
setwd("E:/Second Paper/Graphs/Figures/S10 Distribution of number of hospitalizations")
## pdf
pdf("Hostime_Distr.pdf", width = 9, height = 4.5)
p
dev.off()