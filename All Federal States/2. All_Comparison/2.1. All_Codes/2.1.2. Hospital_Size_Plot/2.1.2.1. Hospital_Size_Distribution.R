#1# Read Data
library(plyr)
## Bavaria
BY_Hsize<-read.csv("E:/Work/Bavaria/2. BY_Comparison/2.2. BY_Data/2.2.5. BY_Hospital_Size Data/BY_Hospital_Size.csv")
BY_Hsize_Density<-count(BY_Hsize$H_S)
BY_Hsize_Density$Prop<-BY_Hsize_Density$freq/sum(BY_Hsize_Density$freq)
BY_Hsize_Density$FS<-"Bavaria"
## Lower Saxony
LS_Hsize<-read.csv("E:/Work/Lower Saxony/2. LS_Comparison/2.2. LS_Data/2.2.5. LS_Hospital_Size Data/LS_Hospital_Size.csv")
LS_Hsize_Density<-count(LS_Hsize$H_S)
LS_Hsize_Density$Prop<-LS_Hsize_Density$freq/sum(LS_Hsize_Density$freq)
LS_Hsize_Density$FS<-"Lower Saxony"
## Saxony and Thuringia
ST_Hsize<-read.csv("E:/Work/Saxony and Thuringia/2. ST_Comparison/2.2. ST_Data/2.2.5. ST_Hospital_Size Data/ST_Hospital_Size.csv")
ST_Hsize_Density<-count(ST_Hsize$H_S)
ST_Hsize_Density$Prop<-ST_Hsize_Density$freq/sum(ST_Hsize_Density$freq)
ST_Hsize_Density$FS<-"Saxony and Thuringia"

#2# Generate the dataframe for plot
Hsize_Density<-rbind(BY_Hsize_Density[,c(1,3,4)], LS_Hsize_Density[,c(1,3,4)], ST_Hsize_Density[,c(1,3,4)])

#3# Plot the density distribution
library(ggplot2)
p<-ggplot(data=Hsize_Density, aes(x=x, y=Prop, fill=FS)) +
   geom_bar(stat="identity", position=position_dodge())+
   geom_text(aes(label=round(Prop, digits = 2)), vjust=-0.5, color="darkblue", position = position_dodge(0.9), size=4)+
   scale_fill_brewer(palette="Paired")+
   theme_minimal()+
   labs(fill="Federal States")+
   xlab("Hospital size")+
   ylab("Density")

#4# Save graphs
setwd("E:/Second Paper/Graphs/Hospital Size Distribution")
pdf("Hosp_Size_Distr.pdf", width = 6.8, height = 4.2)
p
dev.off()