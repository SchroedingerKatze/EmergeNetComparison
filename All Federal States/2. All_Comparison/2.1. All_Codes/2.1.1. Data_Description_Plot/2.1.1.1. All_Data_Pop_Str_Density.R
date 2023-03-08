#1# Read data
## Bavaria Population
BY_Pop_Str<-read.csv("E:/Work/Bavaria/2. BY_Comparison/2.2. BY_Data/2.2.10. BY_Pop_Structure Data/BY_Data_Pop_Str.csv", header = T)
BY_Pop_Str$FS<-"Bavaria"
## Lower Saxony Population
LS_Pop_Str<-read.csv("E:/Work/Lower Saxony/2. LS_Comparison/2.2. LS_Data/2.2.10. LS_Pop_Structure Data/LS_Data_Pop_Str.csv", header = T)
LS_Pop_Str$FS<-"Lower Saxony"
## Saxony and Thuringia Population
ST_Pop_Str<-read.csv("E:/Work/Saxony and Thuringia/2. ST_Comparison/2.2. ST_Data/2.2.10. ST_Pop_Structure Data/ST_Data_Pop_Str.csv", header = T)
ST_Pop_Str$FS<-"Saxony and Thuringia"

#2# Plot dataframe
Pop_Str<-rbind(BY_Pop_Str[,c(1,3,4)], LS_Pop_Str[,c(1,3,4)], ST_Pop_Str[,c(1,3,4)])
Pop_Str$FS<-factor(Pop_Str$FS)

#3# Plot
library(ggplot2)
p <- ggplot(data = Pop_Str, aes(x = Pop_Str$AR, y = Pop_Str$Prop, group=Pop_Str$FS, color = Pop_Str$FS, shape = Pop_Str$FS))+
  geom_point(size = 3)+
  geom_line(linetype="dotdash", size=2, alpha = 0.5)+
  #### Rotate x labels
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
p <- p+theme_bw()+
  ## Change legend title
  labs(color = "Federal States", shape = "Federal States")+
  xlab("Age")+
  ylab("Density")+
  theme(legend.position = "bottom")

#4# Save Graph
setwd("E:/Second Paper/Graphs/Age Distributions")
pdf("Data_Pop_Str.pdf", width=10, height=4)
p
dev.off()