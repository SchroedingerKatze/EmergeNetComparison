#1# Prevalence Function.
Fun_Pr<-function(FS,s_FS){
  ## Read data
  setwd(paste0("E:/Work/",FS,"/2. ",s_FS,"_Comparison/2.2. ",s_FS,"_Data/2.2.8. ",s_FS,"_Hospital_Community_Based_Model Data/2.2.8.2. ",s_FS,"_Modelling_Results"))
  Mat_I<-as.matrix(read.table(paste0(s_FS,"_I_1_1_0.txt")))
  Mat_N<-as.matrix(read.table(paste0(s_FS,"_N_1_1_0.txt")))
  ## Make the plot dataframes
  H_pr<-data.frame(Day = 1:dim(Mat_I)[2], Prevalence = colSums(Mat_I[1:(dim(Mat_I)[1]/2),])/colSums(Mat_N[1:(dim(Mat_N)[1]/2),]), Locations = "Hospital nodes", FS = FS)
  C_pr<-data.frame(Day = 1:dim(Mat_I)[2], Prevalence = colSums(Mat_I[(dim(Mat_I)[1]/2+1):(dim(Mat_I)[1]),])/colSums(Mat_N[(dim(Mat_N)[1]/2+1):(dim(Mat_N)[1]),]), Locations = "Community nodes", FS = FS)
  return(list(H_pr,C_pr))
}
## Hospital prevalence data frame
H_Pr<-rbind(Fun_Pr("Bavaria","BY")[[1]], Fun_Pr("Lower Saxony","LS")[[1]], Fun_Pr("Saxony and Thuringia","ST")[[1]])
C_Pr<-rbind(Fun_Pr("Bavaria","BY")[[2]], Fun_Pr("Lower Saxony","LS")[[2]], Fun_Pr("Saxony and Thuringia","ST")[[2]])

#2# Check the final states
View(H_Pr[H_Pr$Day<=18250 & H_Pr$Day>=18244,])
View(C_Pr[H_Pr$Day<=18250 & H_Pr$Day>=18244,])

#3# Find the final states
Fun_Final_Day<-function(FS){
  FinalState<-365
  Pr<-H_Pr[H_Pr$FS==FS,]
  Weekly_Avg_Pr<-NULL
  for (i in 1:2607) {
    Weekly_Avg_Pr<-c(Weekly_Avg_Pr, mean(Pr$Prevalence[Pr$Day %in% ((i-1)*7+1):(i*7)]))
  }
  j<-FinalState/7
  while (abs((Weekly_Avg_Pr[j]-Weekly_Avg_Pr[2607])/Weekly_Avg_Pr[2607])>0.01) {
    j=j+1
  }
  return(j*7)
}

Fun_Final_Day("Bavaria")
Fun_Final_Day("Lower Saxony")
Fun_Final_Day("Saxony and Thuringia")
