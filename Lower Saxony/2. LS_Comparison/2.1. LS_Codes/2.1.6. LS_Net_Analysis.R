#1 Read data
LS_Hosp<-read.csv("E:/Work/Lower Saxony/2. LS_Comparison/2.2. LS_Data/2.2.1. LS_Basic Data/LS_Hosp_C.csv", header = T)
LS_Hosp$A_DAT<-as.Date(LS_Hosp$A_DAT)
LS_Hosp$D_DAT<-as.Date(LS_Hosp$D_DAT)
LS_Hosp<-LS_Hosp[order(LS_Hosp$P_ID, LS_Hosp$A_DAT, LS_Hosp$D_DAT),]
LS_Hsize<-read.csv("E:/Work/Lower Saxony/2. LS_Comparison/2.2. LS_Data/2.2.5. LS_Hospital_Size Data/LS_Hospital_Size.csv", header=T)
## Identify the patient transfers
bool<-LS_Hosp$P_ID[1:(dim(LS_Hosp)[1]-1)]==LS_Hosp$P_ID[2:dim(LS_Hosp)[1]]

#2 Patient transfer
Fun_matrix<-function(n){
  #1# n means how many times of 0.5/365
  Gamma<-n*(0.5/365)
  
  #2# Create patient transfer data
  LS_Trans<-data.frame(LS_Hosp[c(bool, F),], LS_Hosp[c(F, bool),])
  LS_Trans$Time_bw<-as.numeric(LS_Trans$A_DAT.1-LS_Trans$D_DAT-1)
  ## Probability of each link transmission
  LS_Trans$Pr_Infection<-exp(-Gamma*LS_Trans$Time_bw)
  LS_Htrans<-LS_Trans[, c(5, 15, 10, 22)]
  LS_Htrans<-LS_Htrans[order(LS_Htrans$H_ID, LS_Htrans$H_ID.1),]
  LS_Htrans$Pr_Sum<-LS_Htrans$Pr_Infection*LS_Htrans$Resp
  
  #3# Admatrix Dataframe
  ## Find the locations of same hospital transfers in LS_Htrans. The same hospital transfers start from 0 until next 0 begins.
  LS_Htrans$bool<-as.numeric(c(F, (LS_Htrans$H_ID[1:(dim(LS_Htrans)[1]-1)]==LS_Htrans$H_ID[2:dim(LS_Htrans)[1]])&(LS_Htrans$H_ID.1[1:(dim(LS_Htrans)[1]-1)]==LS_Htrans$H_ID.1[2:dim(LS_Htrans)[1]])))
  names(LS_Htrans)[1:2]<-c("From", "To")
  ## Create dataframe for Admatrix
  LS_Ad_df<-LS_Htrans[LS_Htrans$bool == 0, 1:2]
  ## Transfer frequency calculation Function
  #### Locations of 0 in LS_Htrans showing the start locations of the same hospital transfers
  Location_s<-which(LS_Htrans$bool==0)
  #### Locations of the end location of the same transfers
  Location_e<-c(Location_s[-1]-1, dim(LS_Htrans)[1])
  #### Function of adding the number of transfers from start to end location in a vector together
  Fun_add<-function(s,e,V){
    return(sum(V[s:e]))
  }
  ## Calculate the link weights
  #### Number of transfers
  for (i in 1:length(Location_s)) {
    LS_Ad_df$TransNr[i]<-Fun_add(Location_s[i], Location_e[i], LS_Htrans$Resp)
    LS_Ad_df$I_Contribution[i]<-Fun_add(Location_s[i], Location_e[i], LS_Htrans$Pr_Sum)
  }
  
  #4# Create Adjacency Matrix
  ## Transfer frequency adjacency matrix
  LS_Admatrix<-matrix(0, nrow = length(unique(LS_Hosp$H_ID)), ncol = length(unique(LS_Hosp$H_ID)))
  for (i in 1:dim(LS_Ad_df)[1]) {
    LS_Admatrix[LS_Ad_df$From[i], LS_Ad_df$To[i]]<-LS_Ad_df$TransNr[i]
  }
  ## Matrix elements are presented as number of transfers per day
  LS_Admatrix<-LS_Admatrix/as.numeric(max(LS_Hosp$D_DAT)-min(LS_Hosp$A_DAT))
  ## Adjacency matrix built by weighting the transmission contribution
  LS_TC_Admatrix<-matrix(0, nrow = length(unique(LS_Hosp$H_ID)), ncol = length(unique(LS_Hosp$H_ID)))
  for (i in 1:dim(LS_Ad_df)[1]) {
    LS_TC_Admatrix[LS_Ad_df$From[i], LS_Ad_df$To[i]]<-LS_Ad_df$I_Contribution[i]
  }
  ## Matrix elements are presented as transmission contribution per day
  LS_TC_Admatrix<-LS_TC_Admatrix/as.numeric(max(LS_Hosp$D_DAT)-min(LS_Hosp$A_DAT)+1)
  dimnames(LS_TC_Admatrix)<-list(LS_Hsize$H_S,LS_Hsize$H_S)
  
  #5# Calculate Strengths for single hospitals
  ## Read data
  setwd("E:/Work/Lower Saxony/2. LS_Comparison/2.2. LS_Data/2.2.3. LS_Adjacency_Matrix Data")
  TMatrix<-LS_TC_Admatrix
  ## Strengths
  #### Self transfers
  Self<-diag(TMatrix)
  #### In and out
  diag(TMatrix)<-0
  ###### Function for in
  Fun_S_in<-function(size){
    Mat<-TMatrix[which(rownames(TMatrix)==size),]
    return(colSums(Mat))
  }
  ###### Function for out
  Fun_S_out<-function(size){
    Mat<-TMatrix[,which(colnames(TMatrix)==size)]
    return(rowSums(Mat))
  }
  ## Create dataframe for in- and out-strengths
  Strength_in<-data.frame(H_Rank=rep(1:dim(TMatrix)[1],4),Strength=c(Self,unlist(mapply(Fun_S_in,c("L","M","S"),SIMPLIFY=F))),S_H=rep(c("Self","L","M","S"),each=dim(TMatrix)[1]))
  Strength_out<-data.frame(H_Rank=rep(1:dim(TMatrix)[1],4),Strength=c(Self,unlist(mapply(Fun_S_out,c("L","M","S"),SIMPLIFY=F))),T_H=rep(c("Self","L","M","S"),each=dim(TMatrix)[1]))
  
  #6# Calculate strengths for hospital groups
  TMatrix<-LS_TC_Admatrix
  ## Total In- and out strengths
  #### self
  Self_total<-sum(diag(TMatrix))
  diag(TMatrix)<-0
  #### in
  S_in<-colSums(TMatrix)
  S_total_in<-c(Self_total,sum(S_in[which(names(S_in)=="L")]),sum(S_in[which(names(S_in)=="M")]),sum(S_in[which(names(S_in)=="S")]))
  #### out
  S_out<-rowSums(TMatrix)
  S_total_out<-c(Self_total,sum(S_out[which(names(S_out)=="L")]),sum(S_out[which(names(S_out)=="M")]),sum(S_out[which(names(S_out)=="S")]))
  
  #7# Save Matrices
  setwd("E:/Work/Lower Saxony/2. LS_Comparison/2.2. LS_Data/2.2.3. LS_Adjacency_Matrix Data")
  # write.table(LS_Admatrix, file = "LS_Admatrix.txt", row.names = F, col.names = F)
  write.table(LS_TC_Admatrix, file = paste0("LS_TC_Admatrix", n, ".txt"), row.names = F, col.names = F)
  ## Save strengths
  setwd("E:/Work/Lower Saxony/2. LS_Comparison/2.2. LS_Data/2.2.14. LS_Strength_Distribution Data")
  write.csv(Strength_in, file = paste0("LS_Strength_in", n, ".csv"),row.names = F)
  write.csv(Strength_out, file = paste0("LS_Strength_out", n, ".csv"),row.names = F)
  
  #8# Return in- and out-strengths in total
  return(list(S_total_in,S_total_out))
}

#3 Generate strengths for different hospital sizes
setwd("E:/Work/Lower Saxony/2. LS_Comparison/2.2. LS_Data/2.2.14. LS_Strength_Distribution Data")
Fun_Size_Strength<-function(Size, Direction)
{
  Strength_total<-data.frame(n = rep(0:300, each = 4), S_H = rep(c("Self", "L", "M", "S"), 301), Strength = NA)
  for (n in 0:300) {
    Strength<-read.csv(paste0("LS_Strength_", Direction, n, ".csv"))
    Strength<-Strength[Strength$H_Rank %in% LS_Hsize$Rank[LS_Hsize$H_S==Size],]
    for (S_H in c("Self", "L", "M", "S")) {
      Strength_total$Strength[Strength_total$n == n & Strength_total$S_H == S_H]<-mean(Strength$Strength[Strength$S_H == S_H])
    }
  }
  write.csv(Strength_total, paste0("LS_Strength_", Size, "_", Direction, ".csv"))
}
## Write files
Fun_Size_Strength("L", "in")
Fun_Size_Strength("L", "out")
Fun_Size_Strength("M", "in")
Fun_Size_Strength("M", "out")
Fun_Size_Strength("S", "in")
Fun_Size_Strength("S", "out")

#4 Generate matrices and strengths
## Strength total data frame
Strength_total_in<-NULL
Strength_total_out<-NULL
for (i in 0:300) {
  Strength<-Fun_matrix(i)
  ## in
  Strength_total_in_i<-data.frame(n=i,Strength=Strength[[1]],S_Hgroup=c("Self","L","M","S"))
  Strength_total_in<-rbind(Strength_total_in,Strength_total_in_i)
  ## out
  Strength_total_out_i<-data.frame(n=i,Strength=Strength[[2]],T_Hgroup=c("Self","L","M","S"))
  Strength_total_out<-rbind(Strength_total_out,Strength_total_out_i)
}
## Save total strength files
setwd("E:/Work/Lower Saxony/2. LS_Comparison/2.2. LS_Data/2.2.14. LS_Strength_Distribution Data")
write.csv(Strength_total_in, file = "LS_Strength_total_in.csv",row.names = F)
write.csv(Strength_total_out, file = "LS_Strength_total_out.csv",row.names = F)