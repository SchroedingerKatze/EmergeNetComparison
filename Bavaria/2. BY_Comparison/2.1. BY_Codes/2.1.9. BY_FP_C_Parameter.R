#1# Basic environment
## The first weekday of Bavaria is Friday! (Check the location data)
BY_WD_0<-1

#2# Function for calculating the daily final prevalence, para_list is the parameter list of n_T, n_R, n_C's all possible values
Fun_F_D_Pr<-function(para_list){
  #### location of the changing parameter
  c_loc<-which(lengths(para_list)>1)
  #### Calculate the weekly final prevalence
  ###### Hospital prevalence
  H_Pr<-matrix(0, nrow = 7, ncol = max(lengths(para_list)))
  ###### Community prevalence
  C_Pr<-matrix(0, nrow = 7, ncol = max(lengths(para_list)))
  #### Set row names as weekdays and column names as T_prop
  dimnames(H_Pr)<-list(0:6, para_list[[c_loc]])
  dimnames(C_Pr)<-list(0:6, para_list[[c_loc]])
  for(c in para_list[[c_loc]]){
    #### Create the parameter list for single changing parameter c
    c_para<-para_list
    c_para[[c_loc]]<-c
    #### Parameters
    n_T<-c_para[[1]]
    n_R<-c_para[[2]]
    n_C<-c_para[[3]]
    #### Read data
    setwd("E:/Work/Bavaria/2. BY_Comparison/2.2. BY_Data/2.2.8. BY_Hospital_Community_Based_Model Data/2.2.8.2. BY_Modelling_Results")
    BY_I_Mat<-as.matrix(read.table(paste0("BY_I_",n_T,"_",n_R,"_",n_C,".txt")))
    BY_N_Mat<-as.matrix(read.table(paste0("BY_N_",n_T,"_",n_R,"_",n_C,".txt")))
    #### Each run is the final prevalence on single weekday t
    for (t in dim(BY_I_Mat)[2]:(dim(BY_I_Mat)[2]-6)) {
      H_Pr[as.character((t+(BY_WD_0-1))%%7), as.character(c)]<-sum(BY_I_Mat[1:(dim(BY_I_Mat)[1]/2),t])/sum(BY_N_Mat[1:(dim(BY_N_Mat)[1]/2),t])
      C_Pr[as.character((t+(BY_WD_0-1))%%7), as.character(c)]<-sum(BY_I_Mat[(dim(BY_I_Mat)[1]/2+1):dim(BY_I_Mat)[1],t])/sum(BY_N_Mat[(dim(BY_N_Mat)[1]/2+1):dim(BY_N_Mat)[1],t])
    }
  }
  #### Final Pr
  setwd("E:/Work/Bavaria/2. BY_Comparison/2.2. BY_Data/2.2.8. BY_Hospital_Community_Based_Model Data/2.2.8.3. BY_FP_CParameters")
  Pr<-list(H_Pr,C_Pr)
  names(Pr)<-c("Hospital FP", "Community FP")
  return(Pr)
}  

#3# Save data
## Changing n_T
Pr_n_T<-Fun_F_D_Pr(list(seq(0.5,5,0.5),1,0))
write.table(Pr_n_T[["Hospital FP"]], file="BY_H_FP_n_T.txt", row.names=T, col.names=T)
write.table(Pr_n_T[["Community FP"]], file="BY_C_FP_n_T.txt", row.names=T, col.names=T)
## Changing n_R
Pr_n_R<-Fun_F_D_Pr(list(1,seq(0.5,5,0.5),0))
write.table(Pr_n_R[["Hospital FP"]], file="BY_H_FP_n_R.txt", row.names=T, col.names=T)
write.table(Pr_n_R[["Community FP"]], file="BY_C_FP_n_R.txt", row.names=T, col.names=T)
## Changing n_C
Pr_n_C<-Fun_F_D_Pr(list(1,1,seq(0,0.5,0.05)))
write.table(Pr_n_C[["Hospital FP"]], file="BY_H_FP_n_C.txt", row.names=T, col.names=T)
write.table(Pr_n_C[["Community FP"]], file="BY_C_FP_n_C.txt", row.names=T, col.names=T)
