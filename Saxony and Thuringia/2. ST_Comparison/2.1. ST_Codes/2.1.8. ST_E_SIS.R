#1# Read data
## Transfer Probability
ST_Trans_Pr<-list()
for (i in 1:7) {
  ST_Trans_Pr[[i]]<-as.matrix(read.table(paste0("E:/Work/Saxony and Thuringia/2. ST_Comparison/2.2. ST_Data/2.2.6. ST_Transfer_Probability Data/ST_Trans_Pr", i%%7, ".txt")))
}
names(ST_Trans_Pr)<-c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")
## Total number of patients in every node every weekday
setwd("E:/Work/Saxony and Thuringia/2. ST_Comparison/2.2. ST_Data/2.2.8. ST_Hospital_Community_Based_Model Data/2.2.8.1. ST_Modelling_Preparation")
ST_WD_N<-as.matrix(read.table("ST_WD_N.txt"))
colnames(ST_WD_N)<-ST_WD_N[1,]
ST_WD_N<-ST_WD_N[-1,]

#2# Function for epidemic simulation
## Initial parameter settings
#### Initial proportion of infectious patients in each node
Prop_start<-rep(c(0.005, 0.0005), each = dim(ST_WD_N)[1]/2)
#### Start transmission rate
T_rate_0<-0.03
#### Start recovery rate
R_rate_0<-1/365
## Modelling
#### Calculate the I on day d (after infection process in last hospital and transfers on day d-1. After transfer on day d-1 is the next day's number.)
Fun_epi_d<-function(I,N,d){
  ###### Divide the N of hospitals and communities into two parts
  ######## N
  N_new<-N[d-1,]
  N_H<-N_new[1:(length(N_new)/2)]
  N_C<-N_new[(length(N_new)/2+1):length(N_new)]
  ######## I
  I_new<-I[d-1,]
  I_H<-I_new[1:(length(I_new)/2)]
  I_C<-I_new[(length(I_new)/2+1):length(I_new)]
  ###### Function for calculating I after infection process
  Fun_epi_I<-function(Beta,Gamma,N,I){
    if(Beta!=0){
      I_infinite<-(1-Gamma/Beta)*N
      V<-I_infinite/I-1
      X<-Beta-Gamma
      ###### Infection process result
      I_epi<-I_infinite/(1+V*exp(-X*1))
      I_epi[I_epi<0]<-0
      I_epi[which(I_epi>N)]<-N[which(I_epi>N)]
    }else{
      I_epi<-I/exp(Gamma*1)
    }
    return(I_epi)
  }
  ###### Weekday calculation
  if ((d-1)%%7==0) {
    WD_minus<-7
    WD<-1
  }else{
    WD_minus<-(d-1)%%7
    WD<-WD_minus+1
  }
  ###### N on day d
  N_d<-as.numeric(ST_WD_N[,WD])
  ###### I on day d
  I_d<-c(Fun_epi_I(T_rate_H,R_rate,N_H,I_H),Fun_epi_I(T_rate_C,R_rate,N_C,I_C))%*%ST_Trans_Pr[[WD_minus]]
  ###### S on day d
  S_d<-N_d-I_d
  #### Results
  return(list(I_d,N_d,S_d))
}
## Set the modelling parameters
#### n_T, n_R and n_C are the tunning parameters for transmission rate in hospitals, recovery rate, transmission rate in community with times of the one in hospitals (if keeping the original parameter settings, then n_T,n_R=1, n_C=0)
Fun_Modelling_INS<-function(n_T,n_R,n_C,I,N,S){
  for (d in 2:(365*50)) {
    INS<-Fun_epi_d(I,N,d)
    I<-rbind(I,INS[[1]])
    N<-rbind(N,INS[[2]])
    S<-rbind(S,INS[[3]])
  }
  return(list(t(I),t(N),t(S)))
}

#3# Results
## Initial value with first N and I
N<-t(as.numeric(ST_WD_N[,1]))
I<-N*Prop_start
S<-N-I
## Varying n_T
for (n_T in seq(0.5,5,0.5)) {
  #### Parameters
  n_R<-1
  n_C<-0
  T_rate_H<-n_T*T_rate_0
  T_rate_C<-n_C*T_rate_0
  R_rate<-n_R*R_rate_0
  #### Model
  INS<-Fun_Modelling_INS(n_T,n_R,n_C,I,N,S)
  setwd("E:/Work/Saxony and Thuringia/2. ST_Comparison/2.2. ST_Data/2.2.8. ST_Hospital_Community_Based_Model Data/2.2.8.2. ST_Modelling_Results")
  write.table(INS[[1]],paste0("ST_I_", n_T, "_", n_R, "_", n_C,".txt"))
  write.table(INS[[2]],paste0("ST_N_", n_T, "_", n_R, "_", n_C,".txt"))
  write.table(INS[[3]],paste0("ST_S_", n_T, "_", n_R, "_", n_C,".txt"))
}

## Varying n_R
for (n_R in seq(0.5,5,0.5)) {
  #### Parameters
  n_T<-1
  n_C<-0
  T_rate_H<-n_T*T_rate_0
  T_rate_C<-n_C*T_rate_0
  R_rate<-n_R*R_rate_0
  #### Model
  INS<-Fun_Modelling_INS(n_T,n_R,n_C,I,N,S)
  setwd("E:/Work/Saxony and Thuringia/2. ST_Comparison/2.2. ST_Data/2.2.8. ST_Hospital_Community_Based_Model Data/2.2.8.2. ST_Modelling_Results")
  write.table(INS[[1]],paste0("ST_I_", n_T, "_", n_R, "_", n_C,".txt"))
  write.table(INS[[2]],paste0("ST_N_", n_T, "_", n_R, "_", n_C,".txt"))
  write.table(INS[[3]],paste0("ST_S_", n_T, "_", n_R, "_", n_C,".txt"))
}

## Varying n_C
for (n_C in seq(0,0.5,0.05)) {
  #### Parameters
  n_T<-1
  n_R<-1
  T_rate_H<-n_T*T_rate_0
  T_rate_C<-n_C*T_rate_0
  R_rate<-n_R*R_rate_0
  #### Model
  INS<-Fun_Modelling_INS(n_T,n_R,n_C,I,N,S)
  setwd("E:/Work/Saxony and Thuringia/2. ST_Comparison/2.2. ST_Data/2.2.8. ST_Hospital_Community_Based_Model Data/2.2.8.2. ST_Modelling_Results")
  write.table(INS[[1]],paste0("ST_I_", n_T, "_", n_R, "_", n_C,".txt"))
  write.table(INS[[2]],paste0("ST_N_", n_T, "_", n_R, "_", n_C,".txt"))
  write.table(INS[[3]],paste0("ST_S_", n_T, "_", n_R, "_", n_C,".txt"))
}

I<-as.matrix(read.table("ST_I_5_1_0.txt"))
N<-as.matrix(read.table("ST_N_5_1_0.txt"))
plot(colSums(I[1:126,])/colSums(N[1:126,]))
