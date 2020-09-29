Input_Growth_Norovirus_Plastic<-list(
  b = 137.74,
  n=0.50,
  N0 = 
)

#growht norovirus in plastic
Func_Growth_Sto_Norovirus_Plastic<-function(Condition,DF,TimeVar){
  b<-137.74
  n<-.50
  N0<-0
  if(Condition== "room temp"){
    for (i in 1:nrow(DF)){
      Growth<- -(TimeVar/b)^n +N0
      N<-log10(DF[i,colnames(DF)== "Contamination"])
      Con_Final<-ifelse(N==0,N,N + Growth)
      Con_Final<-10^Con_Final
      DF[i,colnames(DF)== "Contamination"]<-Con_Final
    }
  } 
  return(DF)
}  

#Growth norovirus in fruit
Func_Growth_Sto_Norovirus<-function(Condition,DF,TimeVar){
  if(Condition== "room temp"){
    f<-0
    b1<--416
    b2<-47.33
    for (i in 1:nrow(DF)){
      TimeVar<-TimeVar/24
      Growth<-log10((2*f/(1+exp(b1*TimeVar)))+(2*(1-f)/(1+exp(b2*TimeVar))))
      N<-log10(DF[i,colnames(DF)== "Contamination"])
      Con_Final<-ifelse(N==0,N,N + Growth)
      Con_Final<-10^Con_Final
      DF[i,colnames(DF)== "Contamination"]<-Con_Final
    }
    return(DF)
  } else if (Condition == "refrigerated"){
    f<-0
    b1<--.08
    b2<-4.63
    for (i in 1:nrow(DF)){
      TimeVar<-TimeVar/24
      Growth<-log10((2*f/(1+exp(b1*TimeVar)))+(2*(1-f)/(1+exp(b2*TimeVar))))
      N<-log10(DF[i,colnames(DF)== "Contamination"])
      Con_Final<-ifelse(N==0,N,N + Growth)
      Con_Final<-10^Con_Final
      DF[i,colnames(DF)== "Contamination"]<-Con_Final
    }
    return(DF)
  }
} 


do.call(Func_Growth_Sto_Ecoli,c(list(Condition=,DF=,Timevar=,),Inputs_Cont_HuNov_Fr))