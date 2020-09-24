#Functions for Growth Models

# Growth Model for Enteric -------------------------------------------------------------


Func_Growth_Sto_Ecoli<-function(Condition,DF,TimeVar){
  b<-.023
  k<-rnorm(1,.013,.001)/2.303
  Tmin<-(1.17)
  if(Condition== "refrigerated"){
    if(Temp_Ref<5){
      for (i in 1:nrow(DF)){
        Die_off<-((-k)*TimeVar)
        N<-log10(DF[i,colnames(DF)== "Contamination"])
        Con_Final<-ifelse(N==0,N,N + Die_off)
        Con_Final<-10^Con_Final
        DF[i,colnames(DF)== "Contamination"]<-Con_Final
        return(DF)
      }
    } else if (Temp_Ref>=5){
      rate<-(b*(Temp_Ref-Tmin))^2/2.303
      for (i in 1:nrow(DF)){
        Con_Change<-rate*TimeVar
        N<-log10(DF[i,colnames(DF)== "Contamination"])
        Con_Final<-ifelse(N==0,N,N + Con_Change )
        Con_Final<-10^Con_Final
        DF[i,colnames(DF)== "Contamination"]<-Con_Final
        return(DF)
      }
    }
  } else if (Condition=="room temp"){
    if(Temp_RT<5){
      for (i in 1:nrow(DF)){
        Die_off<-(-k)*TimeVar
        N<-log10(DF[i,colnames(DF)== "Contamination"])
        Con_Final<-ifelse(N==0,N,N + Die_off)
        Con_Final<-10^Con_Final
        DF[i,colnames(DF)== "Contamination"]<-Con_Final
        return(DF)
      }
    } else if (Temp_RT>=5){
      rate<-(b*(Temp_RT-Tmin))^2/2.303
      for (i in 1:nrow(DF)){
        Con_Change<-rate*TimeVar
        N<-log10(DF[i,colnames(DF)== "Contamination"])
        Con_Final<-ifelse(N==0,N,N + Con_Change )
        Con_Final<-10^Con_Final
        DF[i,colnames(DF)== "Contamination"]<-Con_Final
        return(DF)
      }
    }  
  }
}


Func_Growth_Sto_Salmonella<-function(Condition,DF,TimeVar){
  b<-.020
  k<-.0128/2.303
  Tmin<-(-0.571)
  if(Condition== "refrigerated"){
    if(Temp_Ref<5){
      for (i in 1:nrow(DF)){
        Die_off<-((-k)*TimeVar)
        N<-log10(DF[i,colnames(DF)== "Contamination"])
        Con_Final<-ifelse(N==0,N,N + Die_off)
        Con_Final<-10^Con_Final
        DF[i,colnames(DF)== "Contamination"]<-Con_Final
        return(DF)
      }
    } else if (Temp_Ref>=5){
      rate<-(b*(Temp_Ref-Tmin))^2/2.303
      for (i in 1:nrow(DF)){
        Con_Change<-rate*TimeVar
        N<-log10(DF[i,colnames(DF)== "Contamination"])
        Con_Final<-ifelse(N==0,N,N + Con_Change )
        Con_Final<-10^Con_Final
        DF[i,colnames(DF)== "Contamination"]<-Con_Final
        return(DF)
      }
    }
  } else if (Condition=="room temp"){
    if(Temp_RT<5){
      for (i in 1:nrow(DF)){
        Die_off<-(-k)*TimeVar
        N<-log10(DF[i,colnames(DF)== "Contamination"])
        Con_Final<-ifelse(N==0,N,N + Die_off)
        Con_Final<-10^Con_Final
        DF[i,colnames(DF)== "Contamination"]<-Con_Final
        return(DF)
      }
    } else if (Temp_RT>=5){
      rate<-(b*(Temp_RT-Tmin))^2/2.303
      for (i in 1:nrow(DF)){
        Con_Change<-rate*TimeVar
        N<-log10(DF[i,colnames(DF)== "Contamination"])
        Con_Final<-ifelse(N==0,N,N + Con_Change )
        Con_Final<-10^Con_Final
        DF[i,colnames(DF)== "Contamination"]<-Con_Final
        return(DF)
      }
    }  
  }
}



# Growth Model For Norovirus ----------------------------------------------

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


# Spoilage of Organisms. ----------------------------------------------

Func_Growth_Milk_Spoilage<-function(Temp,DF,TimeVar){
  b<-.03772
  Tmin<-(-6.1)
  Tmax<-(41.2)
  c<-.1719
  k<-(b*(Temp-Tmin)*(1-exp(c*(Temp-Tmax))))^2
  for (i in 1:nrow(DF)){
    Growth<-TimeVar*k
    N<-DF[i,colnames(DF)== "SpoilageCon"]
    Con_Final<-N + Growth
    DF[i,colnames(DF)== "SpoilageCon"]<-as.numeric(Con_Final)
  }
  return(DF)
} 