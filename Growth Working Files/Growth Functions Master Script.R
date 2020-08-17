#Growth During Model
Func_Enteric_Growth<-function(enteric,Condition,DF,Pickedvar){
  if(enteric== "E_coli"){
    N<-log10(Func_Index_DF(DF,Pickedvar,"Contamination"))
    b<-.023
    k<-rnorm(1,.013,.001)/2.303
    Tmin<-(1.17)
    if(Condition== "refrigerated"){
      if(Temp_Ref<5){
        Die_off<-(-k)*Time_Ref
        Con_Final<-ifelse(N==0,N,Die_off + N)
        Con_Final<-10^Con_Final
        Con_Final<<-Con_Final
      } else if (Temp_Ref>=5){
        rate<-(b*(Temp_Ref-Tmin))^2/2.303
        Con_Change<-rate*Time_Ref
        Con_Final<-ifelse(N==0,N,Con_Change + N)
        Con_Final<-10^Con_Final
        Con_Final<<-Con_Final
      }
    } else if (Condition == "room temp"){
      if(Temp_RT<5){
        Die_off<-(-k)*Time_ST
        Con_Final<-ifelse(N==0,N,Die_off + N)
        Con_Final<-10^Con_Final
        Con_Final<<-Con_Final
      } else if (Temp_RT>=5){
        rate<-(b*(Temp_RT-Tmin))^2/2.303
        Con_Change<-rate*Time_ST
        Con_Final<-ifelse(N==0,N,Con_Change + N)
        Con_Final<-10^Con_Final
        Con_Final<<-Con_Final
      }
    }
  } else if (enteric == "salmonella" ){
    N<-log10(Func_Index_DF(DF,Pickedvar,"Contamination"))
    b<-.020
    k<-.0128
    Tmin<-(-0.571)
    if(Condition== "refrigerated"){
      if(Temp_Ref<7){
        Die_off<-(-k)*Time_Ref
        Con_Final<-ifelse(N==0,N,Die_off + N)
        Con_Final<-10^Con_Final
        Con_Final<<-Con_Final
      } else if (Temp_Ref>=7){
        rate<-(b*(Temp_Ref-Tmin))^2/2.303
        Con_Change<-rate*Time_Ref
        Con_Final<-ifelse(N==0,N,Con_Change + N)
        Con_Final<-10^Con_Final
        Con_Final<<-Con_Final
      }
    } else if (Condition == "room temp"){
      if(Temp_RT<7){
        Die_off<-(-k)*Time_ST
        Con_Final<-ifelse(N==0,N,Die_off + N)
        Con_Final<-10^Con_Final
        Con_Final<<-Con_Final
      } else if (Temp_RT>=7){
        rate<-(b*(Temp_RT-Tmin))^2/2.303
        Con_Change<-rate*Time_ST
        Con_Final<-ifelse(N==0,N,Con_Change + N)
        Con_Final<-10^Con_Final
        Con_Final<<-Con_Final
      }
    }
  }
  return(Con_Final)
}


#Growth function for all items during storage. 
Func_Enteric_Growth_Storage<-function(enteric,Condition,DF, TimeVar){
  if(enteric == "E_coli"){
    b<-.023
    k<-rnorm(1,.013,.001)/2.303
    Tmin<-(1.17)
    if(Condition== "refrigerated"){
      if(Temp_Ref<5){
        for (i in 1:nrow(DF)){
          Die_off<-(-k)*(DF[i,colnames(DF)== "Time"])
          N<-log10(DF[i,colnames(DF)== "Contamination"])
          Con_Final<-ifelse(N==0,N,N + Die_off)
          Con_Final<-10^Con_Final
          DF[i,colnames(DF)== "Contamination"]<-Con_Final
          DF<<-DF
        }
      } else if (Temp_Ref>=5){
        rate<-(b*(Temp_Ref-Tmin))^2/2.303
        Con_Change<-rate*TimeVar
        for (i in 1:nrow(DF)){
          N<-log10(DF[i,colnames(DF)== "Contamination"])
          Con_Final<-ifelse(N==0,N,N + Con_Change )
          Con_Final<-10^Con_Final
          DF[i,colnames(DF)== "Contamination"]<-Con_Final
          DF<<-DF
        }
      }
    } else if (Condition=="room temp"){
      if(Temp_RT<5){
        Die_off<-(-k)*TimeVar
        for (i in 1:nrow(DF)){
          N<-log10(DF[i,colnames(DF)== "Contamination"])
          Con_Final<-ifelse(N==0,N,N + Die_off)
          Con_Final<-10^Con_Final
          DF[i,colnames(DF)== "Contamination"]<-Con_Final
          DF<<-DF
        }
      } else if (Temp_RT>=5){
        rate<-(b*(Temp_RT-Tmin))^2/2.303
        Con_Change<-rate*TimeVar
        for (i in 1:nrow(DF)){
          N<-log10(DF[i,colnames(DF)== "Contamination"])
          Con_Final<-ifelse(N==0,N,N + Con_Change )
          Con_Final<-10^Con_Final
          DF[i,colnames(DF)== "Contamination"]<-Con_Final
          DF<<-DF
        }
      }  
    }
  }
  if(enteric == "salmonella"){
    b<-.020
    k<-.0128
    Tmin<-(-0.571)
    if(Condition== "refrigerated"){
      if(Temp_Ref<7){
        Die_off<-(-k)*TimeVar
        for (i in 1:nrow(DF)){
          N<-log10(DF[i,colnames(DF)== "Contamination"])
          Con_Final<-ifelse(N==0,N,N + Die_off)
          Con_Final<-10^Con_Final
          DF[i,colnames(DF)== "Contamination"]<-Con_Final
          DF<<-DF
        }
      } else if (Temp_Ref>=7){
        rate<-(b*(Temp_Ref-Tmin))^2/2.303
        Con_Change<-rate*TimeVar
        for (i in 1:nrow(DF)){
          N<-log10(DF[i,colnames(DF)== "Contamination"])
          Con_Final<-ifelse(N==0,N,N + Con_Change )
          Con_Final<-10^Con_Final
          DF[i,colnames(DF)== "Contamination"]<-Con_Final
          DF<<-DF
        }
      }
    } else if (Condition=="room temp"){
      if(Temp_RT<7){
        Die_off<-(-k)*TimeVar
        for (i in 1:nrow(DF)){
          N<-log10(DF[i,colnames(DF)== "Contamination"])
          Con_Final<-ifelse(N==0,N,N + Die_off)
          Con_Final<-10^Con_Final
          DF[i,colnames(DF)== "Contamination"]<-Con_Final
          DF<<-DF
        }
      } else if (Temp_RT>=7){
        rate<-(b*(Temp_RT-Tmin))^2/2.303
        Con_Change<-rate*TimeVar
        for (i in 1:nrow(DF)){
          N<-log10(DF[i,colnames(DF)== "Contamination"])
          Con_Final<-ifelse(N==0,N,N + Con_Change )
          Con_Final<-10^Con_Final
          DF[i,colnames(DF)== "Contamination"]<-Con_Final
          DF<<-DF
        }
      }  
    }
  }
}




# New Functions -----------------------------------------------------------

Func_Growth_Sto_Ecoli<-function(Condition,DF){
  b<-.023
  k<-rnorm(1,.013,.001)/2.303
  Tmin<-(1.17)
  if(Condition== "refrigerated"){
    if(Temp_Ref<5){
      for (i in 1:nrow(DF)){
        TimeVar<-(DF[i,colnames(DF)== "Time"])
        Die_off<-((-k)*TimeVar)
        N<-log10(DF[i,colnames(DF)== "Contamination"])
        Con_Final<-ifelse(N==0,N,N + Die_off)
        Con_Final<-10^Con_Final
        DF[i,colnames(DF)== "Contamination"]<-Con_Final
        DF<<-DF
      }
    } else if (Temp_Ref>=5){
      rate<-(b*(Temp_Ref-Tmin))^2/2.303
      for (i in 1:nrow(DF)){
        TimeVar<-(DF[i,colnames(DF)== "Time"])
        Con_Change<-rate*TimeVar
        N<-log10(DF[i,colnames(DF)== "Contamination"])
        Con_Final<-ifelse(N==0,N,N + Con_Change )
        Con_Final<-10^Con_Final
        DF[i,colnames(DF)== "Contamination"]<-Con_Final
        DF<<-DF
      }
    }
  } else if (Condition=="room temp"){
    if(Temp_RT<5){
      for (i in 1:nrow(DF)){
        TimeVar<-(DF[i,colnames(DF)== "Time"])
        Die_off<-(-k)*TimeVar
        N<-log10(DF[i,colnames(DF)== "Contamination"])
        Con_Final<-ifelse(N==0,N,N + Die_off)
        Con_Final<-10^Con_Final
        DF[i,colnames(DF)== "Contamination"]<-Con_Final
        DF<<-DF
      }
    } else if (Temp_RT>=5){
      rate<-(b*(Temp_RT-Tmin))^2/2.303
      for (i in 1:nrow(DF)){
        TimeVar<-(DF[i,colnames(DF)== "Time"])
        Con_Change<-rate*TimeVar
        N<-log10(DF[i,colnames(DF)== "Contamination"])
        Con_Final<-ifelse(N==0,N,N + Con_Change )
        Con_Final<-10^Con_Final
        DF[i,colnames(DF)== "Contamination"]<-Con_Final
        DF<<-DF
      }
    }  
  }
}


# Norovirus ---------------------------------------------------------------

N0<-3 #Initial contamination 
b<-137.74 #hours to reduce one log
n<-.50 #Shape parameter
t<-700 #time in hours
DFF<-data.frame(
  "Time"= 1:t,
  "Log" = "",
  stringsAsFactors = FALSE
)

for(i in 1:t){
  Timevar<-(DF[i,colnames(DF)== "Time"])
  log_NT<- -(Timevar/b)^n +N0
  DFF[i,colnames(DFF)== "Log"]<-log_NT
}

plot(DFF$Time,DFF$Log)

#Plastic Storage 
Func_Growth_Sto_Norovirus_Plastic<-function(Condition,DF,TimeVar){
  b<-137.74
  n<-.50
  N0<-0
  if(Condition== "room temp"){
      for (i in 1:nrow(DF)){
        Growth<- -(Timevar/b)^n +N0
        N<-log10(DF[i,colnames(DF)== "Contamination"])
        Con_Final<-ifelse(N==0,N,N + Growth)
        Con_Final<-10^Con_Final
        DF[i,colnames(DF)== "Contamination"]<-Con_Final
        DF<<-DF
    }
  } 
}  



f<-0
N0<-0
b1<--416
b2<-47.33
t<-1

logNT<-N0+ log10((2*f/(1+exp(b1*t)))+(2*(1-f)/(1+exp(b2*t)))) 

#Green peppers at 50% RH

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
      DF<<-DF
    }
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
      DF<<-DF
    }
  }
}  


