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


