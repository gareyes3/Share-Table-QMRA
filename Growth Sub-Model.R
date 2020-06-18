#352

enteric<-"E-Coli"
Condition<-c("refrigerated","room temp")

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
        } else if (Temp_Ref>=5){
        rate<-(b*(Temp_Ref-Tmin))^2/2.303
        Con_Change<-rate*Time_Ref
        Con_Final<-ifelse(N==0,N,Con_Change + N)
        Con_Final<-10^Con_Final
        }
    } else if (Condition == "room temp"){
        if(Temp_ST<5){
          Die_off<-(-k)*Time_ST
          Con_Final<-ifelse(N==0,N,Die_off + N)
          Con_Final<-10^Con_Final
        } else if (Temp_ST>=5){
          rate<-(b*(Temp_Ref-Tmin))^2/2.303
          Con_Change<-rate*Time_ST
          Con_Final<-ifelse(N==0,N,Con_Change + N)
          Con_Final<-10^Con_Final
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
      } else if (Temp_Ref>=7){
        rate<-(b*(Temp_Ref-Tmin))^2/2.303
        Con_Change<-rate*Time_Ref
        Con_Final<-ifelse(N==0,N,Con_Change + N)
        Con_Final<-10^Con_Final
      }
    } else if (Condition == "room temp"){
      if(Temp_ST<7){
        Die_off<-(-k)*Time_ST
        Con_Final<-ifelse(N==0,N,Die_off + N)
        Con_Final<-10^Con_Final
        DF[Pickedvar,colnames(DF)== "Contamination"]<-Con_Final
      } else if (Temp_ST>=7){
        rate<-(b*(Temp_Ref-Tmin))^2/2.303
        Con_Change<-rate*Time_ST
        Con_Final<-ifelse(N==0,N,Con_Change + N)
        Con_Final<-10^Con_Final
        Con_Final<<-Con_Final
      }
    }
  }
  return(Con_Final)
  }

Func_Enteric_Growth("salmonella","room temp",Fr_Data.Frame,Fr_ST_Picked)
Fr_Data.Frame[Fr_ST_Picked,colnames(Fr_Data.Frame)== "Contamination"]<-Con_Final

# E Coli  -----------------------------------------------------------

if(E_coli==1){
#INPUTS

  N<-3.5
  #Initial Contamination Log CFU/g
  b<-.023
  # Temperature coefficient from McKellar and Delaquis
  k<-rnorm(1,.013,.001)/2.303
  T<-12
  #Temperature in °C
  Tmin<-(1.17)
  #Minimum growth temperature
  rate<-(b*(T-Tmin))^2/2.303
  #Growth rate equation from Ratkowsky, coonverted to LOG CFU-1 h-1
  Time<-300
  #Time in hours

#Growth
  
  Con_Change<-rate*Time 
  #Change in contamination over the time period
  
  Con_Final<-Con_Change + N
  #Final contamination 

#Die Off
  Die_off<-(-k)*Time



#Data frame for representation (Validation of the McKellar and Delaquis FIG 7a:
  
  data<- data.frame(
    Time = 1:Time,
    Change = "",
    stringsAsFactors = FALSE
  )
  #Creation of data frame
  
  for (i in 1:nrow(data)){
    Con_Change<-rate*i
    data$Change[i]<-Con_Change
  }
  #For Loop to iterate over time
  
  plot(data$Time,data$Change)
  #Simple plot
  
#Data frame for representation (Validation of the McKellar and Delaquis FIG 7b:
  data2<- data.frame(
    Time = 1:Time,
    Change = "",
    stringsAsFactors = FALSE
  )
  #Creation of data frame
  
  for (i in 1:nrow(data)){
    Die_off<-(-k)*i
    data$Change[i]<-Die_off
  }
  #For Loop to iterate over time
  
  plot(data$Time,data$Change)
  #Simple plot
}
  
# Salmonella --------------------------------------------------------------

if(salmonella == 1){
  
  N<-log10(Func_Index_DF(Fr_Data.Frame,Fr_ST_Picked,"Contamination"))
  #Initial Contamination Log CFU/g
  b<-.020
  # Temperature coefficient from McKellar and Delaquis
  k<-.0128
  #Temperature in °C
  Tmin<-(-0.571)
  #Minimum growth temperature
  rate<-(b*(Temp_ST-Tmin))^2/2.303
  #Growth rate equation from Ratkowsky, coonverted to LOG CFU-1 h-1

                              #Growth
  if(Temp_ST>=7){
  Con_Change<-rate*Time_ST
  #Change in contamination over the time period
  
  #Final contamination 
  Con_Final<-ifelse(N==0,N,Con_Change + N)
  Con_Final<-10^Con_Final
  Fr_Data.Frame[Fr_ST_Picked,colnames(Fr_Data.Frame)== "Contamination"]<-Con_Final
  }

 if(Temp_ST<7){                            #Die Off
  Die_off<-(-k)*Time_ST
  Con_Final<-ifelse(N==0,N,Die_off+N)
  Con_Final<-10^Con_Final
  Fr_Data.Frame[Fr_ST_Picked,colnames(Fr_Data.Frame)== "Contamination"]<-Con_Final
 }
}

# NOROVIRUS ---------------------------------------------------------------

if(norovirus==1){
  N<-5000
  b<-.0854
  n<-.5
  Time<-700
  data_noro<-data.frame(
    Time= 0:Time,
    LogNt.N0= "",
    Nt = "",
    stringsAsFactors = FALSE
  )
    
  for (i in 0:Time){
      LogNt.N0<-(-b)*(i^n)
      data_noro$LogNt.N0[i]<-LogNt.N0
      nt<-(10^LogNt.N0)*N
      data_noro$Nt[i]<-nt
  }
  
  
  plot(data_noro$Time,data_noro$Nt)
  
  700^.5
  26.45751*-26.45
  
  (1/.0854)^(1/.50)
}
