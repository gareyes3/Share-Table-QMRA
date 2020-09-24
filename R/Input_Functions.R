#INPUT FUNCTIONS ============================================================================= 

# Function Contamination in Student Hands ------------------------------------------


Func_ICont_Student<-function(){
  #Salmonella
  if(salmonella ==1){
    IC_Student<-8.9*10^6  #CFU/Hand
    return(IC_Student)
  }
  #Norovirus
  if(norovirus ==1){
    mass_feces_hands<- rbetagen(1,4.57,2.55,-8.00,-1.00) #log(g/hands)
    HU_NV_in_Feces<- rlnormTrunc(1,6.65,2.06,0.0,10.98) #log HuNov CG/ g
    Genomic_copies_per_PFU<-rnormTrunc(1,3.65,.98,2.00,5.40)
    Personal_Contamination<-((10^mass_feces_hands) * (10^HU_NV_in_Feces))/(10^Genomic_copies_per_PFU) #PFU/Hand
    IC_Student<- Personal_Contamination #PFU/Hand
    return(IC_Student)
  }
}





# Initial Contamination Functions --------------------------------------------

#this function changes contamination from CFU CM^2 to CFU/g
#NOTE: Not being USED Right now.Here just in case 
func_Cont_Fr<-function(DF, Prevalence, area_av , area_sd, logContamination, weight_av, weight_sd ){
  #Df= Data frame
  #Prevalence = parameter Prevalence of pathogen
  # Contamination = Initial Contamination of the pathogen. 
  # parameter for volume_av
  # parameter for volume_sd
  # parameter for log contamination
  for(i in 1:nrow(DF)){
    Fr_Cont_YN<- ifelse(runif(1)<Prevalence,1,0) 
    Fr_Area<-rnorm(1,area_av,area_sd)
    Fr_Weight<-rnorm(1,weight_av,weight_sd)
    Contamination<-(10^(logContamination)* Fr_Area)/(Fr_Weight)
    if(Fr_Cont_YN==1){
      DF[i,colnames(DF)== "Contamination"]<-Contamination
    } else if (Fr_Cont_YN==0){
      DF[i,colnames(DF)== "Contamination"]<-0
    }
  }
  return(DF)
}

#Function that adds contminations to Data frames and converts from CFU/cm^2 to CFU/Apple or item
func_Cont_cm2<-function(DF, Prevalence, logContamination, Fr_Mean_area ){
  #Df= Data frame
  #Prevalence = parameter Prevalence of pathogen
  # Contamination = Initial Contamination of the pathogen. 
  # parameter for log contamination
  for(i in 1:nrow(DF)){
    Fr_Cont_YN<- ifelse(runif(1)<Prevalence,1,0) 
    Contamination<-10^(logContamination)* Fr_Mean_area
    if(Fr_Cont_YN==1){
      DF[i,colnames(DF)== "Contamination"]<-Contamination
    } else if (Fr_Cont_YN==0){
      DF[i,colnames(DF)== "Contamination"]<-as.numeric(0)
    }
  }
  return(DF)
}

#Spcial Function that adds norovirus to fruit items

func_Cont_HuNoV_Fr<-function(DF, Prevalence){
  for (i in 1:nrow(DF)){
    Fr_Cont_YN<- ifelse(runif(1)<Prevalence,1,0)
    Genomic_copies_per_PFU<-rnormTrunc(1,3.65,.98,2.00,5.40)
    HuNoV_ContFruit<-rlnormTrunc(1,2.38,3.52, 0,6.97) #log HuNoV copies per/ g
    Contamination<-(10^HuNoV_ContFruit)/(10^Genomic_copies_per_PFU) *Fr_Mean_weight #PFU/Apple
    if(Fr_Cont_YN==1){
      DF[i,colnames(DF)== "Contamination"]<-Contamination
    } else if (Fr_Cont_YN==0){
      DF[i,colnames(DF)== "Contamination"]<-as.numeric(0)
    }
  }
  return(DF)
}

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