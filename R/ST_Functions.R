# FUNCTIONS ---------------------------------------------------------------


#--Function for Contamination of Specific Item at Tray
# a = Contamination original b= Tra 1 c= Tra 2 d= Pick yes or no , e= Cont h after 
Funct_Cont_Tray_Item<-function(a,b,c,d,e){
  ifelse(((a+b)-(c))*d>e,
         e,
         ((a+b)-(c))*d)
}


#--Function for searching data frame--
# a=Data Frame looking b= Colum in data frame c= Keywrd using "", d=number of selections
Func_Search_Data<-function(a,b,c,d){
  a[ sample( which(b==c),d),]  
}

Func_seach_Data4<-function(a,b,c,d){
  subset<-a[which(b==c),]
  subset<-head(subset,n=d)
  sample_n(subset,1)
}

#Items touched during selection: 
#a = data frame, b=#touched c#"contamination" col name
Func_Index_DF<-function(a,b,c){
  as.numeric(a[b,colnames(a)==c])
}


#Function Normal
F_norm<-function(a,b,c){
  rnorm(a, b,c)
}


# Log Reduction -----------------------------------------------------------

Func_Logred<-function(a,b){
  #a ,data frame column, b log reduction
    a*(10^b)
  }


# Adding Time -------------------------------------------------------------

Func_Adding_Time<-function(Column, Time){
  #column of data frame
  #Time, time parameter that is being added
  (Column + Time)
}

#Converting to CFU/g

Func_Convert_Log<-function(DF){
  for (i in 1:nrow(DF)){
    N<-DF[i,colnames(DF)== "Contamination"]
    if(N>0){
    N<-log10(DF[i,colnames(DF)== "Contamination"])
    DF[i,colnames(DF)== "Contamination"]<-N
    }
  }
  return(DF)
}

Func_Convert_pergram<-function(DF){
  for(i in 1:nrow(DF)){
    N<-DF[i,colnames(DF)== "Contamination"]
    if(N>0){
      N<-(DF[i,colnames(DF)== "Contamination"])/Fr_Mean_weight
      DF[i,colnames(DF)== "Contamination"]<-N
    }
  }
  return(DF)
}


# Adding Initial Contamination --------------------------------------------

#this function changes contamination from CFU CM^2 to CFU/g
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
        DF<<-DF
      }
    } else if (Temp_Ref>=5){
      rate<-(b*(Temp_Ref-Tmin))^2/2.303
      for (i in 1:nrow(DF)){
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
        DF<<-DF
      }
    } else if (Temp_Ref>=5){
      rate<-(b*(Temp_Ref-Tmin))^2/2.303
      for (i in 1:nrow(DF)){
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


Func_Spoilage_YN<-function(DF){
  for (i in 1:nrow(DF)){
    N<-DF[i,colnames(DF)== "SpoilageCon"]
    if (N>Spoilage_Treshold){
      DF[i,colnames(DF)== "SpoiledYN"]<-TRUE
    }
  }
  return(DF)
}
  

#Allergen Function: 

Func_Allergen_CC<-function(DF, PickedVar){
  if(DF[PickedVar,colnames(DF)== "ExposedAllergen"] == TRUE ){
    Cont_Student_Allergen_YN<-1
  } else if(DF[PickedVar,colnames(DF)== "ExposedAllergen"] == FALSE && Cont_Student_Allergen_YN == 1){
    DF[PickedVar,colnames(DF)== "ExposedAllergen"]<- TRUE
  }
  return(DF)
}



