#INPUT FUNCTIONS ============================================================================= 

# Function Contamination in Student Hands ------------------------------------------
#Inputs for this function #Inputs_ICont_Student


Func_ICont_Student<-function(IC_salmonella,mass_feces_hands,HU_NV_in_Feces,Genomic_copies_per_PFU,... ){
  #Salmonella
  if(salmonella ==1){
    IC_Student<-IC_salmonella  #CFU/Hand
    return(IC_Student)
  } 
  if(norovirus ==1){
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
      DF[i,colnames(DF)== "InContamination"]<-Contamination
    } else if (Fr_Cont_YN==0){
      DF[i,colnames(DF)== "Contamination"]<-as.numeric(0)
      DF[i,colnames(DF)== "InContamination"]<-as.numeric(0)
    }
  }
  return(DF)
}

#Special Function that adds norovirus to fruit items
#Inputs For Function: Inputs_Cont_HuNov_Fr

func_Cont_HuNoV_Fr<-function(DF, Prevalence,Genomic_copies_per_PFU,HuNoV_ContFruit){
  for (i in 1:nrow(DF)){
    Fr_Cont_YN<- ifelse(runif(1)<Prevalence,1,0)
    Contamination<-(10^HuNoV_ContFruit)/(10^Genomic_copies_per_PFU) *Fr_Mean_weight #PFU/Apple
    if(Fr_Cont_YN==1){
      DF[i,colnames(DF)== "Contamination"]<-Contamination
      DF[i,colnames(DF)== "InContamination"]<-Contamination
    } else if (Fr_Cont_YN==0){
      DF[i,colnames(DF)== "Contamination"]<-as.numeric(0)
      DF[i,colnames(DF)== "InContamination"]<-as.numeric(0)
    }
  }
  return(DF)
}

#Functions for Growth Models

# Growth Model for Enteric -------------------------------------------------------------


#Inputs for function: Inputs_Growth_Sto_Ecoli
#Inputs for function: Inputs_Growth_Sto_Salmonella

Func_Growth_Enteric<-function(Condition,DF,TimeVar,b,k,Tmin){
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
#no input necesary, no variability in inputs
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
#no need for inputs, no variability
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
#No variability

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


# Data Frame Creation Functions -------------------------------------------

#Function for creation of Data Frames Service 1 day 1. 
Fuct_DF_Initial<-function(FoodType){
  if (FoodType == "Fruit" ){
    Data_Frame<-data.frame("Apple No." = 1:Initial_Fr,
                           "ID"= paste(l,k,j,1:Initial_Fr),
                           "Location"= "Selection Table",
                           "Contamination" = as.numeric("0"),
                           "InContamination"  = as.numeric("0"),
                           "ExposedAllergen" = FALSE,
                           "TotTime"= as.numeric("0"),
                           "History" = "",
                           "TotServices"=as.numeric("0"),
                           "STtimes"= as.numeric("0"),
                           "Initial Service" = "1",
                           "Service" = j,
                           "Initial Day" = "1",
                           "Day" = k,
                           stringsAsFactors = FALSE
                           
    )
  } else if (FoodType == "Pss"){
    Data_Frame<-data.frame("Pss No." = 1:Initial_Pss,
                           "ID"= paste(l,k,j,1:Initial_Pss),
                           "Location"= "Selection Table",
                           "Contamination" = as.numeric("0"),
                           "InContamination"  = as.numeric("0"),
                           "ExposedAllergen" = FALSE,
                           "TotTime"= as.numeric("0"),
                           "History"= "",
                           "TotServices"=as.numeric("0"),
                           "STtimes"= as.numeric("0"),
                           "Initial Service" = "1", 
                           "Service" = j,
                           "Initial Day" = "1",
                           "Day" = k,
                           stringsAsFactors = FALSE
    )
  }else if (FoodType == "Pre"){
    Data_Frame<-data.frame("Pre No." = 1:Initial_Pss,
                           "ID"= paste(l,k,j,1:Initial_Pre),
                           "Location"= "Selection Table",
                           "Contamination" = as.numeric("0"),
                           "InContamination"  = as.numeric("0"),
                           "ExposedAllergen" = FALSE,
                           "SpoilageCon" = as.numeric(Initial_Spoilage_Con),
                           "SpoiledYN" = FALSE,
                           "TotTime"= as.numeric("0"),
                           "History" = "", 
                           "TotServices"=as.numeric("0"),
                           "STtimes"= as.numeric("0"),
                           "Initial Service" = "1",
                           "Service" = j,
                           "Initial Day" = "1",
                           "Day" = k,
                           stringsAsFactors = FALSE
    )
  }
  return(Data_Frame)
}

#Function for creating data frames that take into consideration food prom previouss data frames. 
Fuct_DF_Reservice<-function(FoodType){
  if (FoodType == "Fruit" ){
    Data_Frame<-data.frame("Apple No." = 1:(Initial_Fr-(No_Left_Selection_Fr)),
                           "ID"= paste(l,k,j,1:(Initial_Fr-(No_Left_Selection_Fr))),
                           "Location"= "Selection Table",
                           "Contamination" = as.numeric("0"),
                           "InContamination"  = as.numeric("0"),
                           "ExposedAllergen" = FALSE,
                           "TotTime"= as.numeric("0"),
                           "History" = "", 
                           "TotServices"=as.numeric("0"),
                           "STtimes"= as.numeric("0"),
                           "Initial Service" = j,
                           "Service" = j,
                           "Initial Day" = k,
                           "Day" = k,
                           stringsAsFactors = FALSE
                           
    )
  } else if (FoodType == "Pss"){
    Data_Frame<-data.frame("Pss No." = 1:(Initial_Pss-(No_Left_Selection_Pss)),
                           "ID"= paste(l,k,j,1:(Initial_Pss-(No_Left_Selection_Pss))),
                           "Location"= "Selection Table",
                           "Contamination" = as.numeric("0"),
                           "InContamination"  = as.numeric("0"),
                           "ExposedAllergen" = FALSE,
                           "TotTime"= as.numeric("0"),
                           "History" = "", 
                           "TotServices"=as.numeric("0"),
                           "STtimes"= as.numeric("0"),
                           "Initial Service" = j,
                           "Service" = j,
                           "Initial Day" = k,
                           "Day" = k,
                           stringsAsFactors = FALSE
                           
    )
  }else if (FoodType == "Pre"){
    Data_Frame<-data.frame("Pre No." = 1:(Initial_Pre-(No_Left_Selection_Pre)),
                           "ID"= paste(l,k,j,1:(Initial_Pre-(No_Left_Selection_Pre))),
                           "Location"= "Selection Table",
                           "Contamination" = as.numeric("0"),
                           "InContamination"  = as.numeric("0"),
                           "ExposedAllergen" = FALSE,
                           "SpoilageCon" = as.numeric(Initial_Spoilage_Con),
                           "SpoiledYN" = FALSE,
                           "TotTime"= as.numeric("0"),
                           "History" = "", 
                           "TotServices"=as.numeric("0"),
                           "STtimes"= as.numeric("0"),
                           "Initial Service" = j,
                           "Service" = j,
                           "Initial Day" = k,
                           "Day" = k,
                           stringsAsFactors = FALSE
                           
    )
  }
  return(Data_Frame)
}

#Function creating Data frame that feeds items if they run out
Fuct_DF_Feeding<-function(FoodType){
  if(FoodType == "Fruit"){
    Data_Frame<-data.frame("Apple No." = 1:Row_size_Fr,
                           "ID"= paste(l,k,j,nrow(Fr_Data.Frame)+1:Row_size_Fr),
                           "Location"= "Selection Table",
                           "Contamination" = as.numeric("0"),
                           "InContamination"  = as.numeric("0"),
                           "ExposedAllergen" = FALSE,
                           "TotTime"= as.numeric("0"),
                           "History" = "", 
                           "TotServices"=as.numeric("0"),
                           "STtimes"= as.numeric("0"),
                           "Initial Service" = "1",
                           "Service" = j,
                           "Initial Day" = "1",
                           "Day" = k,
                           stringsAsFactors = FALSE
                           
    )
  } else if (FoodType == "Pss"){
    Data_Frame<-data.frame("Pss No." = 1:Row_size_Pss,
                           "ID"= paste(l,k,j,nrow(Pss_Data.Frame)+1:Row_size_Pss),
                           "Location"= "Selection Table",
                           "Contamination" = as.numeric("0"),
                           "InContamination"  = as.numeric("0"),
                           "ExposedAllergen" = FALSE,
                           "TotTime"= as.numeric("0"),
                           "History" = "", 
                           "TotServices"=as.numeric("0"),
                           "STtimes"= as.numeric("0"),
                           "Initial Service" = "1",
                           "Service" = j,
                           "Initial Day" = "1",
                           "Day" = k,
                           stringsAsFactors = FALSE
    )
  }else if (FoodType=="Pre"){
    Data_Frame<-data.frame("Pre No." = 1:Row_size_Pre,
                           "ID"= paste(l,k,j,nrow(Pre_Data.Frame)+1:Row_size_Pre),
                           "Location"= "Selection Table",
                           "Contamination" = as.numeric("0"),
                           "InContamination"  = as.numeric("0"),
                           "ExposedAllergen" = FALSE,
                           "SpoilageCon" = as.numeric(Initial_Spoilage_Con),
                           "SpoiledYN" = FALSE,
                           "TotTime"= as.numeric("0"),
                           "History" = "", 
                           "TotServices"=as.numeric("0"),
                           "STtimes"= as.numeric("0"),
                           "Initial Service" = "1",
                           "Service" = j,
                           "Initial Day" = "1",
                           "Day" = k,
                           stringsAsFactors = FALSE
    )
  }
  return(Data_Frame)
}

#Function for adding Contamination of fruit in data frame into the vector
Func_FoodCont_Vector<-function(DF,Vector){
  Items_Added<-DF$Contamination[which(DF$Contamination>0)]
  Vector<-c(Vector,Items_Added)
  return(Vector)
}

