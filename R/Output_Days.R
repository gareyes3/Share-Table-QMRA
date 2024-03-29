
# CREATION OF DAYS DATA FRAMES ---------------------------------------------

  #Fr_DF_Day <- paste("Fr_Data_D", k, sep = "")
  #assign(Fr_DF_Day, Fr_Data)
  
  #Pss_DF_Day <- paste("Pss_Data_D", k, sep = "")
  #assign(Pss_DF_Day, Pss_Data)
  
  #Pre_DF_Day <- paste("Pre_Data_D", k, sep = "")
  #assign(Pre_DF_Day, Pre_Data)



# ITEMS LEFT ON THE LAST SERVICE ------------------------------------

  
  #Fruit that stayed in share table. 
  Left_ST_Fr<-Fr_Data.Frame[which(Fr_Data.Frame$Location == "Shared"),]
  Left_ST_Aside_Fr<-Fr_Data.Frame[which(Fr_Data.Frame$Location == "SharedAside"),]
  if(ST_Aside==1){
    Left_ST_Fr<-bind_rows(Left_ST_Fr, Left_ST_Aside_Fr)
  }else if (ST_Aside==0){
    Left_ST_Fr<-Left_ST_Fr  
  }  
  #Fruit that stayed in Selection table
  Left_Selection_Fr<-Fr_Data.Frame[which(Fr_Data.Frame$Location == "Selection Table"),]
  
  No_Left_ST_Fr<-nrow(Left_ST_Fr)
  No_Left_Selection_Fr<-nrow(Left_Selection_Fr)

  
  #Pss that stayed in share table. 
  Left_ST_Pss<-Pss_Data.Frame[which(Pss_Data.Frame$Location == "Shared"),]
  #Pss that stayed in Selection table
  Left_Selection_Pss<-Pss_Data.Frame[which(Pss_Data.Frame$Location == "Selection Table"),]
  
  No_Left_ST_Pss<-nrow(Left_ST_Pss)
  No_Left_Selection_Pss<-nrow(Left_Selection_Pss)

  #Pre that stayed in share table. 
  Left_ST_Pre<-Pre_Data.Frame[which(Pre_Data.Frame$Location == "Shared"),]
  #Pre that stayed in Selection table
  Left_Selection_Pre<-Pre_Data.Frame[which(Pre_Data.Frame$Location == "Selection Table"),]
  
  No_Left_ST_Pre<-nrow(Left_ST_Pre)
  No_Left_Selection_Pre<-nrow(Left_Selection_Pre)



# OVERNIGHT TIME TO ITEMS ----------------------------------------

  #Overnight Fruit Selection
  Left_Selection_Fr$TotTime<-Func_Adding_Time(Left_Selection_Fr$TotTime, Time_ON)
  #Overnight Fruit Selection Table
  Left_ST_Fr$TotTime<-Func_Adding_Time(Left_ST_Fr$TotTime, Time_ON)
  
  #Overnight Pss Selection
  Left_Selection_Pss$TotTime<-Func_Adding_Time(Left_Selection_Pss$TotTime, Time_ON)
  #Overnight Pss Selection Table
  Left_ST_Pss$TotTime<-Func_Adding_Time(Left_ST_Pss$TotTime, Time_ON)
  
  #Overnight Pre Selection
  Left_Selection_Pre$TotTime<-Func_Adding_Time(Left_Selection_Pre$TotTime, Time_ON)
  #Overnight Pre Selection Table
  Left_ST_Pre$TotTime<-Func_Adding_Time(Left_ST_Pre$TotTime, Time_ON)


#GROWTH OVERNIGHT STORAGE----------------------------------------------


  # Fruit -------------------------------------------------------------
  
  #Selection Items #chose which type of storage. 
    if(E_coli==1 && Growth ==1){
      #Selection Table Items
      if(No_Left_Selection_Fr>0){
       Left_Selection_Fr<-do.call(Func_Growth_Enteric,c(list(Condition="room temp",DF=Left_Selection_Fr,TimeVar=Time_ON),Inputs_Growth_Sto_Ecoli)) #Using function on left over items 
      }
      if(No_Left_ST_Fr>0){
       Left_ST_Fr<-do.call(Func_Growth_Enteric,c(list(Condition="room temp",DF=Left_ST_Fr,TimeVar=Time_ON),Inputs_Growth_Sto_Ecoli)) #using function on left over St items 
      }
      #Share Table Items
    }
  
    if(salmonella==1 && Growth ==1){
      #Selection Table Items
      if(No_Left_Selection_Fr>0){
      Left_Selection_Fr<-do.call(Func_Growth_Enteric,c(list(Condition="room temp",DF=Left_Selection_Fr,TimeVar=Time_ON),Inputs_Growth_Sto_Salmonella)) #Using function on left over items  
      }
      #Share Table Items
      if(No_Left_ST_Fr>0){
       Left_ST_Fr<-do.call(Func_Growth_Enteric,c(list(Condition="room temp",DF=Left_ST_Fr,TimeVar=Time_ON),Inputs_Growth_Sto_Salmonella)) #using function on left over St items 
      }
    }
  
    if(norovirus ==1 && Growth ==1){
      #selection table items
      if(No_Left_Selection_Fr>0){
       Left_Selection_Fr<- Func_Growth_Sto_Norovirus("refrigerated", Left_Selection_Fr, Time_ON) #using function on left over items 
      }
      if(No_Left_ST_Fr>0){
       Left_ST_Fr<- Func_Growth_Sto_Norovirus("refrigerated", Left_ST_Fr, Time_ON) #Share table left over items 
      }
    }
  

  # Pss ---------------------------------------------------------------------

  #Selection Items #chose which type of storage. 
  if(E_coli==1 && Growth_Pss ==1){
    #Selection Table Items
    if(No_Left_Selection_Pss>0){
     Left_Selection_Pss<-do.call(Func_Growth_Enteric,c(list(Condition="room temp",DF=Left_Selection_Pss,TimeVar=Time_ON),Inputs_Growth_Sto_Ecoli)) #Using function on left over items 
    }
    
    #Share Table Items
    Left_ST_Pss<-do.call(Func_Growth_Enteric,c(list(Condition="room temp",DF=Left_ST_Pss,TimeVar=Time_ON),Inputs_Growth_Sto_Ecoli)) #using function on left over St items
  }
  
  if(salmonella==1 && Growth_Pss ==1){
    #Selection Table Items
    if(No_Left_Selection_Pss>0){
      Left_Selection_Pss<-do.call(Func_Growth_Enteric,c(list(Condition="room temp",DF=Left_Selection_Pss,TimeVar=Time_ON),Inputs_Growth_Sto_Salmonella)) #Using function on left over items
    }
    #Share Table Items
    if(No_Left_ST_Pss>0){
      Left_ST_Pss<-do.call(Func_Growth_Enteric,c(list(Condition="room temp",DF=Left_ST_Pss,TimeVar=Time_ON),Inputs_Growth_Sto_Salmonella)) #using function on left over St items
    }
  }
  
  if(norovirus==1 && Growth_Pss ==1){
    #selection table items
    if(No_Left_Selection_Pss>0){
     Left_Selection_Pss<- Func_Growth_Sto_Norovirus_Plastic("room temp", Left_Selection_Pss, Time_ON) #using function on left over items 
    }
    if(No_Left_ST_Pss>0){
          Left_ST_Pss<- Func_Growth_Sto_Norovirus_Plastic("room temp", Left_ST_Pss, Time_ON) #Share table left over items
    }
  }
  

  # Pre ---------------------------------------------------------------------

  #Selection Items #chose which type of storage. 
  if(E_coli==1 && Growth_Pre ==1){
    #Selection Table Items
    if(No_Left_Selection_Pre>0){
      Left_Selection_Pre<-do.call(Func_Growth_Enteric,c(list(Condition="room temp",DF=Left_Selection_Pre,TimeVar=Time_ON),Inputs_Growth_Sto_Ecoli)) #Using function on left over items
    }
    #Share Table Items
    if(No_Left_ST_Pre>0){
      Left_ST_Pre<-do.call(Func_Growth_Enteric,c(list(Condition="room temp",DF=Left_ST_Pre,TimeVar=Time_ON),Inputs_Growth_Sto_Ecoli)) #using function on left over St items
    }
    
  }
  
  if(salmonella==1 && Growth_Pre ==1){
    #Selection Table Items
    if(No_Left_Selection_Pre>0){
      Left_Selection_Pre<-do.call(Func_Growth_Enteric,c(list(Condition="room temp",DF=Left_Selection_Pre,TimeVar=Time_ON),Inputs_Growth_Sto_Salmonella)) #Using function on left over items
    }
    
    #Share Table Items
    if(No_Left_ST_Pre>0){
     Left_ST_Pre<-do.call(Func_Growth_Enteric,c(list(Condition="room temp",DF=Left_ST_Pre,TimeVar=Time_ON),Inputs_Growth_Sto_Salmonella)) #using function on left over St items 
    }
  }
  
  if(norovirus==1 && Growth_Pre ==1){
    #selection table items
    if(No_Left_Selection_Pre>0){
      Left_Selection_Pre<-Func_Growth_Sto_Norovirus_Plastic("room temp", Left_Selection_Pre, Time_ON) #using function on left over items
    }
    #Share table items
    if(No_Left_ST_Pre>0){
     Left_ST_Pre<- Func_Growth_Sto_Norovirus_Plastic("room temp", Left_ST_Pre, Time_ON) #Share table left over items 
    }
    
  }
  
# GROWTH SPOILAGE MILK ----------------------------------------------
  
  
  #GROWTH oVERNIGHT
  if(No_Left_Selection_Pre>0){
    Left_Selection_Pre<-Func_Growth_Milk_Spoilage(Temp_Ref, Left_Selection_Pre, Time_ON)
    Left_Selection_Pre<-Func_Spoilage_YN(Left_Selection_Pre)
  }

  
  if(No_Left_ST_Pre>0){
    if( Share_Table_YN==1){
      Left_ST_Pre<-Func_Growth_Milk_Spoilage(Temp_Ref, Left_ST_Pre, Time_ON)
      Left_ST_Pre<-Func_Spoilage_YN(Left_ST_Pre)
    }
  }



  
  
# Shared Aside Joining ----------------------------------------------
  if (STtoReservice_YN == 0 && ST_Aside==1 ){
    if(No_Left_ST_Fr>0){
     Left_ST_Fr$Location<-"Shared" 
    }
    if(No_Left_ST_Pss>0){
     Left_ST_Pss$Location<-"Shared"  
    } 
    if(No_Left_ST_Pre>0){
     Left_ST_Pre$Location<-"Shared"  
    }
  }  


# WASHING FRUIT ----------------------------------------------

#Washing selection Items
if(Wash_Selection_YN_Fr==1){
  Reduction_wash<-Func_Randomize_Wash(Wash_Method = Wash_Method)
  VectorLeft_Before<-c(VectorLeft_Before,Left_Selection_Fr$Contamination)
  Left_Selection_Fr$Contamination<-Func_Logred(Left_Selection_Fr$Contamination,Reduction_wash)
  VectorLeft_After<-c(VectorLeft_After,Left_Selection_Fr$Contamination)
  Left_Selection_Fr$WashHistory<-(Left_Selection_Fr$WashHistory+1)
}

#washing share table items
if(Wash_ST_YN_Fr==1){
  Reduction_wash<-Func_Randomize_Wash(Wash_Method = Wash_Method)
  Left_ST_Fr$Contamination<-Func_Logred(Left_ST_Fr$Contamination,Reduction_wash)
  Left_ST_Fr$WashHistory<-(Left_ST_Fr$WashHistory+1)
}


# IF Reservice to ST Convert ST to Selection Table ------------------------

if (STtoReservice_YN == 1 ){
  Left_ST_Fr$Reserviced<-(Left_ST_Fr$Reserviced+1)
  Left_ST_Pss$Reserviced<-(Left_ST_Pss$Reserviced+1)
  Left_ST_Pre$Reserviced<-(Left_ST_Pre$Reserviced+1)
  
  Left_Selection_Fr<-rbind(Left_Selection_Fr,Left_ST_Fr)
  No_Left_Selection_Fr<-nrow(Left_Selection_Fr)
  Left_Selection_Pss<-rbind(Left_Selection_Pss,Left_ST_Pss)
  No_Left_Selection_Pss<-nrow(Left_Selection_Pss)
  Left_Selection_Pre<-rbind(Left_Selection_Pre,Left_ST_Pre)
  No_Left_Selection_Pre<-nrow(Left_Selection_Pre)
  if(No_Left_Selection_Fr>0){
    Left_Selection_Fr$Location<-"Selection Table" 
  }
  if(No_Left_Selection_Pss>0){
    Left_Selection_Pss$Location<-"Selection Table"
  }
  if(No_Left_Selection_Pre>0){
    Left_Selection_Pss$Location<-"Selection Table"
  }
  No_Left_Selection_Fr<-nrow(Left_Selection_Fr)
  No_Left_Selection_Pss<-nrow(Left_Selection_Pss)
  No_Left_Selection_Pre<-nrow(Left_Selection_Pre)
  Left_ST_Fr<-Left_ST_Fr[0,]
  Left_ST_Pss<-Left_ST_Pss[0,]
  Left_ST_Pre<-Left_ST_Pre[0,]
}  
  
#Items left per day

  Items_left_everyday[[paste(l,k)]]<-Left_Selection_Fr
  Items_left_everyday_Pss[[paste(l,k)]]<-Left_Selection_Pss
  Items_left_everyday_Pre[[paste(l,k)]]<-Left_Selection_Pre
