
# CREATION OF DAYS DATA FRAMES ---------------------------------------------

  Fr_DF_Day <- paste("Fr_Data_D", k, sep = "")
  assign(Fr_DF_Day, Fr_Data)
  
  Pss_DF_Day <- paste("Pss_Data_D", k, sep = "")
  assign(Pss_DF_Day, Pss_Data)
  
  Pre_DF_Day <- paste("Pre_Data_D", k, sep = "")
  assign(Pre_DF_Day, Pre_Data)




# ITEMS LEFT ON THE LAST SERVICE ------------------------------------

  
  #Fruit that stayed in share table. 
  Left_ST_Fr<-Fr_Data.Frame[which(Fr_Data.Frame$Location == "Shared"),]
  #Fruit that stayed in Selection table
  Left_Selection_Fr<-Fr_Data.Frame[which(Fr_Data.Frame$Location == "Selection Table"),]
  
  #Pss that stayed in share table. 
  Left_ST_Pss<-Pss_Data.Frame[which(Pss_Data.Frame$Location == "Shared"),]
  #Pss that stayed in Selection table
  Left_Selection_Pss<-Pss_Data.Frame[which(Pss_Data.Frame$Location == "Selection Table"),]
  
  #Pre that stayed in share table. 
  Left_ST_Pre<-Pre_Data.Frame[which(Pre_Data.Frame$Location == "Shared"),]
  #Pre that stayed in Selection table
  Left_Selection_Pre<-Pre_Data.Frame[which(Pre_Data.Frame$Location == "Selection Table"),]



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
      Left_Selection_Fr<-Func_Growth_Sto_Ecoli("room temp", Left_Selection_Fr,Time_ON) #Using function on left over items
      #Share Table Items
      Left_ST_Fr<-Func_Growth_Sto_Ecoli("room temp", Left_ST_Fr,Time_ON) #using function on left over St items
    }
  
    if(salmonella==1 && Growth ==1){
      #Selection Table Items
      Left_Selection_Fr<-Func_Growth_Sto_Salmonella("room temp", Left_Selection_Fr,Time_ON) #Using function on left over items
      #Share Table Items
      Left_ST_Fr<-Func_Growth_Sto_Salmonella("room temp", Left_ST_Fr,Time_ON) #using function on left over St items
    }
  
    if(norovirus ==1 && Growth ==1){
      #selection table items
      Left_Selection_Fr<- Func_Growth_Sto_Norovirus("refrigerated", Left_Selection_Fr, Time_ON) #using function on left over items
      #Share table items
      Left_ST_Fr<- Func_Growth_Sto_Norovirus("refrigerated", Left_ST_Fr, Time_ON) #Share table left over items
    }
  

  # Pss ---------------------------------------------------------------------

  #Selection Items #chose which type of storage. 
  if(E_coli==1 && Growth ==1){
    #Selection Table Items
    Left_Selection_Pss<-Func_Growth_Sto_Ecoli("room temp", Left_Selection_Pss,Time_ON) #Using function on left over items
    #Share Table Items
    Left_ST_Fr<-Func_Growth_Sto_Ecoli("room temp", Left_ST_Fr,Time_ON) #using function on left over St items
  }
  
  if(salmonella==1 && Growth ==1){
    #Selection Table Items
    Left_Selection_Pss<-Func_Growth_Sto_Salmonella("room temp", Left_Selection_Pss,Time_ON) #Using function on left over items
    #Share Table Items
    Left_ST_Pss<-Func_Growth_Sto_Salmonella("room temp", Left_ST_Pss,Time_ON) #using function on left over St items
  }
  
  if(norovirus==1 && Growth ==1){
    #selection table items
    Left_Selection_Pss<- Func_Growth_Sto_Norovirus_Plastic("room temp", Left_Selection_Pss, Time_ON) #using function on left over items
    #Share table items
    Left_ST_Pss<- Func_Growth_Sto_Norovirus_Plastic("room temp", Left_ST_Pss, Time_ON) #Share table left over items
  }
  

  # Pre ---------------------------------------------------------------------

  #Selection Items #chose which type of storage. 
  if(E_coli==1 && Growth ==1){
    #Selection Table Items
    Left_Selection_Pre<-Func_Growth_Sto_Ecoli("refrigerated", Left_Selection_Pre,Time_ON) #Using function on left over items
    #Share Table Items
    Left_ST_Pre<-Func_Growth_Sto_Ecoli("refrigerated", Left_ST_Pre,Time_ON) #using function on left over St items
  }
  
  if(salmonella==1 && Growth ==1){
    #Selection Table Items
    Left_Selection_Pre<-Func_Growth_Sto_Salmonella("refrigerated", Left_Selection_Pre,Time_ON) #Using function on left over items
    #Share Table Items
    Left_ST_Pre<-Func_Growth_Sto_Salmonella("refrigerated", Left_ST_Pre,Time_ON) #using function on left over St items
  }
  
  if(norovirus==1 && Growth ==1){
    #selection table items
    Left_Selection_Pre<-Func_Growth_Sto_Norovirus_Plastic("room temp", Left_Selection_Pre, Time_ON) #using function on left over items
    #Share table items
    Left_ST_Pre<- Func_Growth_Sto_Norovirus_Plastic("room temp", Left_ST_Pre, Time_ON) #Share table left over items
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


  
  

# WASHING FRUIT ----------------------------------------------

#Washing selection Items
if(Wash_Selection_YN_Fr==1){
  Left_Selection_Fr$Contamination<-Func_Logred(Left_Selection_Fr$Contamination,Reduction_wash)
}

#washing share table items
if(Wash_ST_YN_Fr==1){
  Left_ST_Fr$Contamination<-Func_Logred(Left_ST_Fr$Contamination,Reduction_wash)
}


# IF Reservice to ST Convert ST to Selection Table ------------------------

if (STtoReservice_YN == 1 ){
  Left_Selection_Fr<-rbind(Left_Selection_Fr,Left_ST_Fr)
  Left_Selection_Fr$Location<-"Selection Table"
  No_Left_Selection_Fr<-nrow(Left_Selection_Fr)
  Left_ST_Fr<-Left_ST_Fr[0,]
}  
