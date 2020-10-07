# FOOD POOL DATAFRAMES ----------------------------------------------------



#SERVICE 1 DAY 1 CREATION ================================================================


# Fruit -------------------------------------------------------------------


if(j==1 && k== 1){
  Fr_Data.Frame<-Fuct_DF_Initial(FoodType = "Fruit")
  
  
  #Adding Initial Contaminations of fruit Data frame based on initial cont and prevalence CFU/Fruit
  
  
  if(salmonella==1 && Calculated_Cont_Fr==1){
    #Adding Contamination to the Dataframe
    Fr_Data.Frame<-func_Cont_cm2(Fr_Data.Frame,Prevalence_Salmonella_Fr,Fr_Contamination_salmonella,Fr_Mean_area)
    #Adding items to know contaminated items
    Vector_Cont_Fr_Serv<-Func_FoodCont_Vector(DF=Fr_Data.Frame)
  } else if (norovirus == 1 && Calculated_Cont_Fr ==1){
    #Adding Contamination to Data Fra
    Fr_Data.Frame<-do.call(func_Cont_HuNoV_Fr,c(list(DF=Fr_Data.Frame),Inputs_Cont_HuNov_Fr))
    #Adding items to know Contaminated Items
    Vector_Cont_Fr_Serv<-Func_FoodCont_Vector(DF=Fr_Data.Frame)
  }
  
  
  # Pss ---------------------------------------------------------------------
  
  
  
  Pss_Data.Frame<-Fuct_DF_Initial(FoodType = "Pss")
  
  #Adding initial contamination based on prevalence and area of the item #CFU/Pss
  
  if(salmonella==1 && Calculated_Cont_Pss==1){
    Pss_Data.Frame<-func_Cont_cm2(Pss_Data.Frame,Prevalence_Salmonella_Pss,Pss_Contamination_salmonella,Pss_Mean_area)
  } else if (norovirus == 1 && Calculated_Cont_Fr ==1){
    Pss_Data.Frame<-func_Cont_cm2(Pss_Data.Frame,Prevalence_Norovirus_Pss,Pss_Contamination_norovirus,Pss_Mean_area)
  }
  
  
  
  # Pre ---------------------------------------------------------------------
  
  
  Pre_Data.Frame<-Fuct_DF_Initial(FoodType = "Pre")
  
  #Adding initial contamination based on prevalence and area of the item #CFU/Pss
  
  if(salmonella==1 && Calculated_Cont_Pss==1){
    Pre_Data.Frame<-func_Cont_cm2(Pre_Data.Frame,Prevalence_Salmonella_Pre,Pre_Contamination_salmonella,Pre_Mean_area)
  } else if (norovirus == 1 && Calculated_Cont_Pss ==1){
    Pre_Data.Frame<-func_Cont_cm2(Pre_Data.Frame,Prevalence_Norovirus_Pre,Pre_Contamination_norovirus,Pre_Mean_area)
  }
  
  
  
} #end of J1


#SERVICE >1 DAY 1 ===============================================================



if(j>1 && k ==1 ){
  
  if(Reservice_YN==1){
    
    
    # Fruit -------------------------------------------------------------------
  
    
    
    Fr_Data.Frame<-Fuct_DF_Reservice(FoodType = "Fruit")
    
    #Adding Initial Contaminations of fruit
    if(salmonella==1 && Calculated_Cont_Fr==1){
      #Adding Contaminations to the Data Frames
      Fr_Data.Frame<-func_Cont_cm2(Fr_Data.Frame,Prevalence_Salmonella_Fr,Fr_Contamination_salmonella,Fr_Mean_area)
      #Adding Contamination to Vector
      Vector_Cont_Fr_Serv<-Func_FoodCont_Vector(DF=Fr_Data.Frame)
    } else if (norovirus == 1 && Calculated_Cont_Fr ==1){
      #Adding Contamination to the Data Frames
      Fr_Data.Frame<-do.call(func_Cont_HuNoV_Fr,c(list(DF=Fr_Data.Frame),Inputs_Cont_HuNov_Fr))
      #Adding Contamination to Vector
      Vector_Cont_Fr_Serv<-Func_FoodCont_Vector(DF=Fr_Data.Frame)
    }
    
    #Adding times that the share table items have been shared
    Left_ST_Fr[,colnames(Left_ST_Fr)=="STtimes"]<-(Left_ST_Fr[,colnames(Left_ST_Fr)=="STtimes"])+1
    #Resharing Toggle
    if(Resharing_YN ==1){
      Fr_Data.Frame<-rbind(Left_Selection_Fr,Fr_Data.Frame,Left_ST_Fr)
    }else if (Resharing_YN == 0){
      Fr_Data.Frame<-rbind(Left_Selection_Fr,Fr_Data.Frame)
    }
    #Reseting Numbers in Data Frames
    Fr_Data.Frame$Apple.No.<- 1:nrow(Fr_Data.Frame)
    row.names(Fr_Data.Frame)<-1:nrow(Fr_Data.Frame)
    Fr_Data.Frame$Service<-j
    Fr_Data.Frame$Day<-k
    
    
    # Pss ---------------------------------------------------------------------
    
    
    
    Pss_Data.Frame<-Fuct_DF_Reservice(FoodType = "Pss")
    
    #Adding initial contamination based on prevalence and area of the item #CFU/Pss
    
    if(salmonella==1 && Calculated_Cont_Pss==1){
      Pss_Data.Frame<-func_Cont_cm2(Pss_Data.Frame,Prevalence_Salmonella_Pss,Pss_Contamination_salmonella,Pss_Mean_area)
    } else if (norovirus == 1 && Calculated_Cont_Pss ==1){
      Pss_Data.Frame<-func_Cont_cm2(Pss_Data.Frame,Prevalence_Norovirus_Pss,Pss_Contamination_norovirus,Pss_Mean_area)
    }
    
    #Adding times that the share table items have been shared
    Left_ST_Pss[,colnames(Left_ST_Pss)=="STtimes"]<-(Left_ST_Pss[,colnames(Left_ST_Pss)=="STtimes"])+1
    #Resharing Toggle
    if(Resharing_YN ==1){
      Pss_Data.Frame<-rbind(Left_Selection_Pss,Pss_Data.Frame,Left_ST_Pss)
    }else if (Resharing_YN == 0){
      Pss_Data.Frame<-rbind(Left_Selection_Pss,Pss_Data.Frame)
    }
    #Reseting Numbers in Data Frames
    Pss_Data.Frame$Pss.No.<- 1:nrow(Pss_Data.Frame)
    row.names(Pss_Data.Frame)<-1:nrow(Pss_Data.Frame)
    Pss_Data.Frame$Service<-j
    Pss_Data.Frame$Day<-k
    
    
    # Pre ---------------------------------------------------------------------
    
    
    Pre_Data.Frame<-Fuct_DF_Reservice(FoodType = "Pre")
    
    #Adding initial contamination based on prevalence and area of the item #CFU/Pss
    
    if(salmonella==1 && Calculated_Cont_Pre==1){
      Pre_Data.Frame<-func_Cont_cm2(Pre_Data.Frame,Prevalence_Salmonella_Pre,Pre_Contamination_salmonella,Pre_Mean_area)
    } else if (norovirus == 1 && Calculated_Cont_Pre ==1){
      Pre_Data.Frame<-func_Cont_cm2(Pre_Data.Frame,Prevalence_Norovirus_Pre,Pre_Contamination_norovirus,Pre_Mean_area)
    }
    
    #Adding times that the share table items have been shared
    Left_ST_Pre[,colnames(Left_ST_Pre)=="STtimes"]<-(Left_ST_Pre[,colnames(Left_ST_Pre)=="STtimes"])+1
    #Resharing Toggle
    if(Resharing_YN ==1){
      Pre_Data.Frame<-rbind(Left_Selection_Pre,Pre_Data.Frame,Left_ST_Pre)
    }else if (Resharing_YN == 0){
      Pre_Data.Frame<-rbind(Left_Selection_Pre,Pre_Data.Frame)
    }
    #Reseting Numbers in Data Frames
    Pre_Data.Frame$Pre.No.<- 1:nrow(Pre_Data.Frame)
    row.names(Pre_Data.Frame)<-1:nrow(Pre_Data.Frame)
    Pre_Data.Frame$Service<-j
    Pre_Data.Frame$Day<-k
    
    #Else if statements for no reservice
    
  } else if (Reservice_YN==0){
    
    
    # Fruit -------------------------------------------------------------------
    
    
    
    Fr_Data.Frame<-Fuct_DF_Initial(FoodType = "Fruit")
    
    #Adding Initial Contaminations of fruit 
    if(salmonella==1 && Calculated_Cont_Fr==1){
      #Adding Contamination to the Data Frames
      Fr_Data.Frame<-func_Cont_cm2(Fr_Data.Frame,Prevalence_Salmonella_Fr,Fr_Contamination_salmonella,Fr_Mean_area)
      #Adding Contamination to Vector
      Vector_Cont_Fr_Serv<-Func_FoodCont_Vector(DF=Fr_Data.Frame)
    } else if (norovirus == 1 && Calculated_Cont_Fr ==1){
      #Adding Contamination to the Data frames
      Fr_Data.Frame<-do.call(func_Cont_HuNoV_Fr,c(list(DF=Fr_Data.Frame),Inputs_Cont_HuNov_Fr))
      #Adding Contamination to Vector
      Vector_Cont_Fr_Serv<-Func_FoodCont_Vector(DF=Fr_Data.Frame)
    }
    
    #Adding Share Times
    
    #Resharing Toggle
    if(Resharing_YN ==1){
      Fr_Data.Frame<-rbind(Fr_Data.Frame,Left_ST_Fr)
    }
    #Resetting Numbers and counters
    Fr_Data.Frame$Apple.No.<- 1:nrow(Fr_Data.Frame)
    row.names(Fr_Data.Frame)<-1:nrow(Fr_Data.Frame)
    Fr_Data.Frame$Service<-j
    Fr_Data.Frame$Day<-k
    
    
    
    # Pss ---------------------------------------------------------------------
    
    
    
    Pss_Data.Frame<-Fuct_DF_Initial(FoodType = "Pss")
    
    #Adding initial contamination based on prevalence and area of the item #CFU/Pss
    
    if(salmonella==1 && Calculated_Cont_Pss==1){
      Pss_Data.Frame<-func_Cont_cm2(Pss_Data.Frame,Prevalence_Salmonella_Pss,Pss_Contamination_salmonella,Pss_Mean_area)
    } else if (norovirus == 1 && Calculated_Cont_Pss ==1){
      Pss_Data.Frame<-func_Cont_cm2(Pss_Data.Frame,Prevalence_Norovirus_Pss,Pss_Contamination_norovirus,Pss_Mean_area)
    }
    
    #Adding Share Times
    Left_ST_Pss[,colnames(Left_ST_Pss)=="STtimes"]<-(Left_ST_Pss[,colnames(Left_ST_Pss)=="STtimes"])+1
    #Resharing Toggle
    if(Resharing_YN ==1){
      Pss_Data.Frame<-rbind(Pss_Data.Frame,Left_ST_Pss)
    }
    #Resetting Numbers and counters
    Pss_Data.Frame$Pss.No.<- 1:nrow(Pss_Data.Frame)
    row.names(Pss_Data.Frame)<-1:nrow(Pss_Data.Frame)
    Pss_Data.Frame$Service<-j
    Pss_Data.Frame$Day<-k
    
    
    # Pre ---------------------------------------------------------------------
    
    
    
    Pre_Data.Frame<-Fuct_DF_Initial(FoodType = "Pre")
    
    #Adding initial contamination based on prevalence and area of the item #CFU/Pss
    
    if(salmonella==1 && Calculated_Cont_Pre==1){
      Pre_Data.Frame<-func_Cont_cm2(Pre_Data.Frame,Prevalence_Salmonella_Pre,Pre_Contamination_salmonella,Pre_Mean_area)
    } else if (norovirus == 1 && Calculated_Cont_Pre ==1){
      Pre_Data.Frame<-func_Cont_cm2(Pre_Data.Frame,Prevalence_Norovirus_Pre,Pre_Contamination_norovirus,Pre_Mean_area)
    }
    
    #Adding Share Times
    Left_ST_Pre[,colnames(Left_ST_Pre)=="STtimes"]<-(Left_ST_Pre[,colnames(Left_ST_Pre)=="STtimes"])+1
    #Resharing Toggle
    if(Resharing_YN ==1){
      Pre_Data.Frame<-rbind(Pre_Data.Frame,Left_ST_Pre)
    }
    #Resetting Numbers and counters
    Pre_Data.Frame$Pre.No.<- 1:nrow(Pre_Data.Frame)
    row.names(Pre_Data.Frame)<-1:nrow(Pre_Data.Frame)
    Pre_Data.Frame$Service<-j
    Pre_Data.Frame$Day<-k
    
  } #end of resahring if
  
  
}#end of j loop




# SERVICES MULTIPLE AND MULTIPLE DAYS --------------------------------------------

if(j>0 && k>1 ){
  
  if(Reservice_YN==1){
    
    
    # Fruit ----------------------------------------------------------------------
    
    
    Fr_Data.Frame<-Fuct_DF_Reservice(FoodType = "Fruit")
    
    #Adding Initial Contaminations of fruit 
    if(salmonella==1 && Calculated_Cont_Fr==1){
      #Adding Contamination to the Data Frames
      Fr_Data.Frame<-func_Cont_cm2(Fr_Data.Frame,Prevalence_Salmonella_Fr,Fr_Contamination_salmonella,Fr_Mean_area)
      #Adding Contamination to Vector
      Vector_Cont_Fr_Serv<-Func_FoodCont_Vector(DF=Fr_Data.Frame)
    } else if (norovirus == 1 && Calculated_Cont_Fr ==1){
      #Adding Contamination to the Data Frames
      Fr_Data.Frame<-do.call(func_Cont_HuNoV_Fr,c(list(DF=Fr_Data.Frame),Inputs_Cont_HuNov_Fr))
      #Adding Contamination to Vector
      Vector_Cont_Fr_Serv<-Func_FoodCont_Vector(DF=Fr_Data.Frame)
    }
    
    #Adding times that the share table items have been shared
    Left_ST_Fr[,colnames(Left_ST_Fr)=="STtimes"]<-(Left_ST_Fr[,colnames(Left_ST_Fr)=="STtimes"])+1
    LeftAAAAA2<-Left_ST_Fr
    #Resharing Toggle
    if(Resharing_YN ==1){
      Fr_Data.Frame<-rbind(Left_Selection_Fr,Fr_Data.Frame,Left_ST_Fr)
    }else if (Resharing_YN == 0){
      Fr_Data.Frame<-rbind(Left_Selection_Fr,Fr_Data.Frame)
    }
    #Reseting Numbers in Data Frames
    Fr_Data.Frame$Apple.No.<- 1:nrow(Fr_Data.Frame)
    row.names(Fr_Data.Frame)<-1:nrow(Fr_Data.Frame)
    Fr_Data.Frame$Service<-j
    Fr_Data.Frame$Day<-k
    
    
    
    # Pss ---------------------------------------------------------------------
    
    
    
    Pss_Data.Frame<-Fuct_DF_Reservice(FoodType = "Pss")
    
    #Adding initial contamination based on prevalence and area of the item #CFU/Pss
    
    if(salmonella==1 && Calculated_Cont_Pss==1){
      Pss_Data.Frame<-func_Cont_cm2(Pss_Data.Frame,Prevalence_Salmonella_Pss,Pss_Contamination_salmonella,Pss_Mean_area)
    } else if (norovirus == 1 && Calculated_Cont_Pss ==1){
      Pss_Data.Frame<-func_Cont_cm2(Pss_Data.Frame,Prevalence_Norovirus_Pss,Pss_Contamination_norovirus,Pss_Mean_area)
    }
    
    #Adding times that the share table items have been shared
    Left_ST_Pss[,colnames(Left_ST_Pss)=="STtimes"]<-(Left_ST_Pss[,colnames(Left_ST_Pss)=="STtimes"])+1
    #Resharing Toggle
    if(Resharing_YN ==1){
      Pss_Data.Frame<-rbind(Left_Selection_Pss,Pss_Data.Frame,Left_ST_Pss)
    }else if (Resharing_YN == 0){
      Pss_Data.Frame<-rbind(Left_Selection_Pss,Pss_Data.Frame)
    }
    #Reseting Numbers in Data Frames
    Pss_Data.Frame$Pss.No.<- 1:nrow(Pss_Data.Frame)
    row.names(Pss_Data.Frame)<-1:nrow(Pss_Data.Frame)
    Pss_Data.Frame$Service<-j
    Pss_Data.Frame$Day<-k
    
    
    # Pre ---------------------------------------------------------------------
    
    
    
    Pre_Data.Frame<-Fuct_DF_Reservice(FoodType = "Pre")
    
    #Adding initial contamination based on prevalence and area of the item #CFU/Pss
    
    if(salmonella==1 && Calculated_Cont_Pre==1){
      Pre_Data.Frame<-func_Cont_cm2(Pre_Data.Frame,Prevalence_Salmonella_Pre,Pre_Contamination_salmonella,Pre_Mean_area)
    } else if (norovirus == 1 && Calculated_Cont_Pre ==1){
      Pre_Data.Frame<-func_Cont_cm2(Pre_Data.Frame,Prevalence_Norovirus_Pre,Pre_Contamination_norovirus,Pre_Mean_area)
    }
    
    #Adding times that the share table items have been shared
    Left_ST_Pre[,colnames(Left_ST_Pre)=="STtimes"]<-(Left_ST_Pre[,colnames(Left_ST_Pre)=="STtimes"])+1
    #Resharing Toggle
    if(Resharing_YN ==1){
      Pre_Data.Frame<-rbind(Left_Selection_Pre,Pre_Data.Frame,Left_ST_Pre)
    }else if (Resharing_YN == 0){
      Pre_Data.Frame<-rbind(Left_Selection_Pre,Pre_Data.Frame)
    }
    #Reseting Numbers in Data Frames
    Pre_Data.Frame$Pre.No.<- 1:nrow(Pre_Data.Frame)
    row.names(Pre_Data.Frame)<-1:nrow(Pre_Data.Frame)
    Pre_Data.Frame$Service<-j
    Pre_Data.Frame$Day<-k
    
    #Else if statements for no reservice
    
  } else if (Reservice_YN==0){
    
    
    
    # Fruit -------------------------------------------------------------------
    
    
    
    Fr_Data.Frame<-Fuct_DF_Initial(FoodType = "Fruit")
    
    #Adding Initial Contaminations of fruit 
    if(salmonella==1 && Calculated_Cont_Fr==1){
      #Adding Contamination to the Data Frames
      Fr_Data.Frame<-func_Cont_cm2(Fr_Data.Frame,Prevalence_Salmonella_Fr,Fr_Contamination_salmonella,Fr_Mean_area)
      #Adding Contamination to Vector
      Vector_Cont_Fr_Serv<-Func_FoodCont_Vector(DF=Fr_Data.Frame)
    } else if (norovirus == 1 && Calculated_Cont_Fr ==1){
      #Adding Contamination the the Data Frames
      Fr_Data.Frame<-do.call(func_Cont_HuNoV_Fr,c(list(DF=Fr_Data.Frame),Inputs_Cont_HuNov_Fr))
      #Adding Contamination to Vector
      Vector_Cont_Fr_Serv<-Func_FoodCont_Vector(DF=Fr_Data.Frame)
    }
    
    #Adding Share Times
    
    #Resharing Toggle
    if(Resharing_YN ==1){
      Fr_Data.Frame<-rbind(Fr_Data.Frame,Left_ST_Fr)
    }
    #Resetting Numbers and counters
    Fr_Data.Frame$Apple.No.<- 1:nrow(Fr_Data.Frame)
    row.names(Fr_Data.Frame)<-1:nrow(Fr_Data.Frame)
    Fr_Data.Frame$Service<-j
    Fr_Data.Frame$Day<-k
    
    
    
    # Pss ---------------------------------------------------------------------
    
    
    
    Pss_Data.Frame<-Fuct_DF_Initial(FoodType = "Pss")
    
    #Adding initial contamination based on prevalence and area of the item #CFU/Pss
    
    if(salmonella==1 && Calculated_Cont_Pss==1){
      Pss_Data.Frame<-func_Cont_cm2(Pss_Data.Frame,Prevalence_Salmonella_Pss,Pss_Contamination_salmonella,Pss_Mean_area)
    } else if (norovirus == 1 && Calculated_Cont_Pss ==1){
      Pss_Data.Frame<-func_Cont_cm2(Pss_Data.Frame,Prevalence_Norovirus_Pss,Pss_Contamination_norovirus,Pss_Mean_area)
    }
    
    #Adding Share Times
    Left_ST_Pss[,colnames(Left_ST_Pss)=="STtimes"]<-(Left_ST_Pss[,colnames(Left_ST_Pss)=="STtimes"])+1
    #Resharing Toggle
    if(Resharing_YN ==1){
      Pss_Data.Frame<-rbind(Pss_Data.Frame,Left_ST_Pss)
    }
    #Resetting Numbers and counters
    Pss_Data.Frame$Pss.No.<- 1:nrow(Pss_Data.Frame)
    row.names(Pss_Data.Frame)<-1:nrow(Pss_Data.Frame)
    Pss_Data.Frame$Service<-j
    Pss_Data.Frame$Day<-k
    
    
    # Pre ---------------------------------------------------------------------
    
    
    Pre_Data.Frame<-Fuct_DF_Initial(FoodType = "Pre")
    
    #Adding initial contamination based on prevalence and area of the item #CFU/Pss
    
    if(salmonella==1 && Calculated_Cont_Pre==1){
      Pre_Data.Frame<-func_Cont_cm2(Pre_Data.Frame,Prevalence_Salmonella_Pre,Pre_Contamination_salmonella,Pre_Mean_area)
    } else if (norovirus == 1 && Calculated_Cont_Pre ==1){
      Pre_Data.Frame<-func_Cont_cm2(Pre_Data.Frame,Prevalence_Norovirus_Pre,Pre_Contamination_norovirus,Pre_Mean_area)
    }
    
    #Adding Share Times
    Left_ST_Pre[,colnames(Left_ST_Pre)=="STtimes"]<-(Left_ST_Pre[,colnames(Left_ST_Pre)=="STtimes"])+1
    #Resharing Toggle
    if(Resharing_YN ==1){
      Pre_Data.Frame<-rbind(Pre_Data.Frame,Left_ST_Pre)
    }
    #Resetting Numbers and counters
    Pre_Data.Frame$Pre.No.<- 1:nrow(Pre_Data.Frame)
    row.names(Pre_Data.Frame)<-1:nrow(Pre_Data.Frame)
    Pre_Data.Frame$Service<-j
    Pre_Data.Frame$Day<-k
    
  } #end of resahring if
  
  
}#end of j loop


# Vectors -----------------------------------------------------------------

V_Shared_Fr<-c(0)
V_Shared_Pss<-c(0)
V_Shared_Pre<-c(0)