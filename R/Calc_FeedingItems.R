#Adding Items in bulk, fruit

  Fr_Available<-Fr_Data.Frame$Location == "Selection Table" 
  Sum_Fr_Available<-as.numeric(sum(Fr_Available,na.rm = TRUE))
  
  if(Sum_Fr_Available<2){
    #Creating Data frame of extra items
    Extra_Items_Fr<-Fuct_DF_Feeding(FoodType = "Fruit")
    
    if(salmonella==1 && Calculated_Cont_Fr==1){
      Extra_Items_Fr<-func_Cont_cm2(Extra_Items_Fr,Prevalence_Salmonella_Fr,Fr_Contamination_salmonella,Fr_Mean_area)
      #Adding Contamination to Vector
      Vector_Cont_Fr_Serv<-Func_FoodCont_Vector(DF=Extra_Items_Fr)
    } else if (norovirus == 1 && Calculated_Cont_Fr ==1){
      Extra_Items_Fr<-do.call(func_Cont_HuNoV_Fr,c(list(DF=Extra_Items_Fr),Inputs_Cont_HuNov_Fr))
      #Adding Contamination to Vector
      Vector_Cont_Fr_Serv<-Func_FoodCont_Vector(DF=Extra_Items_Fr)
    }
    
    Fr_Data.Frame<-rbind(Extra_Items_Fr,Fr_Data.Frame)
    Fr_Data.Frame$Apple.No.<- 1:nrow(Fr_Data.Frame)
    row.names(Fr_Data.Frame)<-1:nrow(Fr_Data.Frame)
    
  }

#Adding Pss if they run out

  Pss_Available<-Pss_Data.Frame$Location == "Selection Table" 
  Sum_Pss_Available<-sum(Pss_Available, na.rm = TRUE)
  
  if(Sum_Pss_Available<2){
    #Creating Data frame of new items
    Extra_Items_Pss<-Fuct_DF_Feeding(FoodType = "Pss")
    
    if(salmonella==1 && Calculated_Cont_Pss==1){
      Extra_Items_Pss<-func_Cont_cm2(Extra_Items_Pss,Prevalence_Salmonella_Pss,Pss_Contamination_salmonella,Pss_Mean_area)
      Vector_Cont_Pre_Serv<-Func_FoodCont_Vector(DF=Extra_Items_Pss)
    } else if (norovirus == 1 && Calculated_Cont_Fr ==1){
      Extra_Items_Pss<-func_Cont_cm2(Extra_Items_Pss,Prevalence_Norovirus_Pss,Pss_Contamination_norovirus,Pss_Mean_area)
      Vector_Cont_Pre_Serv<-Func_FoodCont_Vector(DF=Extra_Items_Pss)
    }
    
    Pss_Data.Frame<-rbind(Extra_Items_Pss,Pss_Data.Frame)
    Pss_Data.Frame$Pss.No.<- 1:nrow(Pss_Data.Frame)
    row.names(Pss_Data.Frame)<-1:nrow(Pss_Data.Frame)
    
  }



#Adding Pre if they run out
  
  Pre_Available<-Pre_Data.Frame$Location == "Selection Table" 
  Sum_Pre_Available<-sum(Pre_Available, na.rm = TRUE)
  
  if(Sum_Pre_Available<2){
    #Creating data frame of items that are fed. 
    Extra_Items_Pre<-Fuct_DF_Feeding(FoodType = "Pre")
    
    
    if(salmonella==1 && Calculated_Cont_Pss==1){
      Extra_Items_Pre<-func_Cont_cm2(Extra_Items_Pre,Prevalence_Salmonella_Pre,Pre_Contamination_salmonella,Pre_Mean_area)
      Vector_Cont_Pre_Serv<-Func_FoodCont_Vector(DF=Extra_Items_Pre)
    } else if (norovirus == 1 && Calculated_Cont_Pss ==1){
      Extra_Items_Pre<-func_Cont_cm2(Extra_Items_Pre,Prevalence_Norovirus_Pre,Pre_Contamination_norovirus,Pre_Mean_area)
      Vector_Cont_Pre_Serv<-Func_FoodCont_Vector(DF=Extra_Items_Pre)
    }
    
    Pre_Data.Frame<-rbind(Extra_Items_Pre,Pre_Data.Frame)
    Pre_Data.Frame$Pre.No.<- 1:nrow(Pre_Data.Frame)
    row.names(Pre_Data.Frame)<-1:nrow(Pre_Data.Frame)
    
  }