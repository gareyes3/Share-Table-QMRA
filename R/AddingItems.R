#Adding Items in bulk, fruit

  Fr_Available<-Fr_Data.Frame$Location == "Selection Table" 
  Sum_Fr_Available<-as.numeric(sum(Fr_Available,na.rm = TRUE))
  
  if(Sum_Fr_Available<2){
    Extra_Items<-data.frame("Apple No." = 1:Row_size_Fr,
                              "Location"= "Selection Table",
                              "Contamination" = as.numeric("0"),
                              "ExposedAllergen" = FALSE,
                              "TotTime"= as.numeric("0"),
                              "History" = "", 
                              "STtimes"= as.numeric("0"),
                              "Initial Service" = "1",
                              "Service" = j,
                              "Initial Day" = "1",
                              "Day" = k,
                              stringsAsFactors = FALSE
                              
    )
    
    if(salmonella==1 && Calculated_Cont_Fr==1){
      Extra_Items<-func_Cont_cm2(Extra_Items,Prevalence_Salmonella_Fr,Fr_Contamination_salmonella,Fr_Mean_area)
    } else if (norovirus == 1 && Calculated_Cont_Fr ==1){
      Extra_Items<-func_Cont_HuNoV_Fr(Extra_Items,Prevalence_Norovirus_Fr)
    }
    
    Fr_Data.Frame<-rbind(Extra_Items,Fr_Data.Frame)
    Fr_Data.Frame$Apple.No.<- 1:nrow(Fr_Data.Frame)
    row.names(Fr_Data.Frame)<-1:nrow(Fr_Data.Frame)
    
  }

#Adding Pss if they run out

  Pss_Available<-Pss_Data.Frame$Location == "Selection Table" 
  Sum_Pss_Available<-sum(Pss_Available, na.rm = TRUE)
  
  if(Sum_Pss_Available<2){
    Extra_Items<-data.frame("Pre No." = 1:Initial_Pss,
                                              "Location"= "Selection Table",
                                              "Contamination" = as.numeric("0"),
                                              "ExposedAllergen" = FALSE,
                                              "SpoilageCon" = as.numeric(Initial_Spoilage_Con),
                                              "SpoiledYN" = FALSE,
                                              "TotTime"= as.numeric("0"),
                                              "History" = "", 
                                              "STtimes"= as.numeric("0"),
                                              "Initial Service" = "1",
                                              "Service" = j,
                                              "Initial Day" = "1",
                                              "Day" = k,
                                              stringsAsFactors = FALSE
    )
    
    if(salmonella==1 && Calculated_Cont_Pss==1){
      Pss_Data.Frame<-func_Cont_cm2(Pss_Data.Frame,Prevalence_Salmonella_Pss,Pss_Contamination_salmonella,Pss_Mean_area)
    } else if (norovirus == 1 && Calculated_Cont_Fr ==1){
      Pss_Data.Frame<-func_Cont_cm2(Pss_Data.Frame,Prevalence_Norovirus_Pss,Pss_Contamination_norovirus,Pss_Mean_area)
    }
    
    Pss_Data.Frame<-rbind(Extra_Items,Pss_Data.Frame)
    Pss_Data.Frame$Apple.No.<- 1:nrow(Pss_Data.Frame)
    row.names(Pss_Data.Frame)<-1:nrow(Pss_Data.Frame)
    
  }



#Adding Pre if they run out
  
  Pre_Available<-Pre_Data.Frame$Location == "Selection Table" 
  Sum_Pre_Available<-sum(Pre_Available, na.rm = TRUE)
  
  if(Sum_Pre_Available<2){
    Extra_Items<-data.frame("Pre No." = 1:Initial_Pss,
                            "Location"= "Selection Table",
                            "Contamination" = as.numeric("0"),
                            "ExposedAllergen" = FALSE,
                            "SpoilageCon" = as.numeric(Initial_Spoilage_Con),
                            "SpoiledYN" = FALSE,
                            "TotTime"= as.numeric("0"),
                            "History" = "", 
                            "STtimes"= as.numeric("0"),
                            "Initial Service" = "1",
                            "Service" = j,
                            "Initial Day" = "1",
                            "Day" = k,
                            stringsAsFactors = FALSE
    )
    
    
    if(salmonella==1 && Calculated_Cont_Fr==1){
      Extra_Items<-func_Cont_cm2(Extra_Items,Prevalence_Salmonella_Fr,Fr_Contamination_salmonella,Fr_Mean_area)
    } else if (norovirus == 1 && Calculated_Cont_Fr ==1){
      Extra_Items<-func_Cont_HuNoV_Fr(Extra_Items,Prevalence_Norovirus_Fr)
    }
    
    Fr_Data.Frame<-rbind(Extra_Items,Fr_Data.Frame)
    Fr_Data.Frame$Apple.No.<- 1:nrow(Fr_Data.Frame)
    row.names(Fr_Data.Frame)<-1:nrow(Fr_Data.Frame)
    
  }