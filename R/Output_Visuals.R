




# CREATION OF VISUALS -----------------------------------------------------


  # Exposure Graphs Staggered ------------------------------------------------
  
  Plot_Total_Fr_Contamination_Stag<-Exposure_Staggered_Function2(Total_Consumed_Fr_Bind,Contamination = Contamination, Type = Type, "Total Exposure Fruit", "Contamination per Fruit log CFU/Fruit", "Count of Fruit Consumed")
  Plot_Total_Pss_Contamination_Stag<-Exposure_Staggered_Function2(Total_Consumed_Pss_Bind,Contamination = Contamination, Type = Type, "Total Exposure Pss","Contamination per Pss log CFU/Pss", "Count of PSs Consumed")
  Plot_Total_Pss_Contamination_Stag<-Exposure_Staggered_Function2(Total_Consumed_Pre_Bind,Contamination = Contamination, Type = Type, "Total Exposure Pre", "Contamination per Pre log CFU/Pre", "Count of Pre Consumed")

  Func_GGsave(Plot_Total_Fr_Contamination_Stag,"Graphs","Histogram")  
  # Histogram Exposure Visuals Not Staggered ----------------------------------------------
  
  #Fruit
  #Total Exposure
  Plot_Total_Fr_Contamination<-Exposure_Plot_Function3(Total_Consumed_Fr, "Exposure total Fruit Consumed", "Contamination per Fruit log CFU/Fruit", "Count of Fruit Consumed")
  #Total Exposure from Share Table Items
  if(Share_Table_YN==1){
    Plot_Total_Fr_ST_Contamination<-Exposure_Plot_Function3(Total_Consumed_ST_Fr, "Exposure Consumed Fruit from Share Tables", "Contamination per Fruit log CFU/Fruit", "Count of Fruit Consumed")
  }
  #Total Exposure from Selection Table Items
  Plot_Total_Fr_Sel_Contamination<-Exposure_Plot_Function3(Total_Consumed_Sel_Fr, "Exposure Consumed Fruit from Selection Tables", "Contamination per Fruit log CFU/Fruit", "Count of Fruit Consumed")
  
  
  
  #Pss
  #Total Exposure
  Exposure_Plot_Function3(Total_Consumed_Pss, "Exposure total Consumed Pss", "Contamination per Pss log CFU/Pss", "Count of PSs Consumed")
  #Total Exposure from Share Table Items
  if(Share_Table_YN){
    Exposure_Plot_Function3(Total_Consumed_ST_Pss, "Exposure Consumed Pss from Share Tables" ,"Contamination per Pss log CFU/Pss", "Count of PSs Consumed")
  }
  #Total Exposure from Selection Table Items
  Exposure_Plot_Function3(Total_Consumed_Sel_Pss, "Exposure Consumed Pss from Selection Table" ,"Contamination per Pss log CFU/Pss", "Count of PSs Consumed")
  
  #Pre    
  #Total Exposure
  Exposure_Plot_Function3(Total_Consumed_Pre, "Exposure total Consumed Pre", "Contamination per Pre log CFU/Pre", "Count of Pre Consumed")
  #Total Exposure from Share Table Items
  if(Share_Table_YN==1){
    Exposure_Plot_Function3(Total_Consumed_ST_Pre, "Exposure Consumed Pre from Share Tables",  "Contamination per Pre log CFU/Pre", "Count of Pre Consumed")
  }
  #Total Exposure from Selection Table Items
  Exposure_Plot_Function3(Total_Consumed_Sel_Pre, "Exposure Consumed Pre from Selection Table", "Contamination per Pre log CFU/Pre", "Count of Pre Consumed")
  
  
  #Sum of the total partciles consumed.   
  sum(Total_Consumed_Fr$Contamination)

  # Bar Chart for Location --------------------------------------------------

  if(Resharing_YN==0){
    Location_BarC_Function(Fr_Data,"Fruit Location Re-Service & Re-Sharing off")
    Location_BarC_Function(Pss_Data,"Pss Location Re-Service & Re-Sharing off")  
    Location_BarC_Function(Pre_Data,"Pre Location Re-Service & Re-Sharing off")  
  }
  
  if(Resharing_YN==1){
    Location_BarC_Function(Fr_Data,"Fruit Location Re-Service & Re-Sharing on")
    Location_BarC_Function(Pss_Data,"Pss Location Re-Service & Re-Sharing on")  
    Location_BarC_Function(Pre_Data,"Pre Location Re-Service & Re-Sharing on") 
  }
  



  

  # Boxplot Visuals ---------------------------------------------------------


  Box_Plot_Function(Total_Consumed_Fr_Bind, "Exposure per location fruit")
  Box_Plot_Function(Total_Consumed_Pss_Bind, "Exposure per location Pss")
  Box_Plot_Function(Total_Consumed_Pre_Bind, "Exposure per location Pre")


  Box_Plot_Function2(Total_Consumed_Fr,"Exposure of Total Consumed Fruit")
  Box_Plot_Function2(Total_Consumed_Pss,"Exposure of Total Consumed Pss")
  Box_Plot_Function2(Total_Consumed_Pre,"Exposure of Total Consumed Pre")
  
  
  Disc_Consumed_Function(Fruit_Data_Consumed_Discarded, " Fruit Consumed vs Discarded Share Tables ON")
  Disc_Consumed_Function(Pss_Data_Consumed_Discarded, " Pss Consumed vs Discarded Share Tables ON")
  Disc_Consumed_Function(Pre_Data_Consumed_Discarded, " Pre Consumed vs Discarded Share Tables ON")
  





