
# CREATION OF DAYS DATA FRAME ---------------------------------------------


Fr_Data_Days = do.call(rbind,datalistFr_days)
Pss_Data_Days = do.call(rbind,datalistPss_days)
Pre_Data_Days = do.call(rbind,datalistPre_days)

# DATA CLEANING DATA FOR SUMMARY STATISTICS ------------------------------------
  
  
# Fruit Data frame preparation --------------------------------------------

  #Fruit Total Consumed
  Total_Consumed_Fr<-Fr_Data_Days[which(Fr_Data_Days$Location == "Consumed"),] #All consumed items
  Total_Consumed_Fr$Type<- "Total Consumed"
  if(Units_Per_gram == 1){
    Total_Consumed_Fr<-Func_Convert_pergram(Total_Consumed_Fr)   #Toggle if we want our units to be log CFU/g instead of Log CFU/Fruit
  }
  Total_Consumed_Fr<-Func_Convert_Log(Total_Consumed_Fr) #Converting to Log
  
  #Share Table Total consumed
  Total_Consumed_ST_Fr<-Total_Consumed_Fr[which(Total_Consumed_Fr$STtimes > 0),]
  Total_Consumed_ST_Fr$Type<-" Consumed Share Table"
  
  #Selection Table Total consumed
  Total_Consumed_Sel_Fr<-Total_Consumed_Fr[which(Total_Consumed_Fr$STtimes ==0),]
  Total_Consumed_Sel_Fr$Type<-" Consumed Selection Table"
  
  #Binding all three Data Frames Categorized by type
  Total_Consumed_Fr_Bind<-rbind(Total_Consumed_Fr,Total_Consumed_ST_Fr,Total_Consumed_Sel_Fr)
  
  #Discarded Fruit due to Scenario.
  Fruit_Data_Consumed_Discarded<-rbind(Fr_Data_Days[which(Fr_Data_Days$Location == "Consumed"),],Fr_Data_Days[which(Fr_Data_Days$Location == "Discarded"),])  


# Pss Data frame preparation --------------------------------------------

  #Pss Total Consumed
  Total_Consumed_Pss<-Pss_Data_Days[which(Pss_Data_Days$Location == "Consumed"),]
  Total_Consumed_Pss$Type<- "Total Consumed"
  if(Units_Per_gram == 1){
    Total_Consumed_Pss<-Func_Convert_pergram(Total_Consumed_Pss) #Toggle if we want our units to be log CFU/g instead of Log CFU/Pss
  }
  Total_Consumed_Pss<-Func_Convert_Log(Total_Consumed_Pss) #Converting to Log
  
  #Share Table Total Consumed
  Total_Consumed_ST_Pss<-Total_Consumed_Pss[which(Total_Consumed_Pss$STtimes > 0),]
  Total_Consumed_ST_Pss$Type<-" Consumed Share Table"
  
  #Selection Table Total Consumed
  Total_Consumed_Sel_Pss<-Total_Consumed_Pss[which(Total_Consumed_Pss$STtimes ==0),] 
  Total_Consumed_Sel_Pss$Type<-" Consumed Selection Table"
  
  #Binding all three Data Frames Categorized by type
  Total_Consumed_Pss_Bind<-rbind(Total_Consumed_Pss,Total_Consumed_ST_Pss,Total_Consumed_Sel_Pss)
  
  #Discarded Pss due to Scenario.
  Pss_Data_Consumed_Discarded<-rbind(Pss_Data_Days[which(Pss_Data_Days$Location == "Consumed"),],Pss_Data_Days[which(Pss_Data_Days$Location == "Discarded"),])  


# PRe Data frame preparation --------------------------------------------

  #Pre Total Consumed
  Total_Consumed_Pre<-Pre_Data_Days[which(Pre_Data_Days$Location == "Consumed"),]
  Total_Consumed_Pre$Type<- "Total Consumed"
  if(Units_Per_gram == 1){
    Total_Consumed_Pre<-Func_Convert_pergram(Total_Consumed_Pre) #Toggle if we want our units to be log CFU/g instead of Log CFU/Pss
  }
  Total_Consumed_Pre<-Func_Convert_Log(Total_Consumed_Pre) #Converting to Log
  
  #Share Table Total Consumed
  Total_Consumed_ST_Pre<-Total_Consumed_Pre[which(Total_Consumed_Pre$STtimes > 0),]
  Total_Consumed_ST_Pre$Type<-" Consumed Share Table"
  
  #Selection Table Total Consumed
  Total_Consumed_Sel_Pre<-Total_Consumed_Pre[which(Total_Consumed_Pss$STtimes ==0),] 
  Total_Consumed_Sel_Pre$Type<-" Consumed Selection Table"
  
  #Binding all three Data Frames Categorized by type
  Total_Consumed_Pre_Bind<-rbind(Total_Consumed_Pre,Total_Consumed_ST_Pre,Total_Consumed_Sel_Pre)
  
  #Discarded Pre due to Scenario.
  Pre_Data_Consumed_Discarded<-rbind(Pre_Data_Days[which(Pre_Data_Days$Location == "Consumed"),],Pre_Data_Days[which(Pre_Data_Days$Location == "Discarded"),])


# CREATING OUTPUTS OF INTEREST --------------------------------------------

  #Consumed Items Fruit Mean + Median
  Fr_Consumed_Mean_Contanmination<-mean(Total_Consumed_Fr$Contamination)
  Fr_Consumed_Median_Contanmination<-median(Total_Consumed_Fr$Contamination)
  
#ADDING DATA TO DATA_FRAME
  AFr_Summary_DF[l,colnames(AFr_Summary_DF)== "MeanCont"]<-Fr_Consumed_Mean_Contanmination
  AFr_Summary_DF[l,colnames(AFr_Summary_DF)== "MedianCont"]<-Fr_Consumed_Median_Contanmination
  
