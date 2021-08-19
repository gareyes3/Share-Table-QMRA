#Summary Dose Response Weekly

#Creating Summary Data Frame
  Fr_Data_Days_DR = do.call(rbind,datalistFr_days)
  Pss_Data_Days_DR = do.call(rbind,datalistPss_days)
  Pre_Data_Days_DR = do.call(rbind,datalistPre_days)

#Removing Duplicates
  #this step filters replicated based on the ID
  Fr_Data_Days_DR<-Fr_Data_Days_DR %>% 
    group_by(ID) %>% 
    filter(TotServices==max(TotServices))

  Pss_Data_Days_DR<-Pss_Data_Days_DR %>% 
    group_by(ID) %>% 
    filter(TotServices==max(TotServices))
  
  Pre_Data_Days_DR<-Pre_Data_Days_DR %>% 
    group_by(ID) %>% 
    filter(TotServices==max(TotServices))
  
#Narrowing to Consumed Items Only
  Fr_Data_Days_DR<-Fr_Data_Days_DR[which(Fr_Data_Days_DR$Location == "Consumed"),]
  Pss_Data_Days_DR<-Pss_Data_Days_DR[which(Pss_Data_Days_DR$Location == "Consumed"),]
  Pre_Data_Days_DR<-Pre_Data_Days_DR[which(Pre_Data_Days_DR$Location == "Consumed"),]
  
#Analyzing Ill and Infected
  
  Fr_Data_Days_DR<-Func_DR_Infection(Fr_Data_Days_DR)
  Fr_Data_Days_DR<-Func_DR_Illness(Fr_Data_Days_DR)
  
  Pss_Data_Days_DR<-Func_DR_Infection(Pss_Data_Days_DR)
  Pss_Data_Days_DR<-Func_DR_Illness(Pss_Data_Days_DR)
  
  Pre_Data_Days_DR<-Func_DR_Infection(Pre_Data_Days_DR)
  Pre_Data_Days_DR<-Func_DR_Illness(Pre_Data_Days_DR)

#summary
  Number_Inf_Fr<-sum(Individual_Analysis_Fr$Infection==TRUE)
  Number_Ill_Fr<-sum(Individual_Analysis_Fr$Illness==TRUE)
  
  Number_Inf_Pss<-sum(Individual_Analysis_Fr$Infection==TRUE)
  Number_Ill_Pre<-sum(Individual_Analysis_Fr$Illness==TRUE)
  
  Number_Inf_Pre<-sum(Individual_Analysis_Fr$Infection==TRUE)
  Number_Ill_Pre<-sum(Individual_Analysis_Fr$Illness==TRUE)
  
#Adding it to the summary DataFrame
  
  AFr_DR_Summary_DF[l,colnames(AFr_DR_Summary_DF)== "NoInfected"]<- Number_Inf_Fr
  AFr_DR_Summary_DF[l,colnames(AFr_DR_Summary_DF)== "NoIll"]<- Number_Ill_Fr