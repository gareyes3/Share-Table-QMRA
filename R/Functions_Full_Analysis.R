#Functions for Analysis


#1.creating main data frame.
Func_Asys_MainDF<-function(SummaryList, FoodType){
  #SummaryLisr = List_Sens_Fr
  #FoodType= = "Fruit"
  #1. Start from here
  Individual_Analysis_Fr<-bind_rows(SummaryList)
  
  #Adding Type Column
  Individual_Analysis_Fr$Type<-FoodType
  
  #2. find the dupplicates
  #this step filters replicated based on the ID
  Individual_Analysis_Fr<-Individual_Analysis_Fr %>% 
    group_by(ID) %>% 
    filter(TotServices==max(TotServices))%>%
    filter(Location=="Consumed")%>%
    select(ID,Type,ConsumedBy,Contamination, ContConsumed, Infection,Illness,week)
  #3
  AnalysysDF<-Individual_Analysis_Fr
  return(AnalysysDF)
}






Func_Asys_ContbyStudent<-function(AnalysisDF){
  #Analysis DF = ST_ON_Analysis
  AnalysisDF<-AnalysisDF%>%
    group_by(ConsumedBy)%>%
    summarise(Contamination = sum(Contamination))
  
  AnalysisDF$week <- substr(AnalysisDF$ConsumedBy, 1, 3) 
  AnalysisDF$Infection<-as.logical("")
  AnalysisDF$Illness<-as.logical("")
  return(AnalysisDF)
}



Func_DR_Main<-function(AnalysisDF, Reps_DR){
  #AnalysisDF = ST_ON_Analysis
  List_week_DR<-split(x=AnalysisDF,f=AnalysisDF$week)
  lista<-replicate(Reps_DR,lapply(List_week_DR,Func_Rep_DR),simplify = FALSE)
  list_Df_Rep<-lapply(lista, Func_List2DF)
  list_Df_Rep<-lapply(list_Df_Rep, setNames, c("Infections", "Illness"))
  list_Df_Rep<-lapply(list_Df_Rep,bind_rows)
  list_Df_Rep<-bind_cols(list_Df_Rep)
  
  df_inf_Week = list_Df_Rep[,seq(1, ncol(list_Df_Rep), 2) ]
  df_ill_Week = list_Df_Rep[,seq(2, ncol(list_Df_Rep), 2) ]
  
  df_inf_Week<-as.data.frame(t(df_inf_Week))
  df_ill_Week<-as.data.frame(t(df_ill_Week))
  
  rownames(df_inf_Week)<-paste("Rep",1:Reps_DR)
  rownames(df_ill_Week)<-paste("Rep",1:Reps_DR)
  colnames(df_inf_Week)<-paste("Week",1:Sens_Iterations)
  colnames(df_ill_Week)<-paste("Week",1:Sens_Iterations)
  
  OutputsDR<-list(df_inf_Week = df_inf_Week, df_ill_Week=df_ill_Week)
}


Func_DF_Prevalence<-function(AnalysisDFCop, df_Ill_Week, Intervention){
  #AnalysisDFCop= ST_ON_Analysis_Copy
  #df_Ill_Week = df_ill_Week_ON
  #Interventions = "Int"
  
  df1<-AnalysisDFCop %>% 
    group_by(week) %>% 
    summarize(count=n())
  df1$week<-paste("Week",df1$week)
  
  df_Ill_Week_Box <- melt(df_Ill_Week, measure.var = paste("Week",1:Sens_Iterations))
  
  df_Ill_Week_Box_Prev<-merge(df_Ill_Week_Box, df1, by.x='variable', by.y='week')
  df_Ill_Week_Box_Prev$Prev<-df_Ill_Week_Box_Prev$value/df_Ill_Week_Box_Prev$count
  
  df_Ill_Week_Box_Prev$Int<-Intervention
  
  return(df_Ill_Week_Box_Prev)
}


func_remove_repeats<-function(DF){
  DF %>% 
    group_by(ID) %>% 
    filter(TotServices==max(TotServices))->DF
  return(DF)
}

Func_DF_Locations_1<-function(){
  ST_OFF_Analysis_Con<-func_remove_repeats(Individual_Analysis_Fr_CopOFF)
  ST_ON_Analysis_Con<-func_remove_repeats(Individual_Analysis_Fr_CopON)
  ST_ONWash_Analysis_Con<-func_remove_repeats(Individual_Analysis_Fr_CopONWash)
  ST_ONWr_Analysis_Con<-func_remove_repeats(Individual_Analysis_Fr_CopONWr)
  ST_OFFWash_Analysis_Con<-func_remove_repeats(Individual_Analysis_Fr_CopOFFWash)
  ST_OFFWr_Analysis_Con<-func_remove_repeats(Individual_Analysis_Fr_CopOFFWr)
  
  Total_Items_ON<-nrow(ST_ON_Analysis_Con)
  Total_Items_OFF<-nrow(ST_OFF_Analysis_Con)
  
  ST_OFF_Analysis_Con$Type<-"OFF"
  ST_ON_Analysis_Con$Type<-"ON"
  ST_ONWash_Analysis_Con$Type<-"ONWash"
  ST_ONWr_Analysis_Con$Type<-"ONWr"
  ST_OFFWash_Analysis_Con$Type<-"OFFWash"
  ST_OFFWr_Analysis_Con$Type<-"OFFWr"
  
  ST_Comb_Analysis_Con<-bind_rows(ST_OFF_Analysis_Con,ST_ON_Analysis_Con,ST_ONWash_Analysis_Con,ST_ONWr_Analysis_Con,ST_OFFWash_Analysis_Con,ST_OFFWr_Analysis_Con)
  
  return(ST_Comb_Analysis_Con)
}



Func_DF_Locations<-function(){
  ST_OFF_Analysis_Con<-func_remove_repeats(Individual_Analysis_Fr_CopOFF)
  ST_ON_Analysis_Con<-func_remove_repeats(Individual_Analysis_Fr_CopON)
  ST_ONWash_Analysis_Con<-func_remove_repeats(Individual_Analysis_Fr_CopONWash)
  ST_ONWr_Analysis_Con<-func_remove_repeats(Individual_Analysis_Fr_CopONWr)
  ST_OFFWash_Analysis_Con<-func_remove_repeats(Individual_Analysis_Fr_CopOFFWash)
  ST_OFFWr_Analysis_Con<-func_remove_repeats(Individual_Analysis_Fr_CopOFFWr)
  
  T1<-nrow(ST_ON_Analysis_Con)
  T2<-nrow(ST_OFF_Analysis_Con)
  T3<-nrow(ST_ONWash_Analysis_Con)
  T4<-nrow(ST_ONWr_Analysis_Con)
  T5<-nrow(ST_OFFWash_Analysis_Con)
  T6<-nrow(ST_OFFWr_Analysis_Con)
  
  T1C<-sum(ST_ON_Analysis_Con$Location=="Consumed")
  T2C<-sum(ST_OFF_Analysis_Con$Location=="Consumed")
  T3C<-sum(ST_ONWash_Analysis_Con$Location=="Consumed")
  T4C<-sum(ST_ONWr_Analysis_Con$Location=="Consumed")
  T5C<-sum(ST_OFFWash_Analysis_Con$Location=="Consumed")
  T6C<-sum(ST_OFFWr_Analysis_Con$Location=="Consumed")
  
  T1D<-sum(ST_ON_Analysis_Con$Location=="Discarded")
  T2D<-sum(ST_OFF_Analysis_Con$Location=="Discarded")
  T3D<-sum(ST_ONWash_Analysis_Con$Location=="Discarded")
  T4D<-sum(ST_ONWr_Analysis_Con$Location=="Discarded")
  T5D<-sum(ST_OFFWash_Analysis_Con$Location=="Discarded")
  T6D<-sum(ST_OFFWr_Analysis_Con$Location=="Discarded")
  
 Vector_Total_Items<-c(T1,T2,T3,T4,T5,T6)
 Vector_Total_Item_Con<-c(T1C,T2C,T3C,T4C,T5C,T6C)
 Vector_Total_Item_Dis<-c(T1D,T2D,T3D,T4D,T5D,T6D)
  
  outputsLocations<-list(Vector_Total_Items=Vector_Total_Items,Vector_Total_Item_Con=Vector_Total_Item_Con,Vector_Total_Item_Dis=Vector_Total_Item_Dis)
  
  return(outputsLocations)
}


Func_DF_Histogram_Log<-function(AnalysisDF,Intervention){
  #AnalysisDF = AnalysisDF
  #Intervention= "OnOFF
  ST_Analysis_Log<-AnalysisDF
  ST_Analysis_Log$Contamination[ST_Analysis_Log$Contamination==0]<-(.99)
  ST_Analysis_Log$Log<-log10(ST_Analysis_Log$Contamination)
  ST_Analysis_Log$Category<-""
  ST_Analysis_Log$OnOff<-Intervention
  
  
  for (i in 1:nrow(ST_Analysis_Log)){
    Conta<-as.numeric( ST_Analysis_Log$Contamination[i])
    if(Conta == .99 ){
      ST_Analysis_Log$Category[i] <-"0"
    } else if(Conta >0 && Conta< 100 ){
      ST_Analysis_Log$Category[i]  <-"<0 - 99"
    }else if(Conta >= 100 && Conta < 1000 ){
      ST_Analysis_Log$Category[i]  <-"100 - 999"
    }else if(Conta >=1000 ){
      ST_Analysis_Log$Category[i]  <-">=1000"
    }
  }
  return(ST_Analysis_Log)
}


Func_DF_Barplot_Log<-function(ST_Analysis_Log,Intervention){
  PlotOrder<-c("0","<0 - 99","100 - 999",">=1000")
  
  ST_Analysis_Log %>% 
    count(Category) %>% 
    mutate(perc = n / nrow(ST_Analysis_Log)) %>%
    slice(match(Category,PlotOrder))-> tips2
  
  tips2$OnOff<-Intervention
  
  return(tips2)
}



Func_NSA_Summary<-function( Trial){
  #Outlocation = "MaxOut
  #Trial = "T1"
  #Creating Data Frame of Consumed Items for all the products
  #Copy of Compiled Data Frame
  T1_Original<-bind_rows(List_Sens_Fr)
  
  #Data Frame without repeats and Consumed Items
  Treatment1<-Func_Asys_MainDF(SummaryList = List_Sens_Fr,FoodType = "Fruit")
  
  #Treatment 1
  
  Treatment1_Copy<-Treatment1
  #ST_ON_Analysis<-ST_ON_Analysis_Copy
  Treatment1<-Func_Asys_ContbyStudent(Treatment1)
  #Drop NAs
  Treatment1<- Treatment1[!is.na(Treatment1$Contamination), ]
  
  #Analysis For weekly Dose Response ON
  
  OutputsDRT1<-Func_DR_Main(AnalysisDF = Treatment1, Reps_DR = 100)
  df_inf_Week_T1<-OutputsDRT1$df_inf_Week
  df_ill_Week_T1<-OutputsDRT1$df_ill_Week
  
  df_ill_Week_T1_Prev<-Func_DF_Prevalence(AnalysisDFCop = Treatment1_Copy, df_Ill_Week = df_ill_Week_T1,Intervention = Trial)
  
  meanT1<-mean(df_ill_Week_T1_Prev$Prev)
  
  
  return(meanT1)
}
