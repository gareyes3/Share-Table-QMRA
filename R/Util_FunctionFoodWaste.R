


# Functions ---------------------------------------------------------------


Func_LocationST<-function(DF){
  nrow(DF %>% 
      filter(ConsumedAt=="ShareTable"))
}


Func_LocationSel<-function(DF){
  nrow(DF %>% 
         filter(ConsumedAt=="ServiceLine"))
}


Func_LocationReusedSel<-function(DF){
  nrow(DF %>% 
         filter(ConsumedAt=="ServiceLine") %>% 
         filter(STtimes>0))
}




Func_LocationReusedDisc<-function(DF){
  nrow(DF %>% 
         filter(Location=="Discarded") %>% 
         filter(STtimes>0))
}


Function_FoodWaste<-function(DF){
  TotalT<-nrow(DF)
  TotalC<-sum(DF$Location=="Consumed")
  TotalD<-sum(DF$Location=="Discarded")
  TotalR<-sum(DF$Reserviced>0)
  ConSel<-Func_LocationSel(DF)
  ConST<-Func_LocationST(DF)
  Output_FW<-c(TotalT,TotalC,TotalD,TotalR,ConSel,ConST)
  
  return(Output_FW)
}

Function_FoodWaste_byweek<-function(DF){
  TotalT<-nrow(DF)
  TotalC<-sum(DF$Location=="Consumed")
  TotalD<-sum(DF$Location=="Discarded")
  TotalR<-sum(DF$Reserviced>0)
  ConSel<-Func_LocationSel(DF)
  ConST<-Func_LocationST(DF)
  Output_FW<-c(TotalT,TotalC,TotalD,TotalR,ConSel,ConST)
  
  return(Output_FW)
}

#UPdate this
Analysis_1<-func_remove_repeats(Individual_Analysis_Fr_CopOFF)

ST_OFF_Summary<-Individual_Analysis_Fr_CopOFF %>% 
  func_remove_repeats() %>% 
  group_by(week) %>% 
  summarise(Consumed = sum(Location =="Consumed"),
            Discarded = sum(Location =="Discarded"),
            Reserviced = sum(Reserviced>0),
            Consumed_Sel = sum(ConsumedAt=="ServiceLine"),
            Consumed_ST  = sum(ConsumedAt=="ShareTable"),
            Total = n()
            ) 

ST_ON_Summary <-Individual_Analysis_Fr_CopON %>% 
  func_remove_repeats() %>% 
  group_by(week) %>% 
  summarise(Consumed = sum(Location =="Consumed"),
            Discarded = sum(Location =="Discarded"),
            Reserviced = sum(Reserviced>0),
            Consumed_Sel = sum(ConsumedAt=="ServiceLine"),
            Consumed_ST  = sum(ConsumedAt=="ShareTable"),
            Total = n()
  ) 

Differences_Landfilled<-(ST_OFF_Summary$Discarded-ST_ON_Summary$Discarded)/ST_OFF_Summary$Discarded

quantile(Differences_Landfilled, c(0.025,0.975))

Difference_Consumed<-(ST_OFF_Summary$Consumed-ST_ON_Summary$Consumed)/ST_OFF_Summary$Consumed

quantile(Difference_Consumed, c(0.025,0.975))

Difference_Used<-(ST_OFF_Summary$Total-ST_ON_Summary$Total)/ST_OFF_Summary$Total

quantile(Difference_Used, c(0.025,0.975))

hist(Difference_Used)

# Main Function --------------------------------------------------------------

Function_FoodWaste_Full<-function(){
  ST_OFF_Analysis_Con<-func_remove_repeats(Individual_Analysis_Fr_CopOFF)
  ST_ON_Analysis_Con<-func_remove_repeats(Individual_Analysis_Fr_CopON)
  ST_STClosed_Analysis_Con<-func_remove_repeats(Individual_Analysis_Fr_STClosed)
  ST_Exc_Analysis_Con<-func_remove_repeats(Individual_Analysis_Fr_CopExc)
  ST_STAside_Analysis_Con<-func_remove_repeats(Individual_Analysis_Fr_CopSTAside)
  
  FW_OFF<-Function_FoodWaste(ST_OFF_Analysis_Con)
  FW_ON<-Function_FoodWaste(ST_ON_Analysis_Con)
  FW_STClosed<-Function_FoodWaste(ST_STClosed_Analysis_Con)
  FW_Exc<-Function_FoodWaste(ST_Exc_Analysis_Con)
  FW_STAside<-Function_FoodWaste(ST_STAside_Analysis_Con)
  
  FoodWaste_Analysis[1,2:7]<-FW_OFF
  FoodWaste_Analysis[2,2:7]<-FW_ON
  FoodWaste_Analysis[3,2:7]<- FW_STClosed
  FoodWaste_Analysis[4,2:7]<-FW_Exc
  FoodWaste_Analysis[5,2:7]<-FW_STAside
 
  return(FoodWaste_Analysis) 
}




