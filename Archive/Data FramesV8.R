# FOOD POOL DATAFRAMES ----------------------------------------------------



#Meal Day 1================================================================


#Fruit Data Frame
if(j==1){
Fr_Data.Frame<-data.frame("Apple No." = 1:Initial_Fr,
                          "Location"= "Selection Table",
                          "Contamination" = as.numeric(Initial_Cont_Fr),
                          "Meal" = "1",
                          "History" = "", 
                          "STtimes"= as.numeric("0"),
                          "Service" = j,
                          stringsAsFactors = FALSE
                          
)


#Packaged Shelf Stable Data Frame
Pss_Data.Frame<-data.frame("Pss No." = 1:Initial_Pss,
                           "Location"= "Selection Table",
                           "Contamination" = as.numeric(Initial_Cont_Pss),
                           "Meal" = "1", 
                           "History"= "",
                           "STtimes"= as.numeric("0"),
                           "Service" = j,
                           stringsAsFactors = FALSE
)

#Packaged Shelf Stable Data Frame
Pre_Data.Frame<-data.frame("Pre No." = 1:Initial_Pss,
                           "Location"= "Selection Table",
                           "Contamination" = as.numeric(Initial_Cont_Pre),
                           "Meal" = "1",
                           "History" = "", 
                           "STtimes"= as.numeric("0"),
                           "Service" = j,
                            stringsAsFactors = FALSE
)
} #end of J1


#Meal Day 2 ===============================================================



if(j>1 ){
  
  if(Reservice_YN==1){
    
                                  #Fruit Data Frame
    
    Fr_Data.Frame<-data.frame("Apple No." = 1:(Initial_Fr-(No_Left_Selection_Fr)),
                              "Location"= "Selection Table",
                              "Contamination" = as.numeric(Initial_Cont_Fr),
                              "Meal" = j,
                              "History" = "", 
                              "STtimes"= as.numeric("0"),
                              "Service" = j,
                              stringsAsFactors = FALSE
                              
    )
    
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
    
                            #Packaged Shelf Stable Data Frame
    
    Pss_Data.Frame<-data.frame("Pss No." = 1:(Initial_Pss-(No_Left_Selection_Pss)),
                              "Location"= "Selection Table",
                              "Contamination" = as.numeric(Initial_Cont_Pss),
                              "Meal" = j,
                              "History" = "", 
                              "STtimes"= as.numeric("0"),
                              "Service" = j,
                              stringsAsFactors = FALSE
                              
    )
    
    
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
    
                          #Packaged refrigerated Data Frame
    
    Pre_Data.Frame<-data.frame("Pre No." = 1:(Initial_Pre-(No_Left_Selection_Pre)),
                               "Location"= "Selection Table",
                               "Contamination" = as.numeric(Initial_Cont_Pre),
                               "Meal" = j,
                               "History" = "", 
                               "STtimes"= as.numeric("0"),
                               "Service" = j,
                               stringsAsFactors = FALSE
                               
    )
    
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
    
  #Else if statements for no reservice
    
  } else if (Reservice_YN==0){
    
                                #Fruit Data Frame
    
    
     Fr_Data.Frame<-data.frame("Apple No." = 1:Initial_Fr,
                              "Location"= "Selection Table",
                              "Contamination" = as.numeric(Initial_Cont_Fr),
                              "Meal" = "1",
                              "History" = "", 
                              "STtimes"= as.numeric("0"),
                              "Service" = j,
                              stringsAsFactors = FALSE
                              
    )
    
    #Adding Share Times
    
    #Resharing Toggle
    if(Resharing_YN ==1){
      Fr_Data.Frame<-rbind(Fr_Data.Frame,Left_ST_Fr)
    }
    #Resetting Numbers and counters
    Fr_Data.Frame$Apple.No.<- 1:nrow(Fr_Data.Frame)
    row.names(Fr_Data.Frame)<-1:nrow(Fr_Data.Frame)
    Fr_Data.Frame$Service<-j
    
    
                             #Packaged Shelf Stable Data Frame
    
    Pss_Data.Frame<-data.frame("Pss No." = 1:Initial_Pss,
                               "Location"= "Selection Table",
                               "Contamination" = as.numeric(Initial_Cont_Pss),
                               "Meal" = "1", 
                               "History"= "",
                               "STtimes"= as.numeric("0"),
                               "Service" = j,
                               stringsAsFactors = FALSE
    )
    
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
    
                              #Packaged Refrigerated Data Frame
    
    
    Pre_Data.Frame<-data.frame("Pre No." = 1:Initial_Pss,
                               "Location"= "Selection Table",
                               "Contamination" = as.numeric(Initial_Cont_Pre),
                               "Meal" = "1",
                               "History" = "", 
                               "STtimes"= as.numeric("0"),
                               "Service" = j,
                               stringsAsFactors = FALSE
    )
    
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
    
  } #end of resahring if
  
  
}#end of j loop


# Vectors -----------------------------------------------------------------

V_Shared_Fr<-c(0)
V_Shared_Pss<-c(0)
V_Shared_Pre<-c(0)



