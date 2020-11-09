# Data Frame Creation Functions -------------------------------------------

#Function for creation of Data Frames Service 1 day 1. 
Fuct_DF_Initial<-function(FoodType){
  if (FoodType == "Fruit" ){
    Data_Frame<-data.frame("Apple No." = 1:Initial_Fr,
                           "ID"= paste(l,k,j,1:Initial_Fr),
                           "Location"= "Selection Table",
                           "Contamination" = as.numeric("0"),
                           "ContConsumed"=as.numeric("0"),
                           "InContamination"  = as.numeric("0"),
                           "ExposedAllergen" = FALSE,
                           "TotTime"= as.numeric("0"),
                           "History" = "",
                           "TouchesContHist"= as.numeric(""),
                           "TotServices"=as.numeric("0"),
                           "STtimes"= as.numeric("0"),
                           "Initial Service" = "1",
                           "Service" = j,
                           "Initial Day" = "1",
                           "Day" = k,
                           stringsAsFactors = FALSE
                           
    )
  } else if (FoodType == "Pss"){
    Data_Frame<-data.frame("Pss No." = 1:Initial_Pss,
                           "ID"= paste(l,k,j,1:Initial_Pss),
                           "Location"= "Selection Table",
                           "Contamination" = as.numeric("0"),
                           "ContConsumed"=as.numeric("0"),
                           "InContamination"  = as.numeric("0"),
                           "ExposedAllergen" = FALSE,
                           "TotTime"= as.numeric("0"),
                           "History"= "",
                           "TouchesContHist"= as.numeric(""),
                           "TotServices"=as.numeric("0"),
                           "STtimes"= as.numeric("0"),
                           "Initial Service" = "1", 
                           "Service" = j,
                           "Initial Day" = "1",
                           "Day" = k,
                           stringsAsFactors = FALSE
    )
  }else if (FoodType == "Pre"){
    Data_Frame<-data.frame("Pre No." = 1:Initial_Pss,
                           "ID"= paste(l,k,j,1:Initial_Pre),
                           "Location"= "Selection Table",
                           "Contamination" = as.numeric("0"),
                           "ContConsumed"=as.numeric("0"),
                           "InContamination"  = as.numeric("0"),
                           "ExposedAllergen" = FALSE,
                           "SpoilageCon" = as.numeric(Initial_Spoilage_Con),
                           "SpoiledYN" = FALSE,
                           "TotTime"= as.numeric("0"),
                           "History" = "", 
                           "TouchesContHist"= as.numeric(""),
                           "TotServices"=as.numeric("0"),
                           "STtimes"= as.numeric("0"),
                           "Initial Service" = "1",
                           "Service" = j,
                           "Initial Day" = "1",
                           "Day" = k,
                           stringsAsFactors = FALSE
    )
  }
  return(Data_Frame)
}

#Function for creating data frames that take into consideration food prom previouss data frames. 
Fuct_DF_Reservice<-function(FoodType){
  if (FoodType == "Fruit" ){
    Data_Frame<-data.frame("Apple No." = 1:(Initial_Fr-(No_Left_Selection_Fr)),
                           "ID"= paste(l,k,j,1:(Initial_Fr-(No_Left_Selection_Fr))),
                           "Location"= "Selection Table",
                           "Contamination" = as.numeric("0"),
                           "ContConsumed"=as.numeric("0"),
                           "InContamination"  = as.numeric("0"),
                           "ExposedAllergen" = FALSE,
                           "TotTime"= as.numeric("0"),
                           "History" = "", 
                           "TouchesContHist"= as.numeric(""),
                           "TotServices"=as.numeric("0"),
                           "STtimes"= as.numeric("0"),
                           "Initial Service" = j,
                           "Service" = j,
                           "Initial Day" = k,
                           "Day" = k,
                           stringsAsFactors = FALSE
                           
    )
  } else if (FoodType == "Pss"){
    Data_Frame<-data.frame("Pss No." = 1:(Initial_Pss-(No_Left_Selection_Pss)),
                           "ID"= paste(l,k,j,1:(Initial_Pss-(No_Left_Selection_Pss))),
                           "Location"= "Selection Table",
                           "Contamination" = as.numeric("0"),
                           "ContConsumed"=as.numeric("0"),
                           "InContamination"  = as.numeric("0"),
                           "ExposedAllergen" = FALSE,
                           "TotTime"= as.numeric("0"),
                           "History" = "", 
                           "TouchesContHist"= as.numeric(""),
                           "TotServices"=as.numeric("0"),
                           "STtimes"= as.numeric("0"),
                           "Initial Service" = j,
                           "Service" = j,
                           "Initial Day" = k,
                           "Day" = k,
                           stringsAsFactors = FALSE
                           
    )
  }else if (FoodType == "Pre"){
    Data_Frame<-data.frame("Pre No." = 1:(Initial_Pre-(No_Left_Selection_Pre)),
                           "ID"= paste(l,k,j,1:(Initial_Pre-(No_Left_Selection_Pre))),
                           "Location"= "Selection Table",
                           "Contamination" = as.numeric("0"),
                           "ContConsumed"=as.numeric("0"),
                           "InContamination"  = as.numeric("0"),
                           "ExposedAllergen" = FALSE,
                           "SpoilageCon" = as.numeric(Initial_Spoilage_Con),
                           "SpoiledYN" = FALSE,
                           "TotTime"= as.numeric("0"),
                           "History" = "", 
                           "TouchesContHist"= as.numeric(""),
                           "TotServices"=as.numeric("0"),
                           "STtimes"= as.numeric("0"),
                           "Initial Service" = j,
                           "Service" = j,
                           "Initial Day" = k,
                           "Day" = k,
                           stringsAsFactors = FALSE
                           
    )
  }
  return(Data_Frame)
}

#Function creating Data frame that feeds items if they run out
Fuct_DF_Feeding<-function(FoodType){
  if(FoodType == "Fruit"){
    Data_Frame<-data.frame("Apple No." = 1:Row_size_Fr,
                           "ID"= paste(l,k,j,nrow(Fr_Data.Frame)+1:Row_size_Fr),
                           "Location"= "Selection Table",
                           "Contamination" = as.numeric("0"),
                           "ContConsumed"=as.numeric("0"),
                           "InContamination"  = as.numeric("0"),
                           "ExposedAllergen" = FALSE,
                           "TotTime"= as.numeric("0"),
                           "History" = "", 
                           "TouchesContHist"= as.numeric(""),
                           "TotServices"=as.numeric("0"),
                           "STtimes"= as.numeric("0"),
                           "Initial Service" = "1",
                           "Service" = j,
                           "Initial Day" = "1",
                           "Day" = k,
                           stringsAsFactors = FALSE
                           
    )
  } else if (FoodType == "Pss"){
    Data_Frame<-data.frame("Pss No." = 1:Row_size_Pss,
                           "ID"= paste(l,k,j,nrow(Pss_Data.Frame)+1:Row_size_Pss),
                           "Location"= "Selection Table",
                           "Contamination" = as.numeric("0"),
                           "ContConsumed"=as.numeric("0"),
                           "InContamination"  = as.numeric("0"),
                           "ExposedAllergen" = FALSE,
                           "TotTime"= as.numeric("0"),
                           "History" = "", 
                           "TouchesContHist"= as.numeric(""),
                           "TotServices"=as.numeric("0"),
                           "STtimes"= as.numeric("0"),
                           "Initial Service" = "1",
                           "Service" = j,
                           "Initial Day" = "1",
                           "Day" = k,
                           stringsAsFactors = FALSE
    )
  }else if (FoodType=="Pre"){
    Data_Frame<-data.frame("Pre No." = 1:Row_size_Pre,
                           "ID"= paste(l,k,j,nrow(Pre_Data.Frame)+1:Row_size_Pre),
                           "Location"= "Selection Table",
                           "Contamination" = as.numeric("0"),
                           "ContConsumed"=as.numeric("0"),
                           "InContamination"  = as.numeric("0"),
                           "ExposedAllergen" = FALSE,
                           "SpoilageCon" = as.numeric(Initial_Spoilage_Con),
                           "SpoiledYN" = FALSE,
                           "TotTime"= as.numeric("0"),
                           "History" = "",
                           "TouchesContHist"= as.numeric(""),
                           "TotServices"=as.numeric("0"),
                           "STtimes"= as.numeric("0"),
                           "Initial Service" = "1",
                           "Service" = j,
                           "Initial Day" = "1",
                           "Day" = k,
                           stringsAsFactors = FALSE
    )
  }
  return(Data_Frame)
}

#Function for adding Contamination of fruit in data frame into the vector
Func_FoodCont_Vector<-function(DF,Vector){
  Items_Added<-DF$Contamination[which(DF$Contamination>0)]
  Vector<-c(Vector,Items_Added)
  return(Vector)
}
