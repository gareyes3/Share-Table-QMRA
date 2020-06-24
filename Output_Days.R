
# Creation of Day Data Frames ---------------------------------------------

Fr_DF_Day <- paste("Fr_Data_D", k, sep = "")
assign(Fr_DF_Day, Fr_Data)

Pss_DF_Day <- paste("Pss_Data_D", k, sep = "")
assign(Pss_DF_Day, Pss_Data)

Pre_DF_Day <- paste("Pre_Data_D", k, sep = "")
assign(Pre_DF_Day, Pss_Data)


# Items That were left on each service ------------------------------------


#Fruit that stayed in share table. 
Left_ST_Fr<-Fr_Data.Frame[which(Fr_Data.Frame$Location == "Shared"),]
#Fruit that stayed in Selection table
Left_Selection_Fr<-Fr_Data.Frame[which(Fr_Data.Frame$Location == "Selection Table"),]

#Pss that stayed in share table. 
Left_ST_Pss<-Pss_Data.Frame[which(Pss_Data.Frame$Location == "Shared"),]
#Pss that stayed in Selection table
Left_Selection_Pss<-Pss_Data.Frame[which(Pss_Data.Frame$Location == "Selection Table"),]

#Pre that stayed in share table. 
Left_ST_Pre<-Pre_Data.Frame[which(Pre_Data.Frame$Location == "Shared"),]
#Pre that stayed in Selection table
Left_Selection_Pre<-Pre_Data.Frame[which(Pre_Data.Frame$Location == "Selection Table"),]



# Adding Overnight Time to Food Items ----------------------------------------

#Overnight Fruit Selection
Left_Selection_Fr$TotTime<-Func_Adding_Time(Left_Selection_Fr$TotTime, Time_ON)
#Overnight Fruit Selection Table
Left_ST_Fr$TotTime<-Func_Adding_Time(Left_ST_Fr$TotTime, Time_ON)

#Overnight Pss Selection
Left_Selection_Pss$TotTime<-Func_Adding_Time(Left_Selection_Pss$TotTime, Time_ON)
#Overnight Pss Selection Table
Left_ST_Pss$TotTime<-Func_Adding_Time(Left_ST_Pss$TotTime, Time_ON)

#Overnight Pre Selection
Left_Selection_Pre$TotTime<-Func_Adding_Time(Left_Selection_Pre$TotTime, Time_ON)
#Overnight Pre Selection Table
Left_ST_Pre$TotTime<-Func_Adding_Time(Left_ST_Pre$TotTime, Time_ON)


#Growth During overnight storage----------------------------------------------

#Selection Items #chose which type of storage. 

if(salmonella ==1 && Growth ==1 ){
  if(length(nrow(Left_Selection_Fr)>0)){
    Func_Enteric_Growth_Storage("salmonella", "room temp",Left_Selection_Fr)
    Left_Selection_Fr<-DF
  }
}else if (E_coli ==1 && Growth ==1 ){
  if(length(nrow(Left_Selection_Fr)>0)){
    Func_Enteric_Growth_Storage("E_coli", "room temp",Left_Selection_Fr)
  }
}

#Share Table Items
if(salmonella ==1 && Growth ==1){
  if(length(nrow(Left_Selection_Fr)>0)){
    Func_Enteric_Growth_Storage("salmonella", "room temp",Left_ST_Fr)
    Left_ST_Fr<-DF
  }
}else if(E_coli ==1 && Growth ==1){
  if(length(nrow(Left_Selection_Fr)>0)){
    Func_Enteric_Growth_Storage("E_coli", "room temp",Left_ST_Fr)
    Left_ST_Fr<-DF
  }
}



# Washing Fruit post Days ----------------------------------------------

#Washing selection Items
if(Wash_Selection_YN_Fr==1){
  Left_Selection_Fr$Contamination<-Func_Logred(Left_Selection_Fr$Contamination,Reduction_wash)
}

#washing share table items
if(Wash_ST_YN_Fr==1){
  Left_ST_Fr$Contamination<-Func_Logred(Left_ST_Fr$Contamination,Reduction_wash)
}

