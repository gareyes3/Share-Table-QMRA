

# Outputs creation on backup data frame -----------------------------------------------------------------

Fr_DF <- paste("Fr_Data.Frame_M", j, sep = "")
assign(Fr_DF, Fr_Data.Frame)

Pss_DF <- paste("Pss_Data.Frame_M", j, sep = "")
assign(Pss_DF, Pss_Data.Frame)

Pre_DF <- paste("Pre_Data.Frame_M", j, sep = "")
assign(Pre_DF, Pre_Data.Frame)


# VECTORS -----------------------------------------------------------------

Fr_Vector_Sh <- paste("V_Shared_Fr_M", j, sep = "")
assign(Fr_Vector_Sh, V_Shared_Fr)

Pss_Vector_Sh <- paste("V_Shared_Pss_M", j, sep = "")
assign(Pss_Vector_Sh, V_Shared_Pss)

Pre_Vector_Sh <- paste("V_Shared_Pre_M", j, sep = "")
assign(Pre_Vector_Sh, V_Shared_Pre)




# OUTPUTS FOR MEALS -----------------------------------------------------

#Fruit


#Fruit that stayed in share table. 
Left_ST_Fr<-Fr_Data.Frame[which(Fr_Data.Frame$Location == "Shared"),]
#Fruit that stayed in Selection table
Left_Selection_Fr<-Fr_Data.Frame[which(Fr_Data.Frame$Location == "Selection Table"),]

#Washing log reduction: 

#Washing selection Items

if(Wash_Selection_YN_Fr==1){
  Left_Selection_Fr$Contamination<-Func_Logred(Left_Selection_Fr$Contamination,Reduction_wash)
}

#washing share table items

if(Wash_ST_YN_Fr==1){
  Left_ST_Fr$Contamination<-Func_Logred(Left_ST_Fr$Contamination,Reduction_wash)
}



#Consumed Fr day 1, for exposure assesment
Consumed_Fr<-Fr_Data.Frame[which(Fr_Data.Frame$Location == "Consumed"),]

#Amounts left of fruit. 
No_Left_ST_Fr<-nrow(Left_ST_Fr)
No_Left_Selection_Fr<-nrow(Left_Selection_Fr)


#Pss


#Pss that stayed in share table. 
Left_ST_Pss<-Pss_Data.Frame[which(Pss_Data.Frame$Location == "Shared"),]
#Pss that stayed in Selection table
Left_Selection_Pss<-Pss_Data.Frame[which(Pss_Data.Frame$Location == "Selection Table"),]
#Consumed Pss day 1, for exposure assesment
Consumed_Pss<-Pss_Data.Frame[which(Pss_Data.Frame$Location == "Consumed"),]

#Amounts left of fruit. 
No_Left_ST_Pss<-nrow(Left_ST_Pss)
No_Left_Selection_Pss<-nrow(Left_Selection_Pss)


#Pre


#Pre that stayed in share table. 
Left_ST_Pre<-Pre_Data.Frame[which(Pre_Data.Frame$Location == "Shared"),]
#Pre that stayed in Selection table
Left_Selection_Pre<-Pre_Data.Frame[which(Pre_Data.Frame$Location == "Selection Table"),]
#Consumed Pre day 1, for exposure assesment
Consumed_Pre<-Pre_Data.Frame[which(Pre_Data.Frame$Location == "Consumed"),]

#Amounts left of fruit. 
No_Left_ST_Pre<-nrow(Left_ST_Pre)
No_Left_Selection_Pre<-nrow(Left_Selection_Pre)


# Left per meal -----------------------------------------------------------

Fr_Left_Sel <- paste("No_Left_Selection_Fr_M", j, sep = "")
assign(Fr_Left_Sel, No_Left_Selection_Fr)



