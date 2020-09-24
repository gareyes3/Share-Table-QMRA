

#Cross_Contamination Functions

Func_Cross_Contamination_Fr<-function(Cont_Student,Fr_Data.Frame, Fr_Picked){
  Tr_H_Fr<-Cont_Student*TE_H_F #Transfer from Hand to Fruit
  Tr_Fr_H<-(Func_Index_DF(Fr_Data.Frame,Fr_Picked,"Contamination")* TE_F_H) #Tranfer from fruit to hand
  Cont_Fr_Updated<- Func_Index_DF(Fr_Data.Frame,Fr_Picked,"Contamination") + Tr_H_Fr - (Tr_Fr_H) #New Contamination of Fruit
  Cont_Fr_Difference<-Func_Index_DF(Fr_Data.Frame,Fr_Picked,"Contamination")-(Cont_Fr_Updated) #Difference in contamination to update student contamination
  Fr_Data.Frame[Fr_Picked,colnames(Fr_Data.Frame)== "Contamination"]<-Cont_Fr_Updated #update the Fr Contamination in Data frame
  Cont_Student<-ifelse(Cont_Student +(Cont_Fr_Difference)<0,0,Cont_Student +(Cont_Fr_Difference)) #Updating Contamination in Student's hands
  #Adding Variables to Global Environment
  Fr_Data.Frame<<-Fr_Data.Frame
  Cont_Student<<-Cont_Student
}

Func_Cross_Contamination_Pss<-function(Cont_Student,Pss_Data.Frame, Pss_Picked){
  Tr_H_Pss<-Cont_Student*TE_H_F #Tranfer from Hand to Fruit
  Tr_Pss_H<-(Func_Index_DF(Pss_Data.Frame,Pss_Picked,"Contamination")* TE_F_H) #Tranfer from Pss to hand
  Cont_Pss_Updated<- Func_Index_DF(Pss_Data.Frame,Pss_Picked,"Contamination") + Tr_H_Pss - (Tr_Pss_H) #New contamination Pss 
  Cont_Pss_Difference<-Func_Index_DF(Pss_Data.Frame,Pss_Picked,"Contamination")-(Cont_Pss_Updated) #difference in contamination to update student contamination
  Pss_Data.Frame[Pss_Picked,colnames(Pss_Data.Frame)== "Contamination"]<-Cont_Pss_Updated #updating Pss contamination in data frame
  Cont_Student<-ifelse(Cont_Student +(Cont_Pss_Difference)<0,0,Cont_Student +(Cont_Pss_Difference)) #Updating Contamination in Student's hands
  #Adding Variable to Global Environment
  Pss_Data.Frame<<-Pss_Data.Frame
  Cont_Student<<-Cont_Student
}

Func_Cross_Contamination_Pre<-function(Cont_Student,Pre_Data.Frame, Pre_Picked){
  Tr_H_Pre<-Cont_Student*TE_H_F #Tranfer from Hand to Fruit
  Tr_Pre_H<-(Func_Index_DF(Pre_Data.Frame,Pre_Picked,"Contamination")* TE_F_H) #Tranfer from Pre to hand
  Cont_Pre_Updated<- Func_Index_DF(Pre_Data.Frame,Pre_Picked,"Contamination") + Tr_H_Pre - (Tr_Pre_H) #New contamination Pre 
  Cont_Pre_Difference<-Func_Index_DF(Pre_Data.Frame,Pre_Picked,"Contamination")-(Cont_Pre_Updated) #difference in contamination to update student contamination
  Pre_Data.Frame[Pre_Picked,colnames(Pre_Data.Frame)== "Contamination"]<-Cont_Pre_Updated #updating Pre contamination in data frame
  Cont_Student<-ifelse(Cont_Student +(Cont_Pre_Difference)<0,0,Cont_Student +(Cont_Pre_Difference)) #Updating Contamination in Student's hands
  #Adding Variable to Global Environment
  Pre_Data.Frame<<-Pre_Data.Frame
  Cont_Student<<-Cont_Student
}

Func_Cross_Contamination_Fr_Consumption_Wrapped<-function(Cont_Student, Fr_Data.Frame, Fr_Picked){
  Tr_H_Fr<-Cont_Student*TE_H_S #Transfer from Hand to Fr
  Tr_Fr_H<-(Func_Index_DF(Fr_Data.Frame,Fr_Picked,"Contamination")* TE_S_H) #Tranfer from Fr to hand
  Cont_Fr_Updated<- Func_Index_DF(Fr_Data.Frame,Fr_Picked,"Contamination") + Tr_H_Fr - (Tr_Fr_H) #New Contamination of Fr
  Cont_Fr_Difference<-Func_Index_DF(Fr_Data.Frame,Fr_Picked,"Contamination")-(Cont_Fr_Updated) #Difference in contamination to update student contamination
  Fr_Data.Frame[Fr_Picked,colnames(Fr_Data.Frame)== "Contamination"]<-Cont_Fr_Updated #update the Fr Contamination in Data frame
  Cont_Student<-ifelse(Cont_Student +(Cont_Fr_Difference)<0,0,Cont_Student +(Cont_Fr_Difference)) #Updating Contamination in Student's hands
  Tr_H_Fr_Inside<-Cont_Student*TE_H_F
  Cont_Fr_Consumed<-Tr_H_Fr_Inside
  Fr_Data.Frame[Fr_Picked,colnames(Fr_Data.Frame)== "Contamination"]<-Cont_Fr_Consumed
  Fr_Data.Frame<<-Fr_Data.Frame
  Cont_Student<<-Cont_Student
}

Func_Cross_Contamination_Pss_Consumption<-function(Cont_Student, Pss_Data.Frame, Pss_Picked ){
  Tr_H_Pss<-Cont_Student*TE_H_S #Transfer from Hand to Pss
  Tr_Pss_H<-(Func_Index_DF(Pss_Data.Frame,Pss_Picked,"Contamination")* TE_S_H) #Tranfer from Pss to hand
  Cont_Pss_Updated<- Func_Index_DF(Pss_Data.Frame,Pss_Picked,"Contamination") + Tr_H_Pss - (Tr_Pss_H) #New Contamination of Pss
  Cont_Pss_Difference<-Func_Index_DF(Pss_Data.Frame,Pss_Picked,"Contamination")-(Cont_Pss_Updated) #Difference in contamination to update student contamination
  Pss_Data.Frame[Pss_Picked,colnames(Pss_Data.Frame)== "Contamination"]<-Cont_Pss_Updated #update the Pss Contamination in Data frame
  Cont_Student<-ifelse(Cont_Student +(Cont_Pss_Difference)<0,0,Cont_Student +(Cont_Pss_Difference)) #Updating Contamination in Student's hands
  Tr_H_Pss_Inside<-Cont_Student*TE_H_F
  Cont_Pss_Consumed<-Tr_H_Pss_Inside
  Pss_Data.Frame[Pss_Picked,colnames(Pss_Data.Frame)== "Contamination"]<-Cont_Pss_Consumed
  Pss_Data.Frame<<-Pss_Data.Frame
  Cont_Student<<-Cont_Student
}

Func_Cross_Contamination_Pre_Consumption<-function(Cont_Student, Pre_Data.Frame, Pre_Picked){
  Tr_H_Pre<-Cont_Student*TE_H_S #Transfer from Hand to Pre
  Tr_Pre_H<-(Func_Index_DF(Pre_Data.Frame,Pre_Picked,"Contamination")* TE_S_H) #Tranfer from Pre to hand
  Cont_Pre_Updated<- Func_Index_DF(Pre_Data.Frame,Pre_Picked,"Contamination") + Tr_H_Pre - (Tr_Pre_H) #New Contamination of Pre
  Cont_Pre_Difference<-Func_Index_DF(Pre_Data.Frame,Pre_Picked,"Contamination")-(Cont_Pre_Updated) #Difference in contamination to update student contamination
  Pre_Data.Frame[Pre_Picked,colnames(Pre_Data.Frame)== "Contamination"]<-Cont_Pre_Updated #update the Pre Contamination in Data frame
  Cont_Student<-ifelse(Cont_Student +(Cont_Pre_Difference)<0,0,Cont_Student +(Cont_Pre_Difference)) #Updating Contamination in Student's hands
  Tr_Pre_Mouth<-(Func_Index_DF(Pre_Data.Frame,Pre_Picked,"Contamination")* TE_Pre_Mouth)
  Cont_Pre_Consumed<-Tr_Pre_Mouth
  Pre_Data.Frame[Pre_Picked,colnames(Pre_Data.Frame)== "Contamination"]<-Cont_Pre_Consumed
  Pre_Data.Frame<<-Pre_Data.Frame
  Cont_Student<<-Cont_Student
}


