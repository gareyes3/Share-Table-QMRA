#Cross Contamination Functions 2. 

#Cross_Contamination Fruit. 

Func_Cross_Contamination<-function(Cont_Student,Data.Frame, Item_Picked, Item){
  if(salmonella==1){
      Conta<-Func_Index_DF(Data.Frame,Item_Picked,"Contamination")
      #update the Fr Contamination in Data frame
      Tr_H_F<-Cont_Student*TE_H_F #Transfer from Hand to Fruit
      Tr_F_H<-(Conta* TE_F_H) #Tranfer from fruit to hand
      #Contamination Tranfered History. 
      Overall_Tr<-(Tr_H_F-Tr_F_H)
      Data.Frame[Item_Picked,colnames(Data.Frame)== "TouchesContHist"]<-paste(Data.Frame[Item_Picked,colnames(Data.Frame)=="TouchesContHist"], as.numeric(Overall_Tr),sep = ",") #Adding Contamination to
      Cont_Updated<- Conta + Overall_Tr #New Contamination of Fruit
      Cont_Difference<-Conta-(Cont_Updated) #Difference in contamination to update student contamination
      Data.Frame[Item_Picked,colnames(Data.Frame)== "Contamination"]<-Cont_Updated #update the Fr Contamination in Data frame
      Cont_Student<-ifelse(Cont_Student +(Cont_Difference)<0,0,Cont_Student +(Cont_Difference)) #Updating Contamination in Student's hands
      #Adding Variables to Global Environment
      if (Item=="Fruit"){
        Data.Frame<<-Data.Frame
      } else if (Item == "PSS"){
        Pss_Data.Frame<<-Data.Frame
      }else if (Item=="PRE"){
        Pre_Data.Frame<<-Data.Frame
      }
      Cont_Student<<-Cont_Student
  } else if (norovirus ==1){
      Conta<-Func_Index_DF(Data.Frame,Item_Picked,"Contamination")
      Tr_H_F<-rbinom(n=1,size = Cont_Student, prob = TrP_H_F)
      Tr_F_H<-rbinom(n=1,size = Conta,prob = TrP_F_H)
      #Overall Transfer for tracking
      Overall_Tr<-(Tr_H_F-Tr_F_H) #GEC tranfered
      Data.Frame[Item_Picked,colnames(Data.Frame)== "TouchesContHist"]<-paste(Data.Frame[Item_Picked,colnames(Data.Frame)=="TouchesContHist"], as.numeric(Overall_Tr),sep = ",") #Adding Contamination to
      #Continuing Contamination: 
      Cont_Updated<- Conta + Overall_Tr #New Contamination of Fruit
      Cont_Difference<-Conta-(Cont_Updated) #Difference in contamination to update student contamination
      Data.Frame[Item_Picked,colnames(Data.Frame)== "Contamination"]<-Cont_Updated #update the Fr Contamination in Data frame
      Cont_Student<-ifelse(Cont_Student +( Cont_Difference)<0,0,Cont_Student +(Cont_Difference)) #Updating Contamination in Student's hands
      if (Item=="Fruit"){
        Fr_Data.Frame<<-Data.Frame
      } else if (Item == "PSS"){
        Pss_Data.Frame<<-Data.Frame
      }else if (Item=="PRE"){
        Pre_Data.Frame<<-Data.Frame
      }
      Cont_Student<<-Cont_Student
  }
}


Func_Cross_Contamination_Fr_Consumption_Wrapped<-function(Cont_Student, Fr_Data.Frame, Fr_Picked){
  Fr_Data.Frame[Fr_Picked,colnames(Fr_Data.Frame)== "TouchesContHist"]<-paste(Fr_Data.Frame[Fr_Picked,colnames(Fr_Data.Frame)=="TouchesContHist"], as.numeric(Cont_Student),sep = ",") #Adding Contamination to
  Tr_H_Fr<-Cont_Student*TE_H_S #Transfer from Hand to Fr
  Tr_Fr_H<-(Func_Index_DF(Fr_Data.Frame,Fr_Picked,"Contamination")* TE_S_H) #Tranfer from Fr to hand
  Cont_Fr_Updated<- Func_Index_DF(Fr_Data.Frame,Fr_Picked,"Contamination") + Tr_H_Fr - (Tr_Fr_H) #New Contamination of Fr
  Cont_Fr_Difference<-Func_Index_DF(Fr_Data.Frame,Fr_Picked,"Contamination")-(Cont_Fr_Updated) #Difference in contamination to update student contamination
  Fr_Data.Frame[Fr_Picked,colnames(Fr_Data.Frame)== "Contamination"]<-Cont_Fr_Updated #update the Fr Contamination in Data frame
  Cont_Student<-ifelse(Cont_Student +(Cont_Fr_Difference)<0,0,Cont_Student +(Cont_Fr_Difference)) #Updating Contamination in Student's hands
  Fr_Data.Frame[Fr_Picked,colnames(Fr_Data.Frame)== "TouchesContHist"]<-paste(Fr_Data.Frame[Fr_Picked,colnames(Fr_Data.Frame)=="TouchesContHist"], as.numeric(Cont_Student),sep = ",") #Adding Contamination to
  Tr_H_Fr_Inside<-Cont_Student*TE_H_F
  Cont_Fr_Consumed<-Tr_H_Fr_Inside
  Fr_Data.Frame[Fr_Picked,colnames(Fr_Data.Frame)== "Contamination"]<- Cont_Fr_Updated
  Fr_Data.Frame[Fr_Picked,colnames(Fr_Data.Frame)== "ContConsumed"]<- Cont_Fr_Consumed
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

