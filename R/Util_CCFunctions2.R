#Cross Contamination Functions 2. 

#Cross_Contamination Fruit. 

Func_Cross_Contamination<-function(Cont_Student,Data.Frame, Item_Picked, Item){
  Conta<-Func_Index_DF(Data.Frame,Item_Picked,"Contamination")
  
  if(salmonella==1){
      #update the Fr Contamination in Data frame
      Tr_H_F<-Cont_Student*TE_H_F #Transfer from Hand to Fruit
      Tr_F_H<-(Conta* TE_F_H) #Tranfer from fruit to hand
  }else if (norovirus ==1 && Wrapping_Apples==0 && Item == "Fruit"){
    Conta<-round(Conta,digits = 0)
    Cont_Student<-round(Cont_Student,digits = 0)
    Tr_H_F<-rbinom(n=1,size = Cont_Student, prob = TrP_H_F)
    Tr_F_H<-rbinom(n=1,size = Conta,prob = TrP_F_H)
  }else if (norovirus ==1 && Wrapping_Apples==0 && (Item=="PSS" || Item=="PRE")){
    Conta<-round(Conta,digits = 0)
    Cont_Student<-round(Cont_Student,digits = 0)
    Tr_H_F<-rbinom(n=1,size = Cont_Student, prob = TrP_H_S)
    Tr_F_H<-rbinom(n=1,size = Conta,prob = TrP_S_H)
  }else if (norovirus ==1 && Wrapping_Apples==1){
    Conta<-round(Conta,digits = 0)
    Cont_Student<-round(Cont_Student,digits = 0)
    Tr_H_F<-rbinom(n=1,size = Cont_Student, prob = TrP_H_S)
    Tr_F_H<-rbinom(n=1,size = Conta,prob = TrP_S_H)
  }
      #Contamination Tranfered History. 
      Overall_Tr<-(Tr_H_F-Tr_F_H)
      Data.Frame[Item_Picked,colnames(Data.Frame)== "TouchesContHist"]<-paste(Data.Frame[Item_Picked,colnames(Data.Frame)=="TouchesContHist"], as.numeric(Overall_Tr),sep = ",") #Adding Contamination to
      #Continuing Contamination
      Cont_Updated<- Conta + Overall_Tr #New Contamination of Fruit
      Cont_Difference<-Conta-(Cont_Updated) #Difference in contamination to update student contamination
      Data.Frame[Item_Picked,colnames(Data.Frame)== "Contamination"]<-Cont_Updated #update the Fr Contamination in Data frame
      Cont_Student<-ifelse(Cont_Student +(Cont_Difference)<0,0,Cont_Student +(Cont_Difference)) #Updating Contamination in Student's hands
      #Adding Variables to Global Environment
      if (Item=="Fruit"){
        Fr_Data.Frame<<-Data.Frame
      } else if (Item == "PSS"){
        Pss_Data.Frame<<-Data.Frame
      }else if (Item=="PRE"){
        Pre_Data.Frame<<-Data.Frame
      }
      Cont_Student<<-Cont_Student
  } 

Func_Cross_Contamination_Consumption_Wrapped<-function(Cont_Student, Data.Frame, Item_Picked, Item){
  #Contamination of Fruit
  Conta<-Func_Index_DF(Data.Frame,Item_Picked,"Contamination")
  
  if(salmonella==1){
    #update the Fr Contamination in Data frame
    Tr_H_F<-Cont_Student*TE_H_F #Transfer from Hand to Fruit
    Tr_F_H<-(Conta* TE_F_H) #Tranfer from fruit to hand
  }else if(norovirus ==1 ){
    Conta<-round(Conta,digits = 0)
    Cont_Student<-round(Cont_Student,digits = 0)
    Tr_H_F<-rbinom(n=1,size = Cont_Student, prob = TrP_H_S)
    Tr_F_H<-rbinom(n=1,size = Conta,prob = TrP_S_H)
  }
  
  Overall_Tr<-(Tr_H_F-Tr_F_H)
  Data.Frame[Item_Picked,colnames(Data.Frame)== "TouchesContHist"]<-paste(Data.Frame[Item_Picked,colnames(Data.Frame)=="TouchesContHist"], as.numeric(Overall_Tr),sep = ",") #Adding Contamination to
  #Continuing Cross Contamination
  Cont_Updated<- Conta + Overall_Tr #New Contamination of Fruit
  Cont_Difference<-Conta-(Cont_Updated) #Difference in contamination to update student contamination
  Data.Frame[Item_Picked,colnames(Data.Frame)== "Contamination"]<-Cont_Updated #update the Fr Contamination in Data frame
  Cont_Student<-ifelse(Cont_Student +(Cont_Difference)<0,0,Cont_Student +(Cont_Difference)) #Updating Contamination in Student's hands
  #Adding Contamination to Inside of Fruit
  if(salmonella==1){
    #update the Fr Contamination in Data frame
    Tr_H_F_Inside<-Cont_Student*TE_H_F
  }else if (norovirus ==1){
    Cont_Student<-round(Cont_Student,digits = 0)
    Tr_H_F_Inside<-rbinom(n=1,size = Cont_Student, prob = TrP_H_F)
  }
  Overall_Tr<-Tr_H_F_Inside
  Data.Frame[Item_Picked,colnames(Data.Frame)== "TouchesContHist"]<-paste(Data.Frame[Item_Picked,colnames(Data.Frame)=="TouchesContHist"], as.numeric(Overall_Tr),sep = ",") #Adding Contamination to
  
  Cont_Consumed<-Tr_H_F_Inside
  Data.Frame[Item_Picked,colnames(Data.Frame)== "Contamination"]<- Cont_Updated
  Data.Frame[Item_Picked,colnames(Data.Frame)== "ContConsumed"]<- Cont_Consumed


  if(Item=="Fruit"){
    Fr_Data.Frame<<-Data.Frame
  }else if(Item=="PSS"){
    Pss_Data.Frame<<-Data.Frame
  }
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

