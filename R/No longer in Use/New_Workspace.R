random_func<-function(){
  x<-3+2
  y<-4+3
  listq<-list(Ex=x,Wai=y)
  return(listq)
}
same<-random_func()


Item<-"Fruit"
Item_Picked<-Fr_Touched
RowSizeVar<-20
Cont_Student<-200
l<-1
j<-1
k<-1
z<-1

Fr_Data.Frame<-data.frame("Item No." = 1:Initial_Fr,
                       "ID"= paste(l,k,j,1:Initial_Fr),
                       "Location"= "Selection Table",
                       "Contamination" = as.numeric("0"),
                       "ContConsumed"=as.numeric("0"),
                       "Infection"=as.logical(""),
                       "Illness"= as.logical(""),
                       "InContamination"  = as.numeric("0"),
                       "ExposedAllergen" = FALSE,
                       "TotTime"= as.numeric("0"),
                       "History" = "",
                       "ConsumedBy"="",
                       "TouchesContHist"= as.numeric(""),
                       "WashHistory"= as.numeric("0"),
                       "TotServices"=as.numeric("0"),
                       "STtimes"= as.numeric("0"),
                       "Initial Day" = "1",
                       "Initial Service" = "1",
                       "Day" = k,
                       "Service" = j,
                       "week"=l,
                       stringsAsFactors = FALSE
                       
)



Fr_Data.Frame[5,3]<-"Shared"

Student_Counter<-(Student_Counter+1)
z<-Student_Counter



Func_Eat_Fr<-function(Eat_YN_Item, DF, Item_Picked,Item){
  #Eat_YN_ITem:Eat_YN_Fr
  #DF: Fr_Data.Frame
  #Item_Picked: Fr_Picked
  #Item= "Friut"
  if(Eat_YN_Item==1){
    DF[Item_Picked,colnames(DF)== "Location"]<-"Consumed"
    DF[Item_Picked,colnames(DF)== "ConsumedBy"]<-(paste(l,k,j,z))
    DF[Item_Picked,colnames(DF)=="History"]<-paste(DF[Item_Picked,colnames(DF)=="History"], "Consumed")
    #Contamination
    if (Wrapping_Apples == 1){
      #Cross Contamination at consumption if apples wrapped
      OutputsCCFW<-Func_Cross_Contamination_Consumption_Wrapped(Cont_Student=Cont_Student,Data.Frame=DF, Item_Picked= Item_Picked)
      Cont_Student<-OutputsCCFW$Cont_Student
      DF<-OutputsCCFW$Data.Frame
      DF<-Func_Allergen_CC(DF,Item_Picked) #Adding Allergen Contamination from touch.
    } else if (Wrapping_Apples == 0){
      #Cross Contamination @ Consumption apples not wrapped. 
      OutputsCCF<-Func_Cross_Contamination(Cont_Student=Cont_Student,Data.Frame=DF, Item_Picked= Item_Picked, Item=Item)
      Cont_Student<-OutputsCCF$Cont_Student
      DF<-OutputsCCF$Data.Frame
    } #end of if wrapp 
  }else if(Eat_YN_Item==0){
    #Updating Location and History for consumption
    DF[Item_Picked,colnames(DF)== "Location"]<-"Not Consumed"
    DF[Item_Picked,colnames(DF)=="History"]<-paste(DF[Item_Picked,colnames(DF)=="History"], "NotConsumed")
    #Cross Crontamination from apples not being Consumed touch to ST/ Trash
    OutputsCCF<-Func_Cross_Contamination(Cont_Student=Cont_Student,Data.Frame=DF, Item_Picked= Item_Picked, Item=Item)
    Cont_Student<-OutputsCCF$Cont_Student
    DF<-OutputsCCF$Data.Frame
    DF<-Func_Allergen_CC(DF,Item_Picked) #Adding Allergen Contamination from touch.
  } #end of eat if statement
  OutputsFEFr<-list(DF=DF, Cont_Student=Cont_Student)
  return(OutputsFEFr)
}


Func_Eat_Pss<-function(Eat_YN_Item,DF,Item_Picked, Item){
  #Eat_YN_Item = Eat_YN_Pss
  #DF=Pss.Data.Frame
  #Item_Picked= Pss_Picked
  #Item = "PSS
  if(Eat_YN_Item==1){
   DF[Item_Picked,colnames(DF)== "Location"]<-"Consumed" 
   DF[Item_Picked,colnames(DF)== "ConsumedBy"]<-(paste(l,k,j,z))
   DF[Item_Picked,colnames(DF)=="History"]<-paste(DF[Item_Picked,colnames(DF)=="History"], "Consumed")
    #Contamination Insdide Pss @Consumption
   OutputsCCFW<-Func_Cross_Contamination_Consumption_Wrapped(Cont_Student=Cont_Student,Data.Frame=DF, Item_Picked= Item_Picked)
   Cont_Student<-OutputsCCFW$Cont_Student
   DF<-OutputsCCFW$Data.Frame
   DF<-Func_Allergen_CC(DF,Item_Picked) #Adding Allergen Contamination from touch.
  }else if(Eat_YN_Pss==1){
    #Updating Data frame Location and History
   DF[Item_Picked,colnames(DF)== "Location"]<-"Not Consumed" 
   DF[Item_Picked,colnames(DF)=="History"]<-paste(DF[Item_Picked,colnames(DF)=="History"], "NotConsumed")
    #Contamination from Touch @ Consumption
   OutputsCCF<-Func_Cross_Contamination(Cont_Student=Cont_Student,Data.Frame=DF, Item_Picked= Item_Picked, Item=Item)
   Cont_Student<-OutputsCCF$Cont_Student
   DF<-OutputsCCF$Data.Frame
   DF<-Func_Allergen_CC(DF,Item_Picked) #Adding Allergen Contamination from touch.
  } #end of Else statement for Eat
  OutputsFEPss<-list(DF=DF, Cont_Student=Cont_Student)
  return(OutputsFEPss)
}



Func_Eat_Pre<-function(Eat_YN_Item,DF, Item_Picked, Item){
  if(Eat_YN_Item==1){
    DF[Item_Picked,colnames(DF)== "Location"]<-"Consumed" 
    DF[Item_Picked,colnames(DF)== "ConsumedBy"]<-(paste(l,k,j,z))
    DF[Item_Picked,colnames(DF)=="History"]<-paste(DF[Item_Picked,colnames(DF)=="History"], "Consumed")
    #Contamination Container Pre to Mouth @ Consumption
    OutputFCCPre<-Func_Cross_Contamination_Pre_Consumption(Cont_Student=Cont_Student,DF = DF, Item_Picked = Item_Picked)
    Cont_Student<-OutputFCCPre$Cont_Student
    DF<-OutputFCCPre$DF
    DF<-Func_Allergen_CC(DF,Item_Picked) #Adding Allergen Contamination from touch.
  }else if(Eat_YN_Pre==0){
    DF[Item_Picked,colnames(DF)== "Location"]<-"Not Consumed"  
    DF[Item_Picked,colnames(DF)=="History"]<-paste(DF[Item_Picked,colnames(DF)=="History"], "NotConsumed")
    
    #Contaminationat Pre Container
    OutputsCCF<-Func_Cross_Contamination(Cont_Student=Cont_Student,Data.Frame=DF, Item_Picked= Item_Picked, Item=Item)
    Cont_Student<-OutputsCCF$Cont_Student
    DF<-OutputsCCF$Data.Frame
    DF<-Func_Allergen_CC(DF,Item_Picked) #Adding Allergen Contamination from touch.
  } 
  OutputFEPre<-list(DF=DF,Cont_Student=Cont_Student)
  return(OutputFEPre)
}

Func_Shared<-function(DF, Item_Picked){
  #DF = Main Data Frame of Item
  #Item Picked = Item picked from Selection
  if(DF[Item_Picked,colnames(DF)=="Location"]== "Not Consumed"){
    if(Share_YN_Food==1){
      DF[Item_Picked,colnames(DF)== "Location"]<-"Shared"
      DF[Item_Picked,colnames(DF)=="History"]<-paste(DF[Item_Picked,colnames(DF)=="History"], "Shared")
      DF[Item_Picked,colnames(DF)=="STtimes"]<-Func_Index_DF(DF,Item_Picked,"STtimes")+1
      V_Shared_Fr<-(V_Shared_Fr+1)
    }else{
      DF[Item_Picked,colnames(DF)== "Location"]<-"Not Shared" 
      DF[Item_Picked,colnames(DF)=="History"]<-paste(DF[Item_Picked,colnames(DF)=="History"], "NotShared")
    }
  }
  return(DF)
}







