  Student_Counter<-(Student_Counter+1)
  z<-Student_Counter
  
  #Random Inputs for every kid. 
  source("Input_Random.R")  #We good!  
  #Calculations for initial contamination of the student. 
  source("Calc_StudentContamination.R") 
  #Feeding Items into the system is any items ran out. 
  source("Calc_FeedingItems.R")
  
  
  #Student Selection ===================================================
  
  #FRUIT
  
  #Student Touching Fruit in line
  
  #Did Student touch other fruit based on probability? 
  Touch_YN_Fr<-ifelse(runif(1)<Pr_touch_Food,1,0) 
  
  #If touched what is the contamination and adding it to data frame?
  if(Touch_YN_Fr==1){
    Fr_Available<-Fr_Data.Frame$Location == "Selection Table" 
    Sum_Fr_Available<-sum(Fr_Available, na.rm = TRUE)
    if(Sum_Fr_Available>ntouched_Fr){
      for (i in ntouched_Fr){
        OutputsFT<-Func_Touched(DF = Fr_Data.Frame, RowSizeVar = Row_size_Fr, Item = "Fruit", Item_Picked = Fr_Touched)
        Cont_Student<-OutputsFT$Cont_Student
        Fr_Data.Frame<-OutputsFT$DF
      }
    }
  }
  
  #Student Picking Fruit from line
  
  Pick_YN_Fr<-ifelse(runif(1,0,1)<Pr_select_Fr,1,0)
  #Picking a Fruit from Fruit Frame 
  if(Pick_YN_Fr==1){
    Fr_Available<-Fr_Data.Frame$Location == "Selection Table" 
    Sum_Fr_Available<-as.numeric(sum(Fr_Available,na.rm = TRUE))
    if(Sum_Fr_Available>0){   
      OutputFP<-Func_Picked(DF = Fr_Data.Frame,Item_Picked = Fr_Picked, Item = "Fruit")
      Fr_Data.Frame<-OutputFP$DF
      Fr_Picked<-OutputFP$Item_Picked
    }
  }
  
  
  
  #Contamination from Hand to fruit Going into Tray
  
  if(Pick_YN_Fr==1){
    #Cross Contamination from Touching Fruit @Tray
    Func_Cross_Contamination(Cont_Student=Cont_Student,Data.Frame=Fr_Data.Frame, Item_Picked= Fr_Picked, Item="Fruit")
    Fr_Data.Frame<-Func_Allergen_CC(Fr_Data.Frame,Fr_Picked) #Adding Allergen Contamination from touch.
  }
  
  #PSS
  #Did student touch other Pss based on probability 
  Touch_YN_Pss<-ifelse(runif(1)<Pr_touch_Food,1,0) 
  
  #If touched what is the contamination and adding it to data frame
  
  if(Touch_YN_Pss==1){
    Pss_Available<-Pss_Data.Frame$Location == "Selection Table" 
    Sum_Pss_Available<-sum(Pss_Available, na.rm = TRUE)
    if(Sum_Pss_Available>ntouched_Pss){
      replicate(ntouched_Pss,Func_Touched(DF = Pss_Data.Frame,Item = "PSS",RowSizeVar = Row_size_Pss,Item_Picked = Pss_Touched))
    }
  }
  
  
  #Student picking Pss from line
  
  Pick_YN_Pss<-ifelse(runif(1)<Pr_select_Pss,1,0) 
  #Picking a Pss from Pss Data Frame 
  if(Pick_YN_Pss==1){
    Pss_Available<-Pss_Data.Frame$Location == "Selection Table" 
    Sum_Pss_Available<-sum(Pss_Available, na.rm = TRUE)
    if(Sum_Pss_Available>0){
      Func_Picked(DF = Pss_Data.Frame,Item_Picked = Pss_Picked, Item = "PSS")
    }
  }
  
  
  #Contamination From Hands to Pss going to Tray
  
  
  if(Pick_YN_Pss==1){
    #Cross Contamination from Touching Pss @Tray
    Func_Cross_Contamination(Cont_Student=Cont_Student,Data.Frame=Pss_Data.Frame, Item_Picked= Pss_Picked, Item="PSS")
    Pss_Data.Frame<-Func_Allergen_CC(Pss_Data.Frame,Pss_Picked) #Adding Allergen Contamination from touch.
  }
  
  
  #PRE
  
  #Did the Student touch any Pre during selection? 
  Touch_YN_Pre<-ifelse(runif(1)<Pr_touch_Food,1,0) 
  
  #If touched what is the contamination and adding it to data frame
  
  if(Touch_YN_Pre==1){
    Pre_Available<-Pre_Data.Frame$Location == "Selection Table" 
    Sum_Pre_Available<-sum(Pre_Available, na.rm = TRUE)
    if(Sum_Pre_Available>ntouched_Pre){
      replicate(ntouched_Pre,Func_Touched(DF = Pre_Data.Frame,Item = "PRE",RowSizeVar = Row_size_Pre,Item_Picked = Pre_Touched))
    }
  }
  
  
  #Picking Pre from line
  Pick_YN_Pre<-ifelse(runif(1)<Pr_select_Pre,1,0) 
  #Picking a Pre from Pre Data Frame 
  if(Pick_YN_Pre==1){
    Pre_Available<-Pre_Data.Frame$Location == "Selection Table" 
    Sum_Pre_Available<-sum(Pre_Available, na.rm = TRUE)
    if(Sum_Pre_Available>0){
      Func_Picked(DF = Pre_Data.Frame,Item_Picked = Pre_Picked, Item = "PRE")
    }
  }
  #Contamination From Hands to Pre going to Tray
  if(Pick_YN_Pre==1){
    #Cross Contamination from Touching Pre @Tray
    Func_Cross_Contamination(Cont_Student=Cont_Student,Data.Frame=Pre_Data.Frame, Item_Picked= Pre_Picked, Item="PRE")
    Pre_Data.Frame<-Func_Allergen_CC(Pre_Data.Frame,Pre_Picked) #Adding Allergen Contamination from touch.
  }  
  
  
  # Consumption==========================================================================
  
  
  
  #Fruit
  
  #Did the student consume the Fruit?
  Eat_YN_Fr<-ifelse(runif(1)<Pr_eat_Fr,1,0)
  
  #Changing Data Frame so it updates when student consumes fruit.
  
  if(Pick_YN_Fr==1){
    if(Eat_YN_Fr==1){
      Fr_Data.Frame[Fr_Picked,colnames(Fr_Data.Frame)== "Location"]<-"Consumed"
      Fr_Data.Frame[Fr_Picked,colnames(Fr_Data.Frame)== "ConsumedBy"]<-(paste(l,k,j,z))
      Fr_Data.Frame[Fr_Picked,colnames(Fr_Data.Frame)=="History"]<-paste(Fr_Data.Frame[Fr_Picked,colnames(Fr_Data.Frame)=="History"], "Consumed")
      #Contamination
      if (Wrapping_Apples == 1){
        #Cross Contamination at consumption if apples wrapped
        Func_Cross_Contamination_Consumption_Wrapped(Cont_Student = Cont_Student,Data.Frame = Fr_Data.Frame,Item_Picked = Fr_Picked,Item = "Fruit")
        Fr_Data.Frame<-Func_Allergen_CC(Fr_Data.Frame,Fr_Picked) #Adding Allergen Contamination from touch.
      } else if (Wrapping_Apples == 0){
        #Cross Contamination @ Consumption apples not wrapped. 
        Func_Cross_Contamination(Cont_Student=Cont_Student,Data.Frame=Fr_Data.Frame, Item_Picked= Fr_Picked, Item="Fruit")
      } #end of if wrapp 
      
    }else{
      #Updating Location and History for consumption
      Fr_Data.Frame[Fr_Picked,colnames(Fr_Data.Frame)== "Location"]<-"Not Consumed"
      Fr_Data.Frame[Fr_Picked,colnames(Fr_Data.Frame)=="History"]<-paste(Fr_Data.Frame[Fr_Picked,colnames(Fr_Data.Frame)=="History"], "NotConsumed")
      #Cross Crontamination from apples not being Consumed touch to ST/ Trash
      Func_Cross_Contamination(Cont_Student=Cont_Student,Data.Frame=Fr_Data.Frame, Item_Picked= Fr_Picked, Item="Fruit")
      Fr_Data.Frame<-Func_Allergen_CC(Fr_Data.Frame,Fr_Picked) #Adding Allergen Contamination from touch.
    } #end of eat if statement
  }#end of pick statement
  
  #Pss
  #Did student consume the Pss
  Eat_YN_Pss<-ifelse(runif(1)<Pr_eat_Pss,1,0)
  
  #Changing Data Frame for consumption of Pss
  if(Pick_YN_Pss==1){
    if(Eat_YN_Pss==1){
      
      Pss_Data.Frame[Pss_Picked,colnames(Pss_Data.Frame)== "Location"]<-"Consumed" 
      Pss_Data.Frame[Pss_Picked,colnames(Pss_Data.Frame)== "ConsumedBy"]<-(paste(l,k,j,z))
      Pss_Data.Frame[Pss_Picked,colnames(Pss_Data.Frame)=="History"]<-paste(Pss_Data.Frame[Pss_Picked,colnames(Pss_Data.Frame)=="History"], "Consumed")
      #Contamination Insdide Pss @Consumption
      Func_Cross_Contamination_Consumption_Wrapped(Cont_Student = Cont_Student,Data.Frame = Pss_Data.Frame,Item_Picked = Pss_Picked,Item = "PSS")
      Pss_Data.Frame<-Func_Allergen_CC(Pss_Data.Frame,Pss_Picked) #Adding Allergen Contamination from touch.
      
    }else{
      #Updating Data frame Location and History
      Pss_Data.Frame[Pss_Picked,colnames(Pss_Data.Frame)== "Location"]<-"Not Consumed" 
      Pss_Data.Frame[Pss_Picked,colnames(Pss_Data.Frame)=="History"]<-paste(Pss_Data.Frame[Pss_Picked,colnames(Pss_Data.Frame)=="History"], "NotConsumed")
      
      #Contamination from Touch @ Consumption
      Func_Cross_Contamination(Cont_Student=Cont_Student,Data.Frame=Pss_Data.Frame, Item_Picked= Pss_Picked, Item="PSS")
      Pss_Data.Frame<-Func_Allergen_CC(Pss_Data.Frame,Pss_Picked) #Adding Allergen Contamination from touch.
      
    } #end of Else statement for Eat
  } #end of If
  
  #Pre
  #Did student consume the Pre
  Eat_YN_Pre<-ifelse(runif(1)<Pr_eat_Pre,1,0)
  
  #Changind Data Frame for consumption of Pre
  if(Pick_YN_Pre==1){
    if(Eat_YN_Pre==1){
      
      Pre_Data.Frame[Pre_Picked,colnames(Pre_Data.Frame)== "Location"]<-"Consumed" 
      Pre_Data.Frame[Pre_Picked,colnames(Pre_Data.Frame)== "ConsumedBy"]<-(paste(l,k,j,z))
      Pre_Data.Frame[Pre_Picked,colnames(Pre_Data.Frame)=="History"]<-paste(Pre_Data.Frame[Pre_Picked,colnames(Pre_Data.Frame)=="History"], "Consumed")
      #Contamination Container Pre to Mouth @ Consumption
      Func_Cross_Contamination_Pre_Consumption(Cont_Student=Cont_Student,Pre_Data.Frame = Pre_Data.Frame, Pre_Picked = Pre_Picked)
      Pre_Data.Frame<-Func_Allergen_CC(Pre_Data.Frame,Pre_Picked) #Adding Allergen Contamination from touch.
      
    }else{
      Pre_Data.Frame[Pre_Picked,colnames(Pre_Data.Frame)== "Location"]<-"Not Consumed"  
      Pre_Data.Frame[Pre_Picked,colnames(Pre_Data.Frame)=="History"]<-paste(Pre_Data.Frame[Pre_Picked,colnames(Pre_Data.Frame)=="History"], "NotConsumed")
      
      #Contaminationat Pre Container
      Func_Cross_Contamination(Cont_Student=Cont_Student,Data.Frame=Pre_Data.Frame, Item_Picked= Pre_Picked, Item="PRE")
      Pre_Data.Frame<-Func_Allergen_CC(Pre_Data.Frame,Pre_Picked) #Adding Allergen Contamination from touch.
    }  
  }
  
  
  
  
  #Not Consumed items to share table=======================================================
  
  if(Share_Table_YN==1){ 
    
    #Proability of the student sharing their food. 
    Share_YN_Food<-ifelse(runif(1)<Pr_share_Food,1,0)
    
    #Fruit 
    #Items in Share Table:
    
    if(Pick_YN_Fr==1){  
      if(Fr_Data.Frame[Fr_Picked,colnames(Fr_Data.Frame)=="Location"]== "Not Consumed"){
        if(Share_YN_Food==1){
          Fr_Data.Frame[Fr_Picked,colnames(Fr_Data.Frame)== "Location"]<-"Shared"
          Fr_Data.Frame[Fr_Picked,colnames(Fr_Data.Frame)=="History"]<-paste(Fr_Data.Frame[Fr_Picked,colnames(Fr_Data.Frame)=="History"], "Shared")
          Fr_Data.Frame[Fr_Picked,colnames(Fr_Data.Frame)=="STtimes"]<-Func_Index_DF(Fr_Data.Frame,Fr_Picked,"STtimes")+1
          V_Shared_Fr<-(V_Shared_Fr+1)
        }else{
          Fr_Data.Frame[Fr_Picked,colnames(Fr_Data.Frame)== "Location"]<-"Not Shared" 
          Fr_Data.Frame[Fr_Picked,colnames(Fr_Data.Frame)=="History"]<-paste(Fr_Data.Frame[Fr_Picked,colnames(Fr_Data.Frame)=="History"], "NotShared")
        }
      }
    }
    #Pss
    
    if(Pick_YN_Pss==1){  
      if(Pss_Data.Frame[Pss_Picked,colnames(Pss_Data.Frame)=="Location"]== "Not Consumed"){
        if(Share_YN_Food==1){
          Pss_Data.Frame[Pss_Picked,colnames(Pss_Data.Frame)== "Location"]<-"Shared"
          Pss_Data.Frame[Pss_Picked,colnames(Pss_Data.Frame)=="History"]<-paste(Pss_Data.Frame[Pss_Picked,colnames(Pss_Data.Frame)=="History"], "Shared")
          Pss_Data.Frame[Pss_Picked,colnames(Pss_Data.Frame)=="STtimes"]<-Func_Index_DF(Pss_Data.Frame,Pss_Picked,"STtimes")+1
          V_Shared_Pss<-(V_Shared_Pss+1)
        }else{
          Pss_Data.Frame[Pss_Picked,colnames(Pss_Data.Frame)== "Location"]<-"Not Shared"
          Pss_Data.Frame[Pss_Picked,colnames(Pss_Data.Frame)=="History"]<-paste(Pss_Data.Frame[Pss_Picked,colnames(Pss_Data.Frame)=="History"], "NotShared")
        }
      }
    }
    #Pre
    
    if(Pick_YN_Pre==1){    
      if(Pre_Data.Frame[Pre_Picked,colnames(Pre_Data.Frame)=="Location"]== "Not Consumed"){
        if(Share_YN_Food==1){
          Pre_Data.Frame[Pre_Picked,colnames(Pre_Data.Frame)== "Location"]<-"Shared"
          Pre_Data.Frame[Pre_Picked,colnames(Pre_Data.Frame)=="History"]<-paste(Pre_Data.Frame[Pre_Picked,colnames(Pre_Data.Frame)=="History"], "Shared")
          Pre_Data.Frame[Pre_Picked,colnames(Pre_Data.Frame)=="STtimes"]<-Func_Index_DF(Pre_Data.Frame,Pre_Picked,"STtimes")+1
          V_Shared_Pre<-(V_Shared_Pre+1)
        }else{
          Pre_Data.Frame[Pre_Picked,colnames(Pre_Data.Frame)== "Location"]<-" Not Shared"
          Pre_Data.Frame[Pre_Picked,colnames(Pre_Data.Frame)=="History"]<-paste(Pre_Data.Frame[Pre_Picked,colnames(Pre_Data.Frame)=="History"], "NotShared")
        }
      }
    }
    
    
    
    #Picking and consuming ST items ==================================================================
    
    
    #Shared Fruit
    Items_Shared<-Fr_Data.Frame$Location == "Shared" 
    Sum_Shared<-sum(Items_Shared, na.rm = TRUE)
    if(Sum_Shared>0){
      
      #Fruit
      
      #Did a student pick an item for the share table? 
      Pick_ST_YN_Fr<-ifelse(runif(1)<Pr_Pick_ST_Fr,1,0) 
      
      #Fruit picked from Share Table. 
      if(Pick_ST_YN_Fr==1){
        Search.df.fr_ST<-Func_Search_Data(Fr_Data.Frame,Fr_Data.Frame$Location,"Shared",1)
        #Fruit from share table selected #
        Fr_ST_Picked<-as.numeric(Search.df.fr_ST$Item.No.)
        Fr_Data.Frame[as.numeric(row.names(Search.df.fr_ST)),colnames(Search.df.fr_ST)== "Location"]<-"Tray"
        Fr_Data.Frame[Fr_ST_Picked,colnames(Fr_Data.Frame)=="History"]<-paste(Fr_Data.Frame[Fr_ST_Picked,colnames(Fr_Data.Frame)=="History"], "Tray")
        Fr_Data.Frame[Fr_ST_Picked,colnames(Fr_Data.Frame)=="History"]<-paste(Fr_Data.Frame[Fr_ST_Picked,colnames(Fr_Data.Frame)=="History"], "Touched")
      }
      
      #Contamination from Hand to Fruit or from Fruit to Hand. 
      if(Pick_ST_YN_Fr==1){
        #Contamination from picking item share table
        Func_Cross_Contamination(Cont_Student=Cont_Student,Data.Frame=Fr_Data.Frame, Item_Picked= Fr_ST_Picked, Item="Fruit")
        Fr_Data.Frame<-Func_Allergen_CC(Fr_Data.Frame,Fr_ST_Picked) #Adding Allergen Contamination
      }
      
      #Consumption of share table item
      
      #Did the student consume the Fruit?
      Eat_YN_ST_Fr<-ifelse(runif(1)<Pr_eat_ST_Fr,1,0)
      
      #Changing Data Frame so it updates when student consumes fruit.
      
      if(Pick_ST_YN_Fr==1){  
        if(Eat_YN_ST_Fr==1){
          Fr_Data.Frame[Fr_ST_Picked,colnames(Fr_Data.Frame)== "Location"]<-"Consumed"
          Fr_Data.Frame[Fr_ST_Picked,colnames(Fr_Data.Frame)== "ConsumedBy"]<-(paste(l,k,j,z))
          Fr_Data.Frame[Fr_ST_Picked,colnames(Fr_Data.Frame)=="History"]<-paste(Fr_Data.Frame[Fr_ST_Picked,colnames(Fr_Data.Frame)=="History"], "Consumed")
          #Contamination
          if (Wrapping_Apples == 1){
            #Cross Contamination at consumption if apples wrapped
            Func_Cross_Contamination_Consumption_Wrapped(Cont_Student = Cont_Student,Data.Frame = Fr_Data.Frame,Item_Picked = Fr_ST_Picked ,Item = "Fruit")
            Fr_Data.Frame<-Func_Allergen_CC(Fr_Data.Frame,Fr_ST_Picked) #Adding Allergen Contamination from touch.
          } else if (Wrapping_Apples == 0){
            #Cross Contamination @ Consumption apples not wrapped. 
            Func_Cross_Contamination(Cont_Student=Cont_Student,Data.Frame=Fr_Data.Frame, Item_Picked= Fr_ST_Picked, Item="Fruit")
          } #end of if wrapp 
          
        }else{
          Fr_Data.Frame[Fr_ST_Picked,colnames(Fr_Data.Frame)== "Location"]<-"Not Consumed"
          Fr_Data.Frame[Fr_ST_Picked,colnames(Fr_Data.Frame)=="History"]<-paste(Fr_Data.Frame[Fr_ST_Picked,colnames(Fr_Data.Frame)=="History"], "NotConsumed")
          #Cross Crontamination from apples not consumed from ST
          Func_Cross_Contamination(Cont_Student=Cont_Student,Data.Frame=Fr_Data.Frame, Item_Picked= Fr_ST_Picked, Item="Fruit")
          Fr_Data.Frame<-Func_Allergen_CC(Fr_Data.Frame,Fr_ST_Picked) #Adding Allergen Contamination from touch.
          
        } #end of eat if
      } # end of picking if
      
    }  #end of if to make sure there are ST items
    
    #Shared Pss
    Items_Shared_Pss<-Pss_Data.Frame$Location == "Shared" 
    Sum_Shared_Pss<-sum(Items_Shared_Pss, na.rm = TRUE)
    if(Sum_Shared_Pss>0){
      
      #Did a student pick an item for the share table? 
      Pick_ST_YN_Pss<-ifelse(runif(1)<Pr_Pick_ST_Pss,1,0) 
      
      #Pss picked from Share Table. 
      if(Pick_ST_YN_Pss==1){
        Search.df.Pss_ST<-Func_Search_Data(Pss_Data.Frame,Pss_Data.Frame$Location,"Shared",1)
        #Pss from share table selected #
        Pss_ST_Picked<-as.numeric(Search.df.Pss_ST$Item.No.)
        Pss_Data.Frame[as.numeric(row.names(Search.df.Pss_ST)),colnames(Search.df.Pss_ST)== "Location"]<-"Tray"
        Pss_Data.Frame[Pss_ST_Picked,colnames(Pss_Data.Frame)=="History"]<-paste(Pss_Data.Frame[Pss_ST_Picked,colnames(Pss_Data.Frame)=="History"], "Tray")
        Pss_Data.Frame[Pss_ST_Picked,colnames(Pss_Data.Frame)=="History"]<-paste(Pss_Data.Frame[Pss_ST_Picked,colnames(Pss_Data.Frame)=="History"], "Touched")
      }
      
      
      #Contamination from Hand to Pss or from Hand to Pss. 
      if(Pick_ST_YN_Pss==1){
        #Contamination at tray
        Func_Cross_Contamination(Cont_Student=Cont_Student,Data.Frame=Pss_Data.Frame, Item_Picked= Pss_ST_Picked, Item="PSS")
        Pss_Data.Frame<-Func_Allergen_CC(Pss_Data.Frame,Pss_ST_Picked) #Adding Allergen Contamination
      }
      
      #Consumption of share table item
      
      #Did the student consume the Fruit?
      Eat_YN_ST_Pss<-ifelse(runif(1)<Pr_eat_ST_Pss,1,0)
      
      #Changing Data Frame so it updates when student consumes fruit.
      
      if(Pick_ST_YN_Pss==1){  
        if(Eat_YN_ST_Pss==1){
          Pss_Data.Frame[Pss_ST_Picked,colnames(Pss_Data.Frame)== "Location"]<-"Consumed"
          Pss_Data.Frame[Pss_ST_Picked,colnames(Pss_Data.Frame)== "ConsumedBy"]<-(paste(l,k,j,z))
          Pss_Data.Frame[Pss_ST_Picked,colnames(Pss_Data.Frame)=="History"]<-paste(Pss_Data.Frame[Pss_ST_Picked,colnames(Pss_Data.Frame)=="History"], "Consumed")
          #Cross Contamination @ Consumption
          Func_Cross_Contamination_Consumption_Wrapped(Cont_Student = Cont_Student,Data.Frame = Pss_Data.Frame,Item_Picked = Pss_ST_Picked,Item = "PSS")
          Pss_Data.Frame<-Func_Allergen_CC(Pss_Data.Frame,Pss_ST_Picked) #Adding Allergen Contamination from touch.
          
        }else{
          Pss_Data.Frame[Pss_ST_Picked,colnames(Pss_Data.Frame)== "Location"]<-"Not Consumed"
          Pss_Data.Frame[Pss_ST_Picked,colnames(Pss_Data.Frame)=="History"]<-paste(Pss_Data.Frame[Pss_ST_Picked,colnames(Pss_Data.Frame)=="History"], "NotConsumed")
          #Contamination from Touch @ Not Consumption
          Func_Cross_Contamination(Cont_Student=Cont_Student,Data.Frame=Pss_Data.Frame, Item_Picked= Pss_ST_Picked, Item="PSS")
          Pss_Data.Frame<-Func_Allergen_CC(Pss_Data.Frame,Pss_ST_Picked) #Adding Allergen Contamination from touch.
        }
      }
    }
    
    #Shared Pre
    
    Items_Shared_Pre<-Pre_Data.Frame$Location == "Shared" 
    Sum_Shared_Pre<-sum(Items_Shared_Pre, na.rm = TRUE)
    if(Sum_Shared_Pre>0){
      
      #Did a student pick an item for the share table? 
      Pick_ST_YN_Pre<-ifelse(runif(1)<Pr_Pick_ST_Pre,1,0) 
      
      #Pre picked from Share Table. 
      if(Pick_ST_YN_Pre==1){
        Search.df.Pre_ST<-Func_Search_Data(Pre_Data.Frame,Pre_Data.Frame$Location,"Shared",1)
        #Pre from share table selected #
        Pre_ST_Picked<-as.numeric(Search.df.Pre_ST$Item.No.)
        Pre_Data.Frame[as.numeric(row.names(Search.df.Pre_ST)),colnames(Search.df.Pre_ST)== "Location"]<-"Tray"
        Pre_Data.Frame[Pre_ST_Picked,colnames(Pre_Data.Frame)=="History"]<-paste(Pre_Data.Frame[Pre_ST_Picked,colnames(Pre_Data.Frame)=="History"], "Tray")
        Pre_Data.Frame[Pre_ST_Picked,colnames(Pre_Data.Frame)=="History"]<-paste(Pre_Data.Frame[Pre_ST_Picked,colnames(Pre_Data.Frame)=="History"], "Touched")
      }
      
      
      #Contamination from Hand to Fruit or from Hand to Fruit. 
      if(Pick_ST_YN_Pre==1){
        Func_Cross_Contamination(Cont_Student=Cont_Student,Data.Frame=Pre_Data.Frame, Item_Picked= Pre_ST_Picked, Item="PRE")
        Pre_Data.Frame<-Func_Allergen_CC(Pre_Data.Frame,Pre_ST_Picked) #Adding Allergen Contamination
      }
      
      #Consumption of share table item
      
      #Did the student consume the Fruit?
      Eat_YN_ST_Pre<-ifelse(runif(1)<Pr_eat_ST_Pre,1,0)
      
      #Changing Data Frame so it updates when student consumes fruit.
      
      if(Pick_ST_YN_Pre==1){  
        if(Eat_YN_ST_Pre==1){
          #Updating Values in DF
          Pre_Data.Frame[Pre_ST_Picked,colnames(Pre_Data.Frame)== "Location"]<-"Consumed"
          Pre_Data.Frame[Pre_ST_Picked,colnames(Pre_Data.Frame)== "ConsumedBy"]<-(paste(l,k,j,z))
          Pre_Data.Frame[Pre_ST_Picked,colnames(Pre_Data.Frame)=="History"]<-paste(Pre_Data.Frame[Pre_ST_Picked,colnames(Pre_Data.Frame)=="History"], "Consumed")
          #Contamination Container Pre to Mouth @ Consumption
          Func_Cross_Contamination_Pre_Consumption(Cont_Student = Cont_Student, Pre_Data.Frame = Pre_Data.Frame, Pre_Picked = Pre_ST_Picked)
          Pre_Data.Frame<-Func_Allergen_CC(Pre_Data.Frame,Pre_ST_Picked) #Adding Allergen Contamination from touch.
          
        }else{
          Pre_Data.Frame[Pre_ST_Picked,colnames(Pre_Data.Frame)== "Location"]<-"Not Consumed"
          Pre_Data.Frame[Pre_ST_Picked,colnames(Pre_Data.Frame)=="History"]<-paste(Pre_Data.Frame[Pre_ST_Picked,colnames(Pre_Data.Frame)=="History"], "NotConsumed")
          #Contaminationat Pre Container
          Func_Cross_Contamination(Cont_Student=Cont_Student,Data.Frame=Pre_Data.Frame, Item_Picked= Pre_ST_Picked, Item="PRE")
          Pre_Data.Frame<-Func_Allergen_CC(Pre_Data.Frame,Pre_ST_Picked) #Adding Allergen Contamination from touch.
        }
      }
    }#end of if there is st items loop
  }#end of toggle loop
  