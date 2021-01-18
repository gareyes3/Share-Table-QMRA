Student_Loop<-function(){
  
  Fr_Data.Frame<-Fr_Data.Frame
  Pss_Data.Frame<-Pss_Data.Frame
  Pre_Data.Frame<-Pre_Data.Frame
  
  #Random Inputs for every kid. 
  source("Input_Random.R")  #We good!  
  #Calculations for initial contamination of the student. 
  source("Calc_StudentContamination.R") 
  #Feeding Items into the system is any items ran out. 
  source("Calc_FeedingItems.R")
  
  
  #STUDENT SELECTION =====================================================================================================
  
  #Fruit---------------------------------------------------------------
  
  #Student Touching Fruit in line
  
  #Did Student touch other fruit based on probability? 
  Touch_YN_Fr<-ifelse(runif(1)<Pr_touch_Food,1,0) 
  
  #If touched what is the contamination and adding it to data frame?
  if(Touch_YN_Fr==1){
    Fr_Available<-Fr_Data.Frame$Location == "Selection Table" 
    Sum_Fr_Available<-sum(Fr_Available, na.rm = TRUE)
    if(Sum_Fr_Available>ntouched_Fr){
      for (i in 1:ntouched_Fr){
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
      OutputFP<-Func_Picked(DF = Fr_Data.Frame,Item_Picked = Fr_Picked,Location = "Selection Table")
      Fr_Data.Frame<-OutputFP$DF
      Fr_Picked<-OutputFP$Item_Picked
      #Cross Contamination from Touching Fruit @Tray
      #Contamination from Hand to fruit Going into Tray
      OutputFCC<-Func_Cross_Contamination(Cont_Student=Cont_Student,Data.Frame=Fr_Data.Frame, Item_Picked= Fr_Picked, Item="Fruit")
      Fr_Data.Frame<-OutputFCC$Data.Frame
      Cont_Student<-OutputFCC$Cont_Student
      #Fr_Data.Frame<-Func_Allergen_CC(Fr_Data.Frame,Fr_Picked) #Adding Allergen Contamination from touch.
    }
  }
  
  
  
  
  

  
  #Pss-----------------------------------------------------------------------
  
  #Did student touch other Pss based on probability 
  Touch_YN_Pss<-ifelse(runif(1)<Pr_touch_Food,1,0) 
  
  #If touched what is the contamination and adding it to data frame
  
  if(Touch_YN_Pss==1){
    Pss_Available<-Pss_Data.Frame$Location == "Selection Table" 
    Sum_Pss_Available<-sum(Pss_Available, na.rm = TRUE)
    if(Sum_Pss_Available>ntouched_Pss){
      for (i in 1:ntouched_Pss){
        OutputsFT<-Func_Touched(DF = Pss_Data.Frame, RowSizeVar = Row_size_Pss, Item = "PSS", Item_Picked = Pss_Touched)
        Cont_Student<-OutputsFT$Cont_Student
        Pss_Data.Frame<-OutputsFT$DF
      }
    }
  }
  
  
  #Student picking Pss from line
  
  Pick_YN_Pss<-ifelse(runif(1)<Pr_select_Pss,1,0) 
  #Picking a Pss from Pss Data Frame 
  if(Pick_YN_Pss==1){
    Pss_Available<-Pss_Data.Frame$Location == "Selection Table" 
    Sum_Pss_Available<-sum(Pss_Available, na.rm = TRUE)
    if(Sum_Pss_Available>0){
      OutputFP<-Func_Picked(DF = Pss_Data.Frame,Item_Picked = Pss_Picked,Location = "Selection Table")
      Pss_Data.Frame<-OutputFP$DF
      Pss_Picked<-OutputFP$Item_Picked
      #Cross Contamination from Touching PSS @Tray
      #Contamination from Hand to PSS Going into Tray
      OutputFCC<-Func_Cross_Contamination(Cont_Student=Cont_Student,Data.Frame=Pss_Data.Frame, Item_Picked= Pss_Picked, Item="PSS")
      Pss_Data.Frame<-OutputFCC$Data.Frame
      Cont_Student<-OutputFCC$Cont_Student
      #Pss_Data.Frame<-Func_Allergen_CC(Pss_Data.Frame,Pss_Picked) #Adding Allergen Contamination from touch.
    }
  }
  
  

  
  

  
  #Pre-------------------------------------------------------------------------------------
  
  #Did the Student touch any Pre during selection? 
  Touch_YN_Pre<-ifelse(runif(1)<Pr_touch_Food,1,0) 
  
  #If touched what is the contamination and adding it to data frame
  
  if(Touch_YN_Pre==1){
    Pre_Available<-Pre_Data.Frame$Location == "Selection Table" 
    Sum_Pre_Available<-sum(Pre_Available, na.rm = TRUE)
    if(Sum_Pre_Available>ntouched_Pre){
      for (i in 1:ntouched_Pre){
        OutputsFT<-Func_Touched(DF = Pre_Data.Frame, RowSizeVar = Row_size_Pre, Item = "PRE", Item_Picked = Pre_Touched)
        Cont_Student<-OutputsFT$Cont_Student
        Pre_Data.Frame<-OutputsFT$DF
      }
    }
  }
  
  
  #Picking Pre from line
  Pick_YN_Pre<-ifelse(runif(1)<Pr_select_Pre,1,0) 
  #Picking a Pre from Pre Data Frame 
  if(Pick_YN_Pre==1){
    Pre_Available<-Pre_Data.Frame$Location == "Selection Table" 
    Sum_Pre_Available<-sum(Pre_Available, na.rm = TRUE)
    if(Sum_Pre_Available>0){
      OutputFP<-Func_Picked(DF = Pre_Data.Frame,Item_Picked = Pre_Picked,Location = "Selection Table")
      Pre_Data.Frame<-OutputFP$DF
      Pre_Picked<-OutputFP$Item_Picked
      #Cross Contamination from Touching Pre @Tray
      #Contamination from Hand to Pre Going into Tray
      OutputFCC<-Func_Cross_Contamination(Cont_Student=Cont_Student,Data.Frame=Pre_Data.Frame, Item_Picked= Pre_Picked, Item="PRE")
      Pre_Data.Frame<-OutputFCC$Data.Frame
      Cont_Student<-OutputFCC$Cont_Student
      #Pre_Data.Frame<-Func_Allergen_CC(Pre_Data.Frame,Pre_Picked) #Adding Allergen Contamination from touch.
    }
  }
  
  

  
  
  # CONSUMPTION===========================================================================================================
  
  
  #Fruit------------------------------------------------------------------
  
  #Did the student consume the Fruit?
  Eat_YN_Fr<-ifelse(runif(1)<Pr_eat_Fr,1,0)
  
  #Changing Data Frame so it updates when student consumes fruit.
  
  if(Pick_YN_Fr==1 && Sum_Fr_Available>0){
    OutputsFEFr<-Func_Eat_Fr(Eat_YN_Item = Eat_YN_Fr, DF = Fr_Data.Frame,Item_Picked = Fr_Picked,Item = "Fruit")
    Cont_Student<-OutputsFEFr$Cont_Student
    Fr_Data.Frame<-OutputsFEFr$DF
  }#end of pick statement
  
  
  #Pss------------------------------------------------------------------
  
  #Did student consume the Pss
  Eat_YN_Pss<-ifelse(runif(1)<Pr_eat_Pss,1,0)
  
  #Changing Data Frame for consumption of Pss
  if(Pick_YN_Pss==1 && Sum_Pss_Available>0){
    OutputsFEPss<-Func_Eat_Pss(Eat_YN_Item = Eat_YN_Pss,DF = Pss_Data.Frame, Item_Picked = Pss_Picked, Item = "PSS")
    Cont_Student<-OutputsFEPss$Cont_Student
    Pss_Data.Frame<-OutputsFEPss$DF
  } #end of If
  
  
  #Pre -----------------------------------------------------------------
  
  #Did student consume the Pre
  Eat_YN_Pre<-ifelse(runif(1)<Pr_eat_Pre,1,0)
  
  #Changind Data Frame for consumption of Pre
  if(Sum_Pre_Available>0){
    if(Pick_YN_Pre==1){
      OutputFEPre<-Func_Eat_Pre(Eat_YN_Item = Eat_YN_Pre,DF = Pre_Data.Frame, Item_Picked = Pre_Picked, Item = "PRE")
      Cont_Student<-OutputFEPre$Cont_Student
      Pre_Data.Frame<-OutputFEPre$DF
    }
  }

  
  
  #NOT CONSUMED TO ST=====================================================================================================
  
  if(Share_Table_YN==1){ 
    
    #Proability of the student sharing their food. 
    Share_YN_Food<-ifelse(runif(1)<Pr_share_Food,1,0)
    
    #Fruit--------------------------------------------------------------------
    
    if(Pick_YN_Fr==1 && Sum_Fr_Available>0){  
      Fr_Data.Frame<-Func_Shared(DF = Fr_Data.Frame, Item_Picked = Fr_Picked,Share_YN_Food=Share_YN_Food)
    }
    
    #Pss--------------------------------------------------------------------
    
    if(Pick_YN_Pss==1 && Sum_Pss_Available>0){  
      Pss_Data.Frame<-Func_Shared(DF = Pss_Data.Frame, Item_Picked = Pss_Picked,Share_YN_Food=Share_YN_Food)
    }
    
    #Pre--------------------------------------------------------------------
    
    if(Pick_YN_Pre==1 && Sum_Pre_Available>0){    
      Pre_Data.Frame<-Func_Shared(DF = Pre_Data.Frame, Item_Picked = Pre_Picked,Share_YN_Food=Share_YN_Food)
    }
    
    
    
    #PICKING AND CONSUMING ST ITEMS =====================================================================================
    
    
    # Select Fruit---------------------------------------------------------------------------------------------
    
    Items_Shared<-Fr_Data.Frame$Location == "Shared" 
    Sum_Shared<-sum(Items_Shared, na.rm = TRUE)
    if(Sum_Shared>0){
      
      #Did a student pick an item for the share table? 
      Pick_ST_YN_Fr<-ifelse(runif(1)<Pr_Pick_ST_Fr,1,0) 
      #Fruit picked from Share Table. 
      if(Pick_ST_YN_Fr==1){
        OutputFP_ST<-Func_Picked_ST(DF = Fr_Data.Frame,Item_Picked_ST = Fr_ST_Picked)
        Fr_Data.Frame<-OutputFP_ST$DF
        Fr_ST_Picked<-OutputFP_ST$Item_Picked_ST
      }
      
      #Contamination from Hand to Fruit or from Fruit to Hand. 
      if(Pick_ST_YN_Fr==1){
        #Contamination from picking item share table
        OutputFCC_ST<-Func_Cross_Contamination(Cont_Student=Cont_Student,Data.Frame=Fr_Data.Frame, Item_Picked= Fr_ST_Picked, Item="Fruit")
        Fr_Data.Frame<-OutputFCC_ST$Data.Frame
        Cont_Student<-OutputFCC_ST$Cont_Student
        #Fr_Data.Frame<-Func_Allergen_CC(Fr_Data.Frame,Fr_ST_Picked) #Adding Allergen Contamination
      }
      
      #Consuming Fruit--------------------------------------------------------------------------
      
      #Did the student consume the Fruit?
      Eat_YN_ST_Fr<-ifelse(runif(1)<Pr_eat_ST_Fr,1,0)
      
      #Changing Data Frame so it updates when student consumes fruit.
      
      if(Pick_ST_YN_Fr==1){  
        OutputsFEFr<-Func_Eat_Fr(Eat_YN_Item = Eat_YN_ST_Fr, DF = Fr_Data.Frame,Item_Picked = Fr_ST_Picked,Item = "Fruit")
        Cont_Student<-OutputsFEFr$Cont_Student
        Fr_Data.Frame<-OutputsFEFr$DF
      } # end of picking if
    }  #end of if to make sure there are ST items
    
    #Picking Pss------------------------------------------------------------------------
    
    Items_Shared_Pss<-Pss_Data.Frame$Location == "Shared" 
    Sum_Shared_Pss<-sum(Items_Shared_Pss, na.rm = TRUE)
    if(Sum_Shared_Pss>0){
      
      #Did a student pick an item for the share table? 
      Pick_ST_YN_Pss<-ifelse(runif(1)<Pr_Pick_ST_Pss,1,0) 
      
      #Pss picked from Share Table. 
      if(Pick_ST_YN_Pss==1){
        OutputFP_ST<-Func_Picked_ST(DF = Pss_Data.Frame,Item_Picked_ST = Pss_ST_Picked)
        Pss_Data.Frame<-OutputFP_ST$DF
        Pss_ST_Picked<-OutputFP_ST$Item_Picked_ST
      }
      
      
      #Contamination from Hand to Pss or from Hand to Pss. 
      if(Pick_ST_YN_Pss==1){
        #Contamination from picking item share table
        OutputFCC_ST<-Func_Cross_Contamination(Cont_Student=Cont_Student,Data.Frame=Pss_Data.Frame, Item_Picked=  Pss_ST_Picked, Item="PSS")
        Pss_Data.Frame<-OutputFCC_ST$Data.Frame
        Cont_Student<-OutputFCC_ST$Cont_Student
        #Pss_Data.Frame<-Func_Allergen_CC(Pss_Data.Frame,Pss_ST_Picked) #Adding Allergen Contamination
      }
      
      #Consuming Pss-------------------------------------------------------------------------
      
      #Did the student consume the Fruit?
      Eat_YN_ST_Pss<-ifelse(runif(1)<Pr_eat_ST_Pss,1,0)
      
      #Changing Data Frame so it updates when student consumes fruit.
      
      if(Pick_ST_YN_Pss==1){  
        OutputsFEPss<-Func_Eat_Pss(Eat_YN_Item = Eat_YN_ST_Pss,DF = Pss_Data.Frame, Item_Picked = Pss_ST_Picked, Item = "PSS")
        Cont_Student<-OutputsFEPss$Cont_Student
        Pss_Data.Frame<-OutputsFEPss$DF
      }
    }
    
    #Picking Pre---------------------------------------------------------------------------
    
    Items_Shared_Pre<-Pre_Data.Frame$Location == "Shared" 
    Sum_Shared_Pre<-sum(Items_Shared_Pre, na.rm = TRUE)
    if(Sum_Shared_Pre>0){
      
      #Did a student pick an item for the share table? 
      Pick_ST_YN_Pre<-ifelse(runif(1)<Pr_Pick_ST_Pre,1,0) 
      
      #Pre picked from Share Table. 
      if(Pick_ST_YN_Pre==1){
        OutputFP_ST<-Func_Picked_ST(DF = Pre_Data.Frame,Item_Picked_ST = Pre_ST_Picked)
        Pre_Data.Frame<-OutputFP_ST$DF
        Pre_ST_Picked<-OutputFP_ST$Item_Picked_ST
      }
      
      if(Pick_ST_YN_Pre==1){
        #Contamination from picking item share table
        OutputFCC_ST<-Func_Cross_Contamination(Cont_Student=Cont_Student,Data.Frame=Pre_Data.Frame, Item_Picked= Pre_ST_Picked, Item="PRE")
        Pre_Data.Frame<-OutputFCC_ST$Data.Frame
        Cont_Student<-OutputFCC_ST$Cont_Student
        #Pre_Data.Frame<-Func_Allergen_CC(Pre_Data.Frame,Pre_ST_Picked) #Adding Allergen Contamination
      }
      
      
      
      #Consuming Pre--------------------------------------------------------------------------
      
      #Did the student consume the Fruit?
      Eat_YN_ST_Pre<-ifelse(runif(1)<Pr_eat_ST_Pre,1,0)
      
      #Changing Data Frame so it updates when student consumes fruit.
      if(Sum_Pre_Available>0){
        if(Pick_ST_YN_Pre==1){  
          if(Eat_YN_ST_Pre==1){
            OutputFEPre<-Func_Eat_Pre(Eat_YN_Item = Eat_YN_ST_Pre,DF = Pre_Data.Frame, Item_Picked = Pre_ST_Picked, Item = "PRE")
            Cont_Student<-OutputFEPre$Cont_Student
            Pre_Data.Frame<-OutputFEPre$DF
          }
        }
      }

      
    }#end of if there is st items loop
  }#end of Share Table  toggle loop
  
  Outputs_Student_Loop<-list(Fr_Data.Frame=Fr_Data.Frame, Pss_Data.Frame=Pss_Data.Frame,Pre_Data.Frame=Pre_Data.Frame)
  return(Outputs_Student_Loop)
}

  