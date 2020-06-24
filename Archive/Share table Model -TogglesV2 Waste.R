
 
setwd("G:/Share Table QMRA/Share-Table-QMRA")
library("ggplot2") 
library("reshape2") 
library("mc2d")
library(dplyr)


 ####RUN FROM HERE####

# Source Files ------------------------------------------------------------


#Inputs
source("InputsV5.R")
#Functions
source("FunctionsV6.R")



# Meal Day ----------------------------------------------------------------

Meal_Day<-10

for (j in 1:Meal_Day){
  
  
  #Data Frames Source Files
  source("Data FramesV7.R")  




  # FUNCTION RUNS ITERATIONS ------------------------------------------------Meal 1 of 200 kids

  for (i in 1:N_Iterations){
  
  #Random Inputs
  source("RandInputsV5.R")   

  #Running Student Contamination based on Probability. 
  Cont_Student<- ifelse(runif(1)<Pr_Student_iC,IC_Student,0) 
  
  #Total transfer of particles=================================================================
  
  Tr_H_Fr<-Cont_Student*TE_H_F
  Tr_Fr_H<-Cont_Fr*TE_F_H
  Tr_H_Pss<- Cont_Student*TE_H_F
  Tr_Pss_H<-Cont_Pss*TE_F_H
  Tr_H_Pre<-Cont_Student*TE_H_F
  Tr_Pre_H<-Cont_Pre*TE_F_H 
  
  #Student Selection ===========================================================================
  
                                                      #FRUIT
  #Student Touching Fruit in line
  
  #Did Student touch other fruit based on probability? 
  Touch_YN_Fr<-ifelse(runif(1)<Pr_touch_Food,1,0) 
  
  #If touched what is the contamination and adding it to data frame?
  if(Touch_YN_Fr==1){
    Fr_Available<-Fr_Data.Frame$Location == "Selection Table" 
    Sum_Fr_Available<-sum(Fr_Available)
    if(Sum_Fr_Available>ntouched_Fr){
    for (i in 1:ntouched_Fr){
      Tr_H_Fr<-Cont_Student*TE_H_F
      Search.df.fr_touched<-Func_seach_Data4(Fr_Data.Frame,Fr_Data.Frame$Location,"Selection Table",Row_size_Fr)
      Fr_Touched<-as.numeric(Search.df.fr_touched$Apple.No.)
      Fr_Data.Frame[as.numeric(row.names(Search.df.fr_touched)),colnames(Fr_Data.Frame)== "Contamination"]<- 
      Func_Index_DF(Fr_Data.Frame,Fr_Touched,"Contamination") + Tr_H_Fr - (Func_Index_DF(Fr_Data.Frame,Fr_Touched,"Contamination")* TE_F_H) 
      Cont_Student<-ifelse(Cont_Student -  Func_Index_DF(Fr_Data.Frame,Fr_Touched,"Contamination")<0,0,Cont_Student -  Func_Index_DF(Fr_Data.Frame,Fr_Touched,"Contamination"))
      Fr_Data.Frame[Fr_Touched,colnames(Fr_Data.Frame)=="History"]<-paste(Fr_Data.Frame[Fr_Touched,colnames(Fr_Data.Frame)=="History"], "Touched")
      
    }
    }
    }
  
  #Student Picking Fruit from line#
  
  Pick_YN_Fr<-ifelse(runif(1,0,1)<Pr_select_Fr,1,0)
  #Picking a Fruit from Fruit Frame 
   if(Pick_YN_Fr==1){
     Fr_Available<-Fr_Data.Frame$Location == "Selection Table" 
     Sum_Fr_Available<-as.numeric(sum(Fr_Available))
     if(Sum_Fr_Available>0){   
      Search.df.fr<-Func_seach_Data4(Fr_Data.Frame,Fr_Data.Frame$Location,"Selection Table",Row_size_Fr)
      #Fruit Selected #
      Fr_Picked<-as.numeric(Search.df.fr$Apple.No.)
      Fr_Data.Frame[Fr_Picked,colnames(Fr_Data.Frame)== "Location"]<-"Tray"
      Fr_Data.Frame[Fr_Picked,colnames(Fr_Data.Frame)=="History"]<-paste(Fr_Data.Frame[Fr_Picked,colnames(Fr_Data.Frame)=="History"], "Tray")
      }
    }
  

  
  #Contamination from Hand to fruit Going into Tray
  
  if(Pick_YN_Fr==1){
    Tr_H_Fr<-Cont_Student*TE_H_F
    #Contamination at tray
    Cont_Tray_Fr<- Func_Index_DF(Fr_Data.Frame,Fr_Picked,"Contamination") + Tr_H_Fr - (Func_Index_DF(Fr_Data.Frame,Fr_Picked,"Contamination")* TE_F_H)
    #Add contamination to chosen fruit in Dataframe
    Fr_Data.Frame[Fr_Picked,colnames(Fr_Data.Frame)== "Contamination"]<-Cont_Tray_Fr
  }
  
                                                      #PSS
  #Did student touch other Pss based on probability 
  Touch_YN_Pss<-ifelse(runif(1)<Pr_touch_Food,1,0) 
  
  #If touched what is the contamination and adding it to data frame
  
  if(Touch_YN_Pss==1){
    Pss_Available<-Pss_Data.Frame$Location == "Selection Table" 
    Sum_Pss_Available<-sum(Pss_Available)
    if(Sum_Pss_Available>ntouched_Pss){
    for (i in 1:ntouched_Pss){
      Tr_H_Fr<-Cont_Student*TE_H_F
      Search.df.Pss_touched<-Func_seach_Data4(Pss_Data.Frame,Pss_Data.Frame$Location,"Selection Table",Row_size_Pss)
      Pss_Touched<-as.numeric(Search.df.Pss_touched$Pss.No.)
      Pss_Data.Frame[as.numeric(row.names(Search.df.Pss_touched)),colnames(Pss_Data.Frame)== "Contamination"]<- 
        Func_Index_DF(Pss_Data.Frame, Pss_Touched,"Contamination") + Tr_H_Pss - (Func_Index_DF(Pss_Data.Frame,Pss_Touched,"Contamination")* TE_F_H) 
      Cont_Student<-ifelse(Cont_Student -  Func_Index_DF(Pss_Data.Frame,Pss_Touched,"Contamination")<0,0,Cont_Student -  Func_Index_DF(Pss_Data.Frame,Pss_Touched,"Contamination"))
      Pss_Data.Frame[Pss_Touched,colnames(Pss_Data.Frame)=="History"]<-paste(Pss_Data.Frame[Pss_Touched,colnames(Pss_Data.Frame)=="History"], "Touched")
    }
    }
  }
  

  #Student picking Pss from line
  
  Pick_YN_Pss<-ifelse(runif(1)<Pr_select_Pss,1,0) 
   #Picking a Pss from Pss Data Frame 
   if(Pick_YN_Pss==1){
     Pss_Available<-Pss_Data.Frame$Location == "Selection Table" 
     Sum_Pss_Available<-sum(Pss_Available)
     if(Sum_Pss_Available>0){
      Search.df.Pss<-Func_seach_Data4(Pss_Data.Frame,Pss_Data.Frame$Location,"Selection Table",Row_size_Pss)
      #Pss Selected #
      Pss_Picked<-as.numeric(Search.df.Pss$Pss.No.)
      Pss_Data.Frame[Pss_Picked,colnames(Pss_Data.Frame)== "Location"]<-"Tray"
      Pss_Data.Frame[Pss_Picked,colnames(Pss_Data.Frame)=="History"]<-paste(Pss_Data.Frame[Pss_Picked,colnames(Pss_Data.Frame)=="History"], "Tray")
     }
  }
  

  #Contamination From Hands to Pss going to Tray
  
  
  if(Pick_YN_Pss==1){
    Tr_H_Fr<-Cont_Student*TE_H_F
    #Contamination at tray
    Cont_Tray_Pss<- Func_Index_DF(Pss_Data.Frame,Pss_Picked,"Contamination") + Tr_H_Pss - (Func_Index_DF(Pss_Data.Frame,Pss_Picked,"Contamination")* TE_F_H)
    #Add contamination to chosen fruit in Dataframe
    Pss_Data.Frame[Pss_Picked,colnames(Pss_Data.Frame)== "Contamination"]<-Cont_Tray_Pss
  }
  
  
                                                  #PRE
  
 #Did the Student touch any Pre during selection? 
  Touch_YN_Pre<-ifelse(runif(1)<Pr_touch_Food,1,0) 
  
  #If touched what is the contamination and adding it to data frame
  
  if(Touch_YN_Pre==1){
    Pre_Available<-Pre_Data.Frame$Location == "Selection Table" 
    Sum_Pre_Available<-sum(Pre_Available)
    if(Sum_Pre_Available>ntouched_Pre){
    for (i in 1:ntouched_Pre){
      Tr_H_Fr<-Cont_Student*TE_H_F
      Search.df.Pre_touched<-Func_seach_Data4(Pre_Data.Frame,Pre_Data.Frame$Location,"Selection Table",Row_size_Pre)
      Pre_Touched<-as.numeric(Search.df.Pre_touched$Pre.No.)
      Pre_Data.Frame[as.numeric(row.names(Search.df.Pre_touched)),colnames(Pre_Data.Frame)== "Contamination"]<- 
        Func_Index_DF(Pre_Data.Frame, Pre_Touched,"Contamination") + Tr_H_Pre - (Func_Index_DF(Pre_Data.Frame,Pre_Touched,"Contamination")* TE_F_H) 
      Cont_Student<-ifelse(Cont_Student -  Func_Index_DF(Pre_Data.Frame,Pre_Touched,"Contamination")<0,0,Cont_Student -  Func_Index_DF(Pre_Data.Frame,Pre_Touched,"Contamination"))
      Pre_Data.Frame[Pre_Touched,colnames(Pre_Data.Frame)=="History"]<-paste(Pre_Data.Frame[Pre_Touched,colnames(Pre_Data.Frame)=="History"], "Touched")
    }
    }
  }
  
  
  #Picking Pre from line
  Pick_YN_Pre<-ifelse(runif(1)<Pr_select_Pre,1,0) 
  #Picking a Pre from Pre Data Frame 
  if(Pick_YN_Pre==1){
    Pre_Available<-Pre_Data.Frame$Location == "Selection Table" 
    Sum_Pre_Available<-sum(Pre_Available)
    if(Sum_Pre_Available>0){
      Search.df.Pre<-Func_seach_Data4(Pre_Data.Frame,Pre_Data.Frame$Location,"Selection Table",Row_size_Pre)
      #Pre Selected #
      Pre_Picked<-as.numeric(Search.df.Pre$Pre.No.)
      Pre_Data.Frame[Pre_Picked,colnames(Pre_Data.Frame)== "Location"]<-"Tray"
      Pre_Data.Frame[Pre_Picked,colnames(Pre_Data.Frame)=="History"]<-paste(Pre_Data.Frame[Pre_Picked,colnames(Pre_Data.Frame)=="History"], "Tray")
        }
    }
  

  
  #Contamination From Hands to Pre going to Tray
  
  
  if(Pick_YN_Pre==1){
    Tr_H_Fr<-Cont_Student*TE_H_F
    #Contamination at tray
    Cont_Tray_Pre<- Func_Index_DF(Pre_Data.Frame,Pre_Picked,"Contamination") + Tr_H_Pre - (Func_Index_DF(Pre_Data.Frame,Pre_Picked,"Contamination")* TE_F_H)
    #Add contamination to chosen fruit in Dataframe
    Pre_Data.Frame[Pre_Picked,colnames(Pre_Data.Frame)== "Contamination"]<-Cont_Tray_Pre
  }  
  
  
  
  # Consumption==========================================================================
  
  
  
                                      #Fruit

  #Did the student consume the Fruit?
  Eat_YN_Fr<-ifelse(runif(1)<Pr_eat_Fr,1,0)

  #Changing Data Frame so it updates when student consumes fruit.

  if(Pick_YN_Fr==1){
  if(Eat_YN_Fr==1){

  Fr_Data.Frame[Fr_Picked,colnames(Fr_Data.Frame)== "Location"]<-"Consumed"
  Fr_Data.Frame[Fr_Picked,colnames(Fr_Data.Frame)=="History"]<-paste(Fr_Data.Frame[Fr_Picked,colnames(Fr_Data.Frame)=="History"], "Consumed")
  }else{
  Fr_Data.Frame[Fr_Picked,colnames(Fr_Data.Frame)== "Location"]<-"Not Consumed"
  Fr_Data.Frame[Fr_Picked,colnames(Fr_Data.Frame)=="History"]<-paste(Fr_Data.Frame[Fr_Picked,colnames(Fr_Data.Frame)=="History"], "NotConsumed")
  }
  }

                                      #Pss
  #Did student consume the Pss
  Eat_YN_Pss<-ifelse(runif(1)<Pr_eat_Pss,1,0)
  
  #Changing Data Frame for consumption of Pss
  if(Pick_YN_Pss==1){
    if(Eat_YN_Pss==1){
    
    Pss_Data.Frame[Pss_Picked,colnames(Pss_Data.Frame)== "Location"]<-"Consumed" 
    Pss_Data.Frame[Pss_Picked,colnames(Pss_Data.Frame)=="History"]<-paste(Pss_Data.Frame[Pss_Picked,colnames(Pss_Data.Frame)=="History"], "Consumed")
  }else{
    Pss_Data.Frame[Pss_Picked,colnames(Pss_Data.Frame)== "Location"]<-"Not Consumed" 
    Pss_Data.Frame[Pss_Picked,colnames(Pss_Data.Frame)=="History"]<-paste(Pss_Data.Frame[Pss_Picked,colnames(Pss_Data.Frame)=="History"], "NotConsumed")
  }
  }
  
                                      #Pre
  #Did student consume the Pre
  Eat_YN_Pre<-ifelse(runif(1)<Pr_eat_Pre,1,0)
  
  #Changind Data Frame for consumption of Pre
  if(Pick_YN_Pre==1){
   if(Eat_YN_Pre==1){
    
    Pre_Data.Frame[Pre_Picked,colnames(Pre_Data.Frame)== "Location"]<-"Consumed" 
    Pre_Data.Frame[Pre_Picked,colnames(Pre_Data.Frame)=="History"]<-paste(Pre_Data.Frame[Pre_Picked,colnames(Pre_Data.Frame)=="History"], "Consumed")
  }else{
    Pre_Data.Frame[Pre_Picked,colnames(Pre_Data.Frame)== "Location"]<-"Not Consumed"  
    Pre_Data.Frame[Pre_Picked,colnames(Pre_Data.Frame)=="History"]<-paste(Pre_Data.Frame[Pre_Picked,colnames(Pre_Data.Frame)=="History"], "NotConsumed")
  }  
  }




  #Consumption to share table=======================================================
  
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
    Sum_Shared<-sum(Items_Shared)
    if(Sum_Shared>0){
  
    #Fruit
  
    #Did a student pick an item for the share table? 
    Pick_ST_YN_Fr<-ifelse(runif(1)<Pr_Pick_ST_Fr,1,0) 
  
    #Fruit picked from Share Table. 
    if(Pick_ST_YN_Fr==1){
    Search.df.fr_ST<-Func_Search_Data(Fr_Data.Frame,Fr_Data.Frame$Location,"Shared",1)
    #Fruit from share table selected #
    Fr_ST_Picked<-as.numeric(Search.df.fr_ST$Apple.No.)
    Fr_Data.Frame[as.numeric(row.names(Search.df.fr_ST)),colnames(Search.df.fr)== "Location"]<-"Tray"
    Fr_Data.Frame[Fr_ST_Picked,colnames(Fr_Data.Frame)=="History"]<-paste(Fr_Data.Frame[Fr_ST_Picked,colnames(Fr_Data.Frame)=="History"], "Tray")
    }
  
    #Contamination from Hand to Fruit or from Fruit to Hand. 
    if(Pick_ST_YN_Fr==1){
    #Contamination at tray
    Cont_Tray_Fr<- Func_Index_DF(Fr_Data.Frame,Fr_ST_Picked,"Contamination") + Tr_H_Fr - (Func_Index_DF(Fr_Data.Frame,Fr_ST_Picked,"Contamination")* TE_F_H)
    #Add contamination to chosen fruit in Dataframe
    Fr_Data.Frame[Fr_ST_Picked,colnames(Fr_Data.Frame)== "Contamination"]<-Cont_Tray_Fr
    }
  
    #Consumption of share table item
  
    #Did the student consume the Fruit?
    Eat_YN_ST_Fr<-ifelse(runif(1)<Pr_eat_ST_Fr,1,0)
  
    #Changing Data Frame so it updates when student consumes fruit.
  
    if(Pick_ST_YN_Fr==1){  
    if(Eat_YN_ST_Fr==1){
    
    Fr_Data.Frame[Fr_ST_Picked,colnames(Fr_Data.Frame)== "Location"]<-"Consumed"
    Fr_Data.Frame[Fr_ST_Picked,colnames(Fr_Data.Frame)=="History"]<-paste(Fr_Data.Frame[Fr_ST_Picked,colnames(Fr_Data.Frame)=="History"], "Consumed")
    }else{
    Fr_Data.Frame[Fr_ST_Picked,colnames(Fr_Data.Frame)== "Location"]<-"Not Consumed"
    Fr_Data.Frame[Fr_ST_Picked,colnames(Fr_Data.Frame)=="History"]<-paste(Fr_Data.Frame[Fr_ST_Picked,colnames(Fr_Data.Frame)=="History"], "NotConsumed")
      }
    }
    }
    
                                                                #Shared Pss
    Items_Shared_Pss<-Pss_Data.Frame$Location == "Shared" 
    Sum_Shared_Pss<-sum(Items_Shared_Pss)
    if(Sum_Shared_Pss>0){
    
  
    
    #Did a student pick an item for the share table? 
    Pick_ST_YN_Pss<-ifelse(runif(1)<Pr_Pick_ST_Pss,1,0) 
    
    #Pss picked from Share Table. 
    if(Pick_ST_YN_Pss==1){
      Search.df.Pss_ST<-Func_Search_Data(Pss_Data.Frame,Pss_Data.Frame$Location,"Shared",1)
      #Pss from share table selected #
      Pss_ST_Picked<-as.numeric(Search.df.Pss_ST$Pss.No.)
      Pss_Data.Frame[as.numeric(row.names(Search.df.Pss_ST)),colnames(Search.df.Pss)== "Location"]<-"Tray"
      Pss_Data.Frame[Pss_ST_Picked,colnames(Pss_Data.Frame)=="History"]<-paste(Pss_Data.Frame[Pss_ST_Picked,colnames(Pss_Data.Frame)=="History"], "Tray")
    }
    
    #Contamination from Hand to Fruit or from Hand to Fruit. 
    if(Pick_ST_YN_Pss==1){
      #Contamination at tray
      Cont_Tray_Pss<- Func_Index_DF(Pss_Data.Frame,Pss_ST_Picked,"Contamination") + Tr_H_Pss - (Func_Index_DF(Pss_Data.Frame,Pss_ST_Picked,"Contamination")* TE_F_H)
      #Add contamination to chosen fruit in Dataframe
      Pss_Data.Frame[Pss_ST_Picked,colnames(Pss_Data.Frame)== "Contamination"]<-Cont_Tray_Pss
    }
    
    #Consumption of share table item
    
    #Did the student consume the Fruit?
    Eat_YN_ST_Pss<-ifelse(runif(1)<Pr_eat_ST_Pss,1,0)
    
    #Changing Data Frame so it updates when student consumes fruit.
    
    if(Pick_ST_YN_Pss==1){  
      if(Eat_YN_ST_Pss==1){
        
        Pss_Data.Frame[Pss_ST_Picked,colnames(Pss_Data.Frame)== "Location"]<-"Consumed"
        Pss_Data.Frame[Pss_ST_Picked,colnames(Pss_Data.Frame)=="History"]<-paste(Pss_Data.Frame[Pss_ST_Picked,colnames(Pss_Data.Frame)=="History"], "Consumed")
      }else{
        Pss_Data.Frame[Pss_ST_Picked,colnames(Pss_Data.Frame)== "Location"]<-"Not Consumed"
        Pss_Data.Frame[Pss_ST_Picked,colnames(Pss_Data.Frame)=="History"]<-paste(Pss_Data.Frame[Pss_ST_Picked,colnames(Pss_Data.Frame)=="History"], "NotConsumed")
      }
      }
    }
  
                                                                  #Shared Pre
  
    Items_Shared_Pre<-Pre_Data.Frame$Location == "Shared" 
    Sum_Shared_Pre<-sum(Items_Shared_Pre)
    if(Sum_Shared_Pre>0){
    
    
    
    #Did a student pick an item for the share table? 
    Pick_ST_YN_Pre<-ifelse(runif(1)<Pr_Pick_ST_Pre,1,0) 
    
    #Pre picked from Share Table. 
    if(Pick_ST_YN_Pre==1){
      Search.df.Pre_ST<-Func_Search_Data(Pre_Data.Frame,Pre_Data.Frame$Location,"Shared",1)
      #Pre from share table selected #
      Pre_ST_Picked<-as.numeric(Search.df.Pre_ST$Pre.No.)
      Pre_Data.Frame[as.numeric(row.names(Search.df.Pre_ST)),colnames(Search.df.Pre)== "Location"]<-"Tray"
      Pre_Data.Frame[Pre_ST_Picked,colnames(Pre_Data.Frame)=="History"]<-paste(Pre_Data.Frame[Pre_ST_Picked,colnames(Pre_Data.Frame)=="History"], "Tray")
    }
    
    #Contamination from Hand to Fruit or from Hand to Fruit. 
    if(Pick_ST_YN_Pre==1){
      #Contamination at tray
      Cont_Tray_Pre<- Func_Index_DF(Pre_Data.Frame,Pre_ST_Picked,"Contamination") + Tr_H_Pre - (Func_Index_DF(Pre_Data.Frame,Pre_ST_Picked,"Contamination")* TE_F_H)
      #Add contamination to chosen fruit in Dataframe
      Pre_Data.Frame[Pre_ST_Picked,colnames(Pre_Data.Frame)== "Contamination"]<-Cont_Tray_Pre
    }
    
    #Consumption of share table item
    
    #Did the student consume the Fruit?
    Eat_YN_ST_Pre<-ifelse(runif(1)<Pr_eat_ST_Pre,1,0)
    
    #Changing Data Frame so it updates when student consumes fruit.
    
    if(Pick_ST_YN_Pre==1){  
      if(Eat_YN_ST_Pre==1){
        
        Pre_Data.Frame[Pre_ST_Picked,colnames(Pre_Data.Frame)== "Location"]<-"Consumed"
        Pre_Data.Frame[Pre_ST_Picked,colnames(Pre_Data.Frame)=="History"]<-paste(Pre_Data.Frame[Pre_ST_Picked,colnames(Pre_Data.Frame)=="History"], "Consumed")
      }else{
        Pre_Data.Frame[Pre_ST_Picked,colnames(Pre_Data.Frame)== "Location"]<-"Not Consumed"
        Pre_Data.Frame[Pre_ST_Picked,colnames(Pre_Data.Frame)=="History"]<-paste(Pre_Data.Frame[Pre_ST_Picked,colnames(Pre_Data.Frame)=="History"], "NotConsumed")
       }
     }
    }#end of if there is st items loop
  }#end of toggle loop
  


}  #end of first loop
 
  #Updated items from not consumed, not shared, etc to wasted. 

  Fr_Data.Frame$Location[Fr_Data.Frame$Location=="Not Shared"]<-"Discarded"
  
  Fr_Data.Frame$Location[Fr_Data.Frame$Location=="Not Consumed"]<-"Discarded"
  if(Reservice_YN==0){
  Fr_Data.Frame$Location[Fr_Data.Frame$Location=="Selection Table"]<-"Discarded"
  }
  if(Resharing_YN==0){
    Fr_Data.Frame$Location[Fr_Data.Frame$Location=="Shared"]<-"Discarded" 
  } 
  
  Pss_Data.Frame$Location[Pss_Data.Frame$Location=="Not Shared"]<-"Discarded"
  
  Pss_Data.Frame$Location[Pss_Data.Frame$Location=="Not Consumed"]<-"Discarded"
  if(Reservice_YN==0){
    Pss_Data.Frame$Location[Pss_Data.Frame$Location=="Selection Table"]<-"Discarded"
  }
  if(Resharing_YN==0){
    Pss_Data.Frame$Location[Pss_Data.Frame$Location=="Shared"]<-"Discarded" 
  } 
   
  Pre_Data.Frame$Location[Pre_Data.Frame$Location=="Not Shared"]<-"Discarded"
  
  Pre_Data.Frame$Location[Pre_Data.Frame$Location=="Not Consumed"]<-"Discarded"
  if(Reservice_YN==0){
    Pre_Data.Frame$Location[Pre_Data.Frame$Location=="Selection Table"]<-"Discarded"
  }
  if(Resharing_YN==0){
    Pre_Data.Frame$Location[Pre_Data.Frame$Location=="Shared"]<-"Discarded" 
  } 
#Adding the data to the loop

datalistFr[[j]]<-Fr_Data.Frame
datalistPss[[j]]<-Pss_Data.Frame
datalistPre[[j]]<-Pre_Data.Frame
  
source("OutputsV5.R") 

message("Service #", j)

} #end of second loop

#Creation of the Final Data Frames
Fr_Data = do.call(rbind,datalistFr)
Pss_Data = do.call(rbind,datalistPss)
Pre_Data = do.call(rbind,datalistPre)


#Visuals

source("VisualsV5.R")

#update: changed outputs v2 for v3, see it it works for naming dfs. 


                                      





