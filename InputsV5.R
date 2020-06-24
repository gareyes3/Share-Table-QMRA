# INPUTS ------------------------------------------------------------------

#Do we want to simulate model with growth? 

  Growth<-0

#Hazards only turn one on at a time. 1 means on. 
  salmonella<-1
  E_coli<-0
  norovirus<-0
  allergen<-0
  
#Average Initial Contamination for Fruit (#CFU/g salmonella, E coli), (Particles norovirus)

  Initial_Cont_Fr<-200
  Initial_Cont_Pss<-0
  Initial_Cont_Pre<-0

# Number of Iteration in service
  N_Iterations<-200
  
#Serivices, number of days we are trying to iterate. 
 Service_No<-4
  
#Days we are trying to Iterate
  Food_Days<-2
  
#Probability that student is initially contaminated. For Viruses or Enterics.
# If students won't carry anything then set Pr os 0
  Pr_Student_iC<-0
  
#Average Student Contamination
  Av_ic<-3000
  Sd_ic<-200


#Initial Number of fruit
  Initial_Fr<-200 #Number of fruit 
  Initial_Pss<-200 #number of Packaged shelf stable
  Initial_Pre<-200 #number of packaged refrigirated

#Probability of student Picking up food from line-

  #Probability of Selecting Fruit
  Pr_select_Fr<-.23
  #Probability of Selecting Pss
  Pr_select_Pss<-0.37
  #Probability of selecting Pre
  Pr_select_Pre<-0.78

#Porbability of Student touching other line items before picking their food. 
  Pr_touch_Food<-.3

#Proabbility of consuming Food
  Pr_eat_Fr<-.48
  Pr_eat_Pss<-.77
  Pr_eat_Pre<-.85

#Probability of sharing food. 

  Pr_share_Food<-.9

#Probability of student picking an additional item from share table. 

  Pr_Pick_ST_Fr<-.1
  Pr_Pick_ST_Pss<-.1
  Pr_Pick_ST_Pre<-.1

#Probability Sutdent eats share table item

  Pr_eat_ST_Fr<-.9
  Pr_eat_ST_Pss<-.9
  Pr_eat_ST_Pre<-.9


#Reduction achieved by washing

  Reduction_wash<-(-2)

#row size in selection table
  Row_size_Fr<-20
  Row_size_Pss<-20
  Row_size_Pre<-20

#Storage Infromation for growth
  
  #Temperature at Share Table
  Temp_RT<-25
  #Temperature at refrigeration
  Temp_Ref<-4
  #Average Time ST
  Time_ST<-2
  #Average Time Ref
  Time_Ref<-12
  #Time over night storage
  Time_ON<-12
  #Time of each service all in hours. 
  Time_Service<-.50
  #Tome between Services
  Time_Turnaround<-.6
  

# Toggles -----------------------------------------------------------------

  
  #All toggles 0 = no 1 == yes
  

  
                                            # Washing Items
  #Washing Between Services
  Wash_Between_Services<-0
  
  
  #Wash Selection Table Fruit
  Wash_Selection_YN_Fr<-0 
  #Wash Share Table Items
  Wash_ST_YN_Fr<-0
  
                                              # Share Table Toggle 
  
  #Include Share Table:
  Share_Table_YN<-1
  
                                            # Re-Sharing, Re-Service 
  
  #this happens in the data frame file
  #Share Table to Re-Service
  STtoReservice_YN<-0  
  #Re-Service of Items left in Share Table 
  Reservice_YN<-1
  #Re-Sharing of items in share table
  Resharing_YN<-1
  
  
  
  
# Counters and Lists ---------------------------------------------------
  
  
  ##Contamination Pool Start# Not input
  Cont_Fr<- 0
  Cont_Pss<-0
  Cont_Pre<-0
  
  #vectors
  V_Available_Fr<-c(0)
  V_Available_Pss<-c(0)
  V_Available_Pre<-c(0)
  
  #Data Lists services
  datalistFr=list()
  datalistPss=list()
  datalistPre=list()
  
  #Datalist Days

  datalistFr_days=list()
  datalistPss_days=list()
  datalistPre_days=list()
  



