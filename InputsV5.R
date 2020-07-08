# INPUTS ------------------------------------------------------------------


#Hazards only turn one on at a time. 1 means on. 
  salmonella<-1
  E_coli<-0
  norovirus<-0
  allergen<-0
  
#Average Initial Contamination for Fruit (#CFU/g salmonella, E coli), (Particles norovirus)
#if there is a function for initial contamination for the pathogen, the function will override these paramneters

  Initial_Cont_Fr<-0
  Initial_Cont_Pss<-0
  Initial_Cont_Pre<-0
  
# DATA FOR STIMATION OF FRUIT Contamination 
  
  Calculated_Cont<-1
  
  #surface area of Fruit
  Fr_Mean_area<-184.7 #cm2
  Fr_sd_area<- 8.6 #cm2
  Fr_Mean_weight<-171.1 #g
  Fr_sd_weight<-6.0 #g
  
  #Fr Contamination per log CFU/g cm2 
  Fr_Contamination<- (-4.16) #Salmonella
  
  #Prevalence of pathogens
  Prevalence_Salmonella<-.04 #Probability of contamination
  

# Number of Iteration in service. Kids going through line
  Students_p_grade<-89
  
  NSLP_rate<-.73
  
  N_Iterations<-round((Students_p_grade*NSLP_rate),0)
  
#Serivices, number of days we are trying to iterate. Lunch periods per day
 Service_No<-5
  
#Days we are trying to Iterate. Days
  Food_Days<-5
  
#Probability that student is initially contaminated. For Viruses or Enterics.
# If students won't carry anything then set Pr os 0
  Pr_Student_iC<-.005
  
#Average Student Contamination can be PFU or CFU
  Av_ic<-(8.9*10^6)/(171.1)
  Sd_ic<-900


#Initial Number of fruit
  Initial_Fr<-75 #Number of fruit 
  Initial_Pss<-75 #number of Packaged shelf stable
  Initial_Pre<-75 #number of packaged refrigirated

#Probability of student Picking up food from line-

  #Probability of Selecting Fruit
  Pr_select_Fr<-.56 #.23 other source
  #Probability of Selecting Pss
  Pr_select_Pss<-.59 #0.37 other source
  #Probability of selecting Pre
  Pr_select_Pre<-.96 #0.78 other source

#Porbability of Student touching other line items before picking their food. 
  Pr_touch_Food<-.3

#Proabbility of consuming Food
  Pr_eat_Fr<-.63 #.48  
  Pr_eat_Pss<-.627 #.77
  Pr_eat_Pre<-.674 #.85

#Probability of sharing food. 

  Pr_share_Food<-.7

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
  

# Growth Inputs ------------------------------------------------------------------
  
#Do we want to simulate model with growth? 
  
  Growth<-1

#Storage Infromation for growth
  
  #Temperature at Share Table
  Temp_RT<-25
  #Temperature at refrigeration
  Temp_Ref<-4
  #Time over night storage
  Time_ON<-12
  #Time of each service all in hours. 
  Time_Service<-.50
  #Time between Services
  Time_Turnaround<-.06
  

# TOGGLES -----------------------------------------------------------------

  
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
  Share_Table_YN<-0
  #NOTE: Turn off Re-Sharing and -STto reservice too
                                            # Re-Sharing, Re-Service 
  
  #this happens in the data frame file
  #Share Table to Re-Service
  STtoReservice_YN<-0  
  #Re-Service of Items left in Share Table 
  Reservice_YN<-1
  #Re-Sharing of items in share table
  Resharing_YN<-0
  
  
  
  
# COUNTER & LISTS -------------------#DO NOT MODIFY-----------------------
  #DO NOT MODIFY
  
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
  

  Student_Cont_Count<-c(0)


  
