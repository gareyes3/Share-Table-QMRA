# INPUTS===========================================================================================================


# Selection of Hazard -----------------------------------------------------

  salmonella<-1
  E_coli<-0
  norovirus<-0
  allergen<-0


# Inputs for Initial Food Contamination -----------------------------------

  
  #Toggle for calculated contamination, defaul is 0
  Calculated_Cont_Fr<-1
  Calculated_Cont_Pss<-1
  Calculated_Cont_Pre<-1
  
  #Fruit physical characteristics used for calculation
  Fr_Mean_area<-184.7 #cm2
  Fr_sd_area<- 8.6 #cm2
  Fr_Mean_weight<-171.1 #g
  Fr_sd_weight<-6.0 #g
  
  #Pss physical Characteristics
  Pss_Mean_area<-100 #cm2
  
  #Pre physical characteristics
  Pre_Mean_area<- 190 #cm2
  
  #Fr Contamination per log CFU/ cm2 
  Fr_Contamination_salmonella<- (-4.16) #Salmonella
  Fr_Contamination_norovirus<-(-4.16)  #norovirus
  
  #Pss Contamination per log CFU/ cm2 
  Pss_Contamination_salmonella<- (-4.16) #Salmonella
  Pss_Contamination_norovirus<-(-4.16)  #norovirus
  
  #Pre Contamination per log CFU/ cm2 
  Pre_Contamination_salmonella<- (-4.16) #Salmonella
  Pre_Contamination_norovirus<-(-4.16)  #norovirus
  
  #Prevalence of pathogens in Fruit
  Prevalence_Salmonella_Fr<-.04 #Probability of contamination.
  Prevalence_Norovirus_Fr<-.05 #probability of food item being contaminated. 
  
  #Prevalence of pathogens in Pss
  Prevalence_Salmonella_Pss<-.04 #Probability of contamination.
  Prevalence_Norovirus_Pss<-.05 #probability of food item being contaminated. 
  
  #Prevalence of pathogens in Pre
  Prevalence_Salmonella_Pre<-.04 #Probability of contamination.
  Prevalence_Norovirus_Pre<-.05 #probability of food item being contaminated. 
  
  
# Inputs for Milk Spilage -------------------------------------------------
  
  Initial_Spoilage_Con<-2.18 # Initial Spoilage organism concentration Aerobic Plate Count in log CFU/g
  Spoilage_Treshold<-5 #Considered spoiled milk. APCs log CFU/g



# Inputs for Iterations. Student, Services, Meal_Days ---------------------

  #Students data: Initial Iterations i
  
  Students_p_grade<-89
  
  NSLP_rate<-.73
  
  N_Iterations<-round((Students_p_grade*NSLP_rate),0)
  
  #Serivices, number of days we are trying to iterate. Lunch periods per day
  Service_No<-5
  
  #Days we are trying to Iterate. Days
  Food_Days<-5
 
# Sevice Line Information -------------------------------------------------
  
  #Initial Number of fruit
  Initial_Fr<-75 #Number of fruit 
  Initial_Pss<-75 #number of Packaged shelf stable
  Initial_Pre<-75 #number of packaged refrigirated
  
  #row size in selection table
  Row_size_Fr<-20
  Row_size_Pss<-20
  Row_size_Pre<-20  
  
  
# Inputs for Calculation of Student Contamination -------------------------
  
  # If students won't carry anything then set Pr os 0
  Pr_Student_iC<-.005 #probability of 1 student being contaminated.

  #Average Student Contamination can be PFU or CFU
  
  #surface are of hand palm for when converting from cm^2 
  Student_PSA<-50.675 #cm^2
  
  #Initial Contamination of salmonella 
  if(salmonella == 1){
    Av_ic<-(8.9*10^6) #CFU/Hand
  }
  
  #Initial Contamination of norovirus
  if(norovirus == 1){
    Av_ic<-(8.9*10^6) #CFU/Hand    
  }
  
  
#Inputs for allergen contamination ----------------------------------------
  
  Pr_Student_Allergen<-0 #probability of student bringing in Allergens
  Number_Student_Allergens<-2
  
  Student_Allergen_No<-c(5,19)  #No Student that is contaminated to enter iteration
  Student_Allergen_Service<-c(2,3) #service in which contaminated kid enter the. 
  Student_Allergen_Day<-c(1,2)#Day in which contaminated kids may enter system. 
  

# Inputs Behavioral Probabilities -----------------------------------------


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


# Washing Parameters ------------------------------------------------------
  
  #Reduction achieved by washing

  Reduction_wash<-(-2) #log Reduction by washing


# Growth Inputs ------------------------------------------------------------------
  

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
  
                                            #Growth
  
  #Do we want to simulate model with growth? 
  
  Growth<-1
  

  
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
  #NOTE: Turn off Re-Sharing and -STto reservice too
                                            # Re-Sharing, Re-Service 
  
  #this happens in the data frame file
  #Share Table to Re-Service
  STtoReservice_YN<-1 
  #Re-Service of Items left in Share Table 
  Reservice_YN<-1
  #Re-Sharing of items in share table
  Resharing_YN<-1
  
                                              #Visuals Toggles
  
  Units_Per_gram<-0
  
  
                                              #Allergen Toggles
  Toggle_SelfAssigned_Allergens<-1
  
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

  Student_Allergen_Count<-1

  
