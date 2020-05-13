# INPUTS ------------------------------------------------------------------

# Number of Iteration in service
  N_Iterations<-200

#Probability that student is initially contaminated
  Pr_Student_iC<-.1

#Initial Number of fruit
  Initial_Fr<-200 #Number of fruit 
  Initial_Pss<-200 #number of Packaged shelf stable
  Initial_Pre<-200 #number of packaged refrigirated

#Probability of student Picking up food from line-

  #Probability of Selecting Fruit
  Pr_select_Fr<-.9
  #Probability of Selecting Pss
  Pr_select_Pss<-0.9
  #Probability of selecting Pre
  Pr_select_Pre<-0.9

#Porbability of Student touching other line items before picking their food. 
  Pr_touch_Food<-.3

#Proabbility of consuming Food
  Pr_eat_Fr<-.9
  Pr_eat_Pss<-.9
  Pr_eat_Pre<-.9

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

#row size
  Row_size_Fr<-20
  Row_size_Pss<-20
  Row_size_Pre<-20



# Toggles -----------------------------------------------------------------

  
  #All toggles 0 = no 1 == yes
  

  
                                            # Washing Share Table Items
  
  #Wash Selection Table Fruit
  Wash_Selection_YN_Fr<-1 
  #Wash Share Table Items
  Wash_ST_YN_Fr<-1
  
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
  
  #Data Lists
  datalistFr=list()
  datalistPss=list()
  datalistPre=list()
  
  




