


# Initial Contamination of Student Inputs ---------------------------------

Inputs_ICont_Student<-list(
  #salmonella parameters
  IC_salmonella=8.9*10^6,
  #Norovirus parameters
  mass_feces_hands=rpert(1,-8,-3,-1,shape = 4), 
  HU_NV_in_Feces=rpert(1,4,8,10,shape = 4), 
  Pr_WashingHand = .42,
  LogRed =rpert(1,0.17,0.45,6,shape = 4)
)

# Transfer Efficiencies ---------------------------------------------------

Res_Trans<-1.97


if(norovirus==1){
  TE_H_F<- rbetagen(1,0.76,1.04,.0126,.46) #0.2013#
  TE_H_S<-rtriang(1,.001,.13,.27) #TE between Hands and Surfaces #add .1340 #
  TE_F_H<-rbetagen(1,0.88,1.01,.048,.164) #add 0.1018#
  TE_F_S<-.0250 #TE between Food and Surfaces
  TE_S_H<- rtriang(1,.036,.07,.22)#.1090 #TE between Surfaces and Hands
  TE_S_F<-.4620 #TE between Surfaces and Foods
  TE_Pre_Mouth<-.339 #TE between Milk to Mouth
  
  #For new implementation of the model. 
  TrP_H_F<-0.5012043#inv.logit(rnorm(1,-3.86,Res_Trans)) #mean=.07197
  TrP_F_H<-inv.logit(rnorm(1,-2.95,Res_Trans)) #mean = 0.067246
  TrP_H_S<-inv.logit(rnorm(1,-3.82,Res_Trans)) #mean = 0.0002117642
  TrP_S_H<-inv.logit(rnorm(1,0.11,Res_Trans)) #mean = 0.0720518
  
  
}


if(salmonella ==1){
  TE_H_F<-.0021
  TE_F_H<-.0328
  TE_H_S<-0.0016 #Chen and DS
  TE_S_H<-0.0229
  TE_Pre_Mouth<-.3397#TE between Surfaces and Hands
}


# Calculation of Items Touches Service Line -------------------------------

#Touched items
ntouched_Fr<-sample(0:3,1) #add
ntouched_Pss<-sample(0:3,1) #add
ntouched_Pre<-sample(0:3,1) #add



#Behavioral Inputs: 


#Probability of sharing food. 

Pr_share_Food<-rpert(1,min=0.5, mode= 0.7,max =0.9 ,4)

#Probability of student picking an additional item from share table. 

Pr_Pick_ST_Fr<-rpert(1,min=0.05, mode= 0.1,max =0.15,4)
Pr_Pick_ST_Pss<-rpert(1,min=0.05, mode= 0.1,max =0.15,4)
Pr_Pick_ST_Pre<-rpert(1,min=0.05, mode= 0.1,max =0.15,4)

#Probability Sutdent eats share table item

Pr_eat_ST_Fr<-rpert(1,min=0.7, mode= 0.8,max =0.9,4)
Pr_eat_ST_Pss<-rpert(1,min=0.7, mode= 0.8,max =0.9,4)
Pr_eat_ST_Pre<-rpert(1,min=0.7, mode= 0.8,max =0.9,4)

#If students won't carry anything then set Pr os 0

Pr_Student_iC<-rpert(1,0.06,0.07,0.09)

  
#####Not Used BACKUP------------------------------------------------------------------------------------------------------------
  
  #Remove _PFU
  #As backup, not used
  Inputs_ICont_Student_PFU<-list(
    #salmonella parameters
    IC_salmonella=8.9*10^6,
    #Norovirus parameters
    mass_feces_hands= rbetagen(1,4.57,2.55,-8.00,-1.00), #log(g/hands) #-3.5, 
    HU_NV_in_Feces=rlnormTrunc(1,6.65,2.06,0.0,10.98), #log HuNov CG/ g #add 6.15,#
    Genomic_copies_per_PFU=rnormTrunc(1,3.65,.98,2.00,5.40) #add 3.66#
  )
  