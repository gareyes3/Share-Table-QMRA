


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
  TrP_H_F<-inv.logit(rnorm(1,-3.86,Res_Trans)) #mean=.07197
  TrP_F_H<-inv.logit(rnorm(1,-2.95,Res_Trans)) #mean = 0.067246
  TrP_H_S<-0.5107151#inv.logit(rnorm(1,-3.82,Res_Trans)) #mean = 0.0002117642
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
  ntouched_Fr<-round(rnorm(1,3,.5),0) #add
  ntouched_Pss<-round(rnorm(1,3,.5),0) #add
  ntouched_Pre<-round(rnorm(1,3,.5),0) #add

  
  
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
  