


# Initial Contamination of Student Inputs ---------------------------------

Inputs_ICont_Student<-list(
  #salmonella parameters
  IC_salmonella=8.9*10^6,
  #Norovirus parameters
  mass_feces_hands= -3.49,#rpert(1,-8,-3,-1,shape = 4), #-3.49,#
  HU_NV_in_Feces=7.66#rpert(1,4,8,10,shape = 4) #7.66#
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
  TrP_H_F<-.07197#inv.logit(rnorm(1,-3.86,Res_Trans)) #mean=
  TrP_F_H<-0.067246 #inv.logit(rnorm(1,-2.95,Res_Trans)) #mean = 
  TrP_H_S<-0.0002117642 #inv.logit(rnorm(1,-3.82,Res_Trans)) #mean = 
  TrP_S_H<-0.0720518 #inv.logit(rnorm(1,0.11,Res_Trans)) #mean = 
  
  
}


if(salmonella ==1){
  TE_H_F<-.0021
  TE_F_H<-.0328
  TE_H_S<-0.0016 #Chen and DS
  TE_S_H<-0.0229
  TE_Pre_Mouth<-.3397#TE between Surfaces and Hands
}

#Adding TE to Vectors
Vector_TE_H_F<-c(Vector_TE_H_F,TE_H_F )
Vector_TE_F_H<-c(Vector_TE_F_H,TE_F_H )
Vector_TE_H_S<-c(Vector_TE_H_S, TE_H_S)
Vector_TE_S_H<-c(Vector_TE_S_H,TE_S_H)
Vector_TE_S_F<-c(Vector_TE_S_F, TE_S_F)
Vector_TE_F_S<-c(Vector_TE_F_S, TE_F_S)
Vector_TE_Pre_Mouth<-c(Vector_TE_Pre_Mouth,TE_Pre_Mouth)

# Calculation of Items Touches Service Line -------------------------------

  #Touched items
  ntouched_Fr<-3#round(rnorm(1,3,.5),0) #add
  ntouched_Pss<-3#round(rnorm(1,3,.5),0) #add
  ntouched_Pre<-3#round(rnorm(1,3,.5),0) #add

  
  
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
  