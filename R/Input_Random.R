


# Initial Contamination of Student Inputs ---------------------------------

Inputs_ICont_Student<-list(
  #salmonella parameters
  IC_salmonella=8.9*10^6,
  #Norovirus parameters
  mass_feces_hands=-3.5, #rbetagen(1,4.57,2.55,-8.00,-1.00), #log(g/hands) #add
  HU_NV_in_Feces=6.15,#rlnormTrunc(1,6.65,2.06,0.0,10.98), #log HuNov CG/ g #add
  Genomic_copies_per_PFU=3.66#rnormTrunc(1,3.65,.98,2.00,5.40) #add
)


# Transfer Efficiencies ---------------------------------------------------


  if(norovirus==1){
  TE_H_F<- 0.2013#rbetagen(1,0.76,1.04,.0126,.46)
  TE_H_S<-.1340 #rtriang(1,.001,.13,.27) #TE between Hands and Surfaces #add
  TE_F_H<-0.1018#rbetagen(1,0.88,1.01,.048,.164) #add
  TE_F_S<-.0250 #TE between Food and Surfaces
  TE_S_H<-.1090 #rtriang(1,.036,.03,.22)#.1090 #TE between Surfaces and Hands
  TE_S_F<-.4620 #TE between Surfaces and Foods
  TE_Pre_Mouth<-.339 #TE between Milk to Mouth
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
  ntouched_Fr<-round(rnorm(1,3,.5),0)
  ntouched_Pss<-round(rnorm(1,3,.5),0)
  ntouched_Pre<-round(rnorm(1,3,.5),0)
