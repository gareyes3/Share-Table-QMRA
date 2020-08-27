

# Transfer Efficiencies ---------------------------------------------------


  if(norovirus==1){
  TE_H_F<- rbetagen(1,0.76,1.04,.0126,.46)
  TE_H_S<-rtriang(1,.001,.13,.27) #.1340 #TE between Hands and Surfaces
  TE_F_H<-rbetagen(1,0.88,1.01,.048,.164)
  TE_F_S<-.0250 #TE between Food and Surfaces
  TE_S_H<-rtriang(1,.036,.03,.22)#.1090 #TE between Surfaces and Hands
  TE_S_F<-.4620 #TE between Surfaces and Foods
  TE_Pre_Mouth<-.1 #TE between Milk to Mouth
    }
  
  if(salmonella ==1){
    TE_H_F<-.0021
    TE_F_H<-.0328
    TE_H_S<-0.0016 #Chen and DS
    TE_S_H<-0.0229 #TE between Surfaces and Hands
  }


# Contamination in Student Hands ------------------------------------------




  #Level of Contamination in Student Hands. 
  
  if(salmonella ==1){
    IC_Student<-8.9*10^6  #CFU/Hand
  }


  #Norovirus Contamination levels. 
  if(norovirus ==1){
    mass_feces_hands<- rbetagen(1,4.57,2.55,-8.00,-1.00) #log(g/hands)
    HU_NV_in_Feces<- rlnormTrunc(1,6.65,2.06,0.0,10.98) #log HuNov CG/ g
    Genomic_copies_per_PFU<-rnormTrunc(1,3.65,.98,2.00,5.40)
    Personal_Contamination<-((10^mass_feces_hands) * (10^HU_NV_in_Feces))/(10^Genomic_copies_per_PFU) #PFU/Hand
    IC_Student<- Personal_Contamination #CFU/Hand 
  }





# Calculation of Items Touches Service Line -------------------------------

  #Touched items
  ntouched_Fr<-round(rnorm(1,3,.5),0)
  ntouched_Pss<-round(rnorm(1,3,.5),0)
  ntouched_Pre<-round(rnorm(1,3,.5),0)
