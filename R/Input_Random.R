


# Initial Contamination of Student Inputs ---------------------------------

Inputs_ICont_Student<-list(
  #salmonella parameters
  IC_salmonella=8.9*10^6,
  #Norovirus parameters
  mass_feces_hands=rbetagen(1,4.57,2.55,-8.00,-1.00), #log(g/hands) #add -3.5, #
  HU_NV_in_Feces=rlnormTrunc(1,6.65,2.06,0.0,10.98), #log HuNov CG/ g #add 6.15,#
  Genomic_copies_per_PFU=rnormTrunc(1,3.65,.98,2.00,5.40) #add 3.66#
)



# Calculation of Items Touches Service Line -------------------------------

  #Touched items
  ntouched_Fr<-3#round(rnorm(1,3,.5),0) #add
  ntouched_Pss<-3#round(rnorm(1,3,.5),0) #add
  ntouched_Pre<-3#round(rnorm(1,3,.5),0) #add
