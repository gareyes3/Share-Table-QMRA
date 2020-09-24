
# #Inputs for Norovirus in fruit. Added to dataframe ----------------------

Inputs_Cont_HuNov_Fr<-list(
  Prevalence = 0.4,
  Genomic_copies_per_PFU<-rnormTrunc(1,3.65,.98,2.00,5.40),
  HuNoV_ContFruit<-rlnormTrunc(1,2.38,3.52, 0,6.97) #log HuNoV copies per/ g
)

