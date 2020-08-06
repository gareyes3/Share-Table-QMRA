Surface_hands<- 98  #cm^2
mass_feces_hands<- -3.50 #log(g/hands)
HU_NV_in_Feces<- 6.65 #log HuNov CG/ g
Hu_NV_restroom<- .8 #log HUNOV/cm2
TE_RH<- 10.9
Genomic_copies_per_PFU<-3.65


((10^mass_feces_hands) * (10^HU_NV_in_Feces))/(10^Genomic_copies_per_PFU)
