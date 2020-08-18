Surface_hands<- 50.675  #cm^2
mass_feces_hands<- rbetagen(1,4.57,2.55,-8.00,-1.00) #log(g/hands)
HU_NV_in_Feces<- rlnormTrunc(1,6.65,2.06,0.0,10.98) #log HuNov CG/ g
Hu_NV_restroom<- runif(1,0,2) #log HUNOV/cm2
TE_RH<- rtriang(1,.036,.07,.22) 
Genomic_copies_per_PFU<-rnormTrunc(1,3.65,.98,2.00,5.40)


Personal_Contamination<-((10^mass_feces_hands) * (10^HU_NV_in_Feces))/(10^Genomic_copies_per_PFU) #PFU/Hand
Environmental_Cont<-(10^Hu_NV_restroom) * (Surface_hands) * TE_RH

NV_from_Restroom<-(Personal_Contamination+Environmental_Cont)

Genomic_copies_per_PFU<-rnormTrunc(1,3.65,.98,2.00,5.40)
ContFruit<-rlnormTrunc(1,2.38,3.52, 0,6.97) #log HuNoV copies per/ g
ContFr<-(10^ContFruit)/(10^Genomic_copies_per_PFU) *Fr_Mean_weight #PFU/Apple




Contamination_Dataframe<-data.frame(
  "Trial"=1:500,
  "Other" = as.numeric(0),
  stringsAsFactors = FALSE
  
)
  
for (i in 1:nrow(Contamination_Dataframe)){
  Surface_hands<- 52  #cm^2
  mass_feces_hands<- rbetagen(1,4.57,2.55,-8.00,-1.00) #log(g/hands)
  HU_NV_in_Feces<- rlnormTrunc(1,6.65,2.06,0.0,10.98) #log HuNov CG/ g
  Hu_NV_restroom<- runif(1,0,2) #log HUNOV/cm2
  TE_RH<- rtriang(1,.036,.07,.22) 
  Genomic_copies_per_PFU<-rnormTrunc(1,3.65,.98,2.00,5.40)
  
  Personal_Contamination<-((10^mass_feces_hands) * (10^HU_NV_in_Feces))/(10^Genomic_copies_per_PFU) #PFU/Hand
  Environmental_Cont<-(10^Hu_NV_restroom) * (Surface_hands) * TE_RH
  NV_from_Restroom<-(Personal_Contamination+Environmental_Cont)
  Contamination_Dataframe[i,colnames(Contamination_Dataframe)=="Other"]<-as.numeric(NV_from_Restroom)
}
as.numeric(Contamination_Dataframe$Other)



hist(Contamination_Dataframe$Other, breaks = 20000)
boxplot(Contamination_Dataframe$Other)
