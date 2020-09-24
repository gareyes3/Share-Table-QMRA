Inputs_ICont_Student<-list(
  #salmonella parameters
  IC_salmonella=8.9*10^6,
  #Norovirus parameters
  mass_feces_hands=rbetagen(1,4.57,2.55,-8.00,-1.00) #log(g/hands)
  HU_NV_in_Feces=rlnormTrunc(1,6.65,2.06,0.0,10.98) #log HuNov CG/ g
  Genomic_copies_per_PFU<-rnormTrunc(1,3.65,.98,2.00,5.40)
)

Func_ICont_Student<-function(){
  #Salmonella
  if(salmonella ==1){
    IC_Student  #CFU/Hand
    return(IC_Student)
  }
  #Norovirus
  if(norovirus ==1){
    mass_feces_hands<- rbetagen(1,4.57,2.55,-8.00,-1.00) #log(g/hands)
    HU_NV_in_Feces<- rlnormTrunc(1,6.65,2.06,0.0,10.98) #log HuNov CG/ g
    Genomic_copies_per_PFU<-rnormTrunc(1,3.65,.98,2.00,5.40)
    Personal_Contamination<-((10^mass_feces_hands) * (10^HU_NV_in_Feces))/(10^Genomic_copies_per_PFU) #PFU/Hand
    IC_Student<- Personal_Contamination #PFU/Hand
    return(IC_Student)
  }
}