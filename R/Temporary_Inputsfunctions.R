Inputs_ICont_Student<-list(
  #salmonella parameters
  IC_salmonella=8.9*10^6,
  #Norovirus parameters
  mass_feces_hands=rbetagen(1,4.57,2.55,-8.00,-1.00), #log(g/hands)
  HU_NV_in_Feces=rlnormTrunc(1,6.65,2.06,0.0,10.98), #log HuNov CG/ g
  Genomic_copies_per_PFU=rnormTrunc(1,3.65,.98,2.00,5.40)
)

Func_ICont_Student<-function(IC_salmonella,mass_feces_hands,HU_NV_in_Feces,Genomic_copies_per_PFU,... ){
  #Salmonella
  if(salmonella ==1){
    IC_Student<-IC_salmonella  #CFU/Hand
    return(IC_Student)
  } 
  if(norovirus ==1){
    Personal_Contamination<-((10^mass_feces_hands) * (10^HU_NV_in_Feces))/(10^Genomic_copies_per_PFU) #PFU/Hand
    IC_Student<- Personal_Contamination #PFU/Hand
    return(IC_Student)
  }
}

do.call(Func_ICont_Student,Inputs_ICont_Student)

Inputs_Cont_HuNov_Fr<-list(
  Prevalence = 0.4,
  Genomic_copies_per_PFU<-rnormTrunc(1,3.65,.98,2.00,5.40),
  HuNoV_ContFruit<-rlnormTrunc(1,2.38,3.52, 0,6.97) #log HuNoV copies per/ g
)

func_Cont_HuNoV_Fr<-function(DF, Prevalence,Genomic_copies_per_PFU,HuNoV_ContFruit){
  for (i in 1:nrow(DF)){
    Fr_Cont_YN<- ifelse(runif(1)<Prevalence,1,0)
    Contamination<-(10^HuNoV_ContFruit)/(10^Genomic_copies_per_PFU) *Fr_Mean_weight #PFU/Apple
    if(Fr_Cont_YN==1){
      DF[i,colnames(DF)== "Contamination"]<-Contamination
    } else if (Fr_Cont_YN==0){
      DF[i,colnames(DF)== "Contamination"]<-as.numeric(0)
    }
  }
  return(DF)
}
do.call(func_Cont_HuNoV_Fr,c(list(DF=Fr_Data.Frame),Inputs_Cont_HuNov_Fr))



