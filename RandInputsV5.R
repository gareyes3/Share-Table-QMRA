#Transfers Efficiencies - If Random we will move this to the loop-

if(norovirus==1){
TE_H_F<- rbetagen(1,0.76,1.04,.0126,.46)
TE_H_S<-.1340 #TE between Hands and Surfaces
TE_F_H<-rbetagen(1,0.88,1.01,.048,.164)
TE_F_S<-.0250 #TE between Food and Surfaces
TE_S_H<-.1090 #TE between Surfaces and Hands
TE_S_F<-.4620 #TE between Surfaces and Foods
  }

if(salmonella ==1){
  TE_H_F<-.0021
  TE_F_H<-.0328
}

#Contamination Level if Student is contaminated: 
IC_Student<-F_norm(1,Av_ic,Sd_ic)

#Touched items
ntouched_Fr<-round(rnorm(1,3,.5),0)
ntouched_Pss<-round(rnorm(1,3,.5),0)
ntouched_Pre<-round(rnorm(1,3,.5),0)

