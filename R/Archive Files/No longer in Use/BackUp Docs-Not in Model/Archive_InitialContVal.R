mass_feces_hands=rbetagen(10000,4.57,2.55,-8.00,-1.00) #log(g/hands) #add -3.5, #
hist(mass_feces_hands)
HU_NV_in_Feces=rlnormTrunc(10000,6.65,2.06,0.0,10.98) #log HuNov CG/ g #add 6.15,#
hist(HU_NV_in_Feces)
Genomic_copies_per_PFU=rnormTrunc(10000,3.65,.98,2.00,5.40) #add 3.66#
hist(Genomic_copies_per_PFU)
HU_NV_in_RR=runif(10000,0,2)
hist(HU_NV_in_RR)
Surface_Hands<-runif(10000,89,107)
hist(Surface_Hands)
TE_R_H<-rtriang(10000,.036,.07,.22)
hist(TE_R_H)

HuNoV_ContFruit<-rlnormTrunc(10000,2.38,3.52, 0,6.97)
HuNoV_ContFruit<-rlnorm3(10000,2.38,3.52)
hist(HuNoV_ContFruit,breaks = 100)

Personal_Contamination<-((10^mass_feces_hands) * (10^HU_NV_in_Feces))/(10^Genomic_copies_per_PFU)
RR_Contamination<-(10^HU_NV_in_RR)*Surface_Hands*TE_R_H

Personal_Contamination+RR_Contamination

Final_Conts<-c()
for (i in 1:1000){
  mass_feces_hands=rbetagen(1,4.57,2.55,-8.00,-1.00) #log(g/hands) #add -3.5, #
  HU_NV_in_Feces=rlnormTrunc(1,6.65,2.06,0.0,10.98) #log HuNov CG/ g #add 6.15,#
  Genomic_copies_per_PFU=rnormTrunc(1,3.65,.98,2.00,5.40) #add 3.66#

  
  Personal_Contamination<-((10^mass_feces_hands) * (10^HU_NV_in_Feces))/(10^Genomic_copies_per_PFU)
  
  FinalCont<-Personal_Contamination
  Final_Conts<-c(Final_Conts,FinalCont)
}

Final_Conts<-c()

for (i in 1:1000){
  Hand_Washing<-1.9
  
  mass_feces_hands=rbetagen(1,4.57,2.55,-8.00,-1.00)
  HU_NV_in_Feces=rlnormTrunc(1,6.65,2.06,0,10.98)
  Genomic_copies_per_PFU<-rnormTrunc(1,3.65,.98,2.00,5.40)

  Personal_Contamination<-((10^mass_feces_hands) * (10^HU_NV_in_Feces))/(10^Genomic_copies_per_PFU)
  
  HU_NV_in_RR<-runif(1,0,2)
  Surface_Hands<-runif(1,89,107)
  TE_R_H<-rtriang<-rtriang(1,.036,.07,.22)
  
  RR_Contamination<-(10^HU_NV_in_RR)*Surface_Hands*TE_R_H
  
  (Personal_Contamination+RR_Contamination)/(10^Hand_Washing)
  
  FinalCont<-Personal_Contamination
  Final_Conts<-c(Final_Conts,FinalCont)
}

FinalConts10<-log10(Final_Conts)
plot(FinalConts10)

FinalConts10DF<-data.frame(
  "n"="",
  "FinalConts10"=Final_Conts
)

hist(Final_Conts, breaks = 30)

ggplot(data = FinalConts10DF,aes(x=FinalConts10))+
  geom_density(colour="black", alpha=.2)+
  scale_x_continuous(n.breaks = 10)+
  xlab("Contamination log PFU/Item")+
  ylab("Density")+
  ggtitle("Density Curves Interventions")+ 
  theme(plot.title = element_text(hjust = 0.5))
