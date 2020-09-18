#Summary of Iterations for Sens Analysis

#Initial Contamination

Contaminations<-c(0)

for (i in 1:10000){
  mass_feces_hands<- rbetagen(1,4.57,2.55,-8.00,-1.00) #log(g/hands)
  HU_NV_in_Feces<- rlnormTrunc(1,6.65,2.06,0.0,10.98) #log HuNov CG/ g
  Genomic_copies_per_PFU<-rnormTrunc(1,3.65,.98,2.00,5.40)
  Personal_Contamination<-((10^mass_feces_hands) * (10^HU_NV_in_Feces))/(10^Genomic_copies_per_PFU) #PFU/Hand
  IC_Student<- Personal_Contamination #CFU/Hand
  Contaminations<-c(Contaminations,IC_Student)
}
median(Contaminations)
quantile(Contaminations, .975, na.rm = TRUE)
quantile(Contaminations, .025, na.rm = TRUE)




#For Tab

Test<-function(vector,probs){
  mean(vector$Contamination)
}
Listtest<-sapply(X=List_Sens_Fr,FUN=Test)
Data_Analysis<-data.frame("Input"= 45506.72,
                          "Outputs" = Listtest)
pcc(Data_Analysis, y=with(Data_Analysis,Input + Outputs))


install.packages("sensitivity")
library("sensitivity")

quantile(Fr_Data.Frame$Contamination, probs = c(.25,.5,.75))

