#Summary of Iterations for Sens Analysis

#Initial Contamination

Contaminations<-c()

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



mean(AFr_Summary_DF$MedianCont)

#For Tab


Test<-function(vector){
  vector<-vector[which(vector$Location == "Consumed"),]
  mean(vector$Contamination)
}
Listtest<-sapply(X=List_Sens_Fr,FUN=Test)
mean(Listtest)
median(Listtest)
var(Listtest)

Data_Analysis<-data.frame("Contaminations"= rep(Vector_Contaminations,each=25),
                          "TE_H_F"= Vector_TE_H_F,
                          "Outputs" = Listtest)
Data_Analysis_Week<-data.frame("Input" = Vector_Contaminations[2:11],
                               "Input2"= .04,
                               "Output" = AFr_Summary_DF$MedianCont
                               )
Data_Trial<-data.frame("1"= rnorm(100,4,5),
                       "2"= rnorm(100,4,5),
                       "3" = rnorm(100,5,5)
  
)

pcc(X=Data_Trial[,1:2], y=Data_Trial$X3)
?pcc


pcc(X=Data_Analysis, y = with(Data_Analysis,Data_Analysis$Outputs+Data_Analysis$Input),nboot = 250)
pcc(X=Data_Analysis[,1:2], y=Data_Analysis$Outputs)
pcc(X=Data_Analysis_Week[,1:2], y=Data_Analysis_Week$Output)

Input<-data.frame(
  "Input"=rep(1:10,each=25)
)


quantile(Fr_Data.Frame$Contamination, probs = c(.25,.5,.75))

plot(Data_Analysis$Input, Data_Analysis$Outputs)
