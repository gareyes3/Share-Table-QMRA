#Contants

Res_Trans<-1.97

#Functions
inv.logit<-function(x){
  exp(x)/(1+exp(x))
}

#For Initial Contamination Student
NovSH<-rpert(1,4,8,10,shape = 4)
Mh<-rpert(1,-8,-3,-1,shape = 4)

#For Initial Contamination Food.

Cont<-(10^NovSH)*(10^Mh)

rbinom(1,100,)



TrP_H_Fr<-inv.logit(rnorm(1,-2.95,Res_Trans))
Cont<-round(Cont,0)
Tr_H_Fr<-rbinom(1,Cont,TrP_H_Fr)
Cont_Fr_Updated<-Func_Index_DF(Fr_Data.Frame,Fr_Picked,"Contamination")+ Tr_H_Fr - (Tr_Fr_H)

Func_Cross_Contamination_Fr<-function(Cont_Student,Fr_Data.Frame, Fr_Picked){
  Cont_Fr<-Func_Index_DF(Fr_Data.Frame,Fr_Picked,"Contamination")
  Tr_H_Fr<-rbinom(n=1,size = Cont_Student, prob = TrP_H_Fr)
  Tr_Fr_H<-rbinom(n=1,size = Cont_Fr,prob = TrP_Fr_H)
  #Overall Transfer for tracking
  Overall_Tr<-(Tr_H_Fr-Tr_Fr_H) #GEC tranfered
  Fr_Data.Frame[Fr_Picked,colnames(Fr_Data.Frame)== "TouchesContHist"]<-paste(Fr_Data.Frame[Fr_Picked,colnames(Fr_Data.Frame)=="TouchesContHist"], as.numeric(Overall_Tr),sep = ",") #Adding Contamination to
  #Continuing Contamination: 
  Cont_Fr_Updated<- Cont_Fr + Overall_Tr #New Contamination of Fruit
  Cont_Fr_Difference<-Cont_Fr-(Cont_Fr_Updated) #Difference in contamination to update student contamination
  Fr_Data.Frame[Fr_Picked,colnames(Fr_Data.Frame)== "Contamination"]<-Cont_Fr_Updated #update the Fr Contamination in Data frame
  Cont_Student<-ifelse(Cont_Student +(Cont_Fr_Difference)<0,0,Cont_Student +(Cont_Fr_Difference)) #Updating Contamination in Student's hands
  Fr_Data.Frame<<-Fr_Data.Frame
  Cont_Student<<-Cont_Student
}




Func_Cross_Contamination_Fr<-function(Cont_Student,Fr_Data.Frame, Fr_Picked){
  #update the Fr Contamination in Data frame
  Tr_H_Fr<-Cont_Student*TE_H_F #Transfer from Hand to Fruit
  Tr_Fr_H<-(Func_Index_DF(Fr_Data.Frame,Fr_Picked,"Contamination")* TE_F_H) #Tranfer from fruit to hand
  #Contamination Tranfered History. 
  Overall_Tr<-(Tr_H_Fr-Tr_Fr_H)
  Fr_Data.Frame[Fr_Picked,colnames(Fr_Data.Frame)== "TouchesContHist"]<-paste(Fr_Data.Frame[Fr_Picked,colnames(Fr_Data.Frame)=="TouchesContHist"], as.numeric(Overall_Tr),sep = ",") #Adding Contamination to
  Cont_Fr_Updated<- Func_Index_DF(Fr_Data.Frame,Fr_Picked,"Contamination") + Tr_H_Fr - (Tr_Fr_H) #New Contamination of Fruit
  Cont_Fr_Difference<-Func_Index_DF(Fr_Data.Frame,Fr_Picked,"Contamination")-(Cont_Fr_Updated) #Difference in contamination to update student contamination
  Fr_Data.Frame[Fr_Picked,colnames(Fr_Data.Frame)== "Contamination"]<-Cont_Fr_Updated #update the Fr Contamination in Data frame
  Cont_Student<-ifelse(Cont_Student +(Cont_Fr_Difference)<0,0,Cont_Student +(Cont_Fr_Difference)) #Updating Contamination in Student's hands
  #Adding Variables to Global Environment
  Fr_Data.Frame<<-Fr_Data.Frame
  Cont_Student<<-Cont_Student
}




alpha<-.04
betar<-.055
hunov<-100

1-((gamma((alpha+beta))*gamma(beta+hunov))/(gamma(hunov)*gamma(alpha+beta+hunov)))

beta

1-beta(alpha,(betar+hunov))/beta(alpha,betar)

nw<-0.086
r<-2.55E-3

1-(1+nw*hunov)^(-r)
