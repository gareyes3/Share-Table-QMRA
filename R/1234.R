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


library("gsl")




alpha<-.04
betar<-.055
hunov<-40000

1-hyperg_1F1(a = alpha,b = alpha+betar,x = -hunov)
1-beta(alpha,(betar+hunov))/beta(alpha,betar)




1-((gamma((alpha+beta))*gamma(beta+hunov))/(gamma(hunov)*gamma(alpha+beta+hunov)))

beta

1-beta(alpha,(betar+hunov))/beta(alpha,betar)

nw<-2.55E-3
r<-0.086
hunov<-10E10

1-(1+(nw*hunov))^(-r)

Probill<-1-(1+(nw*hunov))^(-r)
Ill_YN<-ifelse(runif(1)<Probill,1,0)


Func_DR_Infection<-function(DF){
  alpha<-.04
  betar<-.055
  for (i in 1:nrow(DF)){
    hunov<-DF[i,colnames(DF)== "Contamination"]
    Probinf<-(1-beta(alpha,(betar+hunov))/beta(alpha,betar))
    Infected_YN<-ifelse(runif(1)<Probinf,1,0) 
    if(Infected_YN==1){
      DF[i,colnames(DF)== "Infection"]<-TRUE
    }else{
      DF[i,colnames(DF)== "Infection"]<-FALSE
    }
  }
  return(DF)
} 


Func_DR_Illness<-function(DF){
  nw<-0.086
  r<-2.55E-3
  for (i in 1:nrow(DF)){
    hunov<-DF[i,colnames(DF)== "Contamination"]
    if(DF[i,colnames(DF)== "Infection"] == TRUE){
      Probill<-1-(1+nw*hunov)^(-r)
      Ill_YN<-ifelse(runif(1)<Probill,1,0)
      if(Ill_YN==1){
        DF[i,colnames(DF)== "Illness"]<-TRUE
      }else{
        DF[i,colnames(DF)== "Illness"]<-FALSE
      }
    } else{
      DF[i,colnames(DF)== "Illness"]<-FALSE
    }
  }
  return(DF)
} 


Fr_Data_Days<-Func_DR_Infection(Fr_Data_Days)
Fr_Data_Days<-Func_DR_Illness(Fr_Data_Days)
sum(Fr_Data_Days$Infection==TRUE)
sum(Fr_Data_Days$Illness==TRUE)
