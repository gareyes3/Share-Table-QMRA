#Growth Model
  setwd("G:/Share Table QMRA/Share-Table-QMRA")
    
Growthcvecoli<-read.csv("EcoliGrowth.csv")
  
install.packages("nlsMicrobio")
library("nlsMicrobio")

data(growthcurve1)
nls3 <- nls(buchanan, growthcurve1,
            list(lag = 1.36, mumax = 6.417, LOG10N0 = 1, LOG10Nmax = 20))

class(nls3)

plotfit(nls3, smooth = TRUE)
#Variable needed. 



growthcurve2<-read.csv("EcoliGrowth.csv")


#Growth Simulation
CurrentCon<-2500
Temp<-25
Time<-1.3
b<-.023 #growth model parameter #slope
Tmin<-1.2 #min temp
K<-.35 #die off rate
Gr<-(b*(Temp-Tmin))^2/2.303 #growth rate

ConInc<-(Gr*Time)-(.35*Time)
NewCont<-log(CurrentCon)+ConInc
exp(NewCont)


#New Try
setwd("C:/Users/reyes/Documents/GitHub/Share-Table-QMRA")
source("FunctionsV6.r")
library(dplyr)
#Inputs: 

RelativeLag<-0 #we have to reset this after every iteration. 

InitialCon<- 2.3 #CFU/g


Time_ST<-6
Time_StorageST<-.2
Time_Selection<-2.4
Time_Consumption<-.5

Temp_ST<-25
Temp_StorageST<-4
Temp_Selection<-25
Temp_Consumption<-25

Growth_Data<-read.csv("Growth Table.csv") 

#Selection Table

GrowthModel<-Func_seach_Data4(Growth_Data,Growth_Data$Temperature,Temp_Selection,1)

Growth_rate<-GrowthModel$Growth.Rate
LagLength<-GrowthModel$Lag.Phase
Nmax<-GrowthModel$Nmax

RelativeLag<-Time_Selection/LagLength + RelativeLag

if(RelativeLag>1){
 InitialCon<-(Time_Selection-LagLength)*Growth_rate+InitialCon
  }

#Share Table

GrowthModel<-Func_seach_Data4(Growth_Data,Growth_Data$Temperature,Temp_ST,1)

Growth_rate<-GrowthModel$Growth.Rate
LagLength<-GrowthModel$Lag.Phase
Nmax<-GrowthModel$Nmax

RelativeLag<-Time_ST/LagLength + RelativeLag

if(RelativeLag>1){
  InitialCon<-((Time_ST-LagLength)*Growth_rate)+InitialCon 
}
if(InitialCon>=Nmax){
  InitialCon = Nmax
}

