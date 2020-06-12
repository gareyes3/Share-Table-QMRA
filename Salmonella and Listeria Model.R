library(deSolve)
library(brms)

install.packages("brms")

salmonella <- 1

#Salmonella

  N<-3.5
  b<-.023
  T<-12
  Tmin<-(1.17)
  Time<-300
  
  rate<-(b*(T-Tmin))^2/2.303
  
  Growth_Sal<-function(times,y,params){
    DNt.dt<-p[1]*y[1]
    return(list(DNt.dt))
  }
  
  p<-c(rate)
  t<-c(0:80)
  
  sol<-ode(y=N, times = t , func = Growth_Sal, parms = p)
  plot(t,sol[,2])



# E Coli  -----------------------------------------------------------

#INPUTS

  N<-3.5
  #Initial Contamination Log CFU/g
  b<-.023
  # Temperature coefficient from McKellar and Delaquis
  k<-rnorm(1,.013,.001)/2.303
  T<-12
  #Temperature in °C
  Tmin<-(1.17)
  #Minimum growth temperature
  rate<-(b*(T-Tmin))^2/2.303
  #Growth rate equation from Ratkowsky, coonverted to LOG CFU-1 h-1
  Time<-300
  #Time in hours

#Growth
  
  Con_Change<-rate*Time 
  #Change in contamination over the time period
  
  Con_Final<-Con_Change + N
  #Final contamination 

#Die Off
  Die_off<-(-k)*Time



#Data frame for representation (Validation of the McKellar and Delaquis FIG 7a:
  
  data2<- data.frame(
    Time = 1:Time,
    Change = "",
    stringsAsFactors = FALSE
  )
  #Creation of data frame
  
  for (i in 1:nrow(data)){
    Con_Change<-rate*i
    data$Change[i]<-Con_Change
  }
  #For Loop to iterate over time
  
  plot(data$Time,data$Change)
  #Simple plot
  
#Data frame for representation (Validation of the McKellar and Delaquis FIG 7b:
  data2<- data.frame(
    Time = 1:Time,
    Change = "",
    stringsAsFactors = FALSE
  )
  #Creation of data frame
  
  for (i in 1:nrow(data)){
    Die_off<-(-k)*i
    data$Change[i]<-Die_off
  }
  #For Loop to iterate over time
  
  plot(data$Time,data$Change)
  #Simple plot

# Salmonella --------------------------------------------------------------
  N<-3.5
  #Initial Contamination Log CFU/g
  b<-.020
  # Temperature coefficient from McKellar and Delaquis
  k<-.0128
  T<-12
  #Temperature in °C
  Tmin<-(-0.571)
  #Minimum growth temperature
  rate<-(b*(T-Tmin))^2/2.303
  #Growth rate equation from Ratkowsky, coonverted to LOG CFU-1 h-1
  Time<-80
  #Time in hours

  #Growth
  
  Con_Change<-rate*Time 
  #Change in contamination over the time period
  
  Con_Final<-Con_Change + N
  #Final contamination 
  
  #Die Off
  Die_off<-(-k)*Time  

# NOROVIRUS ---------------------------------------------------------------

N<-5000
b<-.0854
n<-.5
Time<-700
data_noro<-data.frame(
  Time= 0:Time,
  LogNt.N0= "",
  Nt = "",
  stringsAsFactors = FALSE
)
  
for (i in 0:Time){
    LogNt.N0<-(-b)*(i^n)
    data_noro$LogNt.N0[i]<-LogNt.N0
    nt<-(10^LogNt.N0)*N
    data_noro$Nt[i]<-nt
}


plot(data_noro$Time,data_noro$Nt)

700^.5
26.45751*-26.45

(1/.0854)^(1/.50)

