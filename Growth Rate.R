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
