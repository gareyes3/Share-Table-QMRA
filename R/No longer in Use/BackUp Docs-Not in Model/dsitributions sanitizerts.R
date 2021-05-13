sd <- sqrt(56)*(0.1-0.07)/4.008
qt(.975,55)

sd <- (sqrt(19)*(7.54 - 6.36))/ 2.660782

qt(0.05,55)
?qt

xx<-rnormTrunc(10000,0.08, 0.0057,min = 0)
xx<-rpert(1,0.06,0.07,0.09)

xx<-rnorm(10000,0.08, 0.057)
quantile(xx,0.025)
quantile(xx,0.975)
median(xx)
hist(xx)

#Weekly Random Input

Wash_Method<-as.numeric(sample(1:3, 1))
Wash_Method_Tracker<-c(Wash_Method_Tracker,Wash_Method)


#Hand Sanitizer Method


Sanitizer_Method<-as.numeric(sample(1:6, 1))
Saniztizer_Method_Tracker<-c(Saniztizer_Method_Tracker,Sanitizer_Method)


LogRed_V<-c()

for ( i in 1:10000){
  Wash_Method<-as.numeric(sample(1:3, 1))
  Wash_Method_Tracker<-c(Wash_Method_Tracker,Wash_Method)
  
  Logred<-Func_Randomize_Wash(Wash_Method = Wash_Method)
  LogRed_V<-c(LogRed_V,Logred)
}

hist(LogRed_V)
quantile(LogRed_V,0.025)
quantile(LogRed_V,0.975)
median(LogRed_V)


xx<-rnorm(10000,0.667,0.33)
hist(xx)


Cont<-1000
Cont*10^0.1

Func_Randomize_Wash(Wash_Method = 2)


Func_Randomize_Wash<-function(Wash_Method){
  if (Wash_Method==1){
    Logred<-runif(1,0.26,1.06)
  } else if (Wash_Method==2){
    Logred<-rnorm(1,0.667,0.33)
  } else if (Wash_Method==3){
    Logred<-runif(1,1,2)
  }
  return(Logred)
}

#Function that randomizes hand sanitizer reduction

LogRed_V<-c()
for ( i in 1:10000){
  Sanitizer_Method<-as.numeric(sample(1:6, 1))
  Saniztizer_Method_Tracker<-c(Saniztizer_Method_Tracker,Sanitizer_Method)
  
  Logred<-Func_Randomize_Sanitizer(Wash_Method = Sanitizer_Method)
  LogRed_V<-c(LogRed_V,Logred)
}

hist(LogRed_V)
quantile(LogRed_V,0.025)
quantile(LogRed_V,0.975)
median(LogRed_V)


Func_Randomize_Sanitizer<-function(Wash_Method){
  if (Wash_Method==1){
    #Wilson, non residual hand sanitizer 30s. 
    Logred<-rnormTrunc(n = 1,mean = 1.06,sd = 0.54,min = 0.15,max = 1.89)
    #Ecudero Abarca 30s high soil load, conservative. 
  } else if (Wash_Method==2){
    Logred<-rnorm(1,2.2,0.07)
    #VF447 70% ethanol Manciaga in vivo 30s
  } else if (Wash_Method==3){
    Logred<-rnorm(1,2.48,0.45)
    #Kampf sterillum virguard 95% ethanol
  } else if (Wash_Method==4){
    Logred<-rnorm(1,2.17,1.065)
    #Liu hand sanitizers in general 62% alcoh
  }else if (Wash_Method==5){
    Logred<-runif(1,0.14,0.34)
    #LAgues 62% ethanol purell
  }else if (Wash_Method==6){
    Logred<-0.5
  }
  return(Logred)
}








Hams<-c(0.146, 0.162, 0.145, 0.149, 0.172, 0.189, 0.147)

boxplot(Hams)
