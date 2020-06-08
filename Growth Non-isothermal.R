#Growth Model

#C Perfringends

GrowthData<-data.frame(
            "Temperature" = c(27 , 30 , 32.5 , 35 , 37.5 , 40 , 42.5 , 45),
            "at" = c(5.6 , 5.6 , 6.0 , 5.9 , 6.0 , 5.9 , 6.0 , 6.0),
            "K" = c(0.34,0.52,0.63,0.74,0.98,1.09,1.28,1.45),
            "Tc"= c(14.4,10.1,7.9,6.5,5.4,4.4,3.8,3.3)
        
            
            
)

#Inputs
Time_In<-0
Time_Fi<-100
a<-5.86
Kp1<-0.017
Kp2<-1.25
Tc1<-159.2
Tc2<-0.017



#Temperature and Time intervals
Temp<-13
Time<-c(Time_In:Time_Fi)

#Creating Main Data Frame
Growth<-data.frame(Time, Temp, "Y" = "", stringsAsFactors=FALSE)

KT<-.017*exp(.125*Temp)
Tc<-159*exp(-.17*Temp)

Y_In<-.001
Y_current<-Y_In
Y_Prev<-Y_In



for(i in 1:nrow(Growth)){
  if (i == 1 ){
    Growth$Y[1] = Y_In
    Y_current<-as.numeric(Growth$Y[i])
  }else {
    Y_Prev<-as.numeric(Growth$Y[i-1])
    tmark2<-(log(
      (exp(KT*Tc)*(a+(Y_current+Y_Prev/2)*(1+exp(KT*Tc))))/ 
        (a*exp(KT*Tc)-(Y_current+Y_Prev/2)*(1+exp(KT-Tc)))  
    ))/KT
    
    Y_current<-(KT*a*exp(KT*(Tc-tmark2)))/((1+exp(KT*(Tc-tmark2)))^2)
    Growth$Y[i] = Y_current
  }
}


plot(Growth$Time,Growth$Y)


for(i in 1:nrow(Growth)){
  if (i == 1 ){
    Growth$Y[1] = Y_In
    Y_current<-as.numeric(Growth$Y[i])
  }else {
  Y_Prev<-as.numeric(Growth$Y[i-1])
  Y_current<-(((KT)*(a+((Y_current+Y_Prev)/2)+exp((KT)*(Tc-tmark))*((Y_current+Y_Prev)/2))*((a*exp((KT)*(Tc))-(1+exp((KT)*(Tc)))*((Y_current+Y_Prev)/2))))/(a*(1+exp((KT)*(Tc-tmark)))^2))
  Growth$Y[i] = Y_current
  }
}

  


                   
                          

tmark<-(1/KT)*log((exp(KT*Tc)*(a+(Y_current)*(1+exp(KT*Tc)))))/(a*exp(KT*Tc)-Y_current*(1+exp(KT*Tc)))
  

tmark2<-(log(
      (exp(KT*Tc)*(a+(Y_current+Y_Prev/2)*(1+exp(KT*Tc))))/ 
      (a*exp(KT*Tc)-(Y_current+Y_Prev/2)*(1+exp(KT-Tc)))  
      ))/KT

Y_current<-(KT*a*exp(KT*(Tc-tmark2)))/((1+exp(KT*(Tc-t)))^2)
