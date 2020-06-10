library(deSolve)

N<-3.2
b<-.02
T<-8
Tmin<-(-.6)

rate<-b*(T-Tmin)^2

Growth_Sal<-function(times,y,params){
  DNt.dt<-p[1]*y[1]
  return(list(DNt.dt))
}

p<-c(rate)
t<-seq(0,2, by=.1)

sol<-ode(y=N, times = t , func = Growth_Sal, parms = p)
plot(t,sol[,2])
