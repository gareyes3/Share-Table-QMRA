#Dose Response Model Norovirus-

install.packages("gsl")
library("gsl")
alpha=1
beta=2
cv=-3.24
Pinf<-hyperg_1F1(a = alpha,b = beta+alpha,x = cv )
plot(Pinf)
1-Pinf

P<-0.72
mu<-400
Dose<-1000

P*(1-(exp(-Dose/mu)))
