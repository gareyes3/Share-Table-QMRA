#Draft for milk spoilage

df1<-data.frame(
  "Time" = 1:900,
  "Variable" = "",
  "Dif" = "",
  stringsAsFactors = FALSE
)

#lipase @ 5°C

Theta1<-(.053)  #scalar
Theta2<-(-.001)  #growth rate
Theta3<- (.218)  #asymptote
Time<-300

X<- (Theta1*exp(-Theta2*Time))+Theta3
X<- Theta3+(Theta1*(exp(Theta2*Time)))

(exp(-Theta2*Time))

10^X

for( i in 1:nrow(df1)){
  Time<-i
  Variable<-Theta1*exp((Theta2)*Time)+Theta3
  df1[i,colnames(df1)== "Variable"]<-as.numeric(Variable)
}

plot(df1$Time, df1$Variable)

exp((Theta2)*Time)


#From Combase


Func_Growth_Milk_Spoilage<-function(Condition,DF,TimeVar){
  if(Condition== "room temp"){
    k<-.367
    for (i in 1:nrow(DF)){
      Growth<-TimeVar*k
      N<-log10(DF[i,colnames(DF)== "SpoilageCon"])
      Con_Final<-N + Growth
      DF[i,colnames(DF)== "SpoilageCon"]<-Con_Final
    }
    return(DF)
  } else if (Condition == "refrigerated"){
    k<-.0334
    for (i in 1:nrow(DF)){
      Growth<-TimeVar*k
      N<-log10(DF[i,colnames(DF)== "SpoilageCon"])
      Con_Final<-N + Growth
      DF[i,colnames(DF)== "SpoilageCon"]<-Con_Final
    }
    return(DF)
  }
} 