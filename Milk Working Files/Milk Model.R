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



################New Model###########

Data_Frame_Milk<-data.frame(
  Iteration = 1:500,
  Shelflife =as.numeric(0),
  stringsAsFactors = FALSE
)


Initial_pop<-rlnorm3(500,1,0.9)
hist(Initial_pop)

for (i in 1:nrow(Data_Frame_Milk)){
  Temp<-rnorm(1,6.5,2)
  Initial_pop<-rlnorm(1,1,0.9)
  Spoilage<-7
  Growth_spoilage<-Spoilage-Initial_pop
  Tempmin<-(-1.19)
  b<-.03578
  k<-((Temp-Tempmin)*b)^2
  Variability<-(1-(rnorm(1,0,17)/100))
  New_K<- k *Variability
  Conversion<-2^New_K
  New_K2<-log10(Conversion)
  Shelf_Life<-Growth_spoilage/New_K2
  Shelf_Life_days<-Shelf_Life/24
  Data_Frame_Milk[i,colnames(Data_Frame_Milk)== "Shelflife"]<-Shelf_Life_days
  
}

hist(Data_Frame_Milk$Shelflife, breaks = 100)
?hist

rlnorm3(500,1,0.9)

  
  Func_Growth_Milk_Spoilage<-function(Condition,DF,TimeVar){
    if(Condition== "room temp"){
      Temp<-25
      Initial_pop<-rlnorm(1,1,0.9)
      Tempmin<-(-1.19)
      b<-.03578
      k<-((Temp-Tempmin)*b)^2
      Variability<-(1-(rnorm(1,0,17)/100))
      New_K<- k *Variability
      Conversion<-2^New_K
      New_K2<-log10(Conversion)
      for (i in 1:nrow(DF)){
        Growth<-TimeVar*New_K2
        N<-DF[i,colnames(DF)== "SpoilageCon"]
        Con_Final<-N + Growth
        DF[i,colnames(DF)== "SpoilageCon"]<-as.numeric(Con_Final)
      }
      return(DF)
    } else if (Condition == "refrigerated"){
      Temp<-25
      Initial_pop<-rlnorm(1,1,0.9)
      Tempmin<-(-1.19)
      b<-.03578
      k<-((Temp-Tempmin)*b)^2
      Variability<-(1-(rnorm(1,0,17)/100))
      New_K<- k *Variability
      Conversion<-2^New_K
      New_K2<-log10(Conversion)
      for (i in 1:nrow(DF)){
        Growth<-TimeVar*New_K2
        N<-DF[i,colnames(DF)== "SpoilageCon"]
        Con_Final<-N + Growth
        DF[i,colnames(DF)== "SpoilageCon"]<-as.numeric(Con_Final)
      }
      return(DF)
    }
  } 

  
  ###more
  
  
  
  results<-c()
  prevalence<-.14
  
  for (i in 1:100000){
    Prevalence_YN<-ifelse(runif(1)<prevalence,1,0)
    if(Prevalence_YN ==1){
      N0<-rnorm(1,-.72,0.99)
    } else if (Prevalence_YN == 0 ){
      N0<-0
    }
    lag <-3.1 #days
    k<-0.4 #log CFU/ml/ day
    time<-21
    GrowthTime<-time-lag
    Growth<-GrowthTime*k
    Concentration<-N0+Growth
    results<-c(results, Concentration)
  }
  hist(results, breaks = 50)
  ?hist
  
  
  
  
  
  #For APC @ 13°C
  Theta1<-(.67)
  Theta2<- (.03)
  Theta3<-(-2.67)
  
  
  
  (Theta1*exp(-Theta2*time))+Theta3
  
  df1<-data.frame(
    Time = 1:900,
    Variable = as.numeric(0)
  )
  
  for( i in 1:nrow(df1)){
    Time<-i
    Variable<-(Theta1*exp(-Theta2*Time))+Theta3
    df1[i,colnames(df1)== "Variable"]<-as.numeric(Variable)
  }
  
  plot(df1$Variable)
