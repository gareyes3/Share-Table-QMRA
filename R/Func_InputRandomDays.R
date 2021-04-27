Func_Randomize_Wash<-function(Wash_Method){
  if (Wash_Method==1){
    Logred<-runif(1,0.26,1.06)
  } else if (Wash_Method==2){
    Logred<-rnorm(1,0.667,0.33)
  } else if (Wash_Method==3){
    Logred<-rnorm(1,1,2)
  }
  return(LogRed)
}



