practice_df<-data.frame(
  "data"= 1:200,
  "cont" = "",
  stringsAsFactors = FALSE
)




func_Cont<-function(DF, Prevalence, area_av , area_sd, logContamination ){
  #Df= Data frame
  #Prevalence = parameter Prevalence of pathogen
  # Contamination = Initial Contamination of the pathogen. 
  # parameter for volume_av
  # parameter for volume_sd
  # parameter for log contamination
    for(i in 1:nrow(DF)){
    Fr_Cont_YN<- ifelse(runif(1)<Prevalence,1,0) 
    Fr_Area<-rnorm(1,area_av,area_sd)
    Contamination<-10^(logContamination)* Fr_Area
    if(Fr_Cont_YN==1){
      DF[i,colnames(DF)== "Contamination"]<-Contamination
    } else if (Fr_Cont_YN==0){
      DF[i,colnames(DF)== "Contamination"]<-0
    }
    }
  return(DF)
}



Fr_Data.Frame<-data.frame("Apple No." = 1:Initial_Fr,
                          "Location"= "Selection Table",
                          "Contamination" = as.numeric("0"),
                          "TotTime"= as.numeric("0"),
                          "History" = "", 
                          "STtimes"= as.numeric("0"),
                          "Initial Service" = "1",
                          "Service" = 2,
                          "Initial Day" = "1",
                          "Day" = 3,
                          stringsAsFactors = FALSE
)

Fr_Data.Frame<-func_Cont_Fr(Fr_Data.Frame,Prevalence_Salmonella,Fr_Mean_area,Fr_sd_area,Fr_Contamination)
