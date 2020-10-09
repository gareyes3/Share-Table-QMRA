Cont_per_Service_Func<-function(vector){
  vector<-vector[which(vector$Location == "Consumed"),]
  mean(vector$Contamination)
}

Cont_per_Service_Med_Func<-function(vector){
  vector<-vector[which(vector$Location == "Consumed"),]
  median(vector$Contamination)
}

Median_Con_Services_Fr<-sapply(X=List_Sens_Fr,FUN=Cont_per_Service_Med_Func)

#Creating Vector of Contaminations for Items consumed
Mean_Con_Services_Fr<-sapply(X=List_Sens_Fr,FUN=Cont_per_Service_Func)
Mean_Con_Services_Pss<-sapply(X=List_Sens_Pss, FUN = Cont_per_Service_Func)
Mean_Con_Services_Pre<-sapply(X=List_Sens_Pre, FUN= Cont_per_Service_Func)

#Creation of Data Frame for sensitivity Analysis of Fruit

Input_DataFrame_Services_Fr<-data.frame(
  "Service"= 1:(Service_No*Food_Days*Sens_Iterations),
  "ContaminationStu" = Vector_Con_Services,
  "No_Cont_Students" = Vector_No_Cont_Stu,
  "ContaminationFr"=Vector_Cont_Fr_Serv_Out,
  "No_Cont_Fr"=Vector_No_Cont_Fr,
  "TE_H_F"=Vector_TE_H_F,
  "TE_F_H"=Vector_TE_F_H,
  "TE_H_S"=Vector_TE_H_S,
  "TE_S_H"=Vector_TE_S_H,
  "TE_S_F"=Vector_TE_S_F,
  "TE_F_S"=Vector_TE_F_S,
  "TE_Pre_Mouth"=TE_Pre_Mouth,
  "OutputContsFr"=Mean_Con_Services_Fr,
  "OutputContMedFr"=Median_Con_Services_Fr,
  stringsAsFactors = FALSE
)

Input_DataFrame_Services_Pss<-data.frame(
  "Service"= 1:(Service_No*Food_Days*Sens_Iterations),
  "ContaminationStu" = Vector_Con_Services,
  "No_Cont_Students" = Vector_No_Cont_Stu,
  "ContaminationPss"=Vector_Cont_Pss_Serv_Out,
  "No_Cont_Pss"=Vector_No_Cont_Pss,
  "TE_H_F"=Vector_TE_H_F,
  "TE_F_H"=Vector_TE_F_H,
  "TE_H_S"=Vector_TE_H_S,
  "TE_S_H"=Vector_TE_S_H,
  "TE_S_F"=Vector_TE_S_F,
  "TE_F_S"=Vector_TE_F_S,
  "TE_Pre_Mouth"=TE_Pre_Mouth,
  "OutputContsPss"=Mean_Con_Services_Pss,
  stringsAsFactors = FALSE
)

Input_DataFrame_Services_Pre<-data.frame(
  "Service"= 1:(Service_No*Food_Days*Sens_Iterations),
  "ContaminationStu" = Vector_Con_Services,
  "No_Cont_Students" = Vector_No_Cont_Stu,
  "ContaminationPss"=Vector_Cont_Pre_Serv_Out,
  "No_Cont_Pss"=Vector_No_Cont_Pre,
  "TE_H_F"=Vector_TE_H_F,
  "TE_F_H"=Vector_TE_F_H,
  "TE_H_S"=Vector_TE_H_S,
  "TE_S_H"=Vector_TE_S_H,
  "TE_S_F"=Vector_TE_S_F,
  "TE_F_S"=Vector_TE_F_S,
  "TE_Pre_Mouth"=TE_Pre_Mouth,
  "OutputContsPre"=Mean_Con_Services_Pre,
  stringsAsFactors = FALSE
)



#Making NA Values 0 
Input_DataFrame_Services_Fr[is.na(Input_DataFrame_Services_Fr)]<-0
Input_DataFrame_Services_Pss[is.na(Input_DataFrame_Services_Pss)]<-0
Input_DataFrame_Services_Pss[is.na(Input_DataFrame_Services_Pre)]<-0

outliers<-boxplot(Input_DataFrame_Services_Fr$OutputContsFr)$out
print(outliers)

Input_DataFrame_Services_Fr<-Input_DataFrame_Services_Fr[-which(Input_DataFrame_Services_Fr$OutputContsFr %in% outliers),]

boxplot(Input_DataFrame_Services_Fr$OutputContsFr)

#Running Partical correlation coefficients
pcc(X=Input_DataFrame_Services_Fr[,2:12], y=Input_DataFrame_Services_Fr$OutputContsFr)


pcc(X=Input_DataFrame_Services_Pss[,2:12], y=Input_DataFrame_Services_Pss$OutputContsPss)


pcc(X=Input_DataFrame_Services_Pre[,2:12], y=Input_DataFrame_Services_Pre$OutputContsPre)


d <- melt(Input_DataFrame_Services_Fr, id.vars="OutputContsFr")


ggplot(data =d , aes(OutputContsFr,value, col=variable)) + 
  geom_point() + 
  stat_smooth() +
  facet_wrap(~variable)
