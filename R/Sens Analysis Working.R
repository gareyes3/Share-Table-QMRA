Cont_per_Service_Func<-function(vector){
  vector<-vector[which(vector$Location == "Consumed"),]
  mean(vector$Contamination)
}

#Creating Vector of Contaminations for Items in Share Table.
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
  stringsAsFactors = FALSE
)



Input_DataFrame_Services_Fr[is.na(Input_DataFrame_Services)]<-0

pcc(X=Input_DataFrame_Services_Fr[,2:12], y=Input_DataFrame_Services_Fr$OutputContsFr)



