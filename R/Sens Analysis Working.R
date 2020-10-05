Test<-function(vector){
  vector<-vector[which(vector$Location == "Consumed"),]
  mean(vector$Contamination)
}


List_Sens_Fr
Mean_Con_Services_Fr<-sapply(X=List_Sens_Fr,FUN=Test)


Input_DataFrame_Services<-data.frame(
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

Input_DataFrame_Services[is.na(Input_DataFrame_Services)]<-0

pcc(X=Input_DataFrame_Services[,2:12], y=Input_DataFrame_Services$OutputContsFr)



#Summary of Iterations for Sens Analysis


#Construction of a sensitive analysis

 #


mean(Mean_Con_Services_Fr)
median(Mean_Con_Services_Fr)
var(Mean_Con_Services_Fr)

#Data Analysis Data frame

chunk <- 65
n <- length(Vector_Contaminations)
r  <- rep(1:ceiling(n/chunk),each=chunk)[1:n]
d <- split(Vector_Contaminations,r)
Mean_cont_Sertvices2<-sapply(d, Test2)

Data_Analysis<-data.frame("Contaminations"= rep(Vector_Contaminations,each=25),
                          "TE_H_F"= Vector_TE_H_F,
                          "Outputs" =Mean_Con_Services_Fr)

pcc(X=Data_Analysis[,1:2], y=Data_Analysis$Outputs)


Input<-data.frame(
  "Input"=rep(1:10,each=25)
)


quantile(Fr_Data.Frame$Contamination, probs = c(.25,.5,.75))

plot(Data_Analysis$Input, Data_Analysis$Outputs)
