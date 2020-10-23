Cont_per_Service_Func<-function(vector){
  vector<-vector[which(vector$Location == "Consumed"),]
  mean(vector$Contamination)
}

Cont_per_Service_Med_Func<-function(vector){
  vector<-vector[which(vector$Location == "Consumed"),]
  median(vector$Contamination)
}

Touches_per_Service_Med_Func<-function(vector){
  vector<-lengths(regmatches(vector$History, gregexpr("Touched", vector$History)))
  mean(vector)
}

#Average Touches per service
Touches_per_service<-sapply(X=List_Sens_Fr, FUN = Touches_per_Service_Med_Func)

Median_Con_Services_Fr<-sapply(X=List_Sens_Fr,FUN=Cont_per_Service_Med_Func)

#Creating Vector of Contaminations for Items consumed
Mean_Con_Services_Fr<-sapply(X=List_Sens_Fr,FUN=Cont_per_Service_Func)
Mean_Con_Services_Pss<-sapply(X=List_Sens_Pss, FUN = Cont_per_Service_Func)
Mean_Con_Services_Pre<-sapply(X=List_Sens_Pre, FUN= Cont_per_Service_Func)

#Creation of Data Frame for sensitivity Analysis of Fruit

#Creating Prevalence of contaminated Students
Vector_Prevalence_Cont_Stu<-c()#Vector for prevalence of contaminated students every service. 
for ( i in 1:length(Vector_No_Cont_Stu)){
  Vector_Prevalence_Cont_Stu<-c(Vector_Prevalence_Cont_Stu,Vector_No_Cont_Stu[i]/(Students_p_grade))
}

Input_DataFrame_Services_Fr<-data.frame(
  "Service"= 1:(Service_No*Food_Days*Sens_Iterations),
  "Initial_Student_Cont" = Vector_Con_Services,
  "No_Cont_Students" = Vector_No_Cont_Stu,
  "Prevalence_Student_Cont"= Vector_Prevalence_Cont_Stu,
  "Initial_Fr_Cont"=Vector_Cont_Fr_Serv_Out,
  "No_Cont_Fr"=Vector_No_Cont_Fr,
  "Mean_Touches_per_service"=Touches_per_service,
  "TE_H_F"=Vector_TE_H_F,
  "TE_F_H"=Vector_TE_F_H,
  "TE_H_S"=Vector_TE_H_S,
  "TE_S_H"=Vector_TE_S_H,
  "TE_S_F"=Vector_TE_S_F,
  "TE_F_S"=Vector_TE_F_S,
  "TE_Pre_Mouth"=TE_Pre_Mouth,
  "OutputContsFr"=Mean_Con_Services_Fr,
  "OutputContMedFr"=Median_Con_Services_Fr,
  "RatingContFr" = "0",
  stringsAsFactors = FALSE
)

for (i in 1:nrow(Input_DataFrame_Services_Fr)){
  a<-Input_DataFrame_Services_Fr[i,colnames(Input_DataFrame_Services_Fr)=="ContaminationStu"]
  if (a<10){
    Input_DataFrame_Services_Fr[i,colnames(Input_DataFrame_Services_Fr)=="RatingContFr"]<-"Low"
  } else if (a>10 && a<1000){
    Input_DataFrame_Services_Fr[i,colnames(Input_DataFrame_Services_Fr)=="RatingContFr"]<-"Med"
  }else if (a>1000 && a<10000){
    Input_DataFrame_Services_Fr[i,colnames(Input_DataFrame_Services_Fr)=="RatingContFr"]<-"MedHigh"
  }else if (a>10000){
    Input_DataFrame_Services_Fr[i,colnames(Input_DataFrame_Services_Fr)=="RatingContFr"]<-"High"
  }
}

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
  "ContaminationPre"=Vector_Cont_Pre_Serv_Out,
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
Input_DataFrame_Services_Pre[is.na(Input_DataFrame_Services_Pre)]<-0

outliers<-boxplot(Input_DataFrame_Services_Fr$OutputContsFr)$out
print(outliers)

Input_DataFrame_Services_Fr<-Input_DataFrame_Services_Fr[-which(Input_DataFrame_Services_Fr$OutputContsFr %in% outliers),]

boxplot(Input_DataFrame_Services_Fr$OutputContsFr)

#Running Partical correlation coefficients
Pcc1<-pcc(X=Input_DataFrame_Services_Fr[,2:13], y=Input_DataFrame_Services_Fr$OutputContsFr,conf = 0.95,nboot = 10000,rank = TRUE)
#p.adjust(p = Pcc1,method = "bonferroni")
#PccFr$PCC$Bonferroni<-p.adjust(PccFr$PCC$original, method = "bonferroni")
Pcc1
plot(Pcc1)
names(Pcc1$PRCC)=c("original", "bias" ,"std.error", "minci","maxci")
Pcc1$PRCC
ggplot(data = Pcc1$PRCC, aes(x=rownames(Pcc1$PRCC),y=original ))+
  geom_bar(stat = "identity", position = "identity")+
  geom_errorbar(aes(ymin=minci, ymax=maxci), width=.1,col="blue")+
  coord_flip()

pcc(X=Input_DataFrame_Services_Pre[,2:14], y=Input_DataFrame_Services_Pre$OutputContsPre)


d <- melt(Input_DataFrame_Services_Fr, id.vars="OutputContsFr")

ggplot(data = Input_DataFrame_Services_Fr , aes(x=OutputContsFr, y = ContaminationFr )) + 
  scale_x_log10()+
  scale_y_log10()+
  geom_point(aes(col=RatingContFr))+
 geom_smooth()

ggplot(data = Input_DataFrame_Services_Fr , aes(x=OutputContsFr, y = ContaminationStu )) + 
  scale_x_log10()+
  scale_y_log10()+
  geom_point()+
  geom_smooth()

ggplot(data = Input_DataFrame_Services_Fr , aes(x=OutputContsFr, y = TE_F_H )) + 
  geom_point()+
  scale_x_log10()+
  geom_smooth()

ggplot(data = Input_DataFrame_Services_Pss , aes(x=OutputContsPss, y = ContaminationPss )) + 
  geom_point()+
  geom_smooth()

ggplot(data = Input_DataFrame_Services_Pre , aes(x=OutputContsPre, y = ContaminationPre )) + 
  geom_point()+
  geom_smooth()


ggplot(data =d , aes(OutputContsFr,value, col=variable)) + 
  scale_x_log10()+
  scale_y_log10(c("ContaminationFr","ContaminationStu"))+
  geom_point() + 
  stat_smooth() +
  facet_wrap(~variable, scales = "free")+
  xlab("Contamination Items Consumed Log CFU/Fruit")+
  ylab("Values")
