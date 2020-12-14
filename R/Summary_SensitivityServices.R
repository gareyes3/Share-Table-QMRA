#-----------------------------------------------------------------------------------------------------------------------


# Analysis of Items in Services -------------------------------------------

#FUNCTIONS

#Functions to extract consumed items from list and mean
Cont_per_Service_Func<-function(vector){
  vector<-vector[which(vector$Location == "Consumed"),]
  mean(vector$Contamination)
}
#function to Extract median out of the infromation
Cont_per_Service_Med_Func<-function(vector){
  vector<-vector[which(vector$Location == "Consumed"),]
  median(vector$Contamination)
}

# Function Determination of mean touches per service  
Touches_per_Service_Med_Func<-function(vector){
  vector<-lengths(regmatches(vector$History, gregexpr("Touched", vector$History)))
  mean(vector)
}

#DATA GENERATION

#Average number of touches per item per service
#Touches per service
Touches_per_service<-sapply(X=List_Sens_Fr, FUN = Touches_per_Service_Med_Func)

#Median Contamination of items per service
#Fr
Median_Con_Services_Fr<-sapply(X=List_Sens_Fr,FUN=Cont_per_Service_Med_Func)
#Mean Contamination of items per service
#Fruit
Mean_Con_Services_Fr<-sapply(X=List_Sens_Fr,FUN=Cont_per_Service_Func)
#Pss
Mean_Con_Services_Pss<-sapply(X=List_Sens_Pss, FUN = Cont_per_Service_Func)
#Pre
Mean_Con_Services_Pre<-sapply(X=List_Sens_Pre, FUN= Cont_per_Service_Func)

#Prevalence of Contaminated Students per Service. To know Prevalence rathen than #. 
Vector_Prevalence_Cont_Stu<-c()#Vector for prevalence of contaminated students every service. 
for ( i in 1:length(Vector_No_Cont_Stu)){
  Vector_Prevalence_Cont_Stu<-c(Vector_Prevalence_Cont_Stu,Vector_No_Cont_Stu[i]/(Students_p_grade))
}

#Creation of Data Frames: 

#Fruit
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

#Pss
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

#Pre
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

#Making NA Values from Data Frames = 0 
Input_DataFrame_Services_Fr[is.na(Input_DataFrame_Services_Fr)]<-0
Input_DataFrame_Services_Pss[is.na(Input_DataFrame_Services_Pss)]<-0
Input_DataFrame_Services_Pre[is.na(Input_DataFrame_Services_Pre)]<-0


#PRCC and Visuals: 

#PRCC all columns vs outputs, 95% confidence itervval 10k bootstrap replicates
Pcc1<-pcc(X=Input_DataFrame_Services_Fr[,2:13], y=Input_DataFrame_Services_Fr$OutputContsFr,conf = 0.95,nboot = 10000,rank = TRUE)
Pcc1

#Visuals: 
#Normal Plot
plot(Pcc1)

#Change Names and Plot Tornado Plot
names(Pcc1$PRCC)=c("original", "bias" ,"std.error", "minci","maxci")

#Tornado Plot #OBS: Not a lot of confidence, lots of variability. 
ggplot(data = Pcc1$PRCC, aes(x=rownames(Pcc1$PRCC),y=original ))+
  geom_bar(stat = "identity", position = "identity")+
  geom_errorbar(aes(ymin=minci, ymax=maxci), width=.1,col="blue")+
  coord_flip() 

#Code to Compare Contaminations
ggplot(data = Input_DataFrame_Services_Fr , aes(x=OutputContsFr, y = Initial_Fr_Cont )) + 
  scale_x_log10()+
  scale_y_log10()+
  geom_point(aes(col=Mean_Touches_per_service))+
  geom_smooth()+
  scale_color_gradient(low="green", high="red")

#Melted Data for Facet_Wrap
d <- melt(Input_DataFrame_Services_Fr, id.vars="OutputContsFr")
#Facet Wrapp
ggplot(data =d , aes(OutputContsFr,value, col=variable)) + 
  scale_x_log10()+
  scale_y_log10(c("ContaminationFr","ContaminationStu"))+
  geom_point() + 
  stat_smooth() +
  facet_wrap(~variable, scales = "free")+
  xlab("Contamination Items Consumed Log CFU/Fruit")+
  ylab("Values")



#------------------------------------------------------------------------------------------------------------------   










#EXTRAS SERVICE ANALYSIS, MIGHT WORK LATER. 

#Here to identify outliers
outliers<-boxplot(Input_DataFrame_Services_Fr$OutputContsFr)$out
print(outliers)

Input_DataFrame_Services_Fr<-Input_DataFrame_Services_Fr[-which(Input_DataFrame_Services_Fr$OutputContsFr %in% outliers),]

boxplot(Input_DataFrame_Services_Fr$OutputContsFr)

#Here to categorize data frames based on level of contamination of student. #s arbitrary. 
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
