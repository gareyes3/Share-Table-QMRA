

library(dplyr)
OD<-Fr_Data_Days%>%
  group_by(ID)%>%
  filter(TotServices==max(TotServices))

OD<-Fr_Data_Days %>% 
  group_by(ID) %>% 
  filter(n()>1)

#Start from here
OD<-bind_rows(List_Sens_Fr)

#find the dupplicated
OD1<-OD %>% 
  group_by(ID) %>% 
  filter(n()>1)

#remove duplicated by leaving the one that was in the system the longest. 
OD2<-OD%>%
  group_by(ID)%>%
  filter(TotServices==max(TotServices))
#Consumed
OD2<-OD%>%
  group_by(ID)%>%
  filter(TotServices==max(TotServices))%>%
  filter(Location=="Consumed")

Vector12<-c()
for (i in 1:nrow(OD2)){
  if (OD2[i,colnames(OD2)=="STtimes"]>0){
    Vector12<-c(Vector12,1)
  }else if(OD2[i,colnames(OD2)=="STtimes"]==0){
    Vector12<-c(Vector12,0)
  }
}

for (i in 1:nrow(OD2)){
  a<-OD2[i,colnames(OD2)=="ContaminationIn"]
  if (a<10){
    OD2[i,colnames(OD2)=="RatingContFr"]<-"Low"
  } else if (a>10 && a<1000){
    OD2[i,colnames(OD2)=="RatingContFr"]<-"Med"
  }else if (a>1000 && a<10000){
    OD2[i,colnames(OD2)=="RatingContFr"]<-"MedHigh"
  }else if (a>10000){
    OD2[i,colnames(OD2)=="RatingContFr"]<-"High"
  }
}


Touches_Number<-lengths(regmatches(OD2$History, gregexpr("Touched", OD2$History)))

Analysis_Consumed<-data.frame(
  "Contamination" = OD2$Contamination,
  "DeltaCont"= (OD2$Contamination-OD2$InContamination),
  "InContamination"=OD2$InContamination,
  "TouchesNo"=Touches_Number,
  "STTimes"= OD2$STtimes,
  "TotServices"=OD2$TotServices,
  "Shared"=Vector12
)

Analysis_Prcc<-data.frame(
  "DeltaCont"= (OD2$Contamination-OD2$InContamination),
  "InContamination"=OD2$InContamination,
  "TouchesNo"=Touches_Number,
  "STTimes"= OD2$STtimes,
  "TotServices"=OD2$TotServices,
  "Shared"=Vector12,
  "Contamination" = OD2$Contamination
)

Analysis_Prcc<-Func_Convert_Log(Analysis_Prcc,Column = "InContamination")
Analysis_Prcc<-Func_Convert_Log(Analysis_Prcc,Column = "Contamination")


d <- melt(Analysis_Consumed, id.vars="Contamination")


Pcc2<-pcc(X=Analysis_Prcc[,2:6], y=Analysis_Prcc$Contamination,rank = TRUE,nboot = 1000)


ggplot(data = Analysis_Prcc, aes(x=Contamination, y = InContamination )) + 
  geom_point(aes(col=TouchesNo))

ggplot(data = Analysis_Prcc, aes(x=Contamination, y = TouchesNo )) + 
  geom_point()+
  geom_smooth()

ggplot(data =d , aes(Contamination,value, col=variable)) + 
  scale_x_log10()+
  scale_y_log10("Contamination")+
  geom_point() + 
  facet_wrap(~variable, scales = "free")+
  xlab("Contamination Items Consumed Log CFU/Fruit")+
  ylab("Values")
