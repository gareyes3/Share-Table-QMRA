###Sens Analysis

#1. Start from here
Individual_Analysis_Fr<-bind_rows(List_Sens_Fr)

#2. find the dupplicated
Individual_Analysis_Fr<-Individual_Analysis_Fr %>% 
  group_by(ID) %>% 
  filter(n()>1)

#3. Create a vector to explain if something were in the share table
STYN_Vector<-c()
for (i in 1:nrow(Individual_Analysis_Fr)){
  if (Individual_Analysis_Fr[i,colnames(Individual_Analysis_Fr)=="STtimes"]>0){
    STYN_Vector<-c(STYN_Vector,1)
  }else if(Individual_Analysis_Fr[i,colnames(Individual_Analysis_Fr)=="STtimes"]==0){
    STYN_Vector<-c(STYN_Vector,0)
  }
}

#4. Number of touches

Touches_Number<-lengths(regmatches(Individual_Analysis_Fr$History, gregexpr("Touched", Individual_Analysis_Fr$History)))

#5. Running Average Contamination of student that touches specific fruit. 

ListTouches<-strsplit(Individual_Analysis_Fr$TouchesContHist,",")
ListTouches<-lapply(ListTouches, function(x) x[x!="NA"])
ListTouches<-lapply(ListTouches, function(x) as.numeric(x))
TouchesContHist<-sapply(X = ListTouches, FUN = mean)
Individual_Analysis_Fr$TouchesContHistAvr<-TouchesContHist

#6. Creation of Dataframe for indvidual Analysis 

Analysis_Individual<-data.frame(
  "DeltaCont"= (Individual_Analysis_Fr$Contamination-Individual_Analysis_Fr$InContamination),
  "InContamination"=Individual_Analysis_Fr$InContamination,
  "TouchesNo"=Touches_Number,
  "TouchesContHistAvr"=Individual_Analysis_Fr$TouchesContHistAvr,
  "STTimes"= Individual_Analysis_Fr$STtimes,
  "TotServices"=Individual_Analysis_Fr$TotServices,
  "Shared"=STYN_Vector,
  "Contamination" = Individual_Analysis_Fr$Contamination
)

#7.  Sensitivity Analysis of table

Pcc2<-pcc(X=Analysis_Individual[,2:7], y=Analysis_Individual$Contamination,rank = TRUE,nboot = 1000)
Pcc2
Pcc2$PRCC

#8 Visuals
names(Pcc2$PRCC)=c("original", "bias" ,"std.error", "minci","maxci")

ggplot(data = Pcc2$PRCC, aes(x=rownames(Pcc2$PRCC),y=original ))+
  geom_bar(stat = "identity", position = "identity")+
  geom_errorbar(aes(ymin=minci, ymax=maxci), width=.1,col="blue")+
  ylab("Partial Correlation Coefficient")+
  xlab("Input")+
  coord_flip()

# More graphs

ggplot(data = Analysis_Individual, aes(x=Contamination, y = TouchesContHistAvr )) + 
  scale_x_log10()+
  scale_y_log10()+
  geom_point(aes(col=Touches_Number))

d <- melt(Analysis_Individual, id.vars="Contamination")
ggplot(data =d , aes(Contamination,value, col=variable)) + 
  scale_x_log10()+
  scale_y_log10(c("Contamination","TouchesContHistAvr"))+
  geom_point() + 
  stat_smooth() +
  facet_wrap(~variable, scales = "free")+
  xlab("Contamination Items Consumed Log CFU/Fruit")+
  ylab("Values")

#END of Analysis

library(dplyr)
OD<-Fr_Data_Days%>%
  group_by(ID)%>%
  filter(TotServices==max(TotServices))

OD<-Fr_Data_Days %>% 
  group_by(ID) %>% 
  filter(n()>1)


#3.NOTE remove duplicated by leaving the one that was in the system the longest. 
Individual_Analysis_Fr<-Individual_Analysis_Fr%>%
  group_by(ID)%>%
  filter(TotServices==max(TotServices))

#4. Consumed Only #Run independent
Individual_Analysis_Fr_Con<-Individual_Analysis_Fr%>%
  group_by(ID)%>%
  filter(TotServices==max(TotServices))%>%
  filter(Location=="Consumed")



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




Analysis_Prcc<-Func_Convert_Log(Analysis_Prcc,Column = "InContamination")
Analysis_Prcc<-Func_Convert_Log(Analysis_Prcc,Column = "Contamination")

#Graphics

d <- melt(Analysis_Consumed, id.vars="Contamination")

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

  



