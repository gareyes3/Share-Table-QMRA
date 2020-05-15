# OUTPUTS FOR MEAL 1 -----------------------------------------------------


# Histograms for Exposure -------------------------------------------------

#Total Consumed Ammounts:
                                #Fruit Total
  Total_Consumed_Fr<-Fr_Data[which(Fr_Data$Location == "Consumed"),]
  Total_Consumed_Fr$Type<- "Total Consumed"
  #Share Table Total
  Total_Consumed_ST_Fr<-Total_Consumed_Fr[which(Total_Consumed_Fr$STtimes > 0),]
  Total_Consumed_ST_Fr$Type<-" Consumed Share Table"
  #Selection Table Total
  Total_Consumed_Sel_Fr<-Total_Consumed_Fr[which(Total_Consumed_Fr$STtimes ==0),]
  Total_Consumed_Sel_Fr$Type<-" Consumed Selection Table"
  
  Total_Consumed_Fr_Bind<-rbind(Total_Consumed_Fr,Total_Consumed_ST_Fr,Total_Consumed_Sel_Fr)

  
                              
                                #Pss Total
  Total_Consumed_Pss<-Pss_Data[which(Pss_Data$Location == "Consumed"),]
  Total_Consumed_Pss$Type<- "Total Consumed"
  #Share Table Total
  Total_Consumed_ST_Pss<-Total_Consumed_Pss[which(Total_Consumed_Pss$STtimes > 0),]
  Total_Consumed_ST_Pss$Type<-" Consumed Share Table"
  #Selection Table Total
  Total_Consumed_Sel_Pss<-Total_Consumed_Pss[which(Total_Consumed_Pss$STtimes ==0),] 
  Total_Consumed_Sel_Pss$Type<-" Consumed Selection Table"
  
  Total_Consumed_Pss_Bind<-rbind(Total_Consumed_Pss,Total_Consumed_ST_Pss,Total_Consumed_Sel_Pss)
  

                                #Pre Total
  Total_Consumed_Pre<-Pre_Data[which(Pre_Data$Location == "Consumed"),]
  Total_Consumed_Pre$Type<- "Total Consumed"
  #Share Table Total
  Total_Consumed_ST_Pre<-Total_Consumed_Pre[which(Total_Consumed_Pre$STtimes > 0),]
  Total_Consumed_ST_Pre$Type<-" Consumed Share Table"
  #Selection Table Total
  Total_Consumed_Sel_Pre<-Total_Consumed_Pre[which(Total_Consumed_Pss$STtimes ==0),] 
  Total_Consumed_Sel_Pre$Type<-" Consumed Selection Table"
  
  Total_Consumed_Pre_Bind<-rbind(Total_Consumed_Pre,Total_Consumed_ST_Pre,Total_Consumed_Sel_Pre)
  
#Exposure plot function
  Exposure_Plot_Function<-function(Consumed,Title){
    ggplot(Consumed, aes(x=Contamination)) + 
    geom_histogram( fill="#69b3a2", color="#e9ecef", binwidth = (Av_ic/60), boundary=.99) +
    ggtitle(Title)+
    theme(plot.title = element_text(hjust = 0.5))+
    stat_bin(binwidth=(Av_ic/60), geom="text", size=3.5 ,aes(label=..count.., vjust=-.3), boundary = .99)+
    scale_x_continuous(breaks = seq(0,Av_ic,(Av_ic/60)))+
    labs(x= "Contamination of Fruit Consumed", y= "Count of Fruit Consumed")+
    theme(axis.text.x=element_text(angle=90, hjust=1))
  }
  
#Exposure Based on type density
  
    Exposure_Staggered_Function<-function(ConsumedDF,Contamination ,Type, Title){
      ggplot(ConsumedDF, aes(x=Contamination, fill= Type)) + 
    geom_histogram(alpha = 0.5, position = 'identity',binwidth = (Av_ic/60), boundary=.99 ) +
    ggtitle(Title)+
    theme(plot.title = element_text(hjust = 0.5))
    }

Exposure_Staggered_Function(Total_Consumed_Fr_Bind,Contamination = Contamination, Type = Type, "Total Exposure")
      
#Histogram Visuals
                                  #Fruit
  #Total Exposure
  Exposure_Plot_Function(Total_Consumed_Fr, "Exposure total Consumed")
  #Total Exposure from Share Table Items
  Exposure_Plot_Function(Total_Consumed_ST_Fr, "Exposure Consumed Fruit from Share Tables")
  #Total Exposure from Selection Table Items
  Exposure_Plot_Function(Total_Consumed_Sel_Fr, "Exposure Consumed Fruit from Selection Tables")
  
                                  #Pss
  #Total Exposure
  Exposure_Plot_Function(Total_Consumed_Pss, "Exposure total Consumed Pss")
  #Total Exposure from Share Table Items
  Exposure_Plot_Function(Total_Consumed_ST_Pss, "Exposure Consumed Pss from Share Tables")
  #Total Exposure from Selection Table Items
  Exposure_Plot_Function(Total_Consumed_Sel_Pss, "Exposure Consumed Pss from Selection Table")
                                
                                  #Pre    
  #Total Exposure
  Exposure_Plot_Function(Total_Consumed_Pre, "Exposure total Consumed Pre")
  #Total Exposure from Share Table Items
  Exposure_Plot_Function(Total_Consumed_ST_Pre, "Exposure Consumed Pre from Share Tables")
  #Total Exposure from Selection Table Items
  Exposure_Plot_Function(Total_Consumed_Sel_Pre, "Exposure Consumed Pre from Selection Table")
  
#Sum of the total partciles consumed.   
  sum(Total_Consumed$Contamination)


  
#Exposure Boxplot. 
  
  ggplot(data=Total_Consumed_Fr, aes(x=Type, y=Contamination))+
    geom_boxplot(fill="#00AFBB", color="black")
  
  ggplot(data=Total_Consumed_Fr_Bind, aes(x=Type, y=Contamination))+
    geom_boxplot(fill=c("#00AFBB", "#E7B800", "#FC4E07"), color="black")




                              #Shared Fruit per days

#creating data frmae
DF_Shared_Fruit<-data.frame(rbind(V_Shared_Fr_M1,V_Shared_Fr_M2,V_Shared_Fr_M3,V_Shared_Fr_M4,V_Shared_Fr_M5))
colnames(DF_Shared_Fruit)<-c("Amount")
row.names(DF_Shared_Fruit)<-c("Meal 1","Meal 2","Meal 3","Meal 4","Meal 5")

ggplot(data=DF_Shared_Fruit, aes(x=row.names(DF_Shared_Fruit),y =Amount)) +
  geom_bar(stat="identity",fill="#69b3a2", color="#e9ecef", width = .5)+
  labs(x= "Meal Number", y= "Count of Fruit Shared")+
  ggtitle("Shared Fruit per meal service")+
  theme(plot.title = element_text(hjust = 0.5))
  
#Left food per day. 

DF_Left_Selection_Fruit<-data.frame(rbind(No_Left_Selection_Fr_M1,No_Left_Selection_Fr_M2,No_Left_Selection_Fr_M3,No_Left_Selection_Fr_M4,No_Left_Selection_Fr_M5))
colnames(DF_Left_Selection_Fruit)<-c("Amount")
row.names(DF_Left_Selection_Fruit)<-c("Meal 1","Meal 2","Meal 3","Meal 4","Meal 5")

ggplot(data=DF_Left_Selection_Fruit, aes(x=row.names(DF_Left_Selection_Fruit),y =Amount)) +
  geom_bar(stat="identity",fill="#69b3a2", color="#e9ecef", width = .5)+
  labs(x= "Meal Number", y= "Count of Fruit Left in selection table")+
  ggtitle("Food Left in Selection Table")+
  theme(plot.title = element_text(hjust = 0.5))

