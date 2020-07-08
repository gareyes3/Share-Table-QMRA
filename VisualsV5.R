# OUTPUTS FOR MEAL 1 -----------------------------------------------------


# Histograms for Exposure -------------------------------------------------

#Total Consumed Ammounts Creating Data:
                                #Fruit Total
  Total_Consumed_Fr<-Fr_Data_Days[which(Fr_Data_Days$Location == "Consumed"),]
  Total_Consumed_Fr$Type<- "Total Consumed"
  Total_Consumed_Fr<-Func_Convert_Log(Total_Consumed_Fr)
  #Share Table Total
  Total_Consumed_ST_Fr<-Total_Consumed_Fr[which(Total_Consumed_Fr$STtimes > 0),]
  Total_Consumed_ST_Fr$Type<-" Consumed Share Table"
  Total_Consumed_ST_Fr<-Func_Convert_Log(Total_Consumed_ST_Fr)
  #Selection Table Total
  Total_Consumed_Sel_Fr<-Total_Consumed_Fr[which(Total_Consumed_Fr$STtimes ==0),]
  Total_Consumed_Sel_Fr$Type<-" Consumed Selection Table"
  Total_Consumed_Sel_Fr<-Func_Convert_Log(Total_Consumed_Sel_Fr)
  
  Total_Consumed_Fr_Bind<-rbind(Total_Consumed_Fr,Total_Consumed_ST_Fr,Total_Consumed_Sel_Fr)
  #Converting Contamination to log CFU/g

  
                              
                                #Pss Total
  Total_Consumed_Pss<-Pss_Data_Days[which(Pss_Data_Days$Location == "Consumed"),]
  Total_Consumed_Pss$Type<- "Total Consumed"
  #Share Table Total
  Total_Consumed_ST_Pss<-Total_Consumed_Pss[which(Total_Consumed_Pss$STtimes > 0),]
  Total_Consumed_ST_Pss$Type<-" Consumed Share Table"
  #Selection Table Total
  Total_Consumed_Sel_Pss<-Total_Consumed_Pss[which(Total_Consumed_Pss$STtimes ==0),] 
  Total_Consumed_Sel_Pss$Type<-" Consumed Selection Table"
  
  Total_Consumed_Pss_Bind<-rbind(Total_Consumed_Pss,Total_Consumed_ST_Pss,Total_Consumed_Sel_Pss)
  

                                #Pre Total
  Total_Consumed_Pre<-Pre_Data_Days[which(Pre_Data_Days$Location == "Consumed"),]
  Total_Consumed_Pre$Type<- "Total Consumed"
  #Share Table Total
  Total_Consumed_ST_Pre<-Total_Consumed_Pre[which(Total_Consumed_Pre$STtimes > 0),]
  Total_Consumed_ST_Pre$Type<-" Consumed Share Table"
  #Selection Table Total
  Total_Consumed_Sel_Pre<-Total_Consumed_Pre[which(Total_Consumed_Pss$STtimes ==0),] 
  Total_Consumed_Sel_Pre$Type<-" Consumed Selection Table"
  
  Total_Consumed_Pre_Bind<-rbind(Total_Consumed_Pre,Total_Consumed_ST_Pre,Total_Consumed_Sel_Pre)
  
#Exposure plot function
#FUNCTION
  Exposure_Plot_Function<-function(Consumed = Total_Consumed_Fr ,Title = "Insert Title"){
    ggplot(Consumed, aes(x=Contamination)) + 
    geom_histogram( fill="#69b3a2", color="#e9ecef", binwidth = (Av_ic/60), boundary=.99) +
    ggtitle(Title)+
    theme(plot.title = element_text(hjust = 0.5))+
    stat_bin(binwidth=(Av_ic/60), geom="text", size=3.5 ,aes(label=..count.., vjust=-.3), boundary = .99)+
    scale_x_continuous(breaks = seq(0,Av_ic,(Av_ic/60)))+
    labs(x= "Contamination of Fruit Consumed", y= "Count of Fruit Consumed")+
    theme(axis.text.x=element_text(angle=90, hjust=1))
  }
  
  Exposure_Plot_Function2<-function(Consumed = Total_Consumed_Fr ,Title = "Insert Title"){
    ggplot(Consumed, aes(x=Contamination)) + 
      geom_histogram( fill="#69b3a2", color="#e9ecef", binwidth = (500), boundary=.99) +
      ggtitle(Title)+
      theme(plot.title = element_text(hjust = 0.5))+
      stat_bin(binwidth=(500), geom="text", size=3.5 ,aes(label=..count.., vjust=-.3), boundary = .99)+
      scale_x_continuous(breaks = seq(0,15000,(500)))+
      labs(x= "Contamination of Fruit Consumed", y= "Count of Fruit Consumed")+
      theme(axis.text.x=element_text(angle=90, hjust=1))
  }
  
  Exposure_Plot_Function3<-function(Consumed = Total_Consumed_Fr ,Title = "Insert Title"){
    ggplot(Consumed, aes(x=Contamination)) + 
      geom_histogram( fill="#69b3a2", color="#e9ecef", binwidth = 1, boundary= -1) +
      stat_bin(binwidth=1, geom="text", size=3.5 ,aes(label=..count.., vjust=-.3),boundary = -1 )+
      ggtitle(Title)+
      theme(plot.title = element_text(hjust = 0.5))+
      labs(x= "Contamination of Fruit Consumed log CFU/g", y= "Count of Fruit Consumed")+
      theme(axis.text.x=element_text(angle=90, hjust=1))
  }
  
#Exposure Based on type density
#FUNCTION
  
    Exposure_Staggered_Function<-function(ConsumedDF = Total_Consumed_Fr_Bind ,Contamination = Contamination ,Type = Type, Title = "Insert Title Here"){
      ggplot(ConsumedDF, aes(x=Contamination, fill= Type)) + 
    geom_histogram(alpha = 0.5, position = 'identity',binwidth = (Av_ic/60), boundary=.99 ) +
    ggtitle(Title)+
    theme(plot.title = element_text(hjust = 0.5))
    }
    Exposure_Staggered_Function2<-function(ConsumedDF = Total_Consumed_Fr_Bind ,Contamination = Contamination ,Type = Type, Title = "Insert Title Here"){
      ggplot(ConsumedDF, aes(x=Contamination, fill= Type)) + 
        geom_histogram(alpha = 0.5, position = 'identity', boundary=-1, binwidth = 1 ) +
        ggtitle(Title)+
        labs(x= "Contamination of Fruit Consumed log CFU/g", y= "Count of Fruit Consumed")+
        theme(plot.title = element_text(hjust = 0.5))
    }
    
#Graphs   
    
  Exposure_Staggered_Function2(Total_Consumed_Fr_Bind,Contamination = Contamination, Type = Type, "Total Exposure Fruit")
  Exposure_Staggered_Function(Total_Consumed_Pss_Bind,Contamination = Contamination, Type = Type, "Total Exposure Pss")
  Exposure_Staggered_Function(Total_Consumed_Pre_Bind,Contamination = Contamination, Type = Type, "Total Exposure Pre")
   
#Bar Chart shwing Final Location

  Location_BarC_Function<-function(Data, Title){
    ggplot(Data, aes(x=Service, fill=Location)) + 
    stat_count()+theme_minimal()+
    scale_x_continuous(breaks = seq(1,Meal_Day, by = 1))+
    ggtitle(Title)+
    theme(plot.title = element_text(hjust = 0.5))
    }
  if(Resharing_YN==0){
    Location_BarC_Function(Fr_Data,"Fruit Location Re-Service & Re-Sharing off")
    Location_BarC_Function(Pss_Data,"Pss Location Re-Service & Re-Sharing off")  
    Location_BarC_Function(Pre_Data,"Pre Location Re-Service & Re-Sharing off")  
  }
  
  if(Resharing_YN==1){
    Location_BarC_Function(Fr_Data,"Fruit Location Re-Service & Re-Sharing on")
    Location_BarC_Function(Pss_Data,"Pss Location Re-Service & Re-Sharing on")  
    Location_BarC_Function(Pre_Data,"Pre Location Re-Service & Re-Sharing on") 
  }
#Histogram Visuals
                                  #Fruit
  #Total Exposure
  Exposure_Plot_Function3(Total_Consumed_Fr, "Exposure total Fruit Consumed")
  Exposure_Plot_Function3(Total_Consumed_Fr_Bind, "Exposure total Fruit Consumed")
  #Total Exposure from Share Table Items
  Exposure_Plot_Function2(Total_Consumed_ST_Fr, "Exposure Consumed Fruit from Share Tables")
  #Total Exposure from Selection Table Items
  Exposure_Plot_Function2(Total_Consumed_Sel_Fr, "Exposure Consumed Fruit from Selection Tables")
  
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
  sum(Total_Consumed_Fr$Contamination)


  
#Exposure Boxplot. 
#FUNCTION  
Box_Plot_Function<-function(data = Total_Consumed_Fr_Bind ,title = "Insert Title Here"){
    ggplot(data=data, aes(x=Type, y=Contamination))+
    geom_boxplot(varwidth = TRUE,fill=c("#00AFBB", "#E7B800", "#FC4E07"), color="black")+
    stat_summary(fun=mean, shape=3, size=1, color="red", fill="red")+
    ggtitle(title)+
    theme(plot.title = element_text(hjust = 0.5))+
    ylab ("Contamination log CFU/g")+
    xlab ("Consumed From")
    }

Box_Plot_Function2<-function(data = Total_Consumed_Fr_Bind ,title = "Insert Title Here"){
  ggplot(data=data, aes(x=Type, y=Contamination))+
    geom_boxplot(varwidth = TRUE,fill=c( "#FC4E07"), color="black")+
    stat_summary(fun=mean, shape=3, size=1, color="red", fill="red")+
    ggtitle(title)+
    theme(plot.title = element_text(hjust = 0.5))+
    ylab ("Contamination log CFU/g")+
    xlab ("Consumed From")
}


Box_Plot_Function(Total_Consumed_Fr_Bind, "Exposure per location fruit")
Box_Plot_Function(Total_Consumed_Pss_Bind, "Exposure per location Pss")
Box_Plot_Function(Total_Consumed_Pre_Bind, "Exposure per location Pre")


Box_Plot_Function2(Total_Consumed_Fr,"Exposure per location fruit")

                              #Shared Fruit per days

#creating data frmae
#DF_Shared_Fruit<-data.frame(rbind(V_Shared_Fr_M1,V_Shared_Fr_M2,V_Shared_Fr_M3,V_Shared_Fr_M4,V_Shared_Fr_M5))
#colnames(DF_Shared_Fruit)<-c("Amount")
#row.names(DF_Shared_Fruit)<-c("Meal 1","Meal 2","Meal 3","Meal 4","Meal 5")

#ggplot(data=DF_Shared_Fruit, aes(x=row.names(DF_Shared_Fruit),y =Amount)) +
  #geom_bar(stat="identity",fill="#69b3a2", color="#e9ecef", width = .5)+
  #labs(x= "Meal Number", y= "Count of Fruit Shared")+
  #ggtitle("Shared Fruit per meal service")+
  #theme(plot.title = element_text(hjust = 0.5))
  
#Left food per day. 

#DF_Left_Selection_Fruit<-data.frame(rbind(No_Left_Selection_Fr_M1,No_Left_Selection_Fr_M2,No_Left_Selection_Fr_M3,No_Left_Selection_Fr_M4,No_Left_Selection_Fr_M5))
#colnames(DF_Left_Selection_Fruit)<-c("Amount")
#row.names(DF_Left_Selection_Fruit)<-c("Meal 1","Meal 2","Meal 3","Meal 4","Meal 5")

#ggplot(data=DF_Left_Selection_Fruit, aes(x=row.names(DF_Left_Selection_Fruit),y =Amount)) +
  #geom_bar(stat="identity",fill="#69b3a2", color="#e9ecef", width = .5)+
  #labs(x= "Meal Number", y= "Count of Fruit Left in selection table")+
  #ggtitle("Food Left in Selection Table")+
  #theme(plot.title = element_text(hjust = 0.5))


Fruit_Data_Consumed_Discarded<-rbind(Fr_Data_Days[which(Fr_Data_Days$Location == "Consumed"),],Fr_Data_Days[which(Fr_Data_Days$Location == "Discarded"),])  

ggplot(Fruit_Data_Consumed_Discarded, aes(x=Location)) + 
  geom_bar(stat = "count", fill= c("#00AFBB", "#E7B800"))+
  ggtitle("Consumed vs Discarded Share Tables OFF")+
  theme(plot.title = element_text(hjust = 0.5))

