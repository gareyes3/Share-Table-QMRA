# OUTPUTS FOR MEAL 1 -----------------------------------------------------


# Histograms for Exposure -------------------------------------------------

Total_Consumed<-Fr_Data[which(Fr_Data$Location == "Consumed"),]

#Total Exposure Histogram. 

Total_Consumed$Apple.No.<- 1:nrow(Total_Consumed)
row.names(Total_Consumed)<-1:nrow(Total_Consumed)
Total_Consumed$Contamination<-as.numeric(Total_Consumed$Contamination)
#Plot
ggplot(Total_Consumed, aes(x=Contamination)) + 
  geom_histogram( fill="#69b3a2", color="#e9ecef", binwidth = 50, boundary=.99) +
  ggtitle("Exposure from consuming fruit after 5 meal services")+
  theme(plot.title = element_text(hjust = 0.5))+
  stat_bin(binwidth=50, geom="text", size=3.5 ,aes(label=..count.., vjust=-.3), boundary = .99)+
  scale_x_continuous(breaks = seq(0,3000,50))+
  labs(x= "Contamination of Fruit Consumed", y= "Count of Fruit Consumed")+
  theme(axis.text.x=element_text(angle=90, hjust=1))
  
sum(Total_Consumed$Contamination)

                                #Consumed From Share Table:

Total_Consumed_ST<-Total_Consumed[which(Total_Consumed$STtimes > 0),]

                                #Consumed from Selection Table

Total_Consumed_NST<-Total_Consumed[which(Total_Consumed$STtimes ==0),]

#Plot SAhare Table
ggplot(Total_Consumed_ST, aes(x=Contamination)) + 
  geom_histogram(bins=10, fill="#69b3a2", color="#e9ecef", binwidth = 50, boundary=.99) +
  ggtitle("Exposure from consuming fruit in Share Table")+
  theme(plot.title = element_text(hjust = 0.5))+
  stat_bin(binwidth=50, geom="text", size=3.5 ,aes(label=..count.., vjust=-.3), boundary =.99)+
  scale_x_continuous(breaks = seq(0,3000,50))+
  labs(x= "Contamination of Fruit Consumed", y= "Count of Fruit Consumed")+
  theme(axis.text.x=element_text(angle=90, hjust=1))

#Plot SAhare Table
ggplot(Total_Consumed_NST, aes(x=Contamination)) + 
  geom_histogram(bins=10, fill="turquoise4", color="turquoise3", binwidth = 50,boundary =.99 ) +
  ggtitle("Exposure from consuming Items from Selection Table")+
  theme(plot.title = element_text(hjust = 0.5))+
  stat_bin(binwidth=50, geom="text", size=3.5 ,aes(label=..count.., vjust=-.3),boundary =.99)+
  scale_x_continuous(breaks = seq(0,3000,50))+
  labs(x= "Contamination of Fruit Consumed", y= "Count of Fruit Consumed")+
  theme(axis.text.x=element_text(angle=90, hjust=1))


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

