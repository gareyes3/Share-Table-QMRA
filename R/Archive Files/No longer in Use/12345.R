#Visual That Matter: 

#Creating Data Frame of Consumed Items

#1. Start from here
Individual_Analysis_Fr<-bind_rows(List_Sens_Fr)

#2. find the dupplicates
#this step filters replicated based on the ID
Individual_Analysis_Fr<-Individual_Analysis_Fr %>% 
  group_by(ID) %>% 
  filter(TotServices==max(TotServices))

#3. Narrowing down to consumed Items for exposure compisons
Individual_Analysis_Fr<-Individual_Analysis_Fr[which(Individual_Analysis_Fr$Location == "Consumed"),]

Individual_Analysis_Fr$Contamination[Individual_Analysis_Fr$Contamination==0]<-(10^-5)
Individual_Analysis_Fr$InContamination[Individual_Analysis_Fr$InContamination==0]<-(10^-5)

Individual_Analysis_Fr$Contamination<-log10(Individual_Analysis_Fr$Contamination)
Individual_Analysis_Fr$InContamination<-log10(Individual_Analysis_Fr$InContamination)

ggplot(data = Individual_Analysis_Fr,aes(x=Contamination))+
  geom_density(colour="black", alpha=.2, fill="blue")+
  scale_x_continuous(n.breaks = 10)+
  xlab("Contamination log GEC/Item")+
  ylab("Density")+
  ggtitle("Density Curve Final Contamination")+ 
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data = Individual_Analysis_Fr, aes(x=Contamination))+
  ggtitle("Histogram Exposure Fruit Consumed, no Intervention")+ 
  theme(plot.title = element_text(hjust = 0.5))+
  geom_histogram(bins = 30, fill="steelblue3")+
  xlab("Contamination log GEC/Item")+
  ylab("Count of Fruit")

ggplot(data = Individual_Analysis_Fr, aes(x=InContamination))+
  ggtitle("Histogram Initial Contamination Fruit")+ 
  theme(plot.title = element_text(hjust = 0.5))+
  geom_histogram(bins = 30, fill="steelblue3")+
  xlab("Contamination log GEC/Item")+
  ylab("Count of Fruit")
