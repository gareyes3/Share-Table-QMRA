

sapply(AFr_Summary_DF, as.numeric)

Total<-AFr_Summary_DF[1:5]
Total$Type<-"Total"
Selection<-AFr_Summary_DF[c(1,6:9)]
names(Selection)<-c("Iteration.N", "MeanCont", "MedianCont", "Cont5th", "Cont95th")
Selection$Type<-"Service Line"
ST<-AFr_Summary_DF[c(1,10:13)]
names(ST)<-c("Iteration.N", "MeanCont", "MedianCont", "Cont5th", "Cont95th")
ST$Type<-"Share Table"
Total<-rbind(Total,Selection, ST)

#Function for Appending

Func_Append_Column_Final<-function(DF = AFr_Summary_DF ){
  sapply(DF, as.numeric)
  Total<-DF[1:5]
  Total$Type<-"Total"
  Selection<-DF[c(1,6:9)]
  names(Selection)<-c("Iteration.N", "MeanCont", "MedianCont", "Cont5th", "Cont95th")
  Selection$Type<-"Service Line"
  ST<-DF[c(1,10:13)]
  names(ST)<-c("Iteration.N", "MeanCont", "MedianCont", "Cont5th", "Cont95th")
  ST$Type<-"Share Table"
  Total<-rbind(Total,Selection, ST)
  return(Total)
}

AFr_Weeks_Append<-Func_Append_Column_Final(AFr_Summary_DF)
APss_Weeks_Append<-Func_Append_Column_Final(APss_Summary_DF)
APre_Weeks_Append<-Func_Append_Column_Final(APre_Summary_DF)

Fruit_Ribbon<-Ribbon_Function_Final(AFr_Weeks_Append,AFr_Summary_DF, "Fruit")
Pss_Ribbon<-Ribbon_Function_Final(APss_Weeks_Append,APss_Summary_DF, "Pss")
Pre_Ribbon<-Ribbon_Function_Final(APre_Weeks_Append,APre_Summary_DF, "Pre")

ggarrange(Fruit_Ribbon,Pss_Ribbon, Pre_Ribbon,
  ncol=1, nrow=3, label.x = "Weeks", label.y ="Contamination Log CFU/g 5th - 95th percentile" 
)



#Function for Plot Weeks

ggplot(Total, aes(x = Iteration.N, y = MedianCont, group = Type)) + 
  geom_ribbon(aes(ymin = Cont5th, ymax = Cont95th, fill=Type), alpha=0.3 )+ 
  geom_point(aes(x = Iteration.N, y = MedianCont, color = Type))+
  geom_line(aes(x= Iteration.N, y= MedianCont,color = Type), size = .5) +   
  labs(x = "Week #", y = "Contamination Log CFU/g 5th - 95th percentile") +
  scale_fill_manual(name = '5th-95th Percentile', values = c("dodgerblue1", "tomato4", "seagreen1"))+ 
  scale_color_manual(name = 'Median', values = c("dodgerblue1", "tomato4", "seagreen1"))+
  theme_bw()+
  scale_x_continuous(breaks = seq(1, nrow(AFr_Summary_DF), by = 1))


Ribbon_Function_Final<-function(DF,DF2, Title){
  ggplot(DF, aes(x = Iteration.N, y = MedianCont, group = Type)) + 
    geom_ribbon(aes(ymin = Cont5th, ymax = Cont95th, fill=Type), alpha=0.3 )+ 
    geom_point(aes(x = Iteration.N, y = MedianCont, color = Type))+
    geom_line(aes(x= Iteration.N, y= MedianCont,color = Type), size = .5) +   
    labs(x = "Week #", y = "Contamination Log CFU/g 5th - 95th percentile") +
    scale_fill_manual(name = '5th-95th Percentile', values = c("dodgerblue1", "tomato4", "seagreen1"))+ 
    scale_color_manual(name = 'Median', values = c("dodgerblue1", "tomato4", "seagreen1"))+
    theme_bw()+
    ggtitle(Title)+
    scale_x_continuous(breaks = seq(1, nrow(DF2), by = 1))
}



  
ggplot(Total) + 
  geom_ribbon(aes(x= Iteration.N,ymin = Cont5th, ymax = Cont95th), fill = "tomato4", alpha=0.5 )+ 
  geom_point(aes(x = Iteration.N, y = MedianCont))+
  geom_line(aes(x= Iteration.N, y= MedianCont,), color = "grey70", size = .5) +   
  labs(x = "Week #", y = "Food Consumed Selection Contamination 5th - 95th percentile") +
  scale_fill_manual(name = 'a name', values = "#BEAED4")+
  scale_x_continuous(breaks = seq(1, nrow(AFr_Summary_DF), by = 1))+
  labs(x = "Week #", y = "Food Consumed Contamination 5th - 95th percentile") +
  theme_bw()
  

ggplot(AFr_Summary_DF) + 
  geom_ribbon(aes(x= Iteration.N,ymin = Cont5th, ymax = Cont95th), fill = "dodgerblue1", alpha=0.5)+
  geom_ribbon(aes(x= Iteration.N,ymin = ContSel5th, ymax = ContSel95th), fill = "tomato4", alpha=0.4)+ 
  geom_ribbon(aes(x= Iteration.N,ymin = ContST5th, ymax = ContST95th), fill = "seagreen1", alpha=0.4)+ 
  geom_point(aes(x = Iteration.N, y = MedianCont))+
  geom_line(aes(x= Iteration.N, y= MedianCont), color = "dodgerblue4", size = .5) +   
  labs(x = "Week #", y = "Food Consumed Contamination 5th - 95th percentile") +
  theme_bw()+
  theme(legend.position = "top")+
  scale_x_continuous(breaks = seq(1, nrow(AFr_Summary_DF), by = 1))+
  geom_point(aes(x = Iteration.N, y = MedianContSelection))+
  geom_line(aes(x= Iteration.N, y= MedianContSelection), color = "tomato1", size = .5)+
  geom_point(aes(x = Iteration.N, y = MedianContST))+
  geom_line(aes(x= Iteration.N, y= MedianContST), color = "seagreen", size = .5)


