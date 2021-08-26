#SUMMARY FINALS FOR RIBBON


#Appending Data Frame by Type
AFr_Weeks_Append<-Func_Append_Column_Final(AFr_Summary_DF)
APss_Weeks_Append<-Func_Append_Column_Final(APss_Summary_DF)
APre_Weeks_Append<-Func_Append_Column_Final(APre_Summary_DF)

#Creating bind 
Fruit_Ribbon<-Ribbon_Function_Final(AFr_Weeks_Append,AFr_Summary_DF, "Fruit Contamination/ week")
Pss_Ribbon<-Ribbon_Function_Final(APss_Weeks_Append,APss_Summary_DF, "Pss Contamination/ week")
Pre_Ribbon<-Ribbon_Function_Final(APre_Weeks_Append,APre_Summary_DF, "Pre Contamination/ week")



Figure1<-ggarrange(Fruit_Ribbon,Pss_Ribbon, Pre_Ribbon,
  ncol=1, nrow=3, labels = NULL, common.legend = TRUE, legend = "right" 
)
Figure1<-annotate_figure(Figure1,
                top = text_grob("Consumption Distribution", color = "Black", face = "bold", size = 14),
                bottom = text_grob("Data source: \n Share Table Model", color = "blue",
                                   hjust = 1, x = 1, face = "italic", size = 10),
                left = text_grob("Contamination Consumed 5th-95th Percentile", color = "Black", rot = 90),
                fig.lab = "Figure 1", fig.lab.face = "bold"
)


print(Figure1)

print(Fruit_Ribbon)

Func_GGsave_Normal(Fruit_Ribbon,"Ribbons","FruitRibbon")
Func_GGsave_Normal(Figure1,"Ribbons","CombinedRibbon")

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


