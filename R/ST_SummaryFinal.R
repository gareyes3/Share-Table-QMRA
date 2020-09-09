sapply(AFr_Summary_DF, as.numeric)

class(AFr_Summary_DF$Cont5th)

Total<-AFr_Summary_DF[1:5]
Selection<-AFr_Summary_DF[c(1,6:9)]
ST<-AFr_Summary_DF[c(1,10:13)]
Total<-rbind(Total,Selection, ST, setNames(Total,names(Total)))

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
  geom_line(aes(x= Iteration.N, y= MedianContST), color = "seagreen", size = .5)+
  scale_fill_manual(name='Categories',
                    values=c('dodgerblue1','tomato4',"seagreen1"),
                    labels=c('Total','Selection','Share Table'))
  

  
  
  
  

ggplot(AFr_Summary_DF) + 
  geom_ribbon(aes(x= Iteration.N,ymin = ContSel5th, ymax = ContSel95th), fill = "tomato4")+ 
  geom_point(aes(x = Iteration.N, y = MedianContSelection))+
  geom_line(aes(x= Iteration.N, y= MedianContSelection), color = "tomato1", size = .5) +   
  labs(x = "Week #", y = "Food Consumed Selection Contamination 5th - 95th percentile") +
  theme_bw()+
  theme(legend.position = "top")

ggplot(AFr_Summary_DF) + 
  geom_ribbon(aes(x= Iteration.N,ymin = ContST5th, ymax = ContST95th), fill = "seagreen1")+ 
  geom_point(aes(x = Iteration.N, y = MedianContST))+
  geom_line(aes(x= Iteration.N, y= MedianContST), color = "seagreen", size = .5) +   
  labs(x = "Week #", y = "Food Consumed ST Contamination 5th - 95th percentile") +
  theme_bw()+
  theme(legend.position = "top")


