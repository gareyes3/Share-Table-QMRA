sapply(AFr_Summary_DF, as.numeric)

class(AFr_Summary_DF$Cont5th)


Histogram_Fr_Total<-Exposure_Plot_Function4(Consumed = AFr_Summary_DF, Title = "One", xlab = "X", ylab = "3")
Histogram_Fr_Total

ggplot(AFr_Summary_DF,aes(Iteration.N, MedianCont)) + 
  geom_ribbon(aes(ymin = Cont5th, ymax = Cont95th), fill = "springgreen1")+  
  geom_line(aes(Iteration.N, Cont5th), color = "grey30", size = 0.1) + 
  geom_line(aes(Iteration.N, Cont95th), color = "grey30", size = 0.1) +   
  geom_line(color = "firebrick", size = 1)
