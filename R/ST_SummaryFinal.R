sapply(AFr_Summary_DF, as.numeric)

class(AFr_Summary_DF$Cont5th)


Histogram_Fr_Total<-Exposure_Plot_Function4(Consumed = AFr_Summary_DF, Title = "One", xlab = "X", ylab = "3")
Histogram_Fr_Total
