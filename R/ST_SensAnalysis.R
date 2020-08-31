#Variables for Sensitivity Analysis

Sens_Iterations<-1
AFr_Summary_DF<-data.frame(
  "Iteration.N" = 1:Sens_Iterations,
  "MeanCont" = "",
  "MedianCont" = "",
  "MeanContSelection"= "",
  "MedianContSelection"= "",
  "MeanContST"= "",
  "MedianContST"= "",
  stringsAsFactors = FALSE
)
