#Variables for Sensitivity Analysis

Sens_Iterations<-10
AFr_Summary_DF<-data.frame(
  "Iteration.N" = 1:Sens_Iterations,
  "MeanCont" = "",
  "MedianCont" = "",
  "MeanContSelection"= "",
  "MedianContSelection"= "",
  "MeanContST"= "",
  "MedianContST"= "",
  "AllergenConsumed"= "",
  stringsAsFactors = FALSE
)


APre_Summary_DF<-data.frame(
  "Iteration.N" = 1:Sens_Iterations,
  "MeanCont" = "",
  "MedianCont" = "",
  "MeanContSelection"= "",
  "MedianContSelection"= "",
  "MeanContST"= "",
  "MedianContST"= "",
  "SpoiledConsumed"= "",
  "AllergenConsumed"= "",
  stringsAsFactors = FALSE
)