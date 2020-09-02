#Variables for Sensitivity Analysis

Sens_Iterations<-2
AFr_Summary_DF<-data.frame(
  "Iteration.N" = 1:Sens_Iterations,
  "MeanCont" = as.numeric(""),
  "MedianCont" =as.numeric(""),
  "MeanContSelection"= as.numeric(""),
  "MedianContSelection"= as.numeric(""),
  "MeanContST"= as.numeric(""),
  "MedianContST"= as.numeric(""),
  "AllergenConsumed"= as.numeric(""),
  stringsAsFactors = FALSE
)


APre_Summary_DF<-data.frame(
  "Iteration.N" = 1:Sens_Iterations,
  "MeanCont" = as.numeric(""),
  "MedianCont" = as.numeric(""),
  "MeanContSelection"= as.numeric(""),
  "MedianContSelection"= as.numeric(""),
  "MeanContST"= as.numeric(""),
  "MedianContST"= as.numeric(""),
  "SpoiledConsumed"= as.numeric(""),
  "AllergenConsumed"= as.numeric(""),
  stringsAsFactors = FALSE
)