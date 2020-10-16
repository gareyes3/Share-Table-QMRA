View(Total_Consumed_Fr)
Total_Consumed_Fr<-Func_Convert_Log(Total_Consumed_Fr, "InContamination")
Total_Consumed_Fr_Bind<-Func_Convert_Log(Total_Consumed_Fr_Bind,"InContamination")

Consumed_Touched<-function(x){
  sum(x[6] =="Touched")
}
apply(X=Total_Consumed_Fr,MARGIN = 1,FUN=Consumed_Touched)

sum(Total_Consumed_Fr[1,6] =="Touched")


Touches_Number<-lengths(regmatches(Total_Consumed_Fr$History, gregexpr("Touched", Total_Consumed_Fr$History)))

Analysis_Consumed<-data.frame(
  "Contamination" = Total_Consumed_Fr$Contamination,
  "InContamination"=Total_Consumed_Fr$InContamination,
  "TouchesNo"=Touches_Number,
  "STTimes"= Total_Consumed_Fr$STtimes
  
)

Touches_NumberST_Con<-lengths(regmatches(Total_Consumed_Fr_Bind$History, gregexpr("Touched", Total_Consumed_Fr_Bind$History)))

Analysis_Bind<-data.frame(
  "Contamination" = Total_Consumed_Fr_Bind$Contamination,
  "InContamination"=Total_Consumed_Fr_Bind$InContamination,
  "TouchesNo"=Touches_NumberST_Con,
  "STTimes"= Total_Consumed_Fr_Bind$STtimes,
  "Type"= Total_Consumed_Fr_Bind$Type
  
)

pcc(X=Analysis_Consumed[,2:4], y=Analysis_Consumed$Contamination)



ggplot(data = Analysis_Bind , aes(x=Contamination, y = InContamination, col=Type )) + 
  geom_point()+
  geom_smooth()

epi.prcc(Analysis_Consumed,sided.test = 1)
