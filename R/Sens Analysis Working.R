#Summary of Iterations for Sens Analysis


#Construction of a sensitive analysis

Test<-function(vector){
  vector<-vector[which(vector$Location == "Consumed"),]
  mean(vector$Contamination)
}

Test2<-function(vector){
  mean(vector$Contamination[Vector$])
}
List_Sens_Fr
Mean_Con_Services_Fr<-sapply(X=List_Sens_Fr,FUN=Test) #


mean(Mean_Con_Services_Fr)
median(Mean_Con_Services_Fr)
var(Mean_Con_Services_Fr)

#Data Analysis Data frame

chunk <- 65
n <- length(Vector_Contaminations)
r  <- rep(1:ceiling(n/chunk),each=chunk)[1:n]
d <- split(Vector_Contaminations,r)
Mean_cont_Sertvices2<-sapply(d, Test2)

Data_Analysis<-data.frame("Contaminations"= rep(Vector_Contaminations,each=25),
                          "TE_H_F"= Vector_TE_H_F,
                          "Outputs" =Mean_Con_Services_Fr)

pcc(X=Data_Analysis[,1:2], y=Data_Analysis$Outputs)


Input<-data.frame(
  "Input"=rep(1:10,each=25)
)


quantile(Fr_Data.Frame$Contamination, probs = c(.25,.5,.75))

plot(Data_Analysis$Input, Data_Analysis$Outputs)
