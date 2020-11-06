#Intervention Analysis

#1. Start from here
#This section creates a data frame with every item, Including repeated items, since it is a snapshot of the end of every service
Individual_Analysis_Fr<-bind_rows(List_Sens_Fr)

#2. find the dupplicates
#this step filters replicated based on the ID
Individual_Analysis_Fr<-Individual_Analysis_Fr %>% 
  group_by(ID) %>% 
  filter(TotServices==max(TotServices))

#####Turning off Share Tables######
Individual_Analysis_Fr$STYN<-FALSE
Individual_Analysis_Fr_NoST<-Individual_Analysis_Fr

#####Turning On Share Tables######
Individual_Analysis_Fr$STYN<-TRUE
Individual_Analysis_Fr_YesST<-Individual_Analysis_Fr

EF<-bind_rows(Individual_Analysis_Fr_NoST,Individual_Analysis_Fr_YesST)


#Change For Washing #########
Individual_Analysis_Fr$WashYN<-FALSE

Individual_Analysis_Fr_Wash<-Individual_Analysis_Fr

Individual_Analysis_Fr_NoWash<-Individual_Analysis_Fr

EF<-bind_rows(Individual_Analysis_Fr_Wash,Individual_Analysis_Fr_NoWash)

ListTouches<-strsplit(EF$TouchesContHist,",")
ListTouches<-lapply(ListTouches, function(x) x[x!="NA"])
ListTouches<-lapply(ListTouches, function(x) as.numeric(x))
TouchesContHist<-sapply(X = ListTouches, FUN = mean)
EF$TouchesContHistAvr<-TouchesContHist
sum(is.na(TouchesContHist))

Analysis_Individual<-data.frame(
  "InContamination"=EF$InContamination,
  "Wash"=EF$STYN,
  "TouchesContHistAvr"=EF$TouchesContHistAvr,
  "STTimes"= EF$STtimes,
  "TotServices"=EF$TotServices,
  "DeltaCont"= (EF$Contamination-EF$InContamination),
  "Contamination" = EF$Contamination
)

Pcc2<-pcc(X=Analysis_Individual[,1:5], y=Analysis_Individual$DeltaCont,rank = TRUE,nboot = 1000)
Pcc2

ggplot()+
  geom_histogram(data = Individual_Analysis_Fr_Wash,aes(x=Contamination), fill = "red", alpha = 0.4,bins = 100)+
  geom_histogram(data = Individual_Analysis_Fr_NoWash,aes(x=Contamination), fill = "blue", alpha = 0.4,bins = 100)+
  scale_x_log10()

ggplot(data = EF, aes(x=Contamination, fill= STYN))+
  geom_histogram(bins = 100)+
  scale_x_log10()+
  facet_wrap(~STYN)
