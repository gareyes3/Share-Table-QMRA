#Intervention Analysis

#1. Start from here
#This section creates a data frame with every item, Including repeated items, since it is a snapshot of the end of every service
Individual_Analysis_Fr<-bind_rows(List_Sens_Fr)

#2. find the dupplicates
#this step filters replicated based on the ID
Individual_Analysis_Fr<-Individual_Analysis_Fr %>% 
  group_by(ID) %>% 
  filter(TotServices==max(TotServices))


#Wrapping
Individual_Analysis_Fr_Consumed<-Individual_Analysis_Fr[which(Individual_Analysis_Fr$Location == "Consumed"),]
Individual_Analysis_Fr_Consumed_melt<-melt(data =Individual_Analysis_Fr_Consumed[,4:5])
Individual_Analysis_Fr_Consumed_melt$ID<-1:2200 #Change based on number o fruit

#boxplot
ggplot(data=Individual_Analysis_Fr_Consumed_melt, aes(x=variable, y=value))+
  geom_boxplot(aes(fill=variable))+
  scale_y_log10()+
  ggtitle("Exposure fruit wrapped vs if fruit not wrapped")+
  xlab("Contamination wrapped fruit vs Contamination consumed")+
  ylab("Contamination PFU/Fruit")

Individual_Analysis_Fr_Consumed$DiffWrapp<-(Individual_Analysis_Fr_Consumed$Contamination-Individual_Analysis_Fr_Consumed$ContConsumed)
log10(mean(Individual_Analysis_Fr_Consumed$DiffWrapp))

ggplot(data=Individual_Analysis_Fr_Consumed_melt, aes(x=value))+
  geom_histogram(aes(fill=variable))+
  scale_x_log10()

ggplot(data=Individual_Analysis_Fr_Consumed)+
  geom_point(aes(y = Contamination, x = rownames(Individual_Analysis_Fr_Consumed)), col="blue", alpha=.5)+
  geom_point(aes(y = ContConsumed, x = rownames(Individual_Analysis_Fr_Consumed)), col="Red", alpha=.5)+
  scale_y_log10()

#Smooth 
ggplot(data=Individual_Analysis_Fr_Consumed_melt, aes(x=ID, y=value))+
  geom_point(aes(col=variable), alpha=.1)+
  geom_smooth(aes(col=variable))+
  scale_y_log10()+
  ggtitle("Reduction contamination consumed due to wrapping fruit")+
  xlab("Fruit #")+
  ylab("Contamination HuNov PFU/Fruit")



#####Turning off Share Tables######
Individual_Analysis_Fr$STYN<-TRUE
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
EF$DeltaCont<-(EF$Contamination-EF$InContamination)


ListTouches<-strsplit(EF$TouchesContHist,",")
ListTouches<-lapply(ListTouches, function(x) x[x!="NA"])
ListTouches<-lapply(ListTouches, function(x) as.numeric(x))
TouchesContHist<-sapply(X = ListTouches, FUN = mean)
EF$TouchesContHistAvr<-TouchesContHist
sum(is.na(TouchesContHist))

Analysis_Individual<-data.frame(
  "InContamination"=EF$InContamination,
  "Wash"=EF$WashYN,
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

ggplot(data = EF, aes(x=Contamination, fill= WashYN))+
  geom_histogram(bins = 100)+
  scale_x_log10()+
  facet_wrap(~WashYN)

ggplot(data = EF, aes(x=DeltaCont, y=TotServices))+
  geom_point()+
  scale_x_log10()+
  facet_wrap(~WashYN)
