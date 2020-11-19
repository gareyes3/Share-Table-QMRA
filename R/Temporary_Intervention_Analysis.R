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
Individual_Analysis_Fr_Consumed_melt$ID<-1:(nrow(Individual_Analysis_Fr_Consumed_melt_Wrap)/2) #Change based on number o fruit


#For Delta Contamination of wrapped. 

#Changing to logs only if necessary
#Individual_Analysis_Fr_Consumed$Contamination<-log10(Individual_Analysis_Fr_Consumed$Contamination)
#Individual_Analysis_Fr_Consumed$InContamination<-log10(Individual_Analysis_Fr_Consumed$InContamination)
#Individual_Analysis_Fr_Consumed$ContConsumed<-log10(Individual_Analysis_Fr_Consumed$ContConsumed)

Individual_Analysis_Fr_Consumed$DeltaCont<-Individual_Analysis_Fr_Consumed$Contamination-Individual_Analysis_Fr_Consumed$InContamination
Individual_Analysis_Fr_Consumed$DeltaContCons<-Individual_Analysis_Fr_Consumed$ContConsumed-Individual_Analysis_Fr_Consumed$InContamination

Individual_Analysis_Fr_Consumed_melt_Wrap<-melt(data =Individual_Analysis_Fr_Consumed[,18:19])
Individual_Analysis_Fr_Consumed_melt_Wrap$ID<-1:(nrow(Individual_Analysis_Fr_Consumed_melt_Wrap)/2) #Change based on number o fruit


#boxplot
ggplot(data=Individual_Analysis_Fr_Consumed_melt, aes(x=variable, y=value))+
  geom_boxplot(aes(fill=variable))+
  scale_y_log10()+
  ggtitle("Exposure fruit wrapped vs if fruit not wrapped")+
  xlab("Contamination wrapped fruit vs Contamination consumed")+
  ylab("Contamination PFU/Fruit")


#Function for median
p_meds <- ddply(Individual_Analysis_Fr_Consumed_melt_Wrap, .(variable), summarise, med = median(value))
library(ggallin)

#Boxplot Deltacont
ggplot(data=Individual_Analysis_Fr_Consumed_melt_Wrap, aes(x=variable, y=value))+
  geom_boxplot(aes(fill=variable))+
  ggtitle("Exposure fruit wrapped vs if fruit not wrapped")+
  xlab("Contamination wrapped fruit vs Contamination consumed")+
  ylab("Contamination PFU/Fruit")+
  scale_y_continuous(trans = scales::pseudo_log_trans())+
  geom_text(data = p_meds, aes(x = variable, y = med, label = med), 
            size = 3, vjust = -1.5)


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
Individual_Analysis_Fr$WashYN<-TRUE

Individual_Analysis_Fr_Wash<-Individual_Analysis_Fr

Individual_Analysis_Fr_NoWash<-Individual_Analysis_Fr

EF<-bind_rows(Individual_Analysis_Fr_Wash,Individual_Analysis_Fr_NoWash)
EF$DeltaCont<-(EF$Contamination-EF$InContamination)

#Delta Conetmination wahs
Individual_Analysis_Fr_Wash$DeltaCont<-(Individual_Analysis_Fr_Wash$Contamination - Individual_Analysis_Fr_Wash$InContamination)


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
  geom_histogram(data = Individual_Analysis_Fr_Wash,aes(x=DeltaCont), fill = "red", alpha = 0.4,bins = 100)+
  #geom_histogram(data = Individual_Analysis_Fr_NoWash,aes(x=Contamination), fill = "blue", alpha = 0.4,bins = 100)+
  scale_x_log10()

ggplot(data = EF, aes(x=Contamination, fill= WashYN))+
  geom_histogram(bins = 100)+
  scale_x_log10()+
  facet_wrap(~WashYN)

ggplot(data = EF, aes(x=DeltaCont, y=TotServices))+
  geom_point()+
  scale_x_log10()+
  facet_wrap(~WashYN)






#Analysis of the washed Items

#1. Start from here
#This section creates a data frame with every item, Including repeated items, since it is a snapshot of the end of every service
Individual_Analysis_Fr<-bind_rows(List_Sens_Fr)

#2. find the dupplicates
#this step filters replicated based on the ID
Individual_Analysis_Fr<-Individual_Analysis_Fr %>% 
  group_by(ID) %>% 
  filter(TotServices==max(TotServices))


#Delta Contamination

Individual_Analysis_Fr$DeltaCont<-(Individual_Analysis_Fr$Contamination-Individual_Analysis_Fr$InContamination)

#No Wash
Individual_Analysis_Fr$WashYN<-FALSE
Individual_Analysis_Fr_NoWash<-Individual_Analysis_Fr

#Wash

Individual_Analysis_Fr$WashYN<-TRUE
Individual_Analysis_Fr_Wash<-Individual_Analysis_Fr

#Binding them

EF<-bind_rows(Individual_Analysis_Fr_Wash,Individual_Analysis_Fr_NoWash)

#Boxplot

give.n <- function(x){
  return(c(y = median(x)*1.05, label = length(x))) 
  # experiment with the multiplier to find the perfect position
}

Individual_Analysis_Fr_Wash<-Func_Convert_Log(Individual_Analysis_Fr_Wash, "Contamination")
Individual_Analysis_Fr_Wash<-Func_Convert_Log(Individual_Analysis_Fr_Wash, "DeltaCont")

ggplot(data= Individual_Analysis_Fr_Wash, aes(y=Contamination, x = WashHistory, group=WashHistory))+
  stat_boxplot(aes(fill=WashHistory))+
  scale_y_log10()+
  stat_summary(fun.data = give.n, geom = "text", fun.y = median,
               position = position_dodge(width = 0.75))

ggplot(data= Individual_Analysis_Fr_NoWash, aes(y=Contamination, x = WashHistory, group=WashHistory))+
  geom_boxplot(aes(fill=WashHistory))+
  scale_y_log10()

ggplot(data = Individual_Analysis_Fr,aes(x=Contamination, group=WashHistory))+
  geom_histogram(aes(fill=WashHistory), alpha = 1,bins = 100)+
  scale_x_log10()+
  scale_fill_gradient(low="green", high="red")

#Density Curve
ggplot(data = Individual_Analysis_Fr,aes(x=Contamination))+
  geom_histogram(aes(y=..density..), alpha = 1,fill = "white",color="black",bins = 100)+
  geom_density(colour="black", fill="red", alpha=.3)+ 
  scale_x_log10()

#Side by Side stacked
ggplot()+
  geom_histogram(data = Individual_Analysis_Fr_Wash,aes(x=Contamination), fill = "Red", alpha = 0.5,bins = 100)+
  geom_density(data = Individual_Analysis_Fr_Wash,aes(x=Contamination))
  geom_histogram(data = Individual_Analysis_Fr_NoWash,aes(x=Contamination), fill = "blue", alpha = 0.5,bins = 100)+
  scale_x_log10()
#Facet wrapp 
ggplot(data=EF, aes(x=Contamination))+
  geom_histogram(aes(fill=WashYN), bins=100)+
  scale_x_log10()+
  facet_wrap(facets = ~WashYN)

ggplot(data=EF, aes(x=DeltaCont))+
  geom_histogram(aes(fill=WashYN), bins=100)+
  scale_x_log10()+
  facet_wrap(facets = ~WashYN)
#Boxplot
ggplot(data=EF, aes(y=Contamination))+
  geom_boxplot(aes(fill=WashYN))+
  scale_y_log10()

ggplot(data=EF, aes(y=DeltaCont))+
  geom_boxplot(aes(fill=WashYN))+
  scale_y_log10()

#Ideas, add tracker if item was washed. Compared where items are that have been washed. 



#Comparing all 3

#1. Start from here
#This section creates a data frame with every item, Including repeated items, since it is a snapshot of the end of every service
Individual_Analysis_Fr<-bind_rows(List_Sens_Fr)

#2. find the dupplicates
#this step filters replicated based on the ID
Individual_Analysis_Fr<-Individual_Analysis_Fr %>% 
  group_by(ID) %>% 
  filter(TotServices==max(TotServices))

#3. Run the iterations with Wash on. 
Individual_Analysis_Fr_Wash<-Individual_Analysis_Fr


Individual_Analysis_Fr_Consumed_W<-Individual_Analysis_Fr_Wash[which(Individual_Analysis_Fr_Wash$Location == "Consumed"),]

###Now Run the process with washing off and Wrapping on. 

#1. Start from here
#This section creates a data frame with every item, Including repeated items, since it is a snapshot of the end of every service
Individual_Analysis_Fr<-bind_rows(List_Sens_Fr)

#2. find the dupplicates
#this step filters replicated based on the ID
Individual_Analysis_Fr<-Individual_Analysis_Fr %>% 
  group_by(ID) %>% 
  filter(TotServices==max(TotServices))

Individual_Analysis_Fr_Consumed<-Individual_Analysis_Fr[which(Individual_Analysis_Fr$Location == "Consumed"),]



ggplot(data = Individual_Analysis_Fr_Consumed_W,aes(x=Contamination))+
  geom_histogram(aes(y=..density..), alpha = 1,fill = "white",color="black",bins = 100)+
  geom_density(colour="black", fill="red", alpha=.3)+ 
  scale_x_log10()

ggplot(data = Individual_Analysis_Fr_Consumed,aes(x=Contamination))+
  geom_histogram(aes(y=..density..), alpha = 1,fill = "white",color="black",bins = 100)+
  geom_density(colour="black", fill="red", alpha=.3)+ 
  scale_x_log10()

ggplot(data = Individual_Analysis_Fr_Consumed,aes(x=ContConsumed))+
  geom_histogram(aes(y=..density..), alpha = 1,fill = "white",color="black",bins = 100)+
  geom_density(colour="black", fill="red", alpha=.3)+ 
  scale_x_log10()

#Making the Delta Cont

Individual_Analysis_Fr_Consumed_W$DeltaCont<-Individual_Analysis_Fr_Consumed_W$Contamination-Individual_Analysis_Fr_Consumed_W$InContamination
Individual_Analysis_Fr_Consumed$DeltaCont<-Individual_Analysis_Fr_Consumed$Contamination-Individual_Analysis_Fr_Consumed$InContamination
Individual_Analysis_Fr_Consumed$DeltaContConsumed<-Individual_Analysis_Fr_Consumed$ContConsumed-Individual_Analysis_Fr_Consumed$InContamination

IA_Wash<-Individual_Analysis_Fr_Consumed_W[,c(1,18)]
IA_Cont<-Individual_Analysis_Fr_Consumed[,c(1,18)]
IA_Wrapp<-Individual_Analysis_Fr_Consumed[,c(1,19)]
IA_Wash$Type<-"Wash"
IA_Cont$Type<-"Before Wrap"
IA_Wrapp$Type<-"After Wrap"

names(IA_Wrapp)[2]<-"DeltaCont"

IA_All<-bind_rows(IA_Wash,IA_Cont,IA_Wrapp)

IA_Wash_c<-Individual_Analysis_Fr_Consumed_W[,c(1,4)]
IA_Cont_c<-Individual_Analysis_Fr_Consumed[,c(1,4)]
IA_Wrapp_c<-Individual_Analysis_Fr_Consumed[,c(1,5)]

IA_Wash_c$Type<-"Wash"
IA_Cont_c$Type<-"Before Wrap"
IA_Wrapp_c$Type<-"After Wrap"
names(IA_Wrapp_c)[2]<-"Contamination"

IA_All_c<-bind_rows(IA_Wash_c,IA_Cont_c,IA_Wrapp_c)

IA_All_cl<-Func_Convert_Log(IA_All_c,"Contamination")

#Contamination with log
ggplot(data = IA_All_cl,aes(x=Contamination, fill=Type))+
  geom_density(colour="black", alpha=.3)
#Contaminatio no 0s 
ggplot(data = IA_All_c,aes(x=Contamination, fill=Type))+
  geom_density(colour="black", alpha=.3)+
  scale_x_log10()

ggplot(data = IA_All_cl,aes(x=Contamination, fill=Type))+
  geom_histogram(colour="black", alpha=.3, bins = 100)

IA_All_cd<-Func_Convert_Log(IA_All,"DeltaCont")

#Delta Contamination
ggplot(data = IA_All,aes(x=DeltaCont, fill=Type))+
  geom_density(colour="black", alpha=.3)+
  scale_x_log10()

#boxplot Contamination
ggplot(data = IA_All_c,aes( y=Contamination))+
  geom_boxplot(aes(fill=Type))+
  scale_y_log10()
