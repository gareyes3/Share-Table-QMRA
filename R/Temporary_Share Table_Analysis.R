# ST comparison -------------------------------------------------


#STEP 1: ST off

#1. Start from here
Individual_Analysis_Fr<-bind_rows(List_Sens_Fr)

#2. find the dupplicates
#this step filters replicated based on the ID
Individual_Analysis_Fr<-Individual_Analysis_Fr %>% 
  group_by(ID) %>% 
  filter(TotServices==max(TotServices))

#3. This created the data frame for items when washed was on
Individual_Analysis_Fr_SToff<-Individual_Analysis_Fr

#4. Narrowing down to consumed Items for exposure compisons
Individual_Analysis_Fr_Consumed_SToff<-Individual_Analysis_Fr_SToff[which(Individual_Analysis_Fr_SToff$Location == "Consumed"),]

#5 Delta Contaminations
Individual_Analysis_Fr_Consumed_SToff$DeltaCont<-Individual_Analysis_Fr_Consumed_SToff$Contamination-Individual_Analysis_Fr_Consumed_SToff$InContamination



#STEP 2: ST on


#1. Start from here
Individual_Analysis_Fr<-bind_rows(List_Sens_Fr)

#2. find the dupplicates
#this step filters replicated based on the ID
Individual_Analysis_Fr<-Individual_Analysis_Fr %>% 
  group_by(ID) %>% 
  filter(TotServices==max(TotServices))

#3. This created the data frame for items when washed was on
Individual_Analysis_Fr_ST<-Individual_Analysis_Fr

#4. Narrowing down to consumed Items for exposure compisons
Individual_Analysis_Fr_Consumed_ST<-Individual_Analysis_Fr_ST[which(Individual_Analysis_Fr_ST$Location == "Consumed"),]

#5 Delta Contaminations
Individual_Analysis_Fr_Consumed_ST$DeltaCont<-Individual_Analysis_Fr_Consumed_ST$Contamination-Individual_Analysis_Fr_Consumed_ST$InContamination




#Step 5: Wrap for the DF, in order to have them as the same type

#1 Creating DF for the DElta Contamination
IA_SToff<-Individual_Analysis_Fr_Consumed_SToff[,c(1,18)]
IA_ST<-Individual_Analysis_Fr_Consumed_ST[,c(1,18)]

IA_SToff$Type<-"ST off"
IA_ST$Type<-"ST on"

IA_AllST<-bind_rows(IA_SToff,IA_ST)

#2. Creating Data Frame for Contamination
IA_SToff_c<-Individual_Analysis_Fr_Consumed_SToff[,c(1,4)]
IA_ST_c<-Individual_Analysis_Fr_Consumed_ST[,c(1,4)]


IA_SToff_c$Type<-"ST off"
IA_ST_c$Type<-"ST on"


IA_AllST_c<-bind_rows(IA_SToff_c,IA_ST_c)

IA_AllST_c[IA_AllST_c==0]<-(10^-20)

#HEre We need to convert 0s to really small or to log.Save as alternate DF please.

IA_AllST_cLog<-IA_AllST_c
IA_AllST_cLog$Contamination<-log10(IA_AllST_cLog$Contamination)


#Contamination with log
ggplot(data = IA_AllST_cLog,aes(x=Contamination, fill=Type, linetype=Type))+
  geom_density(colour="black", alpha=.2)+
  scale_x_continuous(n.breaks = 10)+
  xlab("Contamination log PFU/Item")+
  ylab("Density")+
  ggtitle("Density Curve ST on vs Off")+ 
  theme(plot.title = element_text(hjust = 0.5))

#Funtion for boxplot
give.n <- function(x){
  return(c(y = median(x)*1.05, label = length(x))) 
  # experiment with the multiplier to find the perfect position
}

#function median
p_meds <- ddply(IA_AllST_cLog, .(Type), summarise, median = median(Contamination))

#boxplot Contamination
ggplot(data = IA_AllST_cLog,aes( y=Contamination, x=Type))+
  geom_boxplot(aes(fill=Type),varwidth = TRUE)+
  ylab("Contamination Log PFU/Item")+
  xlab("Intervention Type")+
  ggtitle("Boxplot ST on vs off comparison")+ 
  theme(plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(n.breaks = 15)+
  stat_summary(fun.data = give.n, geom = "text", fun = median,vjust = -3)+
  geom_text(data = p_meds, aes(x = Type, y = median, label = median), 
            size = 3, vjust = +1.5, color="blue")

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
  scale_x_continuous(trans = pseudolog10_trans, n.breaks = 10)

#boxplot Contamination
ggplot(data = IA_All,aes( y=DeltaCont, x=Type))+
  geom_boxplot(aes(fill=Type))+
  scale_y_continuous(trans = pseudolog10_trans)+
  geom_text(data = p_meds, aes(x = Type, y = mean, label = mean), 
            size = 3, vjust = -1.5)+
  ggtitle("Boxplot Change in Contamination")+ 
  theme(plot.title = element_text(hjust = 0.5))

sum(Individual_Analysis_Fr_Consumed_W$WashHistory>0)

library(ggallin)
library(plyr)
p_meds <- ddply(IA_All, .(Type), summarise, mean = mean(DeltaCont))



####Other Initial Contaminations


IA_Wrapp_Ini<-Individual_Analysis_Fr_Consumed_W[,c(1,6)]

IA_Wrapp_Ini$Type<-"Initial"

names(IA_Wrapp_Ini)[2]<-"Contamination"

IA_All_cIni<-bind_rows(IA_Wash_c,IA_Cont_c,IA_Wrapp_c, IA_Wrapp_Ini)

IA_All_clIni<-Func_Convert_Log(IA_All_cIni,"Contamination")



ggplot(data = IA_All_clIni,aes(x=Contamination, fill=Type, linetype=Type))+
  geom_density(colour="black", alpha=.2)+
  scale_x_continuous(n.breaks = 10)+
  xlab("Contamination log PFU/Item")+
  ylab("Density")+
  ggtitle("Density Curves Interventions")+ 
  theme(plot.title = element_text(hjust = 0.5))

#boxplot Contamination
ggplot(data = IA_All_clIni,aes( y=Contamination, x=Type))+
  geom_boxplot(aes(fill=Type))+
  ylab("Contamination Log PFU/Item")+
  xlab("Intervention Type")+
  ggtitle("Boxplot Interventions")+ 
  theme(plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(n.breaks=10)

median(IA_Wrapp$DeltaCont)


View(IA_All_clIni)
