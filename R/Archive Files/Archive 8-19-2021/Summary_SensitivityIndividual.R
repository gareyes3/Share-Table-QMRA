
# Analysis of Individual Fruit Items --------------------------------------

#1. Start from here
  #This section creates a data frame with every item, Including repeated items, since it is a snapshot of the end of every service
  Individual_Analysis_Fr<-bind_rows(List_Sens_Fr)

#2. find the dupplicates
  #this step filters replicated based on the ID
  Individual_Analysis_Fr<-Individual_Analysis_Fr %>% 
    group_by(ID) %>% 
    filter(TotServices==max(TotServices))

#3. Create a vector to explain if something were in the share table
  STYN_Vector<-c()
  for (i in 1:nrow(Individual_Analysis_Fr)){
    if (Individual_Analysis_Fr[i,colnames(Individual_Analysis_Fr)=="STtimes"]>0){
      STYN_Vector<-c(STYN_Vector,TRUE)
    }else if(Individual_Analysis_Fr[i,colnames(Individual_Analysis_Fr)=="STtimes"]==0){
      STYN_Vector<-c(STYN_Vector,FALSE)
    }
  }

#4. Number of touches

  Touches_Number<-lengths(regmatches(Individual_Analysis_Fr$History, gregexpr("Touched", Individual_Analysis_Fr$History)))

#5. Running Average Contamination of student that touches specific fruit. 
  
  ListTouches<-strsplit(Individual_Analysis_Fr$TouchesContHist,",")
  ListTouches<-lapply(ListTouches, function(x) x[x!="NA"])
  ListTouches<-lapply(ListTouches, function(x) as.numeric(x))
  TouchesContHist<-sapply(X = ListTouches, FUN = mean)
  Individual_Analysis_Fr$TouchesContHistAvr<-TouchesContHist
  sum(is.na(TouchesContHist))

#6. Creation of Dataframe for indvidual Analysis 

  Analysis_Individual<-data.frame(
    "InContamination"=Individual_Analysis_Fr$InContamination,
    "TouchesNo"=Touches_Number,
    "TouchesContHistAvr"=Individual_Analysis_Fr$TouchesContHistAvr,
    "STTimes"= Individual_Analysis_Fr$STtimes,
    "TotServices"=Individual_Analysis_Fr$TotServices,
    "Shared"=STYN_Vector,
    "DeltaCont"= (Individual_Analysis_Fr$Contamination-Individual_Analysis_Fr$InContamination),
    "Contamination" = Individual_Analysis_Fr$Contamination
  )

#7.  Sensitivity Analysis of table

  Pcc2<-pcc(X=Analysis_Individual[,1:6], y=Analysis_Individual$Contamination,rank = TRUE,nboot = 1000)
  Pcc2
  plot(Pcc2)
  
  Pcc3<-pcc(X=Analysis_Individual[,1:6], y=Analysis_Individual$DeltaCont,rank = TRUE,nboot = 1000)
  Pcc3
  plot(Pcc3)


#8 Visuals , remaing the columns to that no error in ggplot
  names(Pcc2$PRCC)=c("original", "bias" ,"std.error", "minci","maxci")

#Ggplot, here is similar to a tornado plot. Also there are error bars on the 95th percentile
  

  
  ggplot(data = Pcc2$PRCC, aes(x=rownames(Pcc2$PRCC),y=original ))+
    geom_bar(stat = "identity", position = "identity")+
    geom_errorbar(aes(ymin=minci, ymax=maxci), width=.1,col="blue")+
    ylab("Partial Correlation Coefficient")+
    xlab("Action")+
    ggtitle("Sensitivity Analysis Individual Items: Final Contamination PFU/Item")+
    scale_x_discrete(labels=c("Initial Contamination", "Shared", "Times in ST", "Total services","Overall Transfer From Touches", "Number of Touches")) +
    coord_flip()+
    theme(plot.title = element_text(hjust = 0.5))+
    theme(text = element_text(size=13))

# More graphs
  mean(Analysis_Individual$TouchesNo)
  View(Analysis_Individual)
  
  Analysis_Individual$InContamination[Analysis_Individual$InContamination==0]<-(10^-5)
  Analysis_Individual$Contamination[Analysis_Individual$Contamination==0]<-(10^-5)
  #Analysis_Individual$TouchesContHistAvr[Analysis_Individual$TouchesContHistAvr==0]<-(10^-5)
  Analysis_Individual$InContamination<-log10(Analysis_Individual$InContamination)
  Analysis_Individual$Contamination<-log10(Analysis_Individual$Contamination)
  #Analysis_Individual$TouchesContHistAvr<-log10(Analysis_Individual$TouchesContHistAvr)

  Analysis_Individual$TouchesContHistAvr[Analysis_Individual$TouchesContHistAvr>0]<-"Pos"
  Analysis_Individual$TouchesContHistAvr[Analysis_Individual$TouchesContHistAvr<0]<-"Neg"
  Analysis_Individual$TouchesContHistAvr[Analysis_Individual$TouchesContHistAvr==0]<-"Zero"
  
  
  ggplot(data = Analysis_Individual, aes(x=Contamination, y = InContamination )) + 
    #scale_x_log10(n.breaks=10)+
    #scale_y_log10(n.breaks=10)+
    geom_point(aes(col=TouchesContHistAvr))+
    geom_abline(slope=1, intercept=0)+
    ylab("Contamination Initial Log  GEC/Item")+
    xlab("Contamination Final Log GEC/Item")+
    scale_color_gradient(guide="legend",name="Average TR",low="green", high="red",trans = scales::pseudo_log_trans(base = 10),n.breaks=5)+
    ggtitle("Effect of Touches final Contamination")+
    theme(plot.title = element_text(hjust = 0.5))+
    theme(text = element_text(size=12))  
  
  ggplot(data = Analysis_Individual, aes(x=DeltaCont, y = TouchesContHistAvr )) + 
    scale_x_log10()+
    scale_y_log10()+
    geom_point(aes(col=Contamination))+
    xlab("Change in Contamination")+
    ylab("Contamination on student hands that touched fruit (average)")+
    labs(col = "Final Contamination")+
    scale_color_gradient(low="green", high="red", trans="log")
  
  ggplot(data = Analysis_Individual, aes(x=DeltaCont, y = TouchesContHistAvr )) + 
    scale_x_log10()+
    scale_y_log10()+
    geom_point()+
    geom_smooth()
  
  #Melted Data to create one whole graph
  d <- melt(Analysis_Individual, id.vars="Contamination")
  ggplot(data =d , aes(Contamination,value, col=variable)) + 
    scale_x_log10()+
    scale_y_log10(c("Contamination","TouchesContHistAvr"))+
    geom_point() + 
    stat_smooth() +
    facet_wrap(~variable, scales = "free")+
    xlab("Contamination Items Consumed Log CFU/Fruit")+
    ylab("Values")

#END of Analysis for Inividual ITems


  
    