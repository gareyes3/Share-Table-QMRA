
# FUNCTIONS ---------------------------------------------------------------


# Exposure Plots ----------------------------------------------------------

Exposure_Plot_Function<-function(Consumed = Total_Consumed_Fr ,Title = "Insert Title", xlab, ylab){
  ggplot(Consumed, aes(x=Contamination)) + 
    geom_histogram( fill="#69b3a2", color="#e9ecef", binwidth = (Av_ic/60), boundary=.99) +
    ggtitle(Title)+
    theme(plot.title = element_text(hjust = 0.5))+
    stat_bin(binwidth=(Av_ic/60), geom="text", size=3.5 ,aes(label=..count.., vjust=-.3), boundary = .99)+
    scale_x_continuous(breaks = seq(0,Av_ic,(Av_ic/60)))+
    labs(x= xlab, y= ylab)+
    theme(axis.text.x=element_text(angle=90, hjust=1))
}

Exposure_Plot_Function2<-function(Consumed = Total_Consumed_Fr ,Title = "Insert Title", xlab, ylab){
  ggplot(Consumed, aes(x=Contamination)) + 
    geom_histogram( fill="#69b3a2", color="#e9ecef", binwidth = (500), boundary=.99) +
    ggtitle(Title)+
    theme(plot.title = element_text(hjust = 0.5))+
    stat_bin(binwidth=(500), geom="text", size=3.5 ,aes(label=..count.., vjust=-.3), boundary = .99)+
    scale_x_continuous(breaks = seq(0,15000,(500)))+
    labs(x= xlab, y= ylab)+
    theme(axis.text.x=element_text(angle=90, hjust=1))
}


Exposure_Plot_Function3<-function(Consumed = Total_Consumed_Fr ,Title = "Insert Title", xlab, ylab){
  ggplot(Consumed, aes(x=Contamination)) + 
    geom_histogram( fill="#69b3a2", color="#e9ecef", binwidth = 1, boundary= -1) +
    stat_bin(binwidth=1, geom="text", size=3.5 ,aes(label=..count.., vjust=-.3),boundary = -1 )+
    ggtitle(Title)+
    theme(plot.title = element_text(hjust = 0.5))+
    labs(x= xlab, y= ylab)+
    theme(axis.text.x=element_text(angle=90, hjust=1))
}

Exposure_Plot_Function4<-function(Consumed = Total_Consumed_Fr ,Title = "Insert Title", xlab, ylab){
  ggplot(Consumed, aes(x=MedianCont)) + 
    geom_histogram( fill="#69b3a2", color="#e9ecef", binwidth = 1, boundary= -1) +
    stat_bin(binwidth=1, geom="text", size=3.5 ,aes(label=..count.., vjust=-.3),boundary = -1 )+
    ggtitle(Title)+
    theme(plot.title = element_text(hjust = 0.5))+
    labs(x= xlab, y= ylab)+
    theme(axis.text.x=element_text(angle=90, hjust=1))
}



# Staggered Functions -----------------------------------------------------


Exposure_Staggered_Function<-function(ConsumedDF = Total_Consumed_Fr_Bind ,Contamination = Contamination ,Type = Type, Title = "Insert Title Here", xlab, ylab){
  ggplot(ConsumedDF, aes(x=Contamination, fill= Type)) + 
    geom_histogram(alpha = 0.5, position = 'identity',binwidth = (Av_ic/60), boundary=.99 ) +
    ggtitle(Title)+
    labs(x= xlab, y= ylab)+
    theme(plot.title = element_text(hjust = 0.5))
}

Exposure_Staggered_Function2<-function(ConsumedDF = Total_Consumed_Fr_Bind ,Contamination = Contamination ,Type = Type, Title = "Insert Title Here", xlab, ylab){
  ggplot(ConsumedDF, aes(x=Contamination, fill= Type)) + 
    geom_histogram(alpha = 0.5, position = 'identity', boundary=-1, binwidth = 1) +
    ggtitle(Title)+
    labs(x= xlab, y= ylab)+
    theme(plot.title = element_text(hjust = 0.5))
}


# Location Bar Chart ------------------------------------------------------

Location_BarC_Function<-function(Data, Title){
  ggplot(Data, aes(x=Service, fill=Location)) + 
    stat_count()+theme_minimal()+
    scale_x_continuous(breaks = seq(1,Food_Days, by = 1))+
    ggtitle(Title)+
    theme(plot.title = element_text(hjust = 0.5))
}


# Boxplot Function --------------------------------------------------------

Box_Plot_Function<-function(data = Total_Consumed_Fr_Bind ,title = "Insert Title Here"){
  ggplot(data=data, aes(x=Type, y=Contamination))+
    geom_boxplot(varwidth = TRUE,fill=c("#00AFBB", "#E7B800", "#FC4E07"), color="black")+
    stat_summary(fun=mean, shape=3, size=1, color="red", fill="red")+
    ggtitle(title)+
    theme(plot.title = element_text(hjust = 0.5))+
    ylab ("Contamination log CFU/g")+
    xlab ("Consumed From")
}

Box_Plot_Function2<-function(data = Total_Consumed_Fr_Bind ,title = "Insert Title Here"){
  ggplot(data=data, aes(x=Type, y=Contamination))+
    geom_boxplot(varwidth = TRUE,fill=c( "#FC4E07"), color="black")+
    stat_summary(fun=mean, shape=3, size=1, color="red", fill="red")+
    ggtitle(title)+
    theme(plot.title = element_text(hjust = 0.5))+
    ylab ("Contamination log CFU/g")+
    xlab ("Consumed From")
}


# Discarded Vs Not Discarded Function -------------------------------------

Disc_Consumed_Function<-function(data, title){
  ggplot(data, aes(x=Location)) + 
    geom_bar(stat = "count", fill= c("#00AFBB", "#E7B800"))+
    ggtitle(title)+
    theme(plot.title = element_text(hjust = 0.5))
}



# GGSave Functions --------------------------------------------------------
Func_GGsave<-function(PlotSaved, Path, Filename){
  ggsave(PlotSaved,
         path = Path,
         filename = paste(Filename, l  , ".jpg"),
         height = 6,
         width = 6,
         dpi = 300
  )
}

