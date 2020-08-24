# OUTPUTS FOR MEAL 1 -----------------------------------------------------




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
        geom_histogram(alpha = 0.5, position = 'identity', boundary=-1, binwidth = 1 ) +
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
  

# CREATION OF VISUALS -----------------------------------------------------


  # Exposure Graphs Staggered ------------------------------------------------
  
  Exposure_Staggered_Function2(Total_Consumed_Fr_Bind,Contamination = Contamination, Type = Type, "Total Exposure Fruit", "Contamination per Fruit log CFU/Fruit", "Count of Fruit Consumed")
  Exposure_Staggered_Function2(Total_Consumed_Pss_Bind,Contamination = Contamination, Type = Type, "Total Exposure Pss","Contamination per Pss log CFU/Pss", "Count of PSs Consumed")
  Exposure_Staggered_Function2(Total_Consumed_Pre_Bind,Contamination = Contamination, Type = Type, "Total Exposure Pre", "Contamination per Pre log CFU/Pre", "Count of Pre Consumed")


  # Bar Chart for Location --------------------------------------------------

  if(Resharing_YN==0){
    Location_BarC_Function(Fr_Data,"Fruit Location Re-Service & Re-Sharing off")
    Location_BarC_Function(Pss_Data,"Pss Location Re-Service & Re-Sharing off")  
    Location_BarC_Function(Pre_Data,"Pre Location Re-Service & Re-Sharing off")  
  }
  
  if(Resharing_YN==1){
    Location_BarC_Function(Fr_Data,"Fruit Location Re-Service & Re-Sharing on")
    Location_BarC_Function(Pss_Data,"Pss Location Re-Service & Re-Sharing on")  
    Location_BarC_Function(Pre_Data,"Pre Location Re-Service & Re-Sharing on") 
  }
  
  # Histogram Exposure Visuals ----------------------------------------------
                                  #Fruit
  #Total Exposure
  Exposure_Plot_Function3(Total_Consumed_Fr, "Exposure total Fruit Consumed", "Contamination per Fruit log CFU/Fruit", "Count of Fruit Consumed")
  Exposure_Plot_Function3(Total_Consumed_Fr_Bind, "Exposure total Fruit Consumed", "Contamination per Fruit log CFU/Fruit", "Count of Fruit Consumed")
  #Total Exposure from Share Table Items
  Exposure_Plot_Function3(Total_Consumed_ST_Fr, "Exposure Consumed Fruit from Share Tables", "Contamination per Fruit log CFU/Fruit", "Count of Fruit Consumed")
  #Total Exposure from Selection Table Items
  Exposure_Plot_Function3(Total_Consumed_Sel_Fr, "Exposure Consumed Fruit from Selection Tables", "Contamination per Fruit log CFU/Fruit", "Count of Fruit Consumed")



  
                                  #Pss
  #Total Exposure
  Exposure_Plot_Function3(Total_Consumed_Pss, "Exposure total Consumed Pss", "Contamination per Pss log CFU/Pss", "Count of PSs Consumed")
  #Total Exposure from Share Table Items
  Exposure_Plot_Function3(Total_Consumed_ST_Pss, "Exposure Consumed Pss from Share Tables" ,"Contamination per Pss log CFU/Pss", "Count of PSs Consumed")
  #Total Exposure from Selection Table Items
  Exposure_Plot_Function3(Total_Consumed_Sel_Pss, "Exposure Consumed Pss from Selection Table" ,"Contamination per Pss log CFU/Pss", "Count of PSs Consumed")
                                
                                  #Pre    
  #Total Exposure
  Exposure_Plot_Function3(Total_Consumed_Pre, "Exposure total Consumed Pre", "Contamination per Pre log CFU/Pre", "Count of Pre Consumed")
  #Total Exposure from Share Table Items
  Exposure_Plot_Function3(Total_Consumed_ST_Pre, "Exposure Consumed Pre from Share Tables",  "Contamination per Pre log CFU/Pre", "Count of Pre Consumed")
  #Total Exposure from Selection Table Items
  Exposure_Plot_Function3(Total_Consumed_Sel_Pre, "Exposure Consumed Pre from Selection Table", "Contamination per Pre log CFU/Pre", "Count of Pre Consumed")
  
  
  #Sum of the total partciles consumed.   
  sum(Total_Consumed_Fr$Contamination)


  

  # Boxplot Visuals ---------------------------------------------------------


  Box_Plot_Function(Total_Consumed_Fr_Bind, "Exposure per location fruit")
  Box_Plot_Function(Total_Consumed_Pss_Bind, "Exposure per location Pss")
  Box_Plot_Function(Total_Consumed_Pre_Bind, "Exposure per location Pre")


  Box_Plot_Function2(Total_Consumed_Fr,"Exposure of Total Consumed Fruit")
  Box_Plot_Function2(Total_Consumed_Pss,"Exposure of Total Consumed Pss")
  Box_Plot_Function2(Total_Consumed_Pre,"Exposure of Total Consumed Pre")
  
  
  Disc_Consumed_Function(Fruit_Data_Consumed_Discarded, " Fruit Consumed vs Discarded Share Tables ON")
  Disc_Consumed_Function(Pss_Data_Consumed_Discarded, " Pss Consumed vs Discarded Share Tables ON")
  Disc_Consumed_Function(Pre_Data_Consumed_Discarded, " Pre Consumed vs Discarded Share Tables ON")
  





