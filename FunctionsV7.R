# FUNCTIONS ---------------------------------------------------------------


#--Function for Contamination of Specific Item at Tray
# a = Contamination original b= Tra 1 c= Tra 2 d= Pick yes or no , e= Cont h after 
Funct_Cont_Tray_Item<-function(a,b,c,d,e){
  ifelse(((a+b)-(c))*d>e,
         e,
         ((a+b)-(c))*d)
}


#--Function for searching data frame--
# a=Data Frame looking b= Colum in data frame c= Keywrd using "", d=number of selections
Func_Search_Data<-function(a,b,c,d){
  a[ sample( which(b==c),d),]  
}

Func_seach_Data4<-function(a,b,c,d){
  subset<-a[which(b==c),]
  subset<-head(subset,n=d)
  sample_n(subset,1)
}

#Items touched during selection: 
#a = data frame, b=#touched c#"contamination" col name
Func_Index_DF<-function(a,b,c){
  as.numeric(a[b,colnames(a)==c])
}


#Function Normal
F_norm<-function(a,b,c){
  rnorm(a, b,c)
}


#Log reduction function
  #a ,data frame column, b log reduction

Func_Logred<-function(a,b){
    a*(10^b)
  }

Func_Adding_Time<-function(Column, Time){
  (Column + Time)
}


# Growth Model -------------------------------------------------------------



Func_Growth_Sto_Ecoli<-function(Condition,DF,TimeVar){
  b<-.023
  k<-rnorm(1,.013,.001)/2.303
  Tmin<-(1.17)
  if(Condition== "refrigerated"){
    if(Temp_Ref<5){
      for (i in 1:nrow(DF)){
        Die_off<-((-k)*TimeVar)
        N<-log10(DF[i,colnames(DF)== "Contamination"])
        Con_Final<-ifelse(N==0,N,N + Die_off)
        Con_Final<-10^Con_Final
        DF[i,colnames(DF)== "Contamination"]<-Con_Final
        DF<<-DF
      }
    } else if (Temp_Ref>=5){
      rate<-(b*(Temp_Ref-Tmin))^2/2.303
      for (i in 1:nrow(DF)){
        Con_Change<-rate*TimeVar
        N<-log10(DF[i,colnames(DF)== "Contamination"])
        Con_Final<-ifelse(N==0,N,N + Con_Change )
        Con_Final<-10^Con_Final
        DF[i,colnames(DF)== "Contamination"]<-Con_Final
        DF<<-DF
      }
    }
  } else if (Condition=="room temp"){
    if(Temp_RT<5){
      for (i in 1:nrow(DF)){
        Die_off<-(-k)*TimeVar
        N<-log10(DF[i,colnames(DF)== "Contamination"])
        Con_Final<-ifelse(N==0,N,N + Die_off)
        Con_Final<-10^Con_Final
        DF[i,colnames(DF)== "Contamination"]<-Con_Final
        DF<<-DF
      }
    } else if (Temp_RT>=5){
      rate<-(b*(Temp_RT-Tmin))^2/2.303
      for (i in 1:nrow(DF)){
        Con_Change<-rate*TimeVar
        N<-log10(DF[i,colnames(DF)== "Contamination"])
        Con_Final<-ifelse(N==0,N,N + Con_Change )
        Con_Final<-10^Con_Final
        DF[i,colnames(DF)== "Contamination"]<-Con_Final
        DF<<-DF
      }
    }  
  }
}

