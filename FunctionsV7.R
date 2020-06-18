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


# Grwth Model -------------------------------------------------------------

Func_Enteric_Growth<-function(enteric,Condition,DF,Pickedvar){
  if(enteric== "E_coli"){
    N<-log10(Func_Index_DF(DF,Pickedvar,"Contamination"))
    b<-.023
    k<-rnorm(1,.013,.001)/2.303
    Tmin<-(1.17)
    if(Condition== "refrigerated"){
      if(Temp_Ref<5){
        Die_off<-(-k)*Time_Ref
        Con_Final<-ifelse(N==0,N,Die_off + N)
        Con_Final<-10^Con_Final
      } else if (Temp_Ref>=5){
        rate<-(b*(Temp_Ref-Tmin))^2/2.303
        Con_Change<-rate*Time_Ref
        Con_Final<-ifelse(N==0,N,Con_Change + N)
        Con_Final<-10^Con_Final
      }
    } else if (Condition == "room temp"){
      if(Temp_ST<5){
        Die_off<-(-k)*Time_ST
        Con_Final<-ifelse(N==0,N,Die_off + N)
        Con_Final<-10^Con_Final
      } else if (Temp_ST>=5){
        rate<-(b*(Temp_Ref-Tmin))^2/2.303
        Con_Change<-rate*Time_ST
        Con_Final<-ifelse(N==0,N,Con_Change + N)
        Con_Final<-10^Con_Final
      }
    }
  } else if (enteric == "salmonella" ){
    N<-log10(Func_Index_DF(DF,Pickedvar,"Contamination"))
    b<-.020
    k<-.0128
    Tmin<-(-0.571)
    if(Condition== "refrigerated"){
      if(Temp_Ref<7){
        Die_off<-(-k)*Time_Ref
        Con_Final<-ifelse(N==0,N,Die_off + N)
        Con_Final<-10^Con_Final
      } else if (Temp_Ref>=7){
        rate<-(b*(Temp_Ref-Tmin))^2/2.303
        Con_Change<-rate*Time_Ref
        Con_Final<-ifelse(N==0,N,Con_Change + N)
        Con_Final<-10^Con_Final
      }
    } else if (Condition == "room temp"){
      if(Temp_ST<7){
        Die_off<-(-k)*Time_ST
        Con_Final<-ifelse(N==0,N,Die_off + N)
        Con_Final<-10^Con_Final
        DF[Pickedvar,colnames(DF)== "Contamination"]<-Con_Final
      } else if (Temp_ST>=7){
        rate<-(b*(Temp_Ref-Tmin))^2/2.303
        Con_Change<-rate*Time_ST
        Con_Final<-ifelse(N==0,N,Con_Change + N)
        Con_Final<-10^Con_Final
        Con_Final<<-Con_Final
      }
    }
  }
  return(Con_Final)
}


