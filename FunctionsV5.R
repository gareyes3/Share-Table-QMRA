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

#Function to now the bin we are in
#a = No_PickedFBin, b = Row_size_Fr c = Row_no_Fr
Func_Rows_Change<-function(a,b,c){
  if(a<b){
    c<-1
  } else if (a>=b & a<b*2){
    c<-2
  } else if (a>=b*2 & a<b*3){
    c<-3
  }else if (a>=b*3 & a<b*4){
    c<-4
  }else if (a>=b*4 & a<b*5){
    c<-5
  }else if (a>=b*5 & a<b*6){
    c<-6
  }else if (a>=b*6 & a<b*7){
    c<-7
  }else if (a>=b*7 & a<b*8){
    c<-8
  }else if (a>=b*8 & a<b*9){
    c<-9
  }else if (a>=b*9 & a<b*10){
    c<-10
  }else if (a>=b*10 & a<b*11){
    c<-11
  }else if (a>=b*11 & a<b*12){
    c<-12
  }else if (a>=b*12 & a<b*13){
    c<-13
  }
}

#a. Data frame b. dataframe$column c. "Word" d. datafrmae$column 2 e.Row_no_Fr (row from which we will pick) f =1
Func_Search_Data2<-function(a,b,c,d,e,f){
  a[ sample( which(b==c & d == e),f),] 
}


#function search data 3
#a =data frame b=column name c="column want" d = 2 column name e= number of binrow f, samples collected

Func_Search_Data3<-function(a,b,c,d,e,f){
  sample_n(a[which(b==c & d == e),],f)  
}

Func_seach_Data4<-function(a,b,c,d){
  subset<-a[which(b==c),]
  subset<-head(subset,n=d)
  sample_n(subset,1)
}


