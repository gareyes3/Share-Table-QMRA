Func_Search_Data<-function(a,b,c,d){
  a[ sample( which(b==c),d),]  
}


Fr_Data.Frame85[ sample( which(Fr_Data.Frame85$Location=="Selection Table" & Fr_Data.Frame85$Place.in.Bin == Row_no_Fr),1),] 


Fr_Data.Frame85<-data.frame("Apple No." = 1:Initial_Fr,
                          "Location"= "Selection Table",
                          "Contamination" = as.numeric("0"),
                          "Meal" = "1",
                          "History" = "", 
                          "STtimes"= as.numeric("0"),
                          "PickedFBin" = as.numeric("0"),
                          "BinRow" = rep(1:(Initial_Fr/Row_size),times=1,each=20),
                          stringsAsFactors = FALSE
)

#a. Data frame b. dataframe$column c. "Word" d. datafrmae$column 2 e.Row_no_Fr (row from which we will pick) f =1
Func_Search_Data2<-function(a,b,c,d,e,f){
  a[ sample_n( which(b==c & d == e),f),] 
}



if(No_Consumed_Service<=Row_size_Fr){
  Row_no_Fr<-1
  } else if (No_Consumed_Service>Row_size_Fr & No_Consumed_Service<=Row_size_Fr*2){
  Row_no_Fr<-2
  } else if (No_Consumed_Service>Row_size_Fr*2 & No_Consumed_Service<=Row_size_Fr*3){
    Row_no_Fr<-3
  }else if (No_Consumed_Service>Row_size_Fr*3 & No_Consumed_Service<=Row_size_Fr*4){
    Row_no_Fr<-4
  }else if (No_Consumed_Service>Row_size_Fr*4 & No_Consumed_Service<=Row_size_Fr*5){
    Row_no_Fr<-5
  }else if (No_Consumed_Service>Row_size_Fr*5 & No_Consumed_Service<=Row_size_Fr*6){
    Row_no_Fr<-6
  }else if (No_Consumed_Service>Row_size_Fr*6 & No_Consumed_Service<=Row_size_Fr*7){
    Row_no_Fr<-7
  }else if (No_Consumed_Service>Row_size_Fr*7 & No_Consumed_Service<=Row_size_Fr*8){
    Row_no_Fr<-8
  }else if (No_Consumed_Service>Row_size_Fr*8 & No_Consumed_Service<=Row_size_Fr*9){
    Row_no_Fr<-9
  }else if (No_Consumed_Service>Row_size_Fr*9 & No_Consumed_Service<=Row_size_Fr*10){
    Row_no_Fr<-10
  }else if (No_Consumed_Service>Row_size_Fr*10 & No_Consumed_Service<=Row_size_Fr*11){
    Row_no_Fr<-11
  }else if (No_Consumed_Service>Row_size_Fr*11 & No_Consumed_Service<=Row_size_Fr*12){
    Row_no_Fr<-12
  }else if (No_Consumed_Service>Row_size_Fr*12 & No_Consumed_Service<=Row_size_Fr*13){
    Row_no_Fr<-13
  }
#a = No_Consumed_Service, b = Row_size_Fr c = Row_no_Fr
Func_Rows_Change<-function(a,b,c){
  if(a<=b){
    c<-1
  } else if (a>b & a<=b*2){
    c<-2
  } else if (a>b*2 & a<=b*3){
    c<-3
  }else if (a>b*3 & a<=b*4){
    c<-4
  }else if (a>b*4 & a<=b*5){
    c<-5
  }else if (a>b*5 & a<=b*6){
    c<-6
  }else if (a>b*6 & a<=b*7){
    c<-7
  }else if (a>b*7 & a<=b*8){
    c<-8
  }else if (a>b*8 & a<=b*9){
    c<-9
  }else if (a>b*9 & a<=b*10){
    c<-10
  }else if (a>b*10 & a<=b*11){
    c<-11
  }else if (a>b*11 & a<=b*12){
    c<-12
  }else if (a>b*12 & a<=b*13){
    c<-13
  }
}

Row_no_Fr<-Func_Rows_Change(No_PickedFBin,Row_size_Fr,Row_no_Fr)

library(dplyr)
No_Consumed_Service<-Fr_Data.Frame$Location == "Cons" 

New.Df<-sample_n((subset(Fr_Data.Frame, Location=="Selection Table" & BinRow == 5 )),1)


#sample_n(Fr_Data.Frame[which(Fr_Data.Frame$Location=="Selection Table" & Fr_Data.Frame$BinRow == Row_no_Fr),],1) 


Func_Search_Data3<-function(a,b,c,d,e,f){
  sample_n(a[which(b==c & d == e),],f)  
}

Func_Search_Data3(Fr_Data.Frame,Fr_Data.Frame$Location, "Selection Table", Fr_Data.Frame$BinRow, Row_no_Fr,1)


#subset<-Fr_Data.Frame[which(Fr_Data.Frame$Location=="Selection Table"),]
#subset<-head(subset,n=Row_size_Fr)

Func_seach_Data4<-function(a,b,c,d){
  subset<-a[which(b==c),]
  subset<-head(subset,n=d)
  sample_n(subset,1)
  }

Search.df.fr_touched<-Func_seach_Data4(Fr_Data.Frame,Fr_Data.Frame$Location,"Selection Table",Row_size_Fr)


nrow((Fr_Data[which(Fr_Data$Location == "Discarded"),]))
