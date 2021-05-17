
Individual_Analysis_Fr_CopONWash %>% 
  fil

DF_Washed<-Individual_Analysis_Fr_CopONWash %>% 
  group_by(ID) %>% 
  filter(TotServices==max(TotServices))%>%
  filter(Location=="Consumed")
DF_Washed<-DF_Washed %>% 
  filter(WashHistory>0)


DF_Washed_OFF<-Individual_Analysis_Fr_CopOFFWash %>% 
  group_by(ID) %>% 
  filter(TotServices==max(TotServices))%>%
  filter(Location=="Consumed")
DF_Washed_OFF<-DF_Washed_OFF %>% 
  filter(WashHistory>0)


Consumed_two<-Individual_Analysis_Fr_CopON %>%   
  group_by(ID) %>% 
  filter(TotServices==max(TotServices))%>%
  filter(Location=="Consumed") 
Consumed_two<-Consumed_two %>% 
  group_by(ConsumedBy) %>% 
  count()

Consumed_two %>% 
  filter(n>1)


ST_Aside<-Individual_Analysis_Fr_CopSTAside %>% 
  group_by(ID) %>% 
  filter(TotServices==max(TotServices))





Touches_ON<-Individual_Analysis_Fr_CopON %>% 
  group_by(ID) %>% 
  filter(TotServices==max(TotServices))%>%
  filter(Location=="Consumed")
library(stringr)
TOuhcesONCount<-str_count(Touches_ON$History, "Touched")

mean(TOuhcesONCount)

Touches_OFF<-Individual_Analysis_Fr_CopOFF %>% 
  group_by(ID) %>% 
  filter(TotServices==max(TotServices))%>%
  filter(Location=="Consumed")
library(stringr)
TOuhcesOffCount<-str_count(Touches_OFF$History, "Touched")

mean(TOuhcesOffCount)
