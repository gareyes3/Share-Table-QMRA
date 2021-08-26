Trial22<-Individual_Analysis_Fr_CopOFFWash

Trial22<-Trial22 %>% 
  group_by(ID) %>% 
  filter(TotServices==max(TotServices)) %>% 
  filter(Location== "Consumed")%>% 
  filter(WashHistory>0)


sum(Trial22$WashHistory>0 && Trial22$STtimes>0)/nrow(Trial22)

(Trial22$WashHistory>0 && Trial22$STtimes>0)


Trial23<-Trial22 %>% 
  group_by(ID) %>% 
  filter(TotServices==max(TotServices)) %>% 
  filter(Location== "Consumed") %>% 
  filter(STtimes>0)

Trial24<-Trial22 %>% 
  group_by(ID) %>% 
  filter(TotServices==max(TotServices)) %>% 
  filter(Location== "Consumed") %>% 
  filter(STtimes==0)

mean(Trial23$to)

Touches_Number23<-lengths(regmatches(Trial23$History, gregexpr("Touched", Trial23$History)))
mean(Touches_Number23)
Touches_Number24<-lengths(regmatches(Trial24$History, gregexpr("Touched", Trial24$History)))
mean(Touches_Number24)
