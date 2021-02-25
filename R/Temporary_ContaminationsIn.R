View(df_ill_Week_OFF)
nrow(ST_OFF_Analysis)
Melted101<-melt(df_ill_Week_OFF)

summ1<-summary(Melted101$value)
summ2<-summary(Melted101$value)
summ1

Melted101 %>% 
  group_by(variable) %>% 
  summarise(mean(value))

sd(Melted101$value)

sd(Individual_Analysis_Fr_CopOFF$Contamination)


View(df_ill_Week_ON_Box_Prev)

sd(df_ill_Week_OFF_Box_Prev$Prev)
mean(df_ill_Week_OFF_Box_Prev$Prev)

Contaminations_OFF
sum(Contaminations_ON)

1.96*0.004887664/(0.01117482*5)
0.1714537^2
median(df_ill_Week_OFF_Box_Prev$Prev)
hist(df_ill_Week_OFF_Box_Prev$Prev)

sum(ST_ON_Analysis$Contamination , na.rm = TRUE)
sum(ST_ONWr_Analysis$Contamination , na.rm = TRUE)
sum(ST_ONWash_Analysis$Contamination , na.rm = TRUE)
sum(ST_OFF_Analysis$Contamination, na.rm = TRUE)
sum(ST_OFFWr_Analysis$Contamination, na.rm = TRUE)
sum(ST_OFFWash_Analysis$Contamination, na.rm = TRUE)

View(ST_ON_Analysis)


View(Individual_Analysis_Fr_CopON)

OneOne<-Individual_Analysis_Fr_CopONWash %>% 
  group_by(ID) %>% 
  filter(TotServices==max(TotServices))

OneTwo<-Individual_Analysis_Fr_CopON %>% 
  group_by(ID) %>% 
  filter(TotServices==max(TotServices))

OneThree<-Individual_Analysis_Fr_CopOFF %>% 
  group_by(ID) %>% 
  filter(TotServices==max(TotServices))

OneFour<-Individual_Analysis_Fr_CopOFFWr %>% 
  group_by(ID) %>% 
  filter(TotServices==max(TotServices))

OneFive<-Individual_Analysis_Fr_CopOFFWash %>% 
  group_by(ID) %>% 
  filter(TotServices==max(TotServices))

sum(OneOne$Contamination, na.rm = TRUE)
sum(OneTwo$Contamination, na.rm = TRUE)
sum(OneThree$Contamination,na.rm = TRUE)
sum(OneFour$Contamination,na.rm=TRUE)
sum(OneFive$Contamination,na.rm=TRUE)



