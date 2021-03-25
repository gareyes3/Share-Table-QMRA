
#Boxplot Together

df_ill_Week_Tog_Box<-bind_rows(df_ill_Week_ON_Box_Prev,df_ill_Week_OFF_Box_Prev,df_ill_Week_ONWash_Box_Prev,df_ill_Week_ONWr_Box_Prev,df_ill_Week_OFFWash_Box_Prev,df_ill_Week_OFFWr_Box_Prev,df_ill_Week_Exc_Box_Prev,df_ill_Week_STClosed_Box_Prev,df_ill_Week_STAside_Box_Prev,df_ill_Week_Touch_Box_Prev,df_ill_Week_TouchST_Box_Prev,df_ill_Week_WStation_Box_Prev,df_ill_Week_WBucket_Box_Prev)

Ill_Prev_Comb<-ggplot(df_ill_Week_Tog_Box, aes(x = variable, y = Prev, fill=Int)) + 
  geom_boxplot() + 
  theme_bw()+
  ylab("Prevalence of Ill Students")+
  xlab("Week #") +
  ggtitle("ST Status: Compaison Weekly Dose Response Distribution: Prevalence Ill Students")+ 
  theme(plot.title = element_text(hjust = 0.5))+
  labs(fill = "Interventions")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggsave(filename = "Ill_Prev_Comb3.png", path = pathfolder,width = 40,height = 10,)
print(Ill_Prev_Comb)





