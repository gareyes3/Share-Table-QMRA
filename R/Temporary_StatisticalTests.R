setwd("C:/Users/gareyes3/Documents/GitHub/Share-Table-QMRA/R") 

VIolinPlot<-ggplot(data = binded_prevalences, mapping = aes(y= Prev, x=factor(Int,c('STOFF','OFFWash', 'OFFWr',  'STON', 'ONwash',  'ONWr','Exc','XSTClosed') ), fill=factor(Int,c('STOFF','OFFWash', 'OFFWr',  'STON', 'ONwash',  'ONWr','Exc','XSTClosed') )))+
  geom_violin()+
  geom_boxplot(width=0.1)+
  labs(fill = "Intervention")+
  xlab(label = "Intervention")+
  scale_fill_manual(values = colors_11, labels = c("Baseline: ST OFF, No Intervention", 
                                                   "ST OFF, Wash ON",
                                                   "ST OFF, Wrapping ON",
                                                   "ST ON, No Interventions",
                                                   "ST ON, Wash ON",
                                                   "ST OFF, Wrapping ON",
                                                   "STON, Ill Exclusion",
                                                   "Re-service off"))

ggsave(filename = "Violin.png",path = "Dose Response",width = 10,height = 8,)

library(PMCMR)
install.packages("PMCMR")

View(binded_prevalences)
Anova_1<-aov(Prev~Int, data= binded_prevalences)
Anova_1
Tukey2<-TukeyHSD(Anova_1)
Tukey2
duncan.test(Anova_1, "Prev", alpha = 0.05)
kruskal_1<-kruskal.test(Prev~Int, data= binded_prevalences)
kruskal_1

binded_prevalences$Int<-as.factor(binded_prevalences$Int)
posthoc.kruskal.dunn.test(Prev ~ Int, data = binded_prevalences, p.adjust="bonf")
levels(binded_prevalences$Int)


