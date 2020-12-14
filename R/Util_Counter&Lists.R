# COUNTER & LISTS -------------------#DO NOT MODIFY-----------------------
#DO NOT MODIFY

##Contamination Pool Start# Not input
Cont_Fr<- 0
Cont_Pss<-0
Cont_Pre<-0

#vectors
V_Available_Fr<-c(0)
V_Available_Pss<-c(0)
V_Available_Pre<-c(0)

#Data Lists services
datalistFr=list()
datalistPss=list()
datalistPre=list()

#Datalist Days

datalistFr_days=list()
datalistPss_days=list()
datalistPre_days=list()

#Counters
Student_Cont_Count<-c(0)
Student_Allergen_Count<-1
Student_Pathogen_Count<-1

#Lists for sensitivity

List_Sens_Fr<-list()
List_Sens_Pss<-list()
List_Sens_Pre<-list()

#Vector Contminations, for contamination every iteration

Vector_Contaminations<-c() #Vector for contamination
Vector_No_Cont_Stu<-c() #vector for number of contaminated students
#Fruit
Vector_Cont_Fr_Serv<-c() #vector for Contamination of fruit in serivces
Vector_Cont_Fr_Serv_Out<-c() #vector for final contamination of fruit in services
Vector_No_Cont_Fr<-c() #Vector for number of contaminated fruit every service.
#Pss
Vector_Cont_Pss_Serv<-c() #vector for Contamination of fruit in serivces
Vector_Cont_Pss_Serv_Out<-c() #vector for final contamination of fruit in services
Vector_No_Cont_Pss<-c() #Vector for number of contaminated fruit every service.
#
Vector_Cont_Pre_Serv<-c() #vector for Contamination of fruit in serivces
Vector_Cont_Pre_Serv_Out<-c() #vector for final contamination of fruit in services
Vector_No_Cont_Pre<-c() #Vector for number of contaminated fruit every service.

Total_Fr<-c() #vector for prevalence of contaminated fruit every service



#vector with tranfer efficiencies. 

Vector_TE_H_F<-c()
Vector_TE_F_H<-c()
Vector_TE_H_S<-c()
Vector_TE_S_H<-c()
Vector_TE_S_F<-c()
Vector_TE_F_S<-c()
Vector_TE_Pre_Mouth<-c()

#Input Data Frame for services
Vector_Con_Services<-c()

#Vector to know items left every day

Items_left_everyday<-list()



  





