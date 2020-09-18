
# Script for Initial Contaminations ---------------------------------------

#Running Student Contamination based on Probability. 
Cont_Student<- ifelse(runif(1)<Pr_Student_iC,IC_Student,0) 
if (Cont_Student>0){
  Student_Cont_Count<-(Student_Cont_Count+1)
  print("A student is contaminated with Pathogens")
}

#Runing Self assigned Student pathogen contamination
if(Toggle_SelfAssigned_Pathogens==1){
  if( k == Student_Pathogen_Day[Student_Pathogen_Count] 
      && j == Student_Pathogen_Service[Student_Pathogen_Count] 
      && i == Student_Pathogen_No[Student_Pathogen_Count]){
    Cont_Student<-45506.72  #IC_Student 
    print("A student is contaminated with Pathogens")
    Student_Pathogen_Count<-(Student_Pathogen_Count+1)
    if(Student_Pathogen_Count>Number_Student_Pathogens){
      Student_Pathogen_Count<-Number_Student_Pathogens
    }
  }
}


#Running Student Allergen Contamination based on Probability
Cont_Student_Allergen_YN<-ifelse(runif(1)<Pr_Student_Allergen,1,0)

#Self Assigned Contaminations
if(Toggle_SelfAssigned_Allergens==1){
  if( k == Student_Allergen_Day[Student_Allergen_Count] 
      && j == Student_Allergen_Service[Student_Allergen_Count] 
      && i == Student_Allergen_No[Student_Allergen_Count]){
    Cont_Student_Allergen_YN <- 1 
    print("A student is contaminated with Allergens")
    Student_Allergen_Count<-(Student_Allergen_Count+1)
    if(Student_Allergen_Count>Number_Student_Allergens){
      Student_Allergen_Count<-Number_Student_Allergens
    }
  }
}




#Total transfer of particles=================================================================

Tr_H_Fr<-Cont_Student*TE_H_F
Tr_Fr_H<-Cont_Fr*TE_F_H
Tr_H_Pss<- Cont_Student*TE_H_F
Tr_Pss_H<-Cont_Pss*TE_F_H
Tr_H_Pre<-Cont_Student*TE_H_F
Tr_Pre_H<-Cont_Pre*TE_F_H 
