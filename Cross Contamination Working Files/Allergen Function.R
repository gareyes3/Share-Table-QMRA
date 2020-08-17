

Func_Allergen_CC<-function(DF, PickedVar){
  if(Fr_Data.Frame[Fr_Touched,colnames(Fr_Data.Frame)== "ExposedAllergen"] == "Yes" ){
    Cont_Student_Allergen_YN<-1
  }
  if(Fr_Data.Frame[Fr_Touched,colnames(Fr_Data.Frame)== "ExposedAllergen"] == "No" && Cont_Student_Allergen_YN == 1){
    Fr_Data.Frame[Fr_Touched,colnames(Fr_Data.Frame)== "ExposedAllergen"]<- "Yes"
  }
  return(Fr_Data.Frame)
}
