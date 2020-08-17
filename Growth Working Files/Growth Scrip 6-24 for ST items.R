#Share Table "Storage" Growth Fr ====================================================================
if(Pick_ST_YN_Fr==1){
  if(salmonella ==1 && Growth ==1 ){
    Func_Enteric_Growth("salmonella","room temp",Fr_Data.Frame,Fr_ST_Picked)
    Fr_Data.Frame[Fr_ST_Picked,colnames(Fr_Data.Frame)== "Contamination"]<-Con_Final
  } else if (E_coli == 1 && Growth ==1){
    Func_Enteric_Growth("E_coli","room temp",Fr_Data.Frame,Fr_ST_Picked)
    Fr_Data.Frame[Fr_ST_Picked,colnames(Fr_Data.Frame)== "Contamination"]<-Con_Final
  }
}  

#Share Table "Storage" Growth Pss ====================================================================
if(Pick_ST_YN_Pss==1){
  if(salmonella ==1 && Growth ==1 ){
    Func_Enteric_Growth("salmonella","room temp",Pss_Data.Frame,Pss_ST_Picked)
    Pss_Data.Frame[Pss_ST_Picked,colnames(Pss_Data.Frame)== "Contamination"]<-Con_Final
  } else if (E_coli == 1 && Growth ==1){
    Func_Enteric_Growth("E_coli","room temp",Pss_Data.Frame,Pss_ST_Picked)
    Pss_Data.Frame[Pss_ST_Picked,colnames(Pss_Data.Frame)== "Contamination"]<-Con_Final
  }
} 


#Share Table "Storage" Growth Pre ====================================================================
if(Pick_ST_YN_Pre==1){
  if(salmonella ==1 && Growth ==1 ){
    Func_Enteric_Growth("salmonella","room temp",Pre_Data.Frame,Pre_ST_Picked)
    Pre_Data.Frame[Pre_ST_Picked,colnames(Pre_Data.Frame)== "Contamination"]<-Con_Final
  } else if (E_coli == 1 && Growth ==1){
    Func_Enteric_Growth("E_coli","room temp",Pre_Data.Frame,Pre_ST_Picked)
    Pre_Data.Frame[Pre_ST_Picked,colnames(Pre_Data.Frame)== "Contamination"]<-Con_Final
  }
} 