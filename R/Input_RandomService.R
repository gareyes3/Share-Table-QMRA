
# #Inputs for Norovirus in fruit. Added to dataframe ----------------------


#Input for Func_Cont_HuNov_Fr. Located in Input functions

  Inputs_Cont_HuNov_Fr<-list(
    #Prevalence of NV in Fruit
    Prevalence <- rbetagen(1,0.79,1.03,0.0,0.2),
    #Genormic Copies per PFU
    Genomic_copies_per_PFU<-rnormTrunc(1,3.65,.98,2.00,5.40), #add 3.66,
    #log HuNoV copies per/ g
    HuNoV_ContFruit<-rlnormTrunc(1,2.38,3.52, 0,6.97) #add 1.578 #
  )

#Inputs for Func_Growth_Sto_Ecoli. Located in Input_Functions
  
  Inputs_Growth_Sto_Ecoli<-list(
    #Growth patameter
    b=0.023,
    #Growth parameter
    k=rnorm(1,.013,.001)/2.303, #add 0.00564, #
    #Min Temp
    Tmin = (1.17)
  )
  
#Inputs for Func_Growth_Sto_Salmonella
  
  Inputs_Growth_Sto_Salmonella<-list(
    #Growth patameter
    b=.020,
    #Growth parameter
    k=.0128/2.303,
    #Min Temp
    Tmin =(-0.571)
  )

# Transfer Efficiencies ---------------------------------------------------
  
  
  if(norovirus==1){
    TE_H_F<- rbetagen(1,0.76,1.04,.0126,.46) #0.2013#
    TE_H_S<-rtriang(1,.001,.13,.27) #TE between Hands and Surfaces #add .1340 #
    TE_F_H<-rbetagen(1,0.88,1.01,.048,.164) #add 0.1018#
    TE_F_S<-.0250 #TE between Food and Surfaces
    TE_S_H<- rtriang(1,.036,.07,.22)#.1090 #TE between Surfaces and Hands
    TE_S_F<-.4620 #TE between Surfaces and Foods
    TE_Pre_Mouth<-.339 #TE between Milk to Mouth
  }
  
  if(salmonella ==1){
    TE_H_F<-.0021
    TE_F_H<-.0328
    TE_H_S<-0.0016 #Chen and DS
    TE_S_H<-0.0229
    TE_Pre_Mouth<-.3397#TE between Surfaces and Hands
  }

#Adding TE to Vectors
  Vector_TE_H_F<-c(Vector_TE_H_F,TE_H_F )
  Vector_TE_F_H<-c(Vector_TE_F_H,TE_F_H )
  Vector_TE_H_S<-c(Vector_TE_H_S, TE_H_S)
  Vector_TE_S_H<-c(Vector_TE_S_H,TE_S_H)
  Vector_TE_S_F<-c(Vector_TE_S_F, TE_S_F)
  Vector_TE_F_S<-c(Vector_TE_F_S, TE_F_S)
  Vector_TE_Pre_Mouth<-c(Vector_TE_Pre_Mouth,TE_Pre_Mouth)
  
  
  

