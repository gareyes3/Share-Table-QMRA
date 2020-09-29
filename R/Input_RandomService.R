
# #Inputs for Norovirus in fruit. Added to dataframe ----------------------


#Input for Func_Cont_HuNov_Fr. Located in Input functions

  Inputs_Cont_HuNov_Fr<-list(
    #Prevalence of NV in Fruit
    Prevalence = 0.4,
    #Genormic Copies per PFU
    Genomic_copies_per_PFU<-3.66,#rnormTrunc(1,3.65,.98,2.00,5.40), #add
    #log HuNoV copies per/ g
    HuNoV_ContFruit<-1.578 #rlnormTrunc(1,2.38,3.52, 0,6.97) #add
  )

#Inputs for Func_Growth_Sto_Ecoli. Located in Input_Functions
  
  Inputs_Growth_Sto_Ecoli<-list(
    #Growth patameter
    b=0.023,
    #Growth parameter
    k=0.00564, #rnorm(1,.013,.001)/2.303, #add
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



