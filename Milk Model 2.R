Data_Frame_Milk<-data.frame(
  Iteration = 1:1000,
  Shelflife =as.numeric(0),
  stringsAsFactors = FALSE
)


for (i in 1:nrow(Data_Frame_Milk)){
  Temp<-rnorm(1,6.5,2)
  Initial_pop<-rlnorm(1,1,0.9)
  Spoilage<-7
  Growth_spoilage<-Spoilage-Initial_pop
  Tempmin<-(2.41)
  b<-.05273
  k<-((Temp-Tempmin)*b)^2
  Variability<-(1-(rnorm(1,0,17)/100))
  New_K<- k *Variability
  Conversion<-2^New_K
  New_K2<-log10(Conversion)
  Shelf_Life<-Growth_spoilage/New_K2
  Shelf_Life_days<-Shelf_Life/24
  Data_Frame_Milk[i,colnames(Data_Frame_Milk)== "Shelflife"]<-Shelf_Life_days
}


hist(Data_Frame_Milk$Shelflife, breaks = 1000)

#Psychotrophs

Temp<-4
Initial_pop<-1
Spoilage<-7
Growth_spoilage<-Spoilage-Initial_pop
Tempmin<-(-7.9)
b<-.0272
k<-((Temp-Tempmin)*b)^2
k*60
#Variability<-(1-(rnorm(1,0,17)/100))
New_K<- k #*Variability
Conversion<-2^New_K
New_K2<-log10(Conversion)
Shelf_Life<-Growth_spoilage/New_K2
Shelf_Life_days<-Shelf_Life/24


#Mesophiles
Temp<-4
Initial_pop<-1
Spoilage<-7
Growth_spoilage<-Spoilage-Initial_pop
Tempmin<-(2.41)
b<-.05273
k<-((Temp-Tempmin)*b)^2
#Variability<-(1-(rnorm(1,0,17)/100))
New_K<- k #*Variability
Conversion<-2^New_K
New_K2<-log10(Conversion)
Shelf_Life<-Growth_spoilage/New_K2
Shelf_Life_days<-Shelf_Life/24

#P .putida

b<-.03772
Temp<-4
Tmin<-(-6.1)
Tmax<-(41.2)
c<-.1719
time<-10
k<-(b*(Temp-Tmin)*(1-exp(c*(Temp-Tmax))))^2

