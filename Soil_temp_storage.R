# Ames Fowler
# insert after main command in low_freq.R
###########
soil.storage<-main%>%select(contains("soil_temp"), irt_target_c_)

soilT.1<-main%>%select(contains('soil_temp_1'))
soilT.2<-main%>%select(contains('soil_temp_2'))
soilT.3<-main%>%select(contains('soil_temp_3'))

soilT.1<-mutate(soilT.1, ST1.ave = rowSums(soilT.1)/ncol(soilT.1))
soilT.2<-mutate(soilT.2, ST2.ave = rowSums(soilT.2)/ncol(soilT.2))
soilT.3<-mutate(soilT.3, ST3.ave = rowSums(soilT.3)/ncol(soilT.3))
len<-length(soilT.3$soil_temp_3_1_c_)
# 30 min dT/dt
delta30.ST.1<-(soilT.1[1:len-1,1:4]-soilT.1[2:len,1:4])/1800 #convert .5 hr to day 
delta30.ST.2<-(soilT.2[1:len-1,1:4]-soilT.2[2:len,1:4])/1800
delta30.ST.3<-(soilT.3[1:len-1,1:4]-soilT.3[2:len,1:4])/1800
delta30.ST<- cbind(delta30.ST.1,delta30.ST.2,delta30.ST.3)
delta30.ST<- mutate(delta30.ST, delta30.ST.ave = rowSums(delta30.ST)/ncol(delta30.ST))
########### Soil parameters - Table 8.2 - ##############
p.water <- 1.00 #Mg/m^3   density
p.min <- 2.65 #Mg/m^3
p.o <-1.3  #Mg/m^3 fix for KPA 

c.water <- 4.18    #J/gK   specific heat 
C.min   <- 0.87    #J/gK
c.o   <- 1.92    #J/gK

k.water <- 0.56+0.0018*(273.15+soilT.3$ST3.ave) #(W/mK) thermal conductivity 
k.min   <- 2.5 #(W/mK)
k.o   <- .25#(W/mK)

v.water <- 0.2 # get data !!!!!!!  volumetric  content m^3/m^3
v.min <- 0.5  
v.o <- .02
#soil volumetric heat capacity 
PsCs <- 0.1e6*(v.min*p.min*C.min + v.water*p.water*c.water + v.o*p.o*c.o) # PsCs [J/m^2C] %0.1 is cm to meters %e6 is Mg to g
#soil storage
surf.str.30<-PsCs*delta30.ST$delta30.ST.ave #w/m^2