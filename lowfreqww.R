#Read low frequency data.
#April 26, 2017. Adrienne Marshall, Ames Fowler.
#install.packages(c("tidyverse", "xlsx", "stringr", "reshape2", "ggthemes"))

library(tidyverse)
library(xlsx)
library(stringr)
library(reshape2)
library(ggthemes)
home <- "/Volumes/research_storage/energy_balance"
setwd(home)

#dim(main$atmos41_precip_mm_)
#Get data. 
lowfreq <- read_csv("data/CSV_1073.LowFreqData_Final.dat", col_names = FALSE)
headers <- read_csv("data/EnergyBalanceHeaders.csv", n_max = 0, col_names = TRUE)
names(headers) <- str_replace_all(names(headers),"(([[:punct:]])|\\s)","_")
names(headers) <- str_replace_all(names(headers), "__", "_")
names(lowfreq) <- names(headers)
names(lowfreq) <- tolower(names(lowfreq))

#Select essentials.
main <- lowfreq %>% select(year, day_of_year, hhmm, wind_speed_m_s_, fine_wire_temp_upper_c_,
                           p14_rh, p14_ea_kpa_, net_radiation_w_m2_, contains("soil"),
                           contains("precip"), contains("vapor_pressure"), irt_target_c_, 
                           irt_body_c_, contains("rsw"), contains("rlw"))
main <- main %>% mutate(index = 1:nrow(main))

########### soil Water Module  ------------
VWC1<-read.delim("data/VWC1.csv",header=FALSE,sep=',',skip=4,as.is=TRUE,na.strings ='#N/A') ## getting this to pop was a struggle... learning R on the fly 
VWC2<-read.delim("data/VWC2.csv",header=FALSE,sep=',',skip=4,as.is=TRUE,na.strings ='#N/A' )
MPS1<-read.delim("data/MPS1.csv",header=FALSE,sep=',',skip=4,as.is=TRUE,na.strings ='#N/A')
MPS2<-read.delim("data/MPS2.csv",header=FALSE,sep=',',skip=4,as.is=TRUE,na.strings ='#N/A')



names(VWC1) <- c("date_time","G1.5_moist","G1.5_TEMP","G1.5_EC","G1.15_moist","G1.15_TEMP","G1.15_EC","G1.25_moist",           ### look at the grace...
                 "G1.25_TEMP","G1.25_EC","G3.15_moist","G3.15_TEMP","G3.15_EC","G3.5_moist","G3.5_TEMP","G3.5_EC")
names(VWC2) <- c("date_time", "G2.5_moist","G2.5_TEMP","G2.5_EC","G2.15_moist","G2.15_TEMP","G2.15_EC","G2.25_moist",
                 "G2.25_TEMP","G2.25_EC","G3.25_moist","G3.25_TEMP","G3.25_EC")
names(MPS1) <- c("date_time", "G1.5_potential", "G1.5_TEMP","G1.15_potential", "G1.15_TEMP","G1.25_potential", 
                 "G1.25_TEMP","G3.15_potential", "G3.15_TEMP","G3.5_potential", "G3.5_TEMP")
names(MPS2) <- c("date_time", "G2.5_potential", "G2.5_TEMP","G2.15_potential", "G2.15_TEMP","G2.25_potential", 
                 "G2.25_TEMP","G3.25_potential", "G3.25_TEMP")


julian<-as.numeric(main$hhmm)/2400+as.numeric(main$day_of_year) # HHMM to decimal of days ### we really don't have to use this
julian<-julian[1:800]                                                                     ### I took the first 800 half hours and
                                                                                          ### modified the the soil data to start 
cm5<-data.frame(VWC1$G1.5_moist,VWC2$G2.5_moist)                                          ### at the same time as the clima data
cm5<-cm5[32:831,]                                                                         ### then rounded off to a clean number 
cm5_moist<-rowSums(cm5)/ncol(cm5)

cm15<-data.frame(VWC1$G1.15_moist,VWC1$G3.15_moist,VWC2$G2.15_moist)
cm15<-cm15[32:831,]
cm15_moist<-rowSums(cm15)/ncol(cm15)

cm25<-data.frame(VWC2$G2.25_moist)
cm25_moist<-cm25[32:831,]

png("R/plots/soil_moisture.png", width = 9, height = 9, res = 300, units = 'in')
par(mfrow=c(3,1)) 
plot(julian,cm5_moist,type='l', xlab= 'Julian Day', ylab ='VWC @ 5cm')                   ### comparison of VWC with depth 
plot(julian,cm15_moist,type='l',xlab= 'Julian Day', ylab ='VWC @ 15cm')                  ### and vs precip; effects drop with depth or saturatation
plot(julian,cm25_moist,type='l',xlab= 'Julian Day', ylab ='VWC @ 25cm')
dev.off()

par(mfrow=c(1,1))
plot(julian,main$atmos41_precip_mm_[1:800],type='l',col='blue',xlab = 'Julian Day', ylab = 'precipitation (mm)',title('Precipitation'))

#Ggplot version of above, with dates.



cm5_st<-(cm5_moist[2:800]-cm5_moist[1:(800-1)])*100 #convert to mm per 30min
cm15_st<-(cm15_moist[2:800]-cm15_moist[1:(800-1)])*100 #convert to mm per 30min
cm25_st<-(cm25_moist[2:800]-cm25_moist[1:(800-1)])*100 #convert to mm per 30min
Julian2  = julian[1:length(cm5_st)]


par(mfrow=c(3,1)) 
plot(Julian2,cm5_st,type='l',col='blue', xlab= 'Julian Day', ylab ='detla S (mm/m^2)')  ### plots of change in storage
plot(Julian2,cm15_st,type='l',col='blue', xlab= 'Julian Day', ylab ='detla S (mm/m^2)')
plot(Julian2,cm25_st,type='l',col='blue', xlab= 'Julian Day', ylab ='detla S (mm/m^2)')

net_water_cm5<-sum(cm5_st)
net_water_cm15<-sum(cm15_st)
net_water_cm25<-sum(cm25_st)

net_precip<-sum(main$atmos41_precip_mm_)# water balance not persued as agweather.wsu suggest 10-11 mm 
                            # or precip fell during the study period while the atmo recorded 0.89 mm. 
                            # even if this was in cm the under catch would be 20%. 

#### variable that could be added to mains:

Soilwater<- cbind(cm5_moist,cm15_moist,cm25_moist,cm5_st,cm15_st,cm25_st)
##main <- main %>% mutate(storage = c(NA, surf.str.30))


########### soil storage module -----------
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

v.water <- cm5_moist #### if you want to use this, else = .45 #################################################################### <- 
v.min <- 0.5  
v.o <- .02
#soil volumetric heat capacity 
PsCs <- 0.1e6*(v.min*p.min*C.min + v.water*p.water*c.water + v.o*p.o*c.o) # PsCs [J/m^2C] %0.1 is cm to meters %e6 is Mg to g
#soil storage
surf.str.30<-PsCs*delta30.ST$delta30.ST.ave #w/m^2

#end soil storage module. --------

main <- main %>% mutate(storage = c(NA, surf.str.30))
main %>% group_by(day_of_year) %>% 
  summarise(count = n(),
    daily_storage = sum(storage, na.rm = TRUE))

plot_dat <- melt(main, id.vars = c("year", "day_of_year", "hhmm", "index"))
plot_dat <- mutate(plot_dat, time = paste0(day_of_year, "_", hhmm))

head(plot_dat)
plot_dat$time
plot_dat <- plot_dat %>% filter(!variable %in% c("soil_heat_flux_1_mv_", "soil_heat_flux_2_mv_", 
                                                 "soil_heat_flux_3_mv_", "rsw_in_mv_", "rsw_out_mv_",
                                                 "rlw_in_mv_", "rlw_out_mv_", "rlw_in_bodyt_c_",
                                                 "rlw_out_bodyt_c_"))

ggplot(plot_dat, aes(x = index, y=value)) +
  geom_line() +
  facet_wrap(~variable, scales = "free") + 
  theme_few()

#Energy balance equation:
#Rn - G - LE - H - dS/dt = 0

storage <- main %>% select(day_of_year, hhmm, storage)
write_csv(storage, "data/storage.csv")

  
  
  
