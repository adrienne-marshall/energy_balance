#Read low frequency data.
#April 13, 2017. Adrienne Marshall.

library(tidyverse)
library(xlsx)
library(stringr)
library(reshape2)
library(ggthemes)

home <- "/Volumes/research_storage/energy_balance"
setwd(home)

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

##Precip module: ---------
precip <- main %>% select(day_of_year, hhmm, atmos41_precip_mm_, rsw_in_w_m2_)
precip <- precip %>%   
  mutate(date = as.Date(strptime(paste("2017", day_of_year), format="%Y %j")))
daily_precip <- precip %>% group_by(date) %>% 
  summarise(precip = sum(atmos41_precip_mm_, na.rm = TRUE),
            solar = mean(rsw_in_w_m2_)) %>%
  filter(date>= "2017-04-01") %>%
  filter(date<= "2017-04-18")

ggplot(daily_precip, aes(x = date, y = solar)) + geom_line()

#define rainy and non-rainy days.
daily_precip <- daily_precip %>% mutate(class = ifelse(solar>150, "sunny", "cloudy")) %>%
  select(-precip)

write_csv(daily_precip, "data/daily_solar.csv")

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

v.water <- 0.4 # get data !!!!!!!  volumetric  content m^3/m^3
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

plot_dat <- plot_dat %>% filter(!variable %in% c("soil_heat_flux_1_mv_", "soil_heat_flux_2_mv_", 
                                                 "soil_heat_flux_3_mv_", "rsw_in_mv_", "rsw_out_mv_",
                                                 "rlw_in_mv_", "rlw_out_mv_", "rlw_in_bodyt_c_",
                                                 "rlw_out_bodyt_c_"))


ggplot(plot_dat, aes(x = index, y=value)) +
  geom_line() +
  facet_wrap(~variable, scales = "free") + 
  theme_few()

soil <- plot_dat %>% filter(variable %in% c("soil_heat_flux_1_w_m2_", "soil_heat_flux_2_w_m2_", "soil_heat_flux_3_w_m2_"))
ggplot(soil, aes(x = index, y=value, color = variable)) +
  geom_line() +
  #facet_wrap(~variable, scales = "free") + 
  theme_few()


#Energy balance equation:
#Rn - G - LE - H - dS/dt = 0

#Take a mean of the soil heat flux plates.
main <- cbind(main, G = rowMeans(main[,c("soil_heat_flux_1_w_m2_",
                                         "soil_heat_flux_2_w_m2_",  
                                         "soil_heat_flux_3_w_m2_")]))


#make a csv for export.
dat_out <- main %>% select(year, day_of_year, hhmm, storage, G, rsw_in_w_m2_,
                           rsw_out_w_m2_, rlw_in_w_m2_, rlw_out_w_m2_, net_radiation_w_m2_)
names(dat_out)[6:10] <- c("sw_in", "sw_out", "lw_in", "low_out", "rnet")
names(dat_out)[2] <- "doy"

write_csv(dat_out, "data/G_RN_S.csv", col_names = TRUE)
  
  
  
