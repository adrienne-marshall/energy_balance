# this is following the High frequencey data from Andrieen.
# mine part starts from line 103 


#Before this script: put data files in the "data" folder, rename them "test_highfreq", "test_lowfreq", "wind_vec".

#Set up.
library(tidyverse)

#home <- "/Volumes/research_storage/energy_balance"
home <- "C:/Energy_balance"
setwd(home)

#Set constants.
pa <- 101.3*exp(-719/8200) #Air pressure, kpa
rho_a <- 41.6 #Density of air at 20C, mol/m^3. Should change or measure this for temp dependence
cp <- 29.3 #specific heat of air, J/mol K.


#Read data.
high_base<-read_csv("CSV_1073.HighFreqData_Final.dat",col_names=FALSE)
#high_base <- read_csv("data/CSV_1073.HighFreqData_Final.dat", col_names = FALSE)
names(high_base) <- c("n", "year", "doy", "hhmm", "sec", "ux", "uy", "uz", "sonic_temp", "fwt_low", "fwt_high", 
                 "fwt_p14", "rh", "ea_kpa", "ea_gm3")
high <- high_base %>% select(-n)
high <- high %>% mutate(index = (1:nrow(high)))
#low <- read.csv("data/test_lowfreq.csv", header = TRUE, skip = 1)
#wind <- read.csv("data/wind_vec.csv", header = TRUE, skip = 1)

#Write an ascii plain text file of the high frequency data. 
#small <- high %>% dplyr::select(RECNBR, Ux, Uy, Uz, P14Temp, P14Ea)
#write.table(small, "data/test_highfreq.asc", col.names = TRUE, row.names = FALSE, sep = ",")

#Replace -7999 with NA.
high[high == -7999] <- NaN
#low[low == -7999] <- NaN
#wind[wind == -7999] <- NaN

#Set an averaging period for the high frequency data.
nmin <- 30
freq <- 10 #in Hertz. 
nint <- nmin*60*freq
#Need to change how interval is calculated. ----
high <- high %>% mutate(interval = 1+floor(index/nint))

#Calculate means so we can get fluctuations about the mean.
means <- high %>% group_by(interval) %>%
  summarise(mean_ux = mean(ux, na.rm = TRUE),
            mean_uy = mean(uy, na.rm = TRUE),
            mean_uz = mean(uz, na.rm = TRUE),
            mean_ea = mean(ea_kpa, na.rm = TRUE),
            mean_temp = mean(fwt_high, na.rm = TRUE))

high <- full_join(high, means, by = "interval")

#Calculate fluctuations about the mean.
high <- high %>% mutate(uz_prime = uz - mean_uz) %>%
  mutate(ux_prime = ux - mean_ux) %>%
  mutate(uy_prime = uy - mean_uy) %>%
  mutate(ea_prime = ea_gm3 - mean_ea) %>%
  mutate(temp_prime = fwt_high - mean_temp)

#Calculate covariances.  for H and E
high <- high %>% mutate(wT = uz_prime*temp_prime) %>%
  mutate(wc = uz_prime*ea_prime/pa)

#Take means over intervals.
#Might be better to do better quality control than this?
ans <- high %>% group_by(interval) %>%
  summarise(mean_wT = mean(wT, na.rm = TRUE),
            mean_wc = mean(wc, na.rm = TRUE))

#Multiply by constants to get fluxes.
#Units for H: W/m^2(?), and for E: mol/s m^2
ans <- ans %>% mutate(H = rho_a*cp*mean_wT) %>%
  mutate(E = rho_a*mean_wc)


ans <- ans %>% mutate(lambda_E = E*44*1000)

#E seems to get unreasonably large and is spiky. Constrain it to less than 1000W/m2.
ans$lambda_E[ans$lambda_E > 1000] <- NaN
ans$lambda_E[ans$lambda_E < -1000] <- NaN

#Write a csv with the answers. 
ans <- ans %>% select(-mean_wT, -mean_wc)
times <- high %>% select(year, doy, hhmm, interval)
dat <- left_join(ans, times, by = "interval")
dat_concise <- dat %>% group_by(interval) %>%
  summarise(year = mean(year),
            start_doy = min(doy),
            end_doy = max(doy),
            start = head(hhmm, 1),
            end = tail(hhmm, 1),
            H = mean(H),
            lambda_E = mean(lambda_E))
dat_concise <- dat_concise %>% mutate(start_dt = paste0(start_doy, "-", start))

quartz()
ggplot(dat_concise, aes(x = interval, y = lambda_E)) + geom_line() + theme_few()

dat_concise <- dat_concise %>% mutate(time = ifelse(end>start, end - start, NaN))
#####################################################################################################


# calculate the canopy conductance 
# first calculate the boundary layer conductane for heat and vapor from table 7.6 p 98
# interval way maybe not so accurate
# select data from 91 days 00 to 108 days 00

high_backup<-high
high<-high %>% filter(doy>90 & doy<108)

# first group by 
# choose year doy hhmm interval mean_ea mean_temp 
high<-high %>% mutate(day_hhmm=10000*doy+hhmm)



  
base<-high %>% group_by(day_hhmm) %>% 
  summarise(
  year=mean(year),
  doy=mean(doy),
  ux=mean(ux,na.rm=TRUE),
  uy=mean(uy,na.rm=TRUE),
  uz=mean(uz,na.rm=TRUE),
  sonic_temp=mean(sonic_temp,na.rm=TRUE),
  fwt_low=mean(fwt_low,na.rm=TRUE),
  fwt_high=mean(fwt_high,na.rm=TRUE),
  fwt_p14=mean(fwt_p14,na.rm=TRUE),
  rh=mean(rh,na.rm=TRUE),
  ea_kpa=mean(ea_kpa,na.rm=TRUE),
  ea_gm3=mean(ea_gm3,na.rm=TRUE)
)

base <- base %>% mutate(index=(1:nrow(base)))
head(base)
base<-base %>% mutate(interval= 1+floor(index/30))



# set the constant
z=2 # wind speed height
h=0.1 # grass canopy height
d=0.65*h
zm=0.1*h
zH=0.2*z

FM=0  # correction factor for M if larger than 4 m/s can ignore
FH=0  # correction factor for H

rho_a <- 41.6 #Density of air at 20C, mol/m^3. Should change or measure this for temp dependence

lambda=44*1000

gs.ad=0.153*3 # mol/m2/s
gs.ab=0.356*3  # mol/m2/s

#Adjust humidity data. 
B1 <- -0.001
B2 <- 0.1406
B3 <- 0.023
B4 <- -3.2768

# adjust humidity data.

base <- base %>% mutate(delta = (B1*rh*100 + B2)*fwt_p14 + (B3*rh*100 + B4)) %>%
  mutate(adjusted_RH = rh + delta/100)

#Calculate es(Ta).
a <- 0.611
b <- 17.502
c <- 240.97
base <- base %>% mutate(es_ta = a*exp((b*fwt_high)/(fwt_high+c)))
base <- base %>% mutate(ea = es_ta*adjusted_RH)


# calculate the horizotal wind speed sqrt(ux2+uy2)

base <- base %>% mutate(u_horizon = sqrt(ux^2+uy^2))

# calculate the u* euq 5.1 p68
factor<-log((z-d)/zm)  # in R log=ln
base<-base %>% mutate(u_star=u_horizon*0.4/factor)



# calculate all the constant 
left<-log((z-d)/zm)+FM    # z should be 2m or height of the canopy
right<-log((z-d)/zH)+FH

constant.all<-0.4^2*rho_a/(left*right)


# calculate the gHa=gva

base<-base %>% mutate(gHa=constant.all*u_horizon)



# calculate the gv based on p234 penman monteith gha=gva=0.2*u


base<-base %>% mutate(gva=0.2*u_horizon)

# calculate the gv
base<-base %>% mutate(gv2=0.5*gs.ad*gva/(gs.ad+gva)+0.5*gs.ab*gva/(gs.ab+gva))

# using another method to caculate gv3
base<-base %>% mutate(gv3=0.6*gva/(0.6+gva))


# use the sensible heat H to calculate the canopy temperature? is the irt_target_c_ can be compared
# H=cpgHa(TL-Ta)

base_30m<- base %>% group_by(interval) %>% 
  summarise(year=mean(year),
            doy=mean(doy),
            fwt_high=mean(fwt_high,na.rm=TRUE),
            ea_kpa=mean(ea_kpa,na.rm=TRUE),
            gHa=mean(gHa,na.rm=TRUE),
            gv=mean(gv,na.rm=TRUE),
            ea=mean(ea,na.rm=TRUE),
            u_horizon=mean(u_horizon,na.rm=TRUE),
            u_star=mean(u_star,na.rm=TRUE),
            gva=mean(gva,na.rm=TRUE),
            gv2=mean(gv2,na.rm=TRUE),
            gv3=mean(gv3,na.rm=TRUE))



# delete the last two term 816 and 817
base_30m<-base_30m[-c(nrow(base_30m)-1,nrow(base_30m)),]

# some spike large t
base_30m$ea[base_30m$ea > 7] <- NaN


# merge the high and low frequency together
low_main<-main_short[-1,]

full<-cbind(base_30m,low_main) # check the ea_kpa and p14_ea_kpa if overlap and fwt_high with fine_wire_temp_uupper




# now calculate the H and lambdaE
# first calculate the H =cpgHa(TL-Ta)

full[full == -7999] <- NaN

full<- full %>% mutate(H_gh=cp*gva*(irt_target_c_-fwt_high))

# calculate the lambda_E
#a*exp((b*fwt_high)/(fwt_high+c)))

full<-full %>% mutate(esTL=a*exp((b*irt_target_c_)/(irt_target_c_+c)))
full<-full %>% mutate(E_lam2=lambda*gv2*(esTL-ea)/pa)
full<-full %>% mutate(E_lam3=lambda*gv3*(esTL-ea)/pa)
full<-full %>% mutate(E_min=net_radiation_w_m2_-storage-H_gh-soil_heat_flux_1_w_m2_)


# calculate the residule

full <- full %>% mutate(residue=net_radiation_w_m2_-storage-H_gh-E_lam2)

full_day<-full %>% group_by(day_of_year) %>%
  summarise(year=sum(year,na.rm=TRUE),
            net_rad=sum(net_radiation_w_m2_,na.rm=TRUE),
            storage=sum(storage,na.rm=TRUE),
            H=sum(H_gh,na.rm=TRUE),
            lambdaE=sum(E_lam2,na.rm=TRUE),
            residual=sum(residue,na.rm=TRUE))



# read Andrieen data and calculate the differences


headers <- read_csv("energy_balance_results.csv", col_names = TRUE)
EB1<-headers[c(-(length(headers$G)-1),-length(headers$G)),]

full<- full %>% mutate(type=rep("Penman-Monteith",length(full$H_gh)))
EB1<-EB1 %>% mutate(typy2=rep("Eddy-Covariance",length(full$H_gh)))



full<- full %>% select(-storage)
full<- full %>% select(-doy)

fulllast<-cbind(full,EB1)

# select the plot data 

eddy<-fulllast %>% select(date_time,rnet_calc,G,storage,H,LE,typy2)
penman<- fulllast %>% select(date_time,rnet_calc,G,storage,H_gh,E_lam3,type)



# calculate the residual
eddy<- eddy %>% mutate(residual=rnet_calc-H-LE-storage-G)
penman<-penman  %>% mutate(residual=rnet_calc-H_gh-E_lam3-storage-G)
names(eddy)<-c('date_time','Net_R','G','Storage','H','LE','type','residual')
names(penman)<-names(eddy)



# write csv

write.csv(penman, file = "Penman-montieth.csv")
write.csv(penman_day,file="Penman-montieth_daily.csv")





plot_dat1 <- melt(penman, id.vars = c("date_time", "type"))
plot_dat2<- melt (eddy,id.vars= c("date_time", "type"))

plot_dat<-rbind(plot_dat1,plot_dat2)




names(plot_dat)<-c("Date","Method","variable","Flux")

ggplot(plot_dat, aes(x = Date, y= Flux,colour=Method)) +
  geom_line() +
  facet_wrap(~variable, scales = "free",ncol=1)+
  geom_line(size=1)+
  ylab("Flux(W/m2")+
  facet_grid(variable ~ .)+
  scale_y_continuous(limits = c(-250, 600))+
  theme(text=element_text(size=18, face="bold"))


#############


