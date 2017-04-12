#Before this script: put data files in the "data" folder, rename them "test_highfreq", "test_lowfreq", "wind_vec".

#Set up.
library(tidyverse)

home <- "/Volumes/research_storage/energy_balance"
setwd(home)

#Set constants.
pa <- 101.3 #Air pressure, kpa
rho_a <- 41.6 #Density of air at 20C, mol/m^3. Should change or measure this for temp dependence
cp <- 29.3 #specific heat of air, J/mol K.


#Read data.


high <- read.csv("data/test_highfreq.csv", header = TRUE, skip = 1)
low <- read.csv("data/test_lowfreq.csv", header = TRUE, skip = 1)
wind <- read.csv("data/wind_vec.csv", header = TRUE, skip = 1)

#Write an ascii plain text file of the high frequency data. 
small <- high %>% dplyr::select(RECNBR, Ux, Uy, Uz, P14Temp, P14Ea)
write.table(small, "data/test_highfreq.asc", col.names = TRUE, row.names = FALSE, sep = ",")

#Replace -7999 with NA.
high[high == -7999] <- NaN
low[low == -7999] <- NaN
wind[wind == -7999] <- NaN

#Set an averaging period for the high frequency data.
nmin <- 15
freq <- 10 #in Hertz. 
nint <- nmin*60*freq
high <- high %>% mutate(interval = 1+floor(RECNBR/nint))

#Calculate means so we can get fluctuations about the mean.
means <- high %>% group_by(interval) %>%
  summarise(mean_ux = mean(Ux, na.rm = TRUE),
            mean_uy = mean(Uy, na.rm = TRUE),
            mean_uz = mean(Uz, na.rm = TRUE),
            mean_ea = mean(P14Ea, na.rm = TRUE),
            mean_temp = mean(P14Temp, na.rm = TRUE))

high <- full_join(high, means, by = "interval")

#Calculate fluctuations about the mean.
high <- high %>% mutate(uz_prime = Uz - mean_uz) %>%
  mutate(ux_prime = Ux - mean_ux) %>%
  mutate(uy_prime = Uy - mean_uy) %>%
  mutate(ea_prime = P14Ea - mean_ea) %>%
  mutate(temp_prime = P14Temp - mean_temp)

#Calculate covariances. 
high <- high %>% mutate(wT = ux_prime*temp_prime) %>%
  mutate(wc = ux_prime*ea_prime/pa)

#Take means over intervals.
ans <- high %>% group_by(interval) %>%
  summarise(mean_wT = mean(wT, na.rm = TRUE),
            mean_wc = mean(wc, na.rm = TRUE))

#Multiply by constants to get fluxes.
#Units for H: W/m^2(?), and for E: mol/s m^2
ans <- ans %>% mutate(H = rho_a*cp*mean_wT) %>%
  mutate(E = rho_a*mean_wc)
ans


