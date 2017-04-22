#Before this script: put data files in the "data" folder, rename them "test_highfreq", "test_lowfreq", "wind_vec".

#Set up.
library(tidyverse)

home <- "/Volumes/research_storage/energy_balance"
setwd(home)

#Set constants.
pa <- 101.3*exp(-719/8200) #Air pressure, kpa
rho_a <- 41.6 #Density of air at 20C, mol/m^3. Should change or measure this for temp dependence
cp <- 29.3 #specific heat of air, J/mol K.


#Read data.
high_base <- read_csv("data/CSV_1073.HighFreqData_Final.dat", col_names = FALSE)
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

#Calculate covariances. 
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

