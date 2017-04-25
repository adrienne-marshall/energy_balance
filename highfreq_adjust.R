##April 24, 2017. Adrienne Marshall.

#Prepare high frequency data for use with EddyPro.

#Set up.
library(tidyverse)

home <- "/Volumes/research_storage/energy_balance"
setwd(home)

dat <- read_csv("data/CSV_1073.HighFreqData_Final.dat", 
                col_names = FALSE)
dat[dat == -7999] <- NaN
names(dat) <- c("n", "year", "doy", "hhmm", "sec", "ux", "uy", "uz", "sonic_temp", "fwt_low", "fwt_high", 
                      "fwt_p14", "rh", "ea_kpa", "ea_gm3")
dat <- dat %>% select(-n)

test_dat <- dat[1:1000,]

#Adjust humidity data. Force RH to be less than 1 (?). 
# Ratio = 0.825 - 0.143*Measured RH
# Vapor Pressure = Ratio * Measured Vapor Pressure
# 
# The data logger code also converts from vapor pressure in kPa to g/m3 with
# 
# P14A = 2.16679*((P14Ea*1000))/(FW05+273.15) 
dat <- dat %>% mutate(ratio = 0.825 - 0.143*rh) %>%
  mutate(ea_adjust = ratio*ea_kpa) %>%
  mutate(ea_adjust = ifelse(ea_adjust > 3, NaN, ea_adjust)) %>%
  mutate(ea_adjust = ifelse(ea_adjust < 0, NaN, ea_adjust))

# 
# B1 <- -0.001
# B2 <- 0.1406
# B3 <- 0.023
# B4 <- -3.2768
# dat <- dat %>% mutate(delta = (B1*rh*100 + B2)*fwt_high + (B3*rh*100 + B4)) %>%
#   mutate(adjusted_RH = rh + delta/100) %>%
#   mutate(adjusted_RH = ifelse(adjusted_RH > 1, 1, adjusted_RH))

# #Calculate es(Ta).
# a <- 0.611
# b <- 17.502
# c <- 240.97
# dat <- dat %>% mutate(es_ta = a*exp((b*fwt_high)/(fwt_high+c)))
# dat <- dat %>% mutate(ea = es_ta*adjusted_RH)

#Recalculate concentration of water in air. (mmol/m^3)
Mw <- 18
R <- 8.314
dat <- dat %>% mutate(h2o_conc = ea_adjust*(1000)*1000/(R*(fwt_high+273.15)))

#Select only the relevant rows.
dat <- dat %>% select(year, doy, hhmm, sec, ux, uy, uz, sonic_temp, fwt_high, h2o_conc)

dat <- dat %>% filter(doy > 90) %>%
  filter(doy < 108)

#Fix timestamp. 
dat <- dat %>% 
  mutate(date = as.Date(strptime(paste(year, doy), format="%Y %j"))) %>%
  mutate(hhmm = ifelse(nchar(hhmm)==3, 
                                    paste0("0", hhmm),
                                    ifelse(nchar(hhmm)==2,
                                           paste0("00",hhmm),
                                           ifelse(nchar(hhmm)==1,
                                                  paste0("000",hhmm),
                                                  hhmm)))) %>%
  mutate(hour = substr(hhmm, start = 1, stop = 2)) %>%
  mutate(min = substr(hhmm, start = 3, stop = 4)) %>%
  mutate(date_time = ymd_hms(paste(date, hour, min, sec)))

  
#Write files. ---------
for(i in 1:length(unique(dat$doy))){
  datx <- dat %>% filter(doy == unique(dat$doy)[i])
  y <- datx$date[2]
  z <- datx$hhmm[1]
  datx <- datx %>% select(date_time, doy, ux, uy, uz, sonic_temp, fwt_high, h2o_conc)
  write_csv(datx, paste0("data/eddypro_data/", y, "_", z, "_highfreq.csv"))
}


# 
# write_csv(dat, "data/adjusted_highfreq.csv")
# 
# #------
# dat <- read_csv("data/adjusted_highfreq.csv")
# write_csv(dat, "data/2017-03-01_1857_highfreq-01.csv")
# 
x <- list.files("data/eddypro_data")
test <- read_csv(paste0("data/eddypro_data/", x[1]))


