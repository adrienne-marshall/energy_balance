#Process highfrequency data for calculating sensible and latent heat fluxes.
#Author: Adrienne Masrhall
#Last edited: April 23, 2017.


#Set up.
library(tidyverse)
library(lubridate)

home <- "/Volumes/research_storage/energy_balance"
setwd(home)

#Set constants.--------
pa <- 101.3*exp(-719/8200) #Air pressure, kpa
cp <- 29.3 #specific heat of air, J/mol K.


#Read data.--------
high_base <- read_csv("data/CSV_1073.HighFreqData_Final.dat", col_names = FALSE)
names(high_base) <- c("n", "year", "doy", "hhmm", "sec", "ux", "uy", "uz", "sonic_temp", "fwt_low", "fwt_high", 
                 "fwt_p14", "rh", "ea_kpa", "ea_gm3")
high <- high_base %>% select(-n)
high <- high %>% mutate(index = (1:nrow(high)))

#Replace -7999 with NA.
high[high == -7999] <- NaN

#Add a date-time. Note: this doesn't include tenths of seconds.  --------
#high <- high[1:1000000,]
high <- high %>% 
  mutate(date = as.Date(strptime(paste(year, doy), format="%Y %j")))



# test <- test %>% mutate(b = ifelse(nchar(a) == 3,
#                                          paste0("0", a),
#                                          ifelse(nchar(a) == 2,
#                                                 paste0("00", a),
#                                                 ifelse(nchar(a)== 1,
#                                                       paste0("000", a),
#                                                       a
#                                                       ))))
#Add zeros to hhmm as needed. 
high <- high %>% 
  mutate(hhmm = ifelse(nchar(hhmm)==3, 
                       paste0("0", hhmm),
                       ifelse(nchar(hhmm)==2,
                              paste0("00",hhmm),
                              ifelse(nchar(hhmm)==1,
                                     paste0("000",hhmm),
                                     hhmm))))
unique(high$hhmm)

#Start and end on full days. ------
high <- high %>% filter(date > "2017-03-31") %>%
  filter(date < "2017-04-18")

#Make a date. -----
high <- high %>%
  mutate(hour = substr(hhmm, start = 1, stop = 2)) %>%
  mutate(min = substr(hhmm, start = 3, stop = 4)) %>%
  mutate(date_time = ymd_hms(paste(date, hour, min, sec)))

#Make intervals by averaging period. 

temp <- data.frame(table(cut(high$date_time, breaks = "30 mins"))) 
temp <- temp %>% mutate(interval = 1:nrow(temp)) %>% select(-Freq)
names(temp)[1] <- "date_time"
temp$date_time <- as.POSIXct(temp$date_time, tz = "UTC")

#Add intervals to high data frame.
high <- left_join(high, temp, by = "date_time")
high <- high %>% fill(interval, .direction = "down")

#Add density of air based on temperature. ---------
high <- high %>% mutate(rho_a = 44.6*(pa/101.3)*(273.15/(fwt_high+273.15)))

#Redo ea based on calibration. ------
B1 <- -0.001
B2 <- 0.1406
B3 <- 0.023
B4 <- -3.2768
high <- high %>% mutate(delta = (B1*rh*100 + B2)*fwt_high + (B3*rh*100 + B4)) %>%
  mutate(adjusted_RH = rh + delta/100)

#Calculate es(Ta).
a <- 0.611
b <- 17.502
c <- 240.97
high <- high %>% mutate(es_ta = a*exp((b*fwt_high)/(fwt_high+c)))
high <- high %>% mutate(ea_new = es_ta*adjusted_RH)

#Recalculate concentration of water in air. (don't really need this for calculations) 
#Mw <- 18
#R <- 8.31
#high <- high %>% mutate(h2o_conc = ea*1000*Mw/(8.31*(fwt_high+273.15)))

#high <- high %>% select(interval, sec, ux, uy, uz,, fwt_high, ea)

#Try calculating covariance - not separating out calculations for means. --------
#Answer from this method seems wrong...
# covariance <- high %>% group_by(interval) %>%
#   summarize(cov_ea = cov(x = uz, y = ea_new, use = "pairwise.complete.obs"),
#             cov_h = cov(x = uz, y = fwt_high, use = "pairwise.complete.obs"),
#             rho_a = mean(rho_a)) 
# ans1 <- covariance %>% mutate(H = rho_a*cp*cov_h) %>%
#   mutate(E = rho_a*cov_ea) %>%
#   mutate(lambda_E = E*40.8*1000)
# 
# ans1$lambda_E[ans1$lambda_E > 2000] <- NaN
# ans1$lambda_E[ans1$lambda_E < -2000] <- NaN
# ggplot(ans1, aes(x = interval, y = lambda_E)) + geom_line()

#Calculate means so we can get fluctuations about the mean.-------
means <- high %>% group_by(interval) %>%
  summarise(mean_ux = mean(ux, na.rm = TRUE),
            mean_uy = mean(uy, na.rm = TRUE),
            mean_uz = mean(uz, na.rm = TRUE),
            mean_ea = mean(ea_new, na.rm = TRUE),
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
            mean_wc = mean(wc, na.rm = TRUE),
            rho_a = mean(rho_a, na.rm = TRUE))

#Multiply by constants to get fluxes.
#Units for H: W/m^2(?), and for E: mol/s m^2
ans <- ans %>% mutate(H = rho_a*cp*mean_wT) %>%
  mutate(E = rho_a*mean_wc)


ans <- ans %>% mutate(lambda_E = E*40.8*1000)

#E seems to get unreasonably large and is spiky. Constrain it to less than 1000W/m2.
ans$lambda_E[ans$lambda_E > 1000] <- NaN
ans$lambda_E[ans$lambda_E < -1000] <- NaN

#Combine with date information. 
ans <- ans %>% select(-mean_wT, -mean_wc)
times <- high %>% select(year, doy, hhmm, date_time, interval)
dat <- left_join(ans, times, by = "interval")
dat_concise <- dat %>% group_by(interval) %>%
  summarise(year = mean(year),
            start_doy = min(doy),
            end_doy = max(doy),
            start = head(date_time, 1),
            end = tail(date_time, 1),
            H = mean(H),
            lambda_E = mean(lambda_E))

quartz()
ggplot(dat_concise, aes(x = start, y = lambda_E)) + geom_line() 

#dat_concise <- dat_concise %>% mutate(time = ifelse(end>start, end - start, NaN))

#Write a csv.
write_csv(dat_concise, "data/E_H.csv", col_names = TRUE)

