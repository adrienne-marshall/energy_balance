##April 12, 2017. Adrienne Marshall.

#Check out high frequency data. 

#Set up.
library(tidyverse)

home <- "/Volumes/research_storage/energy_balance"
setwd(home)

dat <- read_csv("data/TOA5_1073.HighFreqData.dat", 
                col_names = TRUE,
                skip = 1, 
                na = "NAN")
dat <- dat[-c(1,2),]
#Make data numeric.
dat <- dat %>% mutate_at(-1, as.numeric)



test_dat <- dat[1:100,]

#Adjust humidity data. 
B1 <- -0.001
B2 <- 0.1406
B3 <- 0.023
B4 <- -3.2768
dat <- dat %>% mutate(delta = (B1*P14RH*100 + B2)*P14Temp + (B3*P14RH*100 + B4)) %>%
  mutate(adjusted_RH = P14RH + delta/100)

#Calculate es(Ta).
a <- 0.611
b <- 17.502
c <- 240.97
dat <- dat %>% mutate(es_ta = a*exp((b*FW3)/(FW3+c)))
dat <- dat %>% mutate(ea = es_ta*adjusted_RH)

#Recalculate concentration of water in air. 
Mw <- 18
R <- 8.31
dat <- dat %>% mutate(h2o_conc = ea*1000*Mw/(8.31*(FW3+273.15)))

#Select only the relevant rows.
dat <- dat %>% select(TIMESTAMP, RECORD, Ux, Uy, Uz, Ts, FW3, h2o_conc)

write_csv(dat, "data/adjusted_highfreq.csv")

#------
dat <- read_csv("data/adjusted_highfreq.csv")
write_csv(dat, "data/2017-03-01_1857_highfreq-01.csv")





