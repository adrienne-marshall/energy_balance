#Check out initial data - will probably be unstructured. 
library(tidyverse); library(reshape2)

home <- "/Volumes/research_storage/energy_balance"
setwd(home)

dat <- read_csv("data/initial_data.csv")

names(dat) <- gsub(" ", "", names(dat))
names(dat) <- gsub("\\[", "", names(dat))
names(dat) <- gsub("\\]", "", names(dat))

dat1 <- dat %>% group_by(dayofyear, hhmm) %>% 
  summarise(sonictemp = mean(SonicTempC),
            finetempL = mean(FineWireTemplowerC),
            finetempH = mean(FineWireTempupperC),
            P14temp = mean(P14FineWireTempC),
            P14RH = mean(P14RH),
            ea = mean(P14EakPa))
dat1 <- dat1 %>% dplyr::mutate(minutes = 1:nrow(dat1))

dat1l <- melt(dat1, id.vars = "minutes")
dat1l <- dat1l %>% mutate(min = c(1:nrow(dat1l)))
