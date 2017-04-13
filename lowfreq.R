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
lowfreq <- read_csv("data/CSV_1073.LowFreqData.dat", col_names = FALSE)
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

#Energy balance equation:
#Rn = G + LE + H 

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
  
  
  
