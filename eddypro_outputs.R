#April 24, 2017. Adrienne Marshall.

#Read eddypro outputs. 

home <- "/Volumes/research_storage/energy_balance"
setwd(home)

files <- list.files("data/eddypro_outputs")

files <- files[grepl(".csv", files)]

#Get the file with "essentials" in the title.
full <- files[grepl("full_output",files)]
full_dat <- read.csv(paste0("data/eddypro_outputs/", full),
                       skip = 1,
                     stringsAsFactors = FALSE)
full_dat <- full_dat[-1,]
full_dat[full_dat == "-9999.0"] <- NaN


data <- full_dat %>% select(date, time, H, qc_H, LE, qc_LE)


#Fix date-time.
data <- data %>% mutate(hour = substr(time, 1, 2)) %>%
  mutate(min = substr(time, 4, 5)) %>%
  mutate(sec = 0) %>%
  mutate(date_time = ymd_hms(paste(date, hour, min, sec)))

data <- data %>% select(date_time, H, qc_H, LE, qc_LE)

write_csv(data, "data/eddypro_output_simple.csv")
