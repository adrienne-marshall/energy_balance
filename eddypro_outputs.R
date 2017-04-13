#April 12, 2017. Adrienne Marshall.

#Read eddypro outputs. 

home <- "/Volumes/research_storage/energy_balance"
setwd(home)

files <- list.files("data/eddypro_outputs")

files <- files[grepl(".csv", files)]

essentials <- files[grepl("essentials",files)]
essentials <- read.csv(paste0("data/eddypro_outputs/", essentials),
                       skip = 1)

# data <- list()
# for(i in 1:length(files)) {
#   data[[i]] <- read_csv(paste0("data/eddypro_outputs/", files[i]),
#                         skip = 18,
#                         na = "-6999")
# }

