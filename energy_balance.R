#Put processed data together in an energy balance.
#Adrienne Marshall
#April 23, 2017

#Read data.
home <- "/Volumes/research_storage/energy_balance"
setwd(home)

library(lubridate)
library(reshape2)
library(ggthemes)

eh <- read_csv("data/E_H.csv")
gsr <- read_csv("data/G_RN_S.csv")

#Add date information to g_s_r.
gsr <- gsr %>% 
  mutate(date = as.Date(strptime(paste(year, doy), format="%Y %j")))%>% 
  filter(date > "2017-03-31") %>%
  filter(date < "2017-04-18") %>%
  mutate(hhmm = ifelse(nchar(hhmm)==3, 
                       paste0("0", hhmm),
                       ifelse(nchar(hhmm)==2,
                              paste0("00",hhmm),
                              ifelse(nchar(hhmm==1),
                                     paste0("000",hhmm))))) %>%
  mutate(hour = substr(hhmm, start = 1, stop = 2)) %>%
  mutate(min = substr(hhmm, start = 3, stop = 4)) %>%
  mutate(date_time = ymd_hm(paste(date, hour, min)))

#Make a pared-down gsr.
gsr_simple <- gsr %>% select(doy, storage, G, rnet, date_time)
eh_simple <- eh %>% select(start, start_doy, H, lambda_E)
names(eh_simple)[1] <- "date_time"
names(eh_simple)[2] <- "doy"

data <- full_join(gsr_simple, eh_simple, by = c("date_time", "doy"))

#Make a plot. ---------
datal <- melt(data, id.vars = c("date_time", "doy"))
png("R/plots/all_fluxes1.png", width = 9, height = 6, res = 300, units = 'in')
ggplot(datal, aes(x = date_time, y = value, group = variable, color = variable)) +
  geom_line() + 
  theme_few()
dev.off()

#Calculate closure at half-hour interval. -------- 
data <- data %>% mutate(residual = rnet + H + G + lambda_E - storage)

png("R/plots/residual.png", width = 9, height = 6, res = 300, units = 'in')
ggplot(data, aes(x = date_time, y = residual)) + 
  geom_line() + 
  theme_few()
dev.off()

#Group by day and calculate residual. ---------
#Should add a sum of NAs to this.
daily <- data %>% group_by(doy) %>%
  summarise(count = n(),
            storage = sum(storage, na.rm = TRUE),
            G = sum(G),
            rnet = sum(rnet, na.rm = TRUE),
            H = sum(H, na.rm = TRUE),
            lambda_E = sum(lambda_E, na.rm = TRUE))
daily <- daily %>% mutate(residual = rnet + H + G + lambda_E - storage)
dailyl <- melt(daily, id.vars = "doy") %>% filter(!variable %in%  c("count"))

#Make plots.

png("R/plots/daily_fluxes.png", width = 9, height = 6, res = 300, units = 'in')
ggplot(dailyl, aes(x = doy, y = value, group = variable, color = variable)) +
  geom_line() + 
  theme_few() +
  labs(x = "Day of year",
       y = "Value (W/m2)")
dev.off()

#Next steps: think about whether directions are correct!!
#And see how well rnet matches LW and SW in and out.

#Try choosing one day.
one_day <- datal %>% filter(doy == "104") %>%
  arrange(date_time, variable)
ggplot(one_day, aes(x = date_time, y = value, group = variable, color = variable)) +
  geom_line() + 
  theme_few() +
  labs(x = "Hour",
       y = "Value (W/m2)")







