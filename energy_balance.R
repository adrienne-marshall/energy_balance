#Put processed data together in an energy balance.
#Adrienne Marshall
#April 23, 2017

#Read data.
home <- "/Volumes/research_storage/energy_balance"
setwd(home)

library(lubridate)
library(tidyverse)
library(reshape2)
library(ggthemes)
library(gridExtra)
library(RColorBrewer)
library(viridis)
library(broom)
library(zoo)

#eh <- read_csv("data/E_H.csv")
eh <- read_csv("data/eddypro_output_simple2.csv")
gsr <- read_csv("data/G_RN_S.csv")
penman <- read_csv("data/Energy_balance_result_Penman-montieth.csv")
sunny <- read_csv("data/daily_solar.csv")
storage <- read_csv("data/storage.csv")

#Use storage from Ames' script (lowfreqww)
gsr$storage <- storage$storage

#Add date information to gsr. ------
gsr <- gsr %>% 
  mutate(date = as.Date(strptime(paste(year, doy), format="%Y %j")))%>% 
  filter(date > "2017-03-31") %>%
  filter(date < "2017-04-18") %>%
  mutate(hhmm = ifelse(nchar(hhmm)==3, 
                       paste0("0", hhmm),
                       ifelse(nchar(hhmm)==2,
                              paste0("00",hhmm),
                              ifelse(nchar(hhmm)==1,
                                     paste0("000",hhmm),
                                     hhmm)))) %>%
  mutate(hour = substr(hhmm, start = 1, stop = 2)) %>%
  mutate(min = substr(hhmm, start = 3, stop = 4)) %>%
  mutate(date_time = ymd_hm(paste(date, hour, min)))
gsr <- gsr %>% select(date, hhmm, date_time, everything())


#Make a pared-down gsr and eh. Change sign on storage. Use calculated rnet. -----------
gsr <- gsr %>% mutate(rnet_calc = sw_in + lw_in - sw_out - low_out)

gsr_simple <- gsr %>% select(doy, storage, G, rnet_calc, date_time) %>%
  mutate(storage = -storage)
eh_simple <- eh %>% select(date_time, H, LE) %>%
  mutate(doy = yday(date_time)) %>%
  filter(doy < 108)
le <- c(penman$LE)
eh_simple <- eh_simple %>% mutate(penman_LE = le)


#eh_simple <- eh %>% select(start, start_doy, H, lambda_E)
#names(eh_simple) <- c("date_time", "doy", "H", "LE")

data <- full_join(gsr_simple, eh_simple, by = c("date_time", "doy")) %>%
  select(date_time, doy, everything())

write_csv(data, "data/energy_balance_results.csv")

#Use a spline function to fill in missing results.
test <- data.frame(sapply(data[,3:7], na.spline))

data_fill <- cbind(data[1:2], test, data[8])
write_csv(data, "data/energy_balance_results_spline.csv")

#Make a plot. ---------
datal <- melt(data_fill, id.vars = c("date_time", "doy"))
#datal <- datal %>% filter(variable != "lambda_E")
d <- ggplot(datal, aes(x = date_time, y = value, group = variable, color = variable)) +
  geom_line() + 
  theme_few() +
  facet_grid(variable~., labeller = as_labeller(c("storage" = "dS/dt",
                                                  "G" = "G",
                                                  "rnet" = "Rnet",
                                                  "H" = "H",
                                                  "LE" = "LE"))) +
  labs(y = "Value (W/m2)",
       x = "Date")

png("R/plots/all_fluxes_faceted.png", width = 9, height = 6, res = 300, units = 'in')
d
dev.off()

d2 <- ggplot(datal, aes(x = date_time, y = value, group = variable, 
                        color = factor(variable, labels = c("dS/dt", "G", "Rnet", "H", "LE")))) +
  geom_line() + 
  theme_few() +
  labs(y = "Value (W/m2)",
       x = "Date",
       color = "Flux:") +
  theme(legend.position = "bottom")
d2

#Calculate closure at half-hour interval. -------- 
data_fill <- data_fill %>% mutate(residual = rnet_calc - H - G - LE - storage)
r <- ggplot(data_fill, aes(x = date_time, y = residual)) + 
  geom_line(show.legend = TRUE) + 
  theme_few()

png("R/plots/residual.png", width = 9, height = 6, res = 300, units = 'in')
r
dev.off()

png("R/plots/data_residual.png", width = 9, height = 6, res = 300, units = 'in')
grid.arrange(d2, r, nrow = 2)
dev.off()

#Group by day and calculate residual. ---------
#Should add a sum of NAs to this.
data_fill <- data_fill %>% mutate(date = date(date_time))
daily <- data_fill %>% group_by(date) %>%
  filter(date < "2017-04-18") %>%
  summarise(count = n(),
            dS_dt = round(sum(storage, na.rm = TRUE),0),
            G = round(sum(G, na.rm = TRUE),0),
            Rnet = round(sum(rnet_calc, na.rm = TRUE),0),
            H = round(sum(H, na.rm = TRUE),0),
            LE = round(sum(LE, na.rm = TRUE),0),
            modeled_LE = round(sum(penman_LE, na.rm = TRUE),0),
            penman_na = sum(is.na(penman_LE)))
daily <- daily %>% mutate(residual = Rnet - H - G - LE - dS_dt) %>%
  mutate(penman_residual = ifelse(penman_na < 5, Rnet - H - G - modeled_LE - dS_dt, NA))

#Merge with cloudy/sunny.
#names(sunny) <- c("date", "solar", "class")
daily <- full_join(daily, sunny[,c("date", "class")], by = "date")

daily <- daily %>% mutate(available = Rnet - G - dS_dt) %>%
  mutate(observed_used = LE + H) %>%
  mutate(modeled_used = ifelse(penman_na < 5, (modeled_LE + H), NA)) %>%
  mutate(observed_residual = (observed_used - available)/available) %>%
  mutate(modeled_residual = (modeled_used - available)/available)

#Add daily incoming radiation (sw_in + lw_in).
rad_in <- gsr %>% select(date, sw_in, lw_in) %>%
  group_by(date) %>%
  summarise(sw_in = sum(sw_in),
            lw_in = sum(lw_in)) %>%
  mutate(rad_in = sw_in + lw_in) %>%
  select(-sw_in, -lw_in)

daily <- full_join(daily, rad_in, by = "date") %>% select(-count, -penman_na)


#calculate error.
daily <- daily %>% mutate(penman_error = penman_residual/rad_in) %>%
  mutate(measured_error = residual/rad_in)

#Melt for plotting (might need to move).
dailyl <- melt(daily, id.vars = "date") %>% filter(!variable %in%  c("count"))

daily_means <- daily <- data_fill %>% group_by(date) %>%
  filter(date < "2017-04-18") %>%
  summarise(count = n(),
            dS_dt = round(mean(storage, na.rm = TRUE),0),
            G = round(mean(G, na.rm = TRUE),0),
            Rnet = round(mean(rnet_calc, na.rm = TRUE),0),
            H = round(mean(H, na.rm = TRUE),0),
            LE = round(mean(LE, na.rm = TRUE),0))
daily_means <- daily_means %>% mutate(residual = Rnet - H - G - LE - dS_dt)
dailyl <- melt(daily, id.vars = "date") %>% filter(!variable %in%  c("count"))


#Make daily plots.-----
plot_dat <- dailyl %>% filter(variable != "residual")
d <- ggplot(plot_dat, aes(x = date, y = value, group = variable, 
                          color = factor(variable, labels = c("dS/dt", "G", "Rnet", "H", "LE")))) +
  geom_line(size = 1) + 
  theme_few() +
  labs(x = "Date",
       y = "Value (W/m2)",
       color = "Flux:") +
  theme(legend.position = "bottom")
d

png("R/plots/daily_fluxes.png", width = 9, height = 6, res = 300, units = 'in')
d
dev.off()

plot_dat <- dailyl %>% filter(variable == "residual")
r <- ggplot(plot_dat, aes(x = date, y = value), color = "black") +
  geom_line(show.legend = TRUE) + 
  theme_few() +
  labs(x = "Day of year",
       y = "Residual (W/m2)")

png("R/plots/daily_fluxes_residual.png", 
    width = 9, height = 6, res = 300, units = 'in')
grid.arrange(d, r, nrow = 2)
dev.off()


#Next steps: think about whether directions are correct!!
#Consider taking out rainy days? Might the rain interfere with hygrometer?
#And see how well rnet matches LW and SW in and out.

#Try choosing one day.
one_day <- datal %>% filter(doy == "98") %>%
  arrange(date_time, variable)

ggplot(one_day, aes(x = date_time, y = value, group = variable, color = variable)) +
  geom_line() + 
  theme_few() +
  labs(x = "Hour",
       y = "Value (W/m2)")

#Get sums over the whole period. -------
data_fill <- data_fill %>% select(date_time, everything())
sapply(data_fill[,3:8], sum, na.rm = TRUE)/length(unique(data_fill$date))

#Make a plot: time of day and net radiation (or incoming solar).

#Compare Rnet and four-component. 
gsr <- gsr %>% mutate(rnet_calc = sw_in + lw_in - sw_out - low_out)

rad <- gsr %>% 
  select(date, hhmm, date_time, doy, sw_in, lw_in, sw_out, low_out, rnet, rnet_calc)

names(rad)[names(rad) == "low_out"] <- "lw_out"

fit <- lm(rnet_calc ~ rnet, data = rad)
slope <- fit$coefficient[2]
int <- fit$coefficient[1]
r2 <- "0.9558"
equation <- paste0("4-Component Rnet = ", slope, "*Rnet + ", int, "\n", 
                   "R2 = ", r2)

lm_eqn <- function(df){
  m <- lm(rnet_calc ~ rnet, df);
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(coef(m)[1], digits = 2), 
                        b = format(coef(m)[2], digits = 2), 
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));                 
}


scat <- ggplot(rad, aes(x = rnet, y = rnet_calc)) + 
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") + 
  theme_few() + 
  geom_text(x = 0, y = 500, label = lm_eqn(rad), parse = TRUE) +
  #annotate("text", x = 0, y = 500, equation) +
  labs(y = "Rnet from four-component (W/m2)",
       x = "Rnet from net radiometer (W/m2)",
       title = "(a)")
scat

#Plot residuals. 
df <- augment(fit)
resid <- ggplot(df, aes(x = .fitted, y = .resid)) + 
  geom_point() +
  theme_few() +
  labs(x = "4-component Rnet (W/m2)",
       y = "Residual (W/m2)",
       title = "(b)")
resid



png("R/plots/radiation_scatter.png", width = 8, height = 4, res = 300, units = 'in')
grid.arrange(scat, resid, nrow = 1, ncol = 2)
dev.off()


#Plot all the radiation components. 
radl <- melt(rad, id.vars = c("date", "hhmm", "date_time", "doy"))
radl <- radl %>% mutate(numeric_hour = hour(date_time) + minute(date_time)/60)

rad_plot <- ggplot(radl, aes(x = date_time, y = value, color = variable)) +
  geom_line() +
  theme_few()
rad_plot

rad_hourly <- ggplot(radl, 
                     aes(x = numeric_hour,
                         y = value, 
                         color = variable)) +
  geom_smooth(se = FALSE) + 
  geom_point() +
  #geom_line() +
  facet_grid(variable ~. , scales = "free") +
  theme_few()

radl$doy <- as.numeric(radl$doy)

rad_day <- ggplot(radl, aes(x = numeric_hour, y = value)) +
  geom_line(aes(color = doy, group = doy), show.legend = FALSE) +
  facet_grid(variable ~., scales = "free", 
             labeller = as_labeller(c("sw_in" = "SWin",
                          "sw_out" = "SWout",
                          "lw_in" = "LWin",
                          "lw_out" = "LWout",
                          "rnet" = "Rnet",
                          "rnet_calc"= "4-comp Rnet"))) +
  scale_color_distiller(type = "qual", palette = "Accent") +
  labs(x = "Hour of day",
       y = "Value (W/m2)")
rad_day
  #theme_few() +
  #theme(panel.grid.major = element_line(color = "grey80"),
  #      panel.grid.minor = element_line(color = "grey90"))

png("R/plots/radiation_summary.png", width = 9, height = 6, res = 300, units = 'in')
rad_day
dev.off()
