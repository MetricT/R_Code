################################################################################
### Use the "rNOMADS" library to map the temperature at a given time.
### R is far too slow for production use, but this is a simple way for 
### interested people to poke around and see what models/data are out there
### and how to use them.  - /u/MetricT
################################################################################

library(tidyverse)
library(tibble)
library(scales)
library(lubridate)
library(cowplot)
library(viridis)
library(rNOMADS)

# What date/time do you want a forecast for.  It will download as close to
# this time as possible, without going over.
forecast_datetime <- "2022-12-23 07:00:00 CST"

# We're going to look at the "gfs_0p25_1hr" model
# See the "available_models" tab to see other models you can choose
our_model <- "gfs_0p25_1hr"

# What going to look at the 2M surface temperature data in the "gfs_0p25_1hr" model
# See the "available_model_datasets" tab to see other models you can choose
our_model_data <- "tmp2m"

# Get a list of available weather models (abrev, name, url)
available_models <- NOMADSRealTimeList("dods") %>% as_tibble()
View(available_models)

# Find the number of hours between forecast_datetime and now so we can
# pull the forecast for that time
forecast_hours <- 
  difftime(as.POSIXct(forecast_datetime), Sys.time(), units = "hours") %>% 
  round() %>% 
  as.integer()

# Fetch model data between c(start_time, end_time), in hours relative to now. 
# 0 for current time.  Recommend only doing one at a time as more than that 
# can easily crash your R session.
time <- c(forecast_hours, forecast_hours)

# Get a map of the world so we can add countries to the final map
map_world <- 
  map_data("world") %>% 
  mutate(long = ifelse(long > 180, long - 360, long))

# Get the available dates and urls for our model
model.urls <- GetDODSDates(our_model)

# These settings are appropriate for the gfs_0p25_1hr forecast, but have to be
# changed for other forecasts which use a different grid size.   Haven't found
# a way to do this programatically yet.
lon  <- c(0, 1439) # A 0.25 deg model has 1440 longitude points (360 / 0.25)
lat  <- c(0, 720)  # A 0.25 deg model has  720 latitude points  (180 / 0.25)
lev  <- NULL       # DO NOT include level if variable is non-level type

# Get the URL for the latest chosen model
latest.model <- tail(model.urls$url, 1)

# Get the latest run of the model
latest.model.run <- 
  GetDODSModelRuns(latest.model) %>%
  .$model.run %>%
  tail(n = 1)

# List the available datasets included with this model
available_model_datasets <- 
  GetDODSModelRunInfo(latest.model, latest.model.run) %>%
  as_tibble() %>% 
  filter(grepl("<c2>", value)) %>% 
  mutate(
    value = gsub("<c2><a0> ", "", value), 
    value = gsub(" \\*\\* ", "|", value), 
    value = gsub(" \\[", "|[", value)) %>% 
  separate(value, into = c("code", "descr", "units"), sep = "\\|") %>%
  arrange(units, code)
View(available_model_datasets)

# Pull 2M surface temperature data
model.data <-
  DODSGrab(latest.model, latest.model.run, our_model_data, time, levels = NULL, lon, lat) %>%
  as_tibble() %>%
  mutate(lon   = ifelse(lon > 180, lon - 360, lon))

model.date <-
  model.data %>% 
  pull(forecast.date) %>% 
  tail(n = 1) %>% 
  with_tz(tzone = "America/Chicago")

g_temperature <- 
  ggplot() + 
  theme_bw() +
  
  # Raster of chosen weather model data
  geom_raster(data = model.data, 
              aes(x = lon, y = lat, fill = 32 + (value - 273.15) * 9/5)) + 
  
  # World map
  geom_map(
    data = map_data("world"), map = map_data("world"),
    aes(x = long, y = lat, map_id = region),
    color = "black", fill = NA, linewidth = 0.3) +
  
  # US State map  
  geom_map(
    data = map_data("state"), map = map_data("state"),
    aes(x = long, y = lat, map_id = region),
    color = "black", fill = NA, linewidth = 0.1) +
  
  scale_fill_gradient2(mid = "white", low = "royalblue4", high = "orangered4", midpoint = 32, 
                       name = "2M Surface\nTemperature", labels = label_number(suffix ="\u00b0 F")) +
  
  scale_x_continuous(breaks = pretty_breaks(15), labels = label_number(suffix ="\u00b0"), limits = c(-124.74, -66.95)) + 
  scale_y_continuous(breaks = pretty_breaks(15), labels = label_number(suffix ="\u00b0"), limits = c(24.52, 49.38)) +
  
  labs(x = "Longitude", y = "Latitude", 
       title = paste("2M Surface Temperature - ", model.date))

print(g_temperature)
