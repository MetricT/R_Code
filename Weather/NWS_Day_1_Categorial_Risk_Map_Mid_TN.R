### Plots the NWS Day 1 Categorical Risk map for Middle TN - /u/MetricT
### Data from:  https://www.spc.noaa.gov/gis/

library(tidyverse)
library(tigris)
library(scales)
library(shadowtext)
library(maps)
library(sf)

### Limiting the map to just neighboring states helps speed up the rendering
tn_neighboring_states <-
  c("01", "05", "13", "17", "18", "21", "28", "29", "37", "39", "45", "47", "51")

### Cache TIGRIS data to speed up future map renders
options(tigris_use_cache = TRUE)

### Bounding box for Tennessee
map_x_min <- -81.5
map_x_max <- -90.5
map_y_min <-  34.5
map_y_max <-  37.0

### Load map of US cities
map_cities <-
  maps::world.cities %>% 
  filter(
    country.etc == "USA",
    pop >= 100000,
    long >= map_x_max, long <= map_x_min, 
    lat >= map_y_min, lat <= map_y_max)

### Load shapefile with the current day's outlook from NOAA
day1_categorical <- read_sf("https://www.spc.noaa.gov/products/outlook/day1otlk_cat.kmz")

### Create a local map using a bounding box, and change colors to their standard representation
map_weather <- day1_categorical %>% st_transform(crs = "NAD83") 

# Pull the start/end valid dates for the map and convert from UTC to CST
weather_start <- map_weather %>% st_drop_geometry() %>% select(begin) %>% unique() %>% pull(begin)
weather_end   <- map_weather %>% st_drop_geometry() %>% select(end)   %>% unique() %>% pull(end)

### Load state/county outline maps from TIGRIS
map_county <- 
  tigris::counties() %>%
  filter(STATEFP %in% tn_neighboring_states)

map_state <- 
  tigris::states()

map_interstates <-
  tigris::primary_roads() %>%
  filter(RTTYP == "I")

weather_map <-
  ggplot() +
  theme_bw() + 
  theme(
    plot.title      = element_text(hjust = 0.5),
    plot.subtitle   = element_text(hjust = 0.5),
    legend.position = "bottom",
    legend.margin=margin(-10, 0, 0, 0)
    ) +
  labs(x = "", y = "", 
       #x = "Longitude", y = "Latitude",
       title = "NWS Day 1 Categorical Risk Map", 
       subtitle = paste("(valid from ", weather_start, " until ", weather_end, ")", sep = "")) + 
  geom_sf(data = map_weather, aes(fill = Name), color = NA, size = 0.2) + 
  geom_sf(data = map_state, fill = NA, size = 0.7) + 
  geom_sf(data = map_county, fill = NA, size = 0.15) +
  geom_sf(data = map_interstates, fill = NA, color = "blue", size = 0.5) +
  geom_sf(data = map_interstates, fill = NA, color = "white", linetype = "dashed", size = 0.4) +
  geom_point(data = map_cities, aes(x = long, y = lat), shape = 21, fill = "steelblue4", size = 3) + 
  geom_shadowtext(data = map_cities, aes(x = long, y = lat, label = name), color = "white", bg.color = "black", size = 5.5, nudge_y = 0.07) + 
  scale_fill_manual(name = "Categorical Risk", values = c(
    "General Thunder" = "#c0e8c0",
    "Marginal Risk"   = "#7fc57f",
    "Slight Risk"     = "#f6f67f",
    "Enhanced Risk"   = "#e6c27f",
    "Moderate Risk"   = "#e67f7f",
    "Severe Risk"     = "#ff7fff"
  )) +
  coord_sf(xlim = c(map_x_min, map_x_max), ylim = c(map_y_min, map_y_max), expand = FALSE)

print(weather_map)
