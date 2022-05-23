################################################################################
### Map NWS Day 1 Categorical Risk data for Middle TN - /u/MetricT 
### Data from:  https://www.spc.noaa.gov/gis/
################################################################################

library(tidyverse)
library(tigris)
library(scales)
library(shadowtext)
library(sf)

### Limiting the map to just neighboring states to speed up endering
tn_neighboring_states <-
  c("01", "05", "13", "17", "18", "21", "28", "29", "37", "39", "45", "47", "51")

### Cache TIGRIS data to speed up future map renders
options(tigris_use_cache = TRUE)

### Bounding box for Tennessee
map_x_min <- -81.5
map_x_max <- -90.5
map_y_min <-  34.5
map_y_max <-  37.0

### Latitude/Longitude of reference cities
map_cities <-
  tribble(
    ~name,            ~lat,      ~long,
    "Nashville",      "36.17", "-86.78",
    "Memphis",        "35.11", "-90.01",
    "Knoxville",      "35.97", "-83.95",
    "Chattanooga",    "35.07", "-85.26",
    "Clarksville",    "36.56", "-87.35",
    "Jackson",        "35.61", "-88.81",
    "Johnson City",   "36.31", "-82.35",
    "Dyersburg",      "36.03", "-89.39",
    "Cookeville",     "36.16", "-85.50",
    "Murfreesboro",   "35.85", "-86.39",
  ) %>% mutate(lat = as.numeric(lat), long = as.numeric(long))

### Load Day 1 Categorical shapefile from NOAA and apply bounding box
map_weather <- 
  "https://www.spc.noaa.gov/products/outlook/day1otlk_cat.kmz" %>%
  read_sf() %>% 
  st_transform(crs = "NAD83") 

# Pull the start/end valid dates for the map and convert from UTC to CST
weather_start <- map_weather %>% st_drop_geometry() %>% select(begin) %>% unique() %>% pull(begin)
weather_end   <- map_weather %>% st_drop_geometry() %>% select(end)   %>% unique() %>% pull(end)

### Load state/county outline maps from TIGRIS
map_county <- tigris::counties() %>% filter(STATEFP %in% tn_neighboring_states)
map_state  <- tigris::states()

### Also fetch data on interstates
map_interstates <- tigris::primary_roads() %>% filter(RTTYP == "I")

### Rivers shapefile download at:  https://www.weather.gov/source/gis/Shapefiles/Misc/rs16my07.zip
map_rivers <- 
  read_sf("../Shapefiles/NWS/rivers_subset") %>%
  st_set_crs(., "NAD83") %>%
  st_crop(., xmin = map_x_min, xmax = map_x_max, ymin = map_y_min, ymax = map_y_max)

### Map the data and done
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
       title = "NWS Day 1 Categorical Risk Map", 
       subtitle = paste("(valid from ", weather_start, " until ", weather_end, ")", sep = "")) + 
  geom_sf(data = map_weather, aes(fill = Name), color = NA, size = 0.2) + 
  geom_sf(data = map_state, fill = NA, size = 0.7) + 
  geom_sf(data = map_county, fill = NA, size = 0.15) +
  geom_sf(data = map_interstates, fill = NA, color = "darkorchid2", size = 0.5) +
  geom_sf(data = map_interstates, fill = NA, color = "white", linetype = "dashed", size = 0.4) +
  geom_sf(data = map_rivers, fill = "blue", color = "steelblue3", size = 0.5) + 
  geom_point(data = map_cities, aes(x = long, y = lat), shape = 21, fill = "steelblue1", size = 3) + 
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
