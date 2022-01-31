################################################################################
# Map:  Bridges in "poor" (rating <= 4) shape - by /u/MetricT
# 
# Pulls and maps data on deficient bridges from the Federal Highway Administration.
# Data URL:  https://www.fhwa.dot.gov/bridge/nbi/ascii.cfm
#
# Bridges in "poor" condition have a numerical rating of 4 or less.   The lowest 
# rating or a specific bridge component (superstructure, substructure, or deck) 
# determines the overall rating of Good/Fair/Poor.
#
# Inspiration:
# https://www.reddit.com/r/dataisbeautiful/comments/seyyz4/percent_of_us_highway_bridges_in_poor_condition/
################################################################################

library(tidyverse)
library(tigris)
library(sf)

# Find the FIPS code for your location. 
# You can edit the query to do another location.
fips <- 
  tigris::fips_codes %>% 
  filter(state_name == "Tennessee",
         county == "Davidson County")

### You shouldn't need to modify anything below here ###

fips_state      <- fips %>% pull("state_code")
fips_county     <- fips %>% pull("county_code")
fips_state_txt  <- fips %>% pull("state")
fips_county_txt <- fips %>% pull("county")

# Load county, water, and road maps from Tigris (crs = NAD83)
counties <-
  counties(
    state = fips_state,
    cb = FALSE, 
    class = "sf") %>%
  filter(COUNTYFP %in% c(fips_county))

areawater   <- area_water(state = fips_state, county = fips_county, class = "sf")
linearwater <- linear_water(state = fips_state, county = fips_county, class = "sf")
roads       <- roads(state = fips_state, county = fips_county, class = "sf")

interstates <- roads %>% filter(RTTYP == "I")
hwy         <- roads %>% filter(RTTYP %in% c("C", "S", "U")) %>% filter(!grepl("^Old", FULLNAME))
rivers      <- linearwater
lakes       <- areawater

# Read bridge data from the Federal Highway Administration and filter it for this location
data_deficient_bridges_raw <- 
  read_csv(paste("https://www.fhwa.dot.gov/bridge/nbi/2021/delimited/", fips_state_txt, "21.txt", sep = ""), 
           col_types = strrep("c", 123))

data_deficient_bridges <-
  data_deficient_bridges_raw %>%
  filter(COUNTY_CODE_003 == fips_county) %>%
  select(STATE_CODE_001, COUNTY_CODE_003, LAT_016, LONG_017, LOWEST_RATING) %>%
  filter(LOWEST_RATING <= 4) %>%
  
  # We have to convert the latitude/longitude from HMS to a decimal
  mutate(
    LAT_016_h  = substr(LAT_016, 0, 2) %>% as.numeric(),
    LAT_016_m  = substr(LAT_016, 3, 4) %>% as.numeric(),
    LAT_016_s  = substr(LAT_016, 5, 8) %>% as.numeric() / 100,
    
    LAT_dec    = LAT_016_h + (LAT_016_m / 60) + (LAT_016_s / (60*60)),
    
    LONG_017_h  = substr(LONG_017, 0, 3) %>% as.numeric(),
    LONG_017_m  = substr(LONG_017, 4, 5) %>% as.numeric(),
    LONG_017_s  = substr(LONG_017, 6, 9) %>% as.numeric() / 100,
    
    LON_dec     = - LONG_017_h - (LONG_017_m / 60) - (LONG_017_s / (60*60))) %>% 
  select(-ends_with("_h"), -ends_with("_m"), -ends_with("_s"))

data_deficient_bridges_points <-
  data_deficient_bridges %>%
  st_as_sf(., coords = c("LON_dec", "LAT_dec"), crs = "NAD83")

# Create the map
color_water       <- "steelblue2"
color_roads       <- "tan4"
color_interstates <- "steelblue4"
color_points      <- "firebrick3"

p_deficient_bridges <- 
  ggplot() +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5)) +

  # County outline
  geom_sf(data = counties, size = 0.1) + 

  # Add lakes
  geom_sf(data = lakes, size = 0.2, fill = color_water, color = color_water) + 
  
  # Add rivers
  geom_sf(data = rivers, size = 0.2, fill = color_water, color = color_water) + 
  
  # Add regular roads
  geom_sf(data = roads, size = 0.2, color = color_roads, alpha = 1.0) +

  # Add highways
  geom_sf(data = hwy, size = 0.5, color = color_roads, alpha = 1.0) +
  
  # Add interstates
  geom_sf(data = interstates, size = 1.2, color = color_interstates, alpha = 1.0) +

  # Bridges in "poor" condition
  geom_sf(data = data_deficient_bridges_points, color = color_points, size = 2) + 
  
  labs(title = paste("Bridges in Poor Shape (rating <= 4)\nin ", fips_county_txt, fips_state_txt, sep = ""))

plot(p_deficient_bridges)
