################################################################################
### Stagflation chart by/u/MetricT
###
### Graphs CPI Inflation vs real GDP growth since 1948 (points/density contours)
### while highlighting the trajectory since 2019, so we can see the before/after
### of the COVID recession.  
###
### Stagflation is loosely defined as an time of high inflation and low growth.
### Stagdeflation is even more loosely defined as high inflation + negative growth
### 
### Since the definitions are rather loose, the delineations between areas is
### a bit subjective.  I chose 6% as the boundary between normal/high inflation 
### somewhat arbitrarily because the majority of inflation points are below 6%.
################################################################################

library(tidyverse)
library(viridis)
library(scales)
library(fredr)

# If you don't already have one, get a FRED API key from
# https://fred.stlouisfed.org/docs/api/api_key.html
api_key_fred   <- "PUT_YOUR_FRED_API_KEY_HERE"
fredr_set_key(api_key_fred)

################################################################################
### Fetch data series from FRED and rename them as needed
################################################################################
fredr_fetch <- function(series_id, frequency, units, scale) {
  val <- 
    fredr(series_id = series_id,  frequency = frequency, units = units) %>%
    mutate(
      series_id = paste(series_id, "_", units, sep = ""),
      series_id = if_else(grepl("_lin$", series_id), gsub("_lin$", "", series_id), series_id),
      series_id = if_else(grepl("_$", series_id), gsub("_$", "", series_id), series_id),
      value = case_when(
        scale == "p" ~ value / 100,
        scale == "k" ~ value * 1000,
        scale == "m" ~ value * 1000000,
        scale == "b" ~ value * 1000000000,
        scale == "t" ~ value * 1000000000000,
        TRUE         ~ value,
      )
    )
  return(val)
}

fred_series <-
  tribble(
    ~series_id,     ~frequency,   ~units, ~scale, ~name,
    "CPIAUCSL",     "q",          "pc1",    "p",  "Consumer Price Index for All Urban Consumers: All Items in U.S. City Average",
    "GDPC1",        "q",          "pc1",    "p",  "Real Gross Domestic Product",
  )

data_fred <-
  purrr::pmap_dfr(.l = fred_series %>% select(-name), .f = fredr_fetch) %>%
  select(date, series_id, value) %>% 
  pivot_wider(id_cols = "date", names_from = "series_id", values_from = "value") %>%
  arrange(date) %>%
  filter(date >= as.Date("1948-01-01"))
  #filter(date >= as.Date("1970-01-01"))
  
# Add in cursory data (CPI from current month CPIAUCSL, GDPC1 from FED GDPNow )
data_fred <-
  data_fred %>% 
  mutate(
    CPIAUCSL_pc1 = ifelse(date == "2022-04-01", 0.082, CPIAUCSL_pc1),
    GDPC1_pc1    = ifelse(date == "2022-04-01", 0.024, GDPC1_pc1)
  )

# Coordinates for the labels on the graph
quad <- tribble(
  ~name,           ~x,     ~y,
  "Recession",      0.054, -0.005,
  "Stagflation",    0.066,  0.015,
  "Stagdeflation",  0.067, -0.005,
)

# Plot graph
ggplot(data = data_fred, aes(x = CPIAUCSL_pc1, y = GDPC1_pc1)) + 
  theme_bw() + 
  theme(
    legend.position='none'
  ) +

  geom_density_2d_filled(alpha = 0.5, bins = 20) + 
  geom_density_2d(color = "white", alpha = 0.5, bins = 20) + 
  scale_fill_viridis_d(option = "mako") +
  
  geom_hline(yintercept = 0, color = "white", linetype = "dashed") + 
  geom_vline(xintercept = 0.06, color = "white", linetype = "dashed") + 
  geom_segment(x = 0.06, xend = 0.15, y = 0.02, yend = 0.02, color = "white", linetype = "dashed") +
  
  geom_path(data = data_fred %>% filter(date >= as.Date("2019-01-01")), aes(x = CPIAUCSL_pc1, y = GDPC1_pc1), color = "goldenrod2", size = 0.5) +
  
  geom_point(data = data_fred %>% filter(!is.na(CPIAUCSL_pc1), !is.na(GDPC1_pc1)), 
             aes(x = CPIAUCSL_pc1, y = GDPC1_pc1),
             size = 0.4, color = "firebrick4") +
  
  geom_point(data = data_fred %>% filter(!is.na(CPIAUCSL_pc1), !is.na(GDPC1_pc1)) %>% tail(n = 1), 
             aes(x = CPIAUCSL_pc1, y = GDPC1_pc1),
            size = 2, color = "yellow") +
  
  geom_label(data = quad, aes(label = name, x = x, y = y)) + 
  
  scale_x_continuous(breaks = pretty_breaks(13), labels = scales::percent_format(accuracy = 1)) +
  scale_y_continuous(breaks = pretty_breaks(13), labels = scales::percent_format(accuracy = 1)) +

  labs(x = "CPI Inflation", y = "Rel GDP Growth", 
       title = "Stagflation - Quarterly CPI Inflation vs Real GDP Growth since 1948",
       subtitle = "Yellow line shows data since 2019, yellow dot shows current")
