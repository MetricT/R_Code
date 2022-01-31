################################################################################
###
### Number of Inverted Yield Curves, Inverted Yield Curves as a % of All Curves
###
###    by /u/MetricT
###
### Most analysts only report on the 10yr/2yr or 10yr/3mo yield curves.   This
### can often lead to ambiguity.   For instance, the 10yr/2yr curve barely
### inverted for 2 days in 2019, so there was a lot of talk about whether that
### inversion "counted" towards a recession.
###
### Instead of looking at individual yield curves, this script computers the
### number of yield curves inverted at any given time.   It also returns the
### percent of all yield curves that are currently inverted, because the number
### of maturities has changed over the years, and this helps provide a more
### apples-to-apples comparison.
###
### Available Treasury data from FRED goes back to 1962-01-02.   If anyone can                             
### point me to earlier Treasury data, I'd appreciate it.
###
################################################################################

# Load necessary libraries
library(tidyverse)
library(fredr)
library(scales)
library(lubridate)
library(forecast)
library(cowplot)

# Set your FRED API key here.  You may request an API key at:
# https://research.stlouisfed.org/useraccount/apikeys
source("../Include/Recession_Bars.R")
source("../Include/API_Keys.R")
api_key_fred <- "PUT_YOUR_FRED_API_KEY_HERE"
fredr_set_key(api_key_fred)

# Graph start date.   Earlier dates have fewer yield curves, so they will
# appear shorter (top graph) or coarser (bottom graph).  For that reason, I
# recommend applying a 2-week moving average to the bottom graph, though that
# is strictly for aesthetics.
graph_start <- as.Date("1962-01-02")

# For a 2-week SMA we actually use 15 days because we need to use an odd number
# Set to 1 to just graph the raw data
SMA_days = 15

# You shouldn't need to change anything below here

####################################################################
### Add recession bars to graphs
####################################################################
geom_recession_bars <- function(date_start, date_end, fill = "darkseagreen4") {
  
  date_start <- as.Date(date_start, origin = "1970-01-01")
  date_end   <- as.Date(date_end,   origin = "1970-01-01")
  
  recessions_tibble <-
    tribble(
      ~peak,     ~trough,
      "1960-04-01", "1961-02-01",
      "1969-12-01", "1970-11-01",
      "1973-11-01", "1975-03-01",
      "1980-01-01", "1980-07-01",
      "1981-07-01", "1982-11-01",
      "1990-07-01", "1991-03-01",
      "2001-03-01", "2001-11-01",
      "2007-12-01", "2009-06-01",
      "2020-02-01", "2020-04-01"
    ) %>%
    mutate(peak   = as.Date(peak),
           trough = as.Date(trough))
  
  recessions_trim <- recessions_tibble %>%
    filter(peak   >= min(date_start) &
             trough <= max(date_end))
  
  if (nrow(recessions_trim) > 0) {
    
    recession_bars <- geom_rect(data        = recessions_trim,
                                inherit.aes = F,
                                fill        = fill,
                                alpha       = 0.25,
                                aes(xmin = as.Date(peak,   origin = "1970-01-01"),
                                    xmax = as.Date(trough, origin = "1970-01-01"),
                                    ymin = -Inf, ymax = +Inf))
  } else {
    recession_bars <- geom_blank()
  }
  
}

################################################################################
### Pull Treasury data from FRED
################################################################################

reg_treasuries <- c("DGS1MO", "DGS3MO", "DGS6MO", "DGS1", "DGS2", "DGS3",
                    "DGS5", "DGS7", "DGS10", "DGS20", "DGS30")

### Use purrr to pull all of the series from FRED
rates <-
  purrr::map_dfr(reg_treasuries,
                 .f = ~ fredr(series_id = .x, frequency = "d")) %>%
  select(date, series_id, value) %>%
  pivot_wider(id_cols = "date",
              names_from = "series_id",
              values_from = "value") %>%
  arrange(date)


################################################################################
### Compute the number of inverted yield curves
################################################################################

all_yield_curves <-
  rates %>%
  mutate(
    yc_30_20 = DGS30 - DGS20,
    yc_30_10 = DGS30 - DGS10,
    yc_30_7  = DGS30 - DGS7,
    yc_30_5  = DGS30 - DGS5,
    yc_30_3  = DGS30 - DGS3,
    yc_30_2  = DGS30 - DGS2,
    yc_30_1  = DGS30 - DGS1,
    yc_30_6m = DGS30 - DGS6MO,
    yc_30_3m = DGS30 - DGS3MO,
    yc_30_1m = DGS30 - DGS1MO,
    yc_20_10 = DGS20 - DGS10,
    yc_20_7  = DGS20 - DGS7,
    yc_20_5  = DGS20 - DGS5,
    yc_20_3  = DGS20 - DGS3,
    yc_20_2  = DGS20 - DGS2,
    yc_20_1  = DGS20 - DGS1,
    yc_20_6m = DGS20 - DGS6MO,
    yc_20_3m = DGS20 - DGS3MO,
    yc_20_1m = DGS20 - DGS1MO,
    yc_10_7  = DGS10 - DGS7,
    yc_10_5  = DGS10 - DGS5,
    yc_10_3  = DGS10 - DGS3,
    yc_10_2  = DGS10 - DGS2,
    yc_10_1  = DGS10 - DGS1,
    yc_10_6m = DGS10 - DGS6MO,
    yc_10_3m = DGS10 - DGS3MO,
    yc_10_1m = DGS10 - DGS1MO,
    yc_7_5   = DGS7  - DGS5,
    yc_7_3   = DGS7  - DGS3,
    yc_7_2   = DGS7  - DGS2,
    yc_7_1   = DGS7  - DGS1,
    yc_7_6m  = DGS7  - DGS6MO,
    yc_7_3m  = DGS7  - DGS3MO,
    yc_7_1m  = DGS7  - DGS1MO,
    yc_5_3   = DGS5  - DGS3,
    yc_5_2   = DGS5  - DGS2,
    yc_5_1   = DGS5  - DGS1,
    yc_5_6m  = DGS5  - DGS6MO,
    yc_5_3m  = DGS5  - DGS3MO,
    yc_5_1m  = DGS5  - DGS1MO,
    yc_3_2   = DGS3  - DGS2,
    yc_3_1   = DGS3  - DGS1,
    yc_3_6m  = DGS3  - DGS6MO,
    yc_3_3m  = DGS3  - DGS3MO,
    yc_3_1m  = DGS3  - DGS1MO,
    yc_2_1   = DGS2  - DGS1,
    yc_2_6m  = DGS2  - DGS6MO,
    yc_2_3m  = DGS2  - DGS3MO,
    yc_2_1m  = DGS2  - DGS1MO,
    yc_1_6m  = DGS1  - DGS6MO,
    yc_1_3m  = DGS1  - DGS3MO,
    yc_1_1m  = DGS1  - DGS1MO,
    yc_6m_3m = DGS6MO - DGS3MO,
    yc_6m_1m = DGS6MO - DGS1MO,
    yc_3m_1m = DGS3MO - DGS1MO
  ) %>%
  select(date, starts_with("yc")) %>%
  pivot_longer(-date, names_to = "maturity", values_to = "value") %>%
  filter(!is.na(value))

# Count the number of yield curves on a given date
yield_curve_count <-
  all_yield_curves %>%
  group_by(date) %>%
  summarize(total_curves = n()) %>%
  ungroup()

# If you want to see the # of yield curves over time
#yield_curve_count %>% plot()

# Count the number of inverted yield curves at a given date, and
# compute the percent of yield curves inverted
all_inverts <-
  all_yield_curves %>%
  select(date) %>%
  mutate(blank = "") %>%
  unique() %>%
  arrange(date) %>%
  full_join(all_yield_curves %>%
              filter(value < 0) %>%
              group_by(date) %>%
              summarize(n_inverts = n()) %>%
              ungroup(), by = "date") %>%
  mutate(n_inverts = ifelse(is.na(n_inverts), 0, n_inverts)) %>%
  select(date, n_inverts) %>%
  left_join(yield_curve_count, by = "date") %>%
  mutate(n_per = n_inverts / total_curves)

################################################################################
### Graphs
################################################################################

g_num_inverts <-
  all_inverts %>%
  filter(date >= as.Date(graph_start)) %>%
  ggplot() +
  theme_bw() +
  geom_line(aes(x = date - (SMA_days - 1)/2, y = SMA(n_inverts, n = SMA_days))) +
  geom_recession_bars(as.Date(graph_start), as.Date(Sys.Date())) +
  scale_x_date(breaks = pretty_breaks(8)) +
  labs(x = "", y = "", title = "Number of Inverted Yield Curves")

g_per_inverts <-
  all_inverts %>%
  filter(date >= as.Date(graph_start)) %>%
  ggplot() +
  theme_bw() +

  # n in SMA() should be an odd number, then subtract (n-1/2) from as.Date(date)
  geom_line(aes(x = as.Date(date) - (SMA_days - 1)/2, y = SMA(n_per, n = SMA_days))) +

  geom_recession_bars(as.Date(graph_start), as.Date(Sys.Date())) +
  scale_x_date(breaks = pretty_breaks(8)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     breaks = pretty_breaks(5)) +
  labs(x = "", y = "",
       title = "Number of Inverted Yield Curves as a % of All Yield Curves")

print(plot_grid(g_num_inverts, g_per_inverts, nrow = 2, ncol = 1, align = "hv"))
