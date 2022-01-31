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
library(glue)
library(TTR)

# Set your FRED API key here.  You may request an API key at:
# https://research.stlouisfed.org/useraccount/apikeys
fredr_set_key("PUT_YOUR_FRED_API_KEY_HERE")

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

# Make a list of all the bond maturities we have data for
bond_maturities <-
  rates %>%
  select(starts_with("DGS")) %>%
  names() %>%
  gsub("DGS", "", .) %>% rev()

# Find every unique combination of maturities two at a time
yield_curve_pairs <-
  bond_maturities %>%
  combn(2) %>%
  t() %>%
  as_tibble(.name_repair = "minimal") %>%
  rename(high = 1, low = 2)

# Pull our initial value of yield curves from the bond rates pulled from FRED
all_yield_curves <- rates

# Iterate over bond maturities and compute yield curve
for (i in seq(1, nrow(yield_curve_pairs))) {
  
  high <- yield_curve_pairs %>% filter(row_number() == i) %>% pull("high")
  low  <- yield_curve_pairs %>% filter(row_number() == i) %>% pull("low")
  
  all_yield_curves <-
    all_yield_curves %>%
    mutate("yc_{ high }_{ low }" := !!sym(glue("DGS{high}")) - 
             !!sym(glue("DGS{low}")))
}

all_yield_curves <-
  all_yield_curves %>%
  select(date, starts_with("yc")) %>%
  pivot_longer(-date, names_to = "maturity", values_to = "value") %>%
  filter(!is.na(value))

# Count the number of yield curves on a given date
yield_curve_count <-
  all_yield_curves %>%
  group_by(date) %>%
  summarize(total_curves = n()) %>%
  ungroup()

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
  geom_line(data = yield_curve_count, aes(x = date, y = total_curves),
            color = "darkred", linetype = "dashed") +
  geom_line(aes(x = date - (SMA_days - 1)/2, y = SMA(n_inverts, n = SMA_days))) +
  geom_recession_bars(as.Date(graph_start), as.Date(Sys.Date())) +
  scale_x_date(breaks = pretty_breaks(8)) +
  scale_y_continuous(breaks = pretty_breaks(8)) +
  labs(x = "", y = "", title = "Number of Inverted Yield Curves", 
       subtitle = "Red line = total number of yield curves as of that date")

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
       title = "Percent of all yield curves that are inverted", 
       caption = "Data:  FRED [https://fred.stlouisfed.org]")

print(plot_grid(g_num_inverts, g_per_inverts, nrow = 2, ncol = 1, align = "hv"))
