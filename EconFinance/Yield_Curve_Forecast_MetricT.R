################################################################################
# Yield Curve Inversion Forecast - /u/MetricT
#
# Graph the 10y/2y yield curve spread and forecast the inversion date using a
# linear regression of the current trend.   Uses historical bond data from
# FRED, and current bond values from Marketwatch
################################################################################

### Load necessary R packages
library(fredr)
library(tidyverse)
library(lubridate)
library(quantmod)
library(broom)
library(scales)
library(cowplot)
library(rvest)

### Set your FRED API key here.  You may request an API key at:
### https://research.stlouisfed.org/useraccount/apikeys
fredr_set_key("YOUR_FRED_API_KEY_HERE")
fredr_set_key("2534759c75ba3f25da07c3c971f98d0b")

### What date should the graph start on.
### Earliest possible data for T10Y2Y is 1976-06-01.
graph_start <- as.Date("2018-01-01")

### What date should be considered the first date of the current trend.
### This is used when doing a linear regression to forecast the inversion date
trend_start <- as.Date("2021-03-29")

### Function to draw recession bars
geom_recession_bars <- function(date_start, date_end, fill = "darkseagreen4") {
  
  date_start <- as.Date(date_start, origin = "1970-01-01")
  date_end   <- as.Date(date_end,   origin = "1970-01-01")
  
  recessions_tibble <-
    tribble(
      ~peak,     ~trough,
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

### Pull historical 10Y/2Y bond data from FRED
yield_curves_historical <-
  fredr(series_id = "T10Y2Y", frequency = "d") %>%
  select(date, value) %>%
  rename(T10Y2Y = value)

### Pull current 10Y/2Y bond data from Marketwatch
fetch_treasury_marketwatch <- function(treasury) {
  url <- paste("https://www.marketwatch.com/investing/bond/tmubmusd", treasury, sep = "")
  val <- url %>% read_html() %>% html_nodes("meta[name=price]") %>%
    html_attr("content") %>% str_replace("%", "") %>% as.numeric()
  return(val)
}

yield_curves_now <-
  tribble(
    ~date, ~T10Y2Y,
    as.Date(Sys.Date()), fetch_treasury_marketwatch("10y") - fetch_treasury_marketwatch("02y")
  )

### Append current and historical 10y/2y data
yield_curves <-
  yield_curves_historical %>%
  bind_rows(yield_curves_now) %>%
  arrange(date) %>%
  unique() %>%
  mutate(decimal_date = decimal_date(date))

### Do a linear regression on the current trend to see when it intercepts zero.
fit_10y2y <- lm(T10Y2Y ~ decimal_date,
                data = yield_curves %>% filter(date >= trend_start))
tidy_10y2y <- fit_10y2y %>% tidy()
invert_date <- date_decimal(- tidy_10y2y$estimate[1] / tidy_10y2y$estimate[2]) %>% as.Date()
print(paste("10y2y inversion date estimate = ", invert_date))

### Create the forecast line using the regression
fc_dates <-
  seq(Sys.Date() - days(270), as.Date(invert_date), "days") %>%
  as_tibble() %>%
  rename(date = value) %>%
  mutate(decimal_date = decimal_date(date)) %>%
  mutate(fc_10y2y = tidy_10y2y$estimate[1] + tidy_10y2y$estimate[2] * decimal_date) %>%
  mutate(fc_10y2y = ifelse(fc_10y2y < 0, NA, fc_10y2y))

yield_curves <-
  yield_curves %>%
  filter(!is.na(T10Y2Y), date >= graph_start)

### Graph the data and done!
g_yc_combined <-
  ggplot(data = yield_curves) +
  theme_bw() +
  theme(legend.position = "none") +
  
  geom_line(aes(x = date,  y = T10Y2Y / 100), color = "black") +
  
  geom_point(data = yield_curves %>% tail(n = 1),
             aes(x = date, y = T10Y2Y / 100), color = "black", shape = 18, size = 3) +
  
  geom_hline(yintercept = 0, linetype = "dashed", size = 0.8) +
  geom_line(data = fc_dates %>% select(date, fc_10y2y) %>% filter(!is.na(fc_10y2y)),
            aes(x = as.Date(date), y = fc_10y2y / 100), color = "steelblue3", linetype = "dashed") +
  
  geom_recession_bars(yield_curves %>% pull("date") %>% min(),
                      yield_curves %>% pull("date") %>% max()) +
  
  annotate("text", x = invert_date + days(30), y = 0.008, angle = 90, color = "firebrick4",
           label = paste(invert_date, " - 10Y / 2Y Yield Curve Inverts")) +
  geom_vline(xintercept = invert_date, linetype = "dashed", color = "firebrick4") +
  
  annotate("text", x = invert_date + days(round(15.1 * 365.25 / 12)), y = 0.008, angle = 90, color = "firebrick4",
           label = paste(invert_date + days(round(15.1 * 365.25 / 12)), " - Average recession starts\n15.1 months after YC inversion")) +
  geom_vline(xintercept = invert_date + days(round(15.1 * 365.25 / 12)), linetype = "dashed", color = "firebrick4") +
  
  labs(title = "Yield Curve Spread:  10 Yr/2 Yr US Treasuries",
       subtitle = paste("Forecast based on data since yield curve peak on", trend_start),
       caption = paste("Historical yield curve data from FRED [T10Y2Y]\nCurrent yield curve data from Marketwatch as of", Sys.Date()),
       x = "", y = "") +
  scale_x_date(breaks = pretty_breaks(10)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1),
                     breaks = pretty_breaks(6))
print(g_yc_combined)