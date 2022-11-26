################################################################################
### Pull US Treasury data from FRED, Marketwatch, and Robert Shiller.   Graph
### the yield curve, mean yield spread, and percent of inverted yield curves
###       by /u/MetricT
################################################################################

# Load necessary libraries
library(tidyverse)
library(fredr)
library(scales)
library(lubridate)
library(forecast)
library(glue)
library(cowplot)
library(tsibble)
library(zoo)
library(quantmod)
library(broom)
library(rvest)

# Set your FRED API key here.  You may request an API key at:
# https://research.stlouisfed.org/useraccount/apikeys
api_key_fred <- "PUT YOUR FRED API KEY HERE"
fredr_set_key(api_key_fred)

# Start/end date for the yield curve spread graphs
graph_start <- as.Date("1970-01-01")
graph_end   <- as.Date(Sys.Date())

# You shouldn't need to change anything below here.

####################################################################
### Add recession bars to ggplot graphs
####################################################################

geom_recession_bars <- function(date_start, date_end, fill = "darkseagreen4") {
  
  date_start <- as.Date(date_start, origin = "1970-01-01")
  date_end   <- as.Date(date_end,   origin = "1970-01-01")
  
  recessions_tibble <-
    tribble(
      ~peak,     ~trough,
      
      # Recessions prior to 1857-06-01 are assumed from year-over-year declines
      # in nominal GDP.   These dates only have year-accuracy.
      # "https://www.cbo.gov/sites/default/files/111th-congress-2009-2010/reports/historicaldebt2000.xls"
      
      "1797-01-01", "1797-12-31",
      "1802-01-01", "1802-12-31",
      "1806-01-01", "1808-12-31",
      "1819-01-01", "1821-12-31",
      "1823-01-01", "1823-12-31",
      "1826-01-01", "1827-12-31",
      "1830-01-01", "1830-12-31",
      "1838-01-01", "1838-12-31",
      "1840-01-01", "1842-12-31",
      "1849-01-01", "1849-12-31",
      "1854-01-01", "1854-12-31",
      
      # Recession dates below are official NBER recession dates
      
      "1857-06-01", "1858-12-01",
      "1860-10-01", "1861-06-01",
      "1865-04-01", "1867-12-01",
      "1869-06-01", "1870-12-01",
      "1873-10-01", "1879-03-01",
      "1882-03-01", "1885-05-01",
      "1887-03-01", "1888-04-01",
      "1890-07-01", "1891-05-01",
      "1893-01-01", "1894-06-01",
      "1895-12-01", "1897-06-01",
      "1899-06-01", "1900-12-01",
      "1902-09-01", "1904-08-01",
      "1907-05-01", "1908-06-01",
      "1910-01-01", "1912-01-01",
      "1913-01-01", "1914-12-01",
      "1918-08-01", "1919-03-01",
      "1920-01-01", "1921-07-01",
      "1923-05-01", "1924-07-01",
      "1926-10-01", "1927-11-01",
      "1929-08-01", "1933-03-01",
      "1937-05-01", "1938-06-01",
      "1945-02-01", "1945-10-01",
      "1948-11-01", "1949-10-01",
      "1953-07-01", "1954-05-01",
      "1957-08-01", "1958-04-01",
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
### Pull historical Treasury data from FRED.   This pulls both current bond
### yield series and historical yield series and combines them to make
### historical analysis possible if you want to tweak things.
################################################################################

### function to fetch data series from FRED and rename them as needed
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

# Can change this down to "w" or "m" to run quicker, with a loss of temporal resolution
min_freq <- "d"

fred_series <-
  tribble(
    ~series_id,       ~frequency,       ~units, ~scale, ~name,
    
    # Current Treasury series
    "DGS1MO",           min_freq,        "lin",    "p",    "Market Yield on U.S. Treasury Securities at 1-Month Constant Maturity",
    "DGS3MO",           min_freq,        "lin",    "p",    "Market Yield on U.S. Treasury Securities at 3-Month Constant Maturity",
    "DGS6MO",           min_freq,        "lin",    "p",    "Market Yield on U.S. Treasury Securities at 6-Month Constant Maturity",
    "DGS1",             min_freq,        "lin",    "p",    "Market Yield on U.S. Treasury Securities at 1-Year Constant Maturity",
    "DGS2",             min_freq,        "lin",    "p",    "Market Yield on U.S. Treasury Securities at 2-Year Constant Maturity",
    "DGS3",             min_freq,        "lin",    "p",    "Market Yield on U.S. Treasury Securities at 3-Year Constant Maturity",
    "DGS5",             min_freq,        "lin",    "p",    "Market Yield on U.S. Treasury Securities at 5-Year Constant Maturity",
    "DGS7",             min_freq,        "lin",    "p",    "Market Yield on U.S. Treasury Securities at 7-Year Constant Maturity",
    "DGS10",            min_freq,        "lin",    "p",    "Market Yield on U.S. Treasury Securities at 10-Year Constant Maturity",
    "DGS20",            min_freq,        "lin",    "p",    "Market Yield on U.S. Treasury Securities at 20-Year Constant Maturity",
    "DGS30",            min_freq,        "lin",    "p",    "Market Yield on U.S. Treasury Securities at 30-Year Constant Maturity",
    
    # Historical Treasury series to add older data than the current series contains
    "DTB3",             min_freq,        "lin",    "p",    "Market Yield on U.S. Treasury Securities at 3-Month Constant Maturity",
    "DTB6",             min_freq,        "lin",    "p",    "Market Yield on U.S. Treasury Securities at 6-Month Constant Maturity",
    "GS1",                   "m",        "lin",    "p",    "Market Yield on U.S. Treasury Securities at 1-Year Constant Maturity",
    "GS2",                   "m",        "lin",    "p",    "Market Yield on U.S. Treasury Securities at 2-Year Constant Maturity",
    "GS3",                   "m",        "lin",    "p",    "Market Yield on U.S. Treasury Securities at 3-Year Constant Maturity",
    "GS5",                   "m",        "lin",    "p",    "Market Yield on U.S. Treasury Securities at 5-Year Constant Maturity",
    "GS7",                   "m",        "lin",    "p",    "Market Yield on U.S. Treasury Securities at 7-Year Constant Maturity",
    "GS10",                  "m",        "lin",    "p",    "Market Yield on U.S. Treasury Securities at 10-Year Constant Maturity",
    "GS20",                  "m",        "lin",    "p",    "Market Yield on U.S. Treasury Securities at 20-Year Constant Maturity",
    "GS30",                  "m",        "lin",    "p",    "Market Yield on U.S. Treasury Securities at 30-Year Constant Maturity",
    
    # Use for 3-month rate
    "TB3MS",                 "m",        "lin",  "p",      "3-Month Treasury Bill Secondary Market Rate",
    "M1329AUSM193NNBR",      "m",        "lin",  "p",      "Yields on Short-Term United States Securities, Three-Six Month Treasury Notes and Certificates, Three Month Treasury Bills for United States", 
    "M13002US35620M156NNBR", "m",        "lin",  "p",      "Commercial Paper Rates for New York, NY [1857 - 1971]", 
    
    # Use for 1-year rate
    "M13056USM193NNBR",      "m",        "lin",  "p",      "United States Government Securities, Nine to Twelve Month Issues for United States [1943 - 1970]",
    
    # Use for 3-year rate
    "M13057USM193NNBR",      "m",        "lin",  "p",      "Yields on United States Government Securities, Three to Five Year Issues for United States [1945 - 1965]",
    
    # Use for 10-year rate
    #"M1333AUSM156NNBR",      "m",        "lin",  "p",      "Yield on Long-Term United States Bonds for United States [1919-1944]",  # Commenting out because it's not terribly accurate
    "M1333BUSM156NNBR",      "m",        "lin",  "p",      "Yield on Long-Term United States Bonds for United States [1941-1967]",
    
    # Use for 20-year rate
    "M13058USM156NNBR",      "m",        "lin",  "p",      "Yields on Twenty Year United States Government Bonds for United States [1942 - 1962]",
    
    # Federal Funds rate (current and historical)
    "EFFR",             min_freq,        "lin",  "p",      "Effective Federal Funds Rate",
    "FEDFUNDS",              "m",        "lin",  "p",      "Federal Funds Effective Rate",
    "FFWSJHIGH",             "m",        "lin",  "p",      "High Value of the Federal Funds Rate for the Indicated Date Published in The Wall Street Journal",
    "FFWSJLOW",              "m",        "lin",  "p",      "Low Value of the Federal Funds Rate for the Indicated Date Published in The Wall Street Journal",
    "FFHTHIGH",              "m",        "lin",  "p",      "High Value of the Federal Funds Rate for the Indicated Date Published in The New York Herald-Tribune",
    "FFHTLOW",               "m",        "lin",  "p",      "Low Value of the Federal Funds Rate for the Indicated Date Published in The New York Herald-Tribune",
    "M13009USM156NNBR",      "m",        "lin",  "p",      "Discount Rates, Federal Reserve Bank of New York for United States",
  )

# Download the FRED Treasury/Fedrate series above
rates_raw <-
  purrr::pmap_dfr(.l = fred_series %>% select(-name), .f = fredr_fetch) %>%
  select(date, series_id, value) %>% 
  pivot_wider(id_cols = "date", names_from = "series_id", values_from = "value") %>%
  arrange(date)


# Rename some of the older series so we can stack up historical rates for each duration
rates <-
  rates_raw %>%
  mutate(
    
    # Combine 3 month series
    DGS3MO = ifelse(is.na(DGS3MO), DTB3,  DGS3MO),
    DGS3MO = ifelse(is.na(DGS3MO), TB3MS, DGS3MO),
    DGS3MO = ifelse(is.na(DGS3MO), M1329AUSM193NNBR, DGS3MO),
    DGS3MO = ifelse(is.na(DGS3MO), M13002US35620M156NNBR, DGS3MO),
    
    # Combine 6 month series
    DGS6MO = ifelse(is.na(DGS6MO), DTB6,  DGS6MO),
    
    # Combine 1 year series
    DGS1   = ifelse(is.na(DGS1),   GS1,   DGS1),
    DGS1   = ifelse(is.na(DGS1),   M13056USM193NNBR,   DGS1),
    
    # Combine 2 year series
    DGS2   = ifelse(is.na(DGS2),   GS2,   DGS2),
    
    # Combine 3 year series
    DGS3   = ifelse(is.na(DGS3),   GS3,   DGS3),
    DGS3   = ifelse(is.na(DGS3),   M13057USM193NNBR,   DGS3),
    
    # Combine 5 year series
    DGS5   = ifelse(is.na(DGS5),   GS5,   DGS5),
    
    # Combine 7 year series
    DGS7   = ifelse(is.na(DGS7),   GS7,   DGS7),
    
    # Combine 10 year series
    DGS10  = ifelse(is.na(DGS10),  GS10,  DGS10),
    DGS10  = ifelse(is.na(DGS10),  M1333BUSM156NNBR,  DGS10),
    #DGS10  = ifelse(is.na(DGS10),  M1333AUSM156NNBR,  DGS10),
    
    # Combine 20 year series
    DGS20  = ifelse(is.na(DGS20),  GS20,  DGS20),
    DGS20  = ifelse(is.na(DGS20),  M13058USM156NNBR,  DGS20),
    
    # Combine 30 year series
    DGS30  = ifelse(is.na(DGS30),  GS30,  DGS30),
    
    # Combine Fed Rate series
    FFHT  = (FFHTHIGH + FFHTLOW) / 2,
    FFWSJ = if_else(!is.na(FFWSJLOW), (FFWSJHIGH + FFWSJLOW) / 2, FFWSJHIGH),
    
    # Use Discount Rates, Federal Reserve Bank of New York for United States for dates before 1928-04-01
    # Use New York Herald-Tribune data for dates between 1928-04-01 and 1932-07-01
    # Use WSJ data for dates between 1932-07-01 and 1954-07-01
    # Use FEDFUNDS for dates between 1954-07-01 and 2000-07-01,
    # Use EFFR for dates after 2000-07-01
    
    M13009USM156NNBR = ifelse(date <  as.Date("1928-04-01"), M13009USM156NNBR, NA),
    FFHT             = ifelse(date >= as.Date("1928-04-01") &
                                date <  as.Date("1932-07-01"), FFHT, NA),
    FFWSJ            = ifelse(date >= as.Date("1932-07-01") & 
                                date <  as.Date("1954-07-01"), FFWSJ, NA),
    
    FedRate = case_when(
      date <  as.Date("1928-04-01")                                  ~ M13009USM156NNBR,
      date >  as.Date("1928-04-01") & date  <  as.Date("1932-07-01") ~ FFHT,
      date >= as.Date("1932-07-01") & date  <  as.Date("1954-07-01") ~ FFWSJ,
      date >= as.Date("1954-07-01") & date  <  as.Date("2000-07-01") ~ FEDFUNDS,
      date >= as.Date("2000-07-01")                                  ~ EFFR)
  ) %>%
  select(date, FedRate, starts_with("DGS"))

################################################################################
### Fetch current Treasury rates from Marketwatch.
################################################################################
fetch_treasury_marketwatch <- function(treasury) {
  url <- paste("https://www.marketwatch.com/investing/bond/tmubmusd", treasury, sep = "")
  val <- url %>% read_html() %>% html_nodes("meta[name=price]") %>% 
    html_attr("content") %>% str_replace("%", "") %>% as.numeric()
  return(val / 100)
}

rates_now <- 
  tribble(
    ~maturity, ~value, 
    "FedRate", rates %>% filter(!is.na(FedRate)) %>% tail(n = 1) %>% pull("FedRate"),
    "DGS1MO",  fetch_treasury_marketwatch("01m"),
    "DGS3MO",  fetch_treasury_marketwatch("03m"),
    "DGS6MO",  fetch_treasury_marketwatch("06m"),
    "DGS1",    fetch_treasury_marketwatch("01y"),
    "DGS2",    fetch_treasury_marketwatch("02y"),
    "DGS3",    fetch_treasury_marketwatch("03y"),
    "DGS5",    fetch_treasury_marketwatch("05y"),
    "DGS7",    fetch_treasury_marketwatch("07y"),
    "DGS10",   fetch_treasury_marketwatch("10y"),
    "DGS20",   fetch_treasury_marketwatch("20y"),
    "DGS30",   fetch_treasury_marketwatch("30y")
  ) %>%
  mutate(date = as.Date(Sys.Date())) %>%
  pivot_wider(names_from = maturity, values_from = value)

# Combine historic and current rate data
rates <- rates %>% filter(date != Sys.Date() %>% as.Date()) %>% bind_rows(rates_now) %>% arrange(date) %>% unique()

################################################################################
### Pull historical 10-year yields from Robert Shiller's data
################################################################################

# Function to read Excel file from URL
read_xls_url <- function(url, ...) {
  tf <- tempfile(fileext = ".xls")
  curl::curl_download(url, tf)
  return(readxl::read_excel(tf, ...))
}

rs_10yr <-
  "http://www.econ.yale.edu/~shiller/data/ie_data.xls" %>%
  read_xls_url(sheet = "Data", skip = 7, col_types = c("text")) %>%
  select(1, 7) %>%
  rename(Year.Month = 1, GS10 = 2) %>%
  mutate(
    Year.Month = round(100 * as.numeric(Year.Month)) / 100, 
    Year.Month = as.character(Year.Month),
    Year.Month = str_pad(Year.Month, 7, side = "right", pad = "0")) %>%
  separate(Year.Month, into = c("Year", "Month"), sep = "\\.") %>% 
  mutate(date = paste(Year, "-", Month, "-01", sep = "")) %>% 
  select(date, GS10) %>% 
  mutate(date = as.Date(date)) %>% 
  filter(!is.na(date)) %>%
  mutate(GS10 = as.numeric(GS10), GS10 = round(GS10, digits = 2)) %>%
  filter(date < as.Date("1941-10-01")) %>%
  mutate(GS10 = GS10 / 100)

rates <- 
  rates %>% 
  left_join(rs_10yr, by = "date") %>% 
  mutate(DGS10 = if_else(!is.na(GS10), GS10, DGS10)) %>%
  select(-GS10)


################################################################################
### Pull yield curve and graph
################################################################################

# Function to return yield curve for a given date
get_yield_curve <- function(this_date) {
  
  # Not all dates have rate data.  So first, find the nearest date with actual data
  print(paste("DEBUG:  Pulling Date subset for", as.Date(this_date)))
  rates_sub <-
    rates %>% 
    filter(date >= as.Date(this_date),
           !is.na(DGS10)) %>%
    head(n = 1)
  
  t_rates <- 
    rates_sub %>%
    pivot_longer(-date, names_to = "maturity", values_to = "value") %>% 
    mutate(maturity_yr = case_when(
      maturity == "EFFR"   ~ 1 / 250,
      maturity == "DGS1MO" ~ 0.083333333333,
      maturity == "DGS3MO" ~ 0.25,
      maturity == "DGS6MO" ~ 0.5,
      maturity == "DGS1"   ~ 1,
      maturity == "DGS2"   ~ 2,
      maturity == "DGS3"   ~ 3,
      maturity == "DGS5"   ~ 5,
      maturity == "DGS7"   ~ 7,
      maturity == "DGS10"  ~ 10,
      maturity == "DGS20"  ~ 20,
      maturity == "DGS30"  ~ 30
    )) %>%
    filter(grepl("DGS|EFFR", maturity)) %>%
    
    # Highlight yield curve inversions
    mutate(
      invert = ifelse(!is.na(value) & (value - lag(value) < 0), TRUE, FALSE),
      invert = ifelse(maturity == "DGS1MO", FALSE, invert),
      invert = ifelse(is.na(invert), FALSE, invert)
    )
  
  # Find the maximum non-inverted yield and mark the global invert peak (use this to update the previous invert detector)
  max_noninvert_rate     <- t_rates %>% filter(invert == FALSE) %>% pull(value) %>% max()
  max_noninvert_maturity <- t_rates %>% filter(invert == FALSE) %>% filter(value == max_noninvert_rate) %>% pull(maturity_yr)
  t_rates <- t_rates %>% mutate(invert = if_else(maturity_yr > max_noninvert_maturity, TRUE, FALSE))
  
  # Find local (non-global) non-inverts
  t_rates <-
    t_rates %>%
    mutate(
      local_invert = case_when(
        invert == TRUE & (value - lag(value) > 0) ~ TRUE,
        TRUE                                      ~ FALSE
      )
    )
  
  # Add row number
  t_rates <- t_rates %>% mutate(row = row_number())
  
  print("DEBUG:  t_rates = ")
  print(t_rates)
  
  # Color the rates.  Red = Global invert, yellow = Global invert but local maxima, blue = normal
  t_rates <-
    t_rates %>%
    mutate(
      color = case_when(
        invert == TRUE & local_invert == TRUE  ~ "goldenrod4",
        invert == TRUE & local_invert == FALSE ~ "red",
        TRUE                                   ~ "blue"
      )
    )
  
  return(t_rates)
}

# Get yield curve data from today and 30 days ago  
yield_curve      <- get_yield_curve(as.Date(Sys.Date()))

# Find the closest date to a month ago that has data
past_date    <- as.Date(as.Date(yield_curve %>% pull(date) %>% unique()) - days(30))
past_yield_curve <- get_yield_curve(past_date)

# Plot the yield curve...
g_yield_curve <-
  ggplot() +
  theme_bw() +
  theme(
    legend.position = "none",
    #  text = element_text(size = 16)
  ) +
  
  geom_point(data = yield_curve, aes(x = row, y = value, size = invert, shape = invert), color = yield_curve$color) +
  geom_line(data = yield_curve,  aes(x = row,  y = value)) +
  geom_text(data = yield_curve,  aes(x = row, y = value + 0.0011, label = paste(value * 100, "%", sep = "")), color = yield_curve$color, size = 4) + 
  
  geom_point(data = past_yield_curve, aes(x = row, y = value, size = invert, shape = invert), color = yield_curve$color, alpha = 0.3) +
  geom_line(data = past_yield_curve,  aes(x = row,  y = value), alpha = 0.3) +
  geom_text(data = past_yield_curve,  aes(x = row, y = value + 0.0011, label = paste(value * 100, "%", sep = "")), color = yield_curve$color, size = 4, alpha = 0.3) + 
  
  labs(
    #title = paste("US Yield Curve with one month change - ", Sys.Date(), sep = ""),
    title = paste("US Yield Curve - ", Sys.Date(), " vs ", past_date, sep = ""), 
    x = "Maturity", y = ""
  ) +
  scale_color_manual(values = c("blue", "red")) + 
  scale_size_manual(values = c(2, 3)) +
  scale_shape_manual(values = c(19, 15)) +
  
  scale_x_continuous(breaks = seq(1, 12),
                     labels = c("Fed Rate", "1 mo", "3 mo", "6 mo", "1 yr", 
                                "2 yr", "3 yr", "5 yr", "7 yr", "10 yr", 
                                "20 yr", "30 yr")) + 
  
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.01), 
                     #trans = "log", 
                     breaks = pretty_breaks(10))
print(g_yield_curve)

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

# Compute useful statistical info on spreads of all durations
yield_curve_spread_properties <-
  all_yield_curves %>%
  arrange(date) %>%
  group_by(date) %>%
  summarize(
    mean   = mean(value),
    median = median(value),
    sd     = sd(value),
    max    = max(value),
    min    = min(value)
  ) %>%
  mutate(color_mean = if_else(mean < 0, "firebrick2", "black"))


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
  mutate(n_per = n_inverts / total_curves)  %>%
  left_join(yield_curve_spread_properties, by = "date") 

combo <- 
  all_inverts %>% 
  mutate(color_grad = case_when(
    mean  <=  0.00  ~ "firebrick2",   # Color blue when the mean of all yield spreads is negative
    mean  >   0.00  ~ "black",
  )) %>%
  mutate(thickness = if_else(color_grad == "black", 0.8, 1.2)) %>%
  filter(!is.na(color_grad)) %>%
  filter(date >= graph_start)

g_yield_curve_spread_properties <-
  ggplot(data = combo) + 
  theme_bw() + 
  theme(legend.position = "none") + 
  geom_line(data = all_yield_curves %>% filter(maturity == "yc_10_2"), color = "darkseagreen4", alpha = 0.4, size = 0.3, aes(x = date, y = value, color = maturity)) + 
  geom_line(data = all_yield_curves %>% filter(maturity == "yc_10_3MO"), color = "steelblue4", alpha = 0.4, size = 0.3, aes(x = date, y = value, color = maturity)) + 
  geom_line(aes(x = date, y = mean), color = combo$color_mean, size = 1.0) + 
  geom_recession_bars(min(combo$date), max(combo$date)) + 
  geom_hline(yintercept = 0, linetype = "dotted") + 
  scale_y_continuous(breaks = pretty_breaks(10), labels = scales::percent_format(accuracy = 0.1), limits = c(-0.01, 0.04)) + 
  scale_x_date(breaks = pretty_breaks(10), limits = c(graph_start, graph_end)) + 
  labs(x = "", y = "", 
       title = "10y/2y Yield Spread (green), 10y/3m Yield Spread (blue), and Average Yield Spread Across All Treasuries (black, with red highlighting inversions)"
  )
print(g_yield_curve_spread_properties)

g_perinverts <-
  combo %>% 
  ggplot() + 
  theme_bw() + 
  geom_bar(stat = "identity", aes(x = date, y = n_per), fill = combo$color_grad, color = combo$color_grad) +
  geom_recession_bars(min(combo$date), max(combo$date)) +
  scale_x_date(breaks = pretty_breaks(10), limits = c(graph_start, graph_end)) + 
  scale_y_continuous(breaks = pretty_breaks(10), labels = scales::percent_format(accuracy = 1)) +
  labs(x = "", y = "", 
       title = "Percent of all US Treasury Yield Spread Combinations That Are Inverted",
  )
print(g_perinverts)

plot_grid(
  g_yield_curve, 
  g_yield_curve_spread_properties,
  g_perinverts,
  nrow = 3, align = "hv"
)
