library(httr)
library(jsonlite)
library(dplyr)
library(lubridate)
library(tidyr)
library(plm)

get_elspot <- function(base, start = NULL, end = NULL, pricearea = "DK1") {
  query <- list()
  
  if (!is.null(start)) query$start <- start
  if (!is.null(end))   query$end   <- end
  
  if (!is.null(pricearea)) {
    query$filter <- sprintf('{"PriceArea":"%s"}', pricearea)
  }
  
  res <- GET(base, query = query)
  stop_for_status(res)
  
  data <- content(res, as = "text", encoding = "UTF-8")
  parsed <- fromJSON(data, flatten = TRUE)
  
  return(as_tibble(parsed$records))
}

# Today
today_end <- Sys.Date()

# One year ago
one_year_ago <- today_end - years(1)

# Format as "YYYY-MM-DDT00:00"
today_end_str <- paste0(format(today_end, "%Y-%m-%d"), "T00:00")
one_year_ago_str <- paste0(format(one_year_ago, "%Y-%m-%d"), "T00:00")

today_end_str
one_year_ago_str

#spot price util 2025-10-01
spotprice1 <- get_elspot("https://api.energidataservice.dk/dataset/Elspotprices", 
                         start = one_year_ago_str, #"2025-02-22T00:00",
                         end   = "2025-10-01T00:00"
)

# Ensure HourDK column are datetime
spotprice1 <- spotprice1 %>%
  mutate(HourDK = ymd_hms(HourDK))


#spot price from 2025-10-01
spotprice2 <- get_elspot("https://api.energidataservice.dk/dataset/DayAheadPrices", 
                         start = "2025-10-01T00:00",
                         end   = today_end_str # "2026-02-23T00:00"
) %>% 
  rename(HourUTC = TimeUTC,
         HourDK = TimeDK,
         SpotPriceDKK = DayAheadPriceDKK,
         SpotPriceEUR = DayAheadPriceEUR)

# Ensure HourDK column are datetime
spotprice2 <- spotprice2 %>%
  mutate(HourDK = ymd_hms(HourDK))

# make spotprice1 to quaterly data 
# create 15-min intervals for each hour
spotprice1_quarter <- spotprice1 %>%
  # for each row, create 4 rows
  slice(rep(1:n(), each = 4)) %>%
  group_by(HourDK) %>%
  mutate(
    # offset minutes by 0, 15, 30, 45
    HourDK = HourDK + minutes(rep(c(0, 15, 30, 45), times = n()/4))
  ) %>%
  ungroup()

# Combine datasets
spotprice_all <- bind_rows(spotprice1_quarter, spotprice2) %>%
  arrange(HourDK)

