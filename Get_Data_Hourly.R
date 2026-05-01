start_time <- Sys.time()
library(httr)
library(jsonlite)
library(dplyr)
library(tidyr)
library(lubridate)
library(plm)
library(ggplot2)
setwd("~/Desktop/P10")
########################
#### GET SPOT PRICE ####
########################
# function til at hente spotpriser i DK1
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
one_year_ago <- today_end - years(6)

# Format as "YYYY-MM-DDT00:00"
today_end_str <- paste0(format(today_end, "%Y-%m-%d"), "T00:00")
one_year_ago_str <- paste0(format(one_year_ago, "%Y-%m-%d"), "T00:00")

today_end_str
one_year_ago_str

#spot price util 2025-10-01
spotprice1 <- get_elspot("https://api.energidataservice.dk/dataset/Elspotprices", 
                         start = one_year_ago_str, # "2025-03-17T00:00",
                         end   = "2025-10-01T00:00" # denne ædres ikke da dataet stopper her
)

# Ensure HourDK column are datetime
spotprice1 <- spotprice1 %>%
  mutate(HourDK = ymd_hms(HourDK))


#spot price from 2025-10-01
spotprice2 <- get_elspot("https://api.energidataservice.dk/dataset/DayAheadPrices", 
                         start = "2025-10-01T00:00", # denne ædres ikke da dataet starter her
                         end   = today_end_str # "2026-03-17T00:00"
) %>% 
  rename(HourUTC = TimeUTC,
         HourDK = TimeDK,
         SpotPriceDKK = DayAheadPriceDKK,
         SpotPriceEUR = DayAheadPriceEUR)

# make spotprice2 to hourly data
spotprice2_hourly <- spotprice2 %>%
  mutate(HourDK = ymd_hms(HourDK)) %>%   # convert to datetime
  filter(minute(HourDK) == 0)            # keep only rows where minutes = 0# 

# Combine datasets
spotprice_all <- bind_rows(spotprice1, spotprice2_hourly) %>%
  arrange(HourDK)

# hourly panel, each hour should be a separate “group”:
spotprice_panel <- spotprice_all %>%
  mutate(
    Date = as_date(HourDK),
    Hour = hour(HourDK),
    Weekday = wday(Date, label = TRUE, week_start = 1),  # Monday = 1
    Month = month(Date, label = TRUE, abbr = TRUE)       # Jan, Feb, ...
  ) %>%
  group_by(Date, Hour) %>%  # handle repeated hours (DST)
  summarise(
    SpotPriceDKK = mean(SpotPriceDKK, na.rm = TRUE),
    SpotPriceEUR = mean(SpotPriceEUR, na.rm = TRUE),
    Weekday = first(Weekday),  # keep Weekday
    Month = first(Month),      # keep Month
    .groups = "drop"
  )

# dubletter for dag med 23 timer elrpis (vinter til sommertid)
spotprice_panel <- spotprice_panel %>%
  group_by(Date) %>%
  mutate(n_hours = n()) %>%
  ungroup()

# Find dage ikke 24 timer
spotprice_panel %>%
  group_by(Date) %>%
  summarise(n_hours = n()) %>%
  filter(n_hours != 24)

# Udvid til fuld 24-timers struktur
spotprice_panel <- spotprice_panel %>%
  group_by(Date) %>%
  complete(Hour = 0:23) %>%   # sikrer alle timer findes
  arrange(Date, Hour) %>%
  fill(Weekday, Month, .direction = "downup") %>%
  ungroup()

# Udfyld manglende priser med forrige time
spotprice_panel <- spotprice_panel %>%
  group_by(Date) %>%
  arrange(Hour) %>%
  fill(SpotPriceDKK, SpotPriceEUR, .direction = "down") %>%
  ungroup()

####################################
#### SPOT PRICE ANALYSIS CHPT.2 ####
####################################
# spotprice_subset <- spotprice_panel %>%
#   filter(Hour %in% c(0, 6, 12, 18)) #%>%
#   # mutate(
#   #   HourLabel = case_when(
#   #     Hour == 0 ~ 24,       # recode midnight to 24
#   #     TRUE ~ Hour
#   #   )
#   # )
# 
# # plot af time 8, 16 og 24
# ggplot(spotprice_subset, aes(x = Date, y = SpotPriceDKK)) +
#   geom_line(color = "steelblue") +
#   facet_wrap(~ Hour, ncol = 1, scales = "free_y") +
#    #facet_wrap(~ HourLabel, ncol = 1, scales = "free_y") +
#   labs(
#     title = "Spot Prices Over Time for Selected Hours (DK time)",
#     x = "",
#     y = "Spot Price (DKK)",# caption = "Hour 24 = midnight"
#   ) +
#   scale_x_date(
#     date_breaks = "3 months",      # hver 2. måned
#     date_labels = "%b \n %Y"          # fx "Jan 2025"
#   ) +
#   theme_minimal()
# ggsave("plots/Hourly/spotprice_hourly_Time0_8_16.png", width = 10, height = 6, dpi = 300)
# 
# 
# # Compute average price per Hour and Weekday
# avg_hourly <- spotprice_panel %>%
#   group_by(Weekday, Hour) %>%
#   summarise(
#     AvgPriceDKK = mean(SpotPriceDKK, na.rm = TRUE),
#     .groups = "drop"
#   )
# 
# # Plot: one line per weekday
# ggplot(avg_hourly, aes(x = Hour, y = AvgPriceDKK, color = Weekday)) +
#   geom_line(size = 1) +
#   scale_x_continuous(breaks = 0:23) +
#   labs(
#     title = "Average Hourly Day-Ahead Spot Price by Weekday",
#     x = "Hour of Day",
#     y = "Average Spot Price (DKK)",
#     color = "Weekday"
#   ) +
#   theme_minimal(base_size = 20)  # <- key change
# ggsave("plots/Hourly/spotprice_avg_hourly_weekday.png", width = 10, height = 6, dpi = 600)
# 
# 
# avg_hourly_month <- spotprice_panel %>%
#   group_by(Month, Hour) %>%
#   summarise(
#     AvgPriceDKK = mean(SpotPriceDKK, na.rm = TRUE),
#     .groups = "drop"
#   )
# 
# # Plot: one line per month
# ggplot(avg_hourly_month, aes(x = Hour, y = AvgPriceDKK, color = Month)) +
#   geom_line(size = 1) +
#   scale_x_continuous(breaks = 0:23) +
#   labs(
#     title = "Average Hourly Day-Ahead Spot Price by Month",
#     x = "Hour of Day",
#     y = "Average Spot Price (DKK)",
#     color = "Month"
#   ) +
#   theme_minimal(base_size = 20)  # <- key change
# ggsave("plots/Hourly/spotprice_avg_hourly_month.png", width = 10, height = 6, dpi = 600)
# 

#########################
#### GET CONSUMPTION ####
#########################
# https://www.energidataservice.dk/dso-electricity/ConsumptionConsumerCategoryHour
get_consumption <- function(base, start = NULL, region = "Region Nordjylland") {
  query <- list()
  
  if (!is.null(start)) query$start <- start
  
  if (!is.null(region)) {
    query$filter <- sprintf('{"RegionName":"%s"}', region)
  }
  
  res <- GET(base, query = query)
  stop_for_status(res)
  
  data <- content(res, as = "text", encoding = "UTF-8")
  parsed <- fromJSON(data, flatten = TRUE)
  
  return(as_tibble(parsed$records))
}

# Brug funktionen:
consumption <- get_consumption("https://api.energidataservice.dk/dataset/ConsumptionConsumerCategoryHour?",
                               start = one_year_ago_str)%>% 
  rename(HourUTC = TimeUTC,
         HourDK = TimeDK)

consumption <- consumption %>%
  mutate(HourDK = ymd_hms(HourDK)) %>%  
  select(HourDK, ConsumptionkWh)

consumption_panel <- consumption %>%
  mutate(
    Date = as_date(HourDK),
    Hour = hour(HourDK),
    Weekday = wday(Date, label = TRUE, week_start = 1),  # Monday = 1
    Month = month(Date, label = TRUE, abbr = TRUE)
  ) %>%
  group_by(Date, Hour) %>%   # hvis der også er dubletter her
  summarise(
    ConsumptionkWh = mean(ConsumptionkWh, na.rm = TRUE),
    Weekday = first(Weekday),  # keep Weekday
    Month = first(Month),
    .groups = "drop"
  )

# tjekker om der er manglende time
consumption_panel <- consumption_panel %>%
  group_by(Date) %>%
  mutate(n_hours = n()) %>%
  ungroup()

# Find dage med kun 23 timer
consumption_panel %>%
  group_by(Date) %>%
  summarise(n_hours = n()) %>%
  filter(n_hours == 23)

# Udvid til fuld 24-timers struktur
consumption_panel <- consumption_panel %>%
  group_by(Date) %>%
  complete(Hour = 0:23) %>%   # sikrer alle timer findes
  arrange(Date, Hour) %>%
  fill(Weekday, Month, .direction = "downup") %>%
  ungroup()

# Udfyld manglende priser med forrige time
consumption_panel <- consumption_panel %>%
  group_by(Date) %>%
  arrange(Hour) %>%
  fill(ConsumptionkWh, .direction = "down") %>%
  ungroup()

#################
### GET WIND ####
#################
# https://energidataservice.dk/tso-electricity/Forecasts_5Min
# 3 variables for wind in 5 min interval agregate to 15 min interval with avg
get_wind <- function(base, start = NULL, end = NULL, pricearea = "DK1") {
  
  query <- list()
  
  if (!is.null(start)) query$start <- start
  if (!is.null(end))   query$end   <- end
  
  # force PriceArea filter (default DK1)
  if (!is.null(pricearea)) {
    query$filter <- sprintf('{"PriceArea":"%s"}', pricearea)
  }
  
  res <- GET(base, query = query)
  stop_for_status(res)
  
  parsed <- fromJSON(content(res, "text", encoding = "UTF-8"), flatten = TRUE)
  
  as_tibble(parsed$records)
}

wind <- get_wind(
  base = "https://api.energidataservice.dk/dataset/ElectricityProdex5MinRealtime?",
  start = one_year_ago_str,
  end   = today_end_str
)

wind_hourly_panel <- wind %>%
  mutate(
    Minutes5DK = ymd_hms(Minutes5DK),
    DateHour = floor_date(Minutes5DK, unit = "hour"),
    Date = as_date(DateHour),
    Hour = hour(DateHour),
    Weekday = wday(Date, label = TRUE, week_start = 1),
    Month = month(Date, label = TRUE, abbr = TRUE)
  ) %>%
  group_by(DateHour) %>%
  summarise(
    OffshoreWindPower = mean(OffshoreWindPower, na.rm = TRUE),
    OnshoreWindPower  = mean(OnshoreWindPower, na.rm = TRUE),
    SolarPower        = mean(SolarPower, na.rm = TRUE),
    Date = first(Date),
    Hour = first(Hour),
    Weekday = first(Weekday),
    Month = first(Month),
    .groups = "drop"
  )

# Find og log manglende timer pr. dag
missing_hours_check <- wind_hourly_panel %>%
  group_by(Date) %>%
  summarise(n_hours = n_distinct(Hour), .groups = "drop") %>%
  filter(n_hours != 24)

missing_hours_check

# Rebuild fuld 24-timers struktur
wind_hourly_panel <- wind_hourly_panel %>%
  group_by(Date) %>%
  complete(Hour = 0:23) %>%
  arrange(Date, Hour) %>%
  ungroup()

# Genopbyg DateTime (vigtigt)
wind_hourly_panel <- wind_hourly_panel %>%
  mutate(
    DateTime = as.POSIXct(Date) + hours(Hour)
  )

wind_hourly_panel <- wind_hourly_panel %>%
  group_by(Date) %>%
  fill(Weekday, Month, OffshoreWindPower, OnshoreWindPower, SolarPower, .direction = "downup") %>%
  ungroup()

NA_wind <- wind_hourly_panel %>%
  filter(if_any(everything(), is.na))

##########
# tecnical error from energinet 2026-03-29
bad_day <- as.Date("2026-03-29")
prev_day <- bad_day - 1
bad_hour <- 3

test <- wind_hourly_panel %>% filter(Date=="2026-03-29")

wind_hourly_panel <- wind_hourly_panel %>%
  filter(!(Date == bad_day & Hour %in% bad_hour)) %>%
  bind_rows(
    wind_hourly_panel %>%
      filter(Date == prev_day, Hour %in% bad_hour) %>%
      mutate(Date = bad_day)
  ) %>%
  mutate(
    Weekday = wday(Date, label = TRUE, week_start = 1)
  ) %>%
  arrange(Date, Hour)

test1 <- wind_hourly_panel %>% filter(Date=="2026-03-29")

##########
# date 2025-11-22 is missing and therefore we copy the day before
bad_day <- as.Date("2025-11-22")
prev_day <- bad_day - 1

test <- wind_hourly_panel %>% filter(Date=="2025-11-22")

wind_hourly_panel <- wind_hourly_panel %>%
  # remove bad day if it exists (safety)
  filter(Date != bad_day) %>%
  
  # copy previous day and relabel it as bad_day
  bind_rows(
    wind_hourly_panel %>%
      filter(Date == prev_day) %>%
      mutate(
        Date = bad_day,
        DateHour = DateHour + lubridate::days(1)
      )
  ) %>%
  
  arrange(DateHour)

test1 <- wind_hourly_panel %>% filter(Date=="2025-11-22")

##################
#### ALL DATA ####
##################
panel_data <- spotprice_panel %>%
  left_join(
    consumption_panel %>%
      select(Date, Hour, ConsumptionkWh),
    by = c("Date", "Hour")
  ) %>%
  left_join(
    wind_hourly_panel %>%
      select(Date, Hour, OffshoreWindPower, OnshoreWindPower, SolarPower),
    by = c("Date", "Hour")
  ) %>% 
  filter(!is.na(ConsumptionkWh))


# tjekker om alt er rigtigt
check_panel <- function(df, value_col) {
  list(
    wrong_hours = df %>% count(Date) %>% filter(n != 24),
    missing_values = sum(is.na(df[[value_col]])),
    duplicates = df %>% count(Date, Hour) %>% filter(n > 1)
  )
}

check_panel(panel_data, "SpotPriceDKK")
check_panel(panel_data, "ConsumptionkWh")
check_panel(panel_data, "SpotPriceDKK")
check_panel(panel_data, "ConsumptionkWh")
check_panel(panel_data, "OffshoreWindPower")
check_panel(panel_data, "OnshoreWindPower")
check_panel(panel_data, "SolarPower")

####################
#### PANEL DATA ####
####################
panel_data <- panel_data %>%
  mutate(Date = as.factor(Date))  # important for FE

pdata <- pdata.frame(panel_data, index = c("Hour", "Date"))

end_time <- Sys.time()
end_time - start_time