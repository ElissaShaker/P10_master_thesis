library(httr)
library(jsonlite)
library(dplyr)
library(tidyr)
library(lubridate)
library(plm)
library(ggplot2)

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
one_year_ago <- today_end - years(2)

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

# Find dage med kun 23 timer
spotprice_panel %>%
  group_by(Date) %>%
  summarise(n_hours = n()) %>%
  filter(n_hours == 23)

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
#     x = "Date",
#     y = "Spot Price (DKK)",# caption = "Hour 24 = midnight"
#   ) +
#   scale_x_date(
#     date_breaks = "1 months",      # hver 2. måned
#     date_labels = "%b \n %Y"          # fx "Jan 2025"
#   ) +
#   theme_minimal()
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
#   theme_minimal()
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
#   theme_minimal()


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

##################
#### ALL DATA ####
##################
panel_data <- spotprice_panel %>%
  left_join(
    consumption_panel %>%
      select(Date, Hour, ConsumptionkWh),
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

check_panel(spotprice_panel, "SpotPriceDKK")
check_panel(consumption_panel, "ConsumptionkWh")
check_panel(panel_data, "SpotPriceDKK")
check_panel(panel_data, "ConsumptionkWh")

#########################
#### MODEL SELECTION ####
#########################
# # Treat 'Hour' as the individual i, 'Date' as the time t
# panel_data <- panel_data %>%
#   dplyr::mutate(
#     Weekday = as.numeric(Weekday),
#     Month = as.numeric(Month)
#     )
panel_data <- panel_data %>%
  mutate(Date = as.factor(Date))  # important for FE

pdata <- pdata.frame(panel_data, index = c("Date", "Hour"))

# Pooled Regression (PR)
pr_model <- plm(SpotPriceDKK ~ ConsumptionkWh + factor(Hour) + Weekday + Month,
  data = pdata,
  model = "pooling"
)

# Fixed Effects (FE)
fe_model <- plm(SpotPriceDKK ~ ConsumptionkWh + factor(Hour), 
  # You cannot include Weekday or Month in FE if they are constant within Date (they get absorbed).
  data = pdata,
  model = "within"
)

# Random Effects (RE)
re_model <- plm(SpotPriceDKK ~ ConsumptionkWh + factor(Hour) + Weekday + Month,
  data = pdata,
  model = "random"
)

##############
#### TEST ####
##############
# FE vs PR → F-test 
## if p-val<0.05 => FE
pFtest(fe_model, pr_model)

# RE vs PR → Breusch-Pagan Lagrange Multiplier test
## if p-val<0.05 => RE
plmtest(pr_model, type = "bp")

# FE vs RE → Hausman test
## if p-val<0.05 => RE
phtest(fe_model, re_model)


#### DYAMIC PANEL DATA ####
dyn_model <- plm(SpotPriceDKK ~ SpotPriceLag1 + Weekday,
                 data = pdata.frame(spotprice_panel_dynamic, index = c("Hour","Date")),
                 model = "pooling")
summary(dyn_model)
# det ses at SpotPriceLag1 p-val<2e-16, hvilket betyder den er significant.
# Dette tyder på at vi har en Dynamic panel data


