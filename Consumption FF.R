# Consumption af FF Forsyning FAK varmeværk
library(httr)
library(jsonlite)
library(dplyr)
library(lubridate)
library(tidyr)
library(plm)
library(ggplot2)

#########################
#### GET DATA ###########
#########################
#########################
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
panel_data <- consumption_panel %>%
  left_join(
    spotprice_panel %>%
      select(Date, Hour, SpotPriceDKK),
    by = c("Date", "Hour")
  ) #%>%
  #filter(!is.na(ConsumptionkWh))


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

pdata <- pdata.frame(panel_data, index = c("Hour", "Date"))

# Pooled Regression (PR)
pr_model <- plm(ConsumptionkWh ~ SpotPriceDKK + factor(Hour) + Weekday + Month,
                data = pdata,
                model = "pooling"
)

# Fixed Effects (FE)
fe_model <- plm(ConsumptionkWh ~ SpotPriceDKK + factor(Hour), 
                # You cannot include Weekday or Month in FE if they are constant within Date (they get absorbed).
                data = pdata,
                model = "within"
)

# Random Effects (RE)
re_model <- plm(ConsumptionkWh ~ SpotPriceDKK + factor(Hour) + Weekday + Month,
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
########## Arrellano Bond #############
dyn_gmm <- pgmm(
  ConsumptionkWh ~ lag(ConsumptionkWh, 1) + Weekday |
    lag(ConsumptionkWh, 2:3),
  data = pdata_consumption,
  effect = "individual",
  model = "twosteps"
)

summary(dyn_gmm)


### HERFRA ELISSA ###
### big boy ###
################################################
consumption_panel_data <- panel_data %>%
  mutate(
    Date = as.Date(Date),
    Weekday = factor(Weekday)
  )

pdata_consumption <- pdata.frame(consumption_panel_data, index = c("Hour", "Date"))
pdata_consumption$Date <- as.Date(as.character(pdata_consumption$Date))

panels <- pdata_consumption %>%
  as.data.frame() %>%
  group_by(Hour) %>%
  group_split()

### Dynamic model creation ###
# Ensure correct contrasts (VERY important)
options(contrasts = c("contr.treatment", "contr.poly"))

# =========================
# 2. Data preparation
# =========================
consumption_panel_data <- panel_data %>%
  mutate(
    Date = as.Date(Date),
    Weekday = factor(Weekday, levels = c("ma","ti","on","to","fr","lø","sø")),
    Hour = as.factor(Hour)
  )

# Create panel structure
pdata_consumption <- pdata.frame(consumption_panel_data, index = c("Hour", "Date"))

# =========================
# 3. Create lags (panel-aware)
# =========================
pdata_consumption$lag1 <- lag(pdata_consumption$ConsumptionkWh, 1)
pdata_consumption$lag7 <- lag(pdata_consumption$ConsumptionkWh, 7)

# =========================
# 4. Estimate model
# =========================
dyn_model <- plm(
  ConsumptionkWh ~ lag1 + lag7 + SpotPriceDKK + Weekday,
  data = pdata_consumption,
  model = "pooling"
)
summary(dyn_model)
coef_dyn <- coef(dyn_model)

dyn_model_FE <- plm(
  ConsumptionkWh ~ lag1 + lag7 + SpotPriceDKK + Weekday,
  data = pdata_consumption,
  model = "within"
)
summary(dyn_model_FE)
coef_dyn_FE <- coef(dyn_model_FE)

# =========================
# 5. Split panels (per hour)
# =========================
panels <- pdata_consumption %>%
  as.data.frame() %>%
  group_by(Hour) %>%
  group_split()

# =========================
# 6. Weekday mapping (FIX)
# =========================
weekday_map <- c(
  "Monday" = "ma",
  "Tuesday" = "ti",
  "Wednesday" = "on",
  "Thursday" = "to",
  "Friday" = "fr",
  "Saturday" = "lø",
  "Sunday" = "sø"
)

# =========================
# 7. Forecast function
# =========================
forecast_panel <- function(panel_df, h, coef_dyn, avg_price) {
  
  panel_df <- panel_df[order(panel_df$Date), ]
  panel_df$Date <- as.Date(panel_df$Date)
  
  last_y  <- tail(panel_df$ConsumptionkWh, 1)
  last_y7 <- tail(panel_df$ConsumptionkWh, 7)[1]  # 7 days ago
  
  last_date <- max(panel_df$Date)
  future_dates <- seq(from = last_date + 1, by = "day", length.out = h)
  
  forecast <- numeric(h)
  
  for (t in 1:h) {
    
    lag1_val <- if (t == 1) last_y else forecast[t - 1]
    
    if (t <= 7) {
      lag7_val <- panel_df$ConsumptionkWh[nrow(panel_df) - 7 + t]
    } else {
      lag7_val <- forecast[t - 7]
    }
    
    # Robust weekday handling
    wd_num <- as.POSIXlt(future_dates[t])$wday
    wd_char <- c("sø","ma","ti","on","to","fr","lø")[wd_num + 1]
    
    weekday_val <- factor(
      wd_char,
      levels = levels(panel_df$Weekday)
    )
    
    wd_mat <- model.matrix(~ Weekday, data = data.frame(Weekday = weekday_val))
    
    forecast[t] <- coef_dyn["(Intercept)"] +
      coef_dyn["lag1"] * lag1_val +
      coef_dyn["lag7"] * lag7_val +
      coef_dyn["SpotPriceDKK"] * avg_price +
      sum(coef_dyn[grep("Weekday", names(coef_dyn))] * wd_mat[1, -1])
  }
  
  data.frame(
    Hour = unique(panel_df$Hour),
    Date = future_dates,
    Forecast = forecast
  )
}

# =========================
# 8. Run forecasts
# =========================
h <- 5  # 5 days ahead (per hour panel)

avg_price <- mean(pdata_consumption$SpotPriceDKK, na.rm = TRUE)

forecast_list <- lapply(
  panels,
  forecast_panel,
  h = h,
  coef_dyn = coef_dyn,
  avg_price = avg_price
)

forecast_df <- do.call(rbind, forecast_list)

# =========================
# 9. Historical data
# =========================
history_df <- pdata_consumption %>%
  as.data.frame() %>%
  select(Hour, Date, ConsumptionkWh)

history_df <- pdata_consumption %>%
  as.data.frame() %>%
  mutate(
    Date = as.Date(Date),
    Hour = as.factor(Hour),
    ConsumptionkWh = as.numeric(ConsumptionkWh)
  ) %>%
  select(Hour, Date, ConsumptionkWh)

forecast_df <- forecast_df %>%
  mutate(
    Date = as.Date(Date),
    Hour = as.factor(Hour),
    Forecast = as.numeric(Forecast)
  )

# =========================
# 10. Plot
# =========================
##########
cutoff_date <- max(history_df$Date) - 30
history_recent <- history_df %>%
  filter(Date >= cutoff_date)

forecast_recent <- forecast_df

ggplot() +
  geom_line(data = history_recent,
            aes(x = Date, y = ConsumptionkWh, group = Hour),
            alpha = 0.2) +
  geom_line(data = forecast_recent,
            aes(x = Date, y = Forecast, group = Hour),
            color = "blue",
            linewidth = 1.2) +
  geom_vline(xintercept = max(history_df$Date),
             linetype = "dashed") +
  labs(
    title = "Zoom: Last 30 Days + Forecast",
    subtitle = "Dashed line = forecast start",
    y = "Consumption (kWh)",
    x = "Date"
  )

# agg_history <- history_recent %>%
#   group_by(Date) %>%
#   summarise(Consumption = sum(ConsumptionkWh))
# 
# agg_forecast <- forecast_recent %>%
#   group_by(Date) %>%
#   summarise(Forecast = sum(Forecast))


#### Combine ts #########
history_ts <- history_df %>%
  mutate(
    Hour = as.numeric(as.character(Hour)),
    Datetime = as.POSIXct(Date) + Hour * 3600
  ) %>%
  arrange(Datetime)

forecast_ts <- forecast_df %>%
  mutate(
    Hour = as.numeric(as.character(Hour)),
    Datetime = as.POSIXct(Date) + Hour * 3600
  ) %>%
  arrange(Datetime)

#### 14 dage ####
cutoff_dt <- max(history_ts$Datetime) - 14*24*3600

history_recent_ts <- history_ts %>%
  filter(Datetime >= cutoff_dt)

scale_x_datetime(
  date_breaks = "12 hours",
  date_labels = "%b %d\n%H:%M"
)
scale_x_datetime(
  date_breaks = "1 day",
  date_labels = "%b %d"
)

ggplot() +
  geom_line(data = history_recent_ts,
            aes(x = Datetime, y = ConsumptionkWh),
            alpha = 0.5) +
  geom_line(data = forecast_ts,
            aes(x = Datetime, y = Forecast),
            color = "blue",
            linewidth = 1.2) +
  geom_vline(xintercept = max(history_ts$Datetime),
             linetype = "dashed") +
  scale_x_datetime(
    date_breaks = "1 day",
    date_labels = "%b %d"
  ) +
  labs(
    title = "Continuous Hourly Forecast (Last 14 Days)",
    subtitle = "Dashed line = forecast start",
    x = "Datetime",
    y = "Consumption (kWh)"
  )

