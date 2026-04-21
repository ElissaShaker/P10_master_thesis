path <- setwd("~/Desktop/P10") 
source("Get_Data_Hourly.R")
#########################
#### MODEL SELECTION ####
#########################
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
re_model <- plm(SpotPriceDKK ~ ConsumptionkWh + Weekday + Month,
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

###########################
#### DYAMIC PANEL DATA ####
###########################
spot_price_panel <- panel_data %>% 
  mutate(
    Date = as.Date(Date),
    Weekday = factor(Weekday)
  )

pdata_spotprice <- pdata.frame(spot_price_panel, index = c("Hour", "Date"))
pdata_spotprice$Date <- as.Date(as.character(pdata_spotprice$Date))

panels <- pdata_spotprice %>%
  as.data.frame() %>%
  group_by(Hour) %>%
  group_split()

### Dynamic model creation ###
# Ensure correct contrasts (VERY important)
options(contrasts = c("contr.treatment", "contr.poly"))

# Data preparation
spot_price_panel <- panel_data %>%
  mutate(
    Date = as.Date(Date),
    Weekday = factor(Weekday, levels = c("ma","ti","on","to","fr","lø","sø")),
    Hour = as.factor(Hour)
  )

# Create panel structure
pdata_spotprice <- pdata.frame(spot_price_panel, index = c("Hour", "Date"))

# Create lags (panel-aware)
pdata_spotprice$lag1 <- plm::lag(pdata_spotprice$SpotPriceDKK, 1)
pdata_spotprice$lag7 <- plm::lag(pdata_spotprice$SpotPriceDKK, 7)

# Estimate model
dyn_model <- plm(
  SpotPriceDKK ~ lag1 + lag7 + ConsumptionkWh,
  data = pdata_spotprice,
  model = "pooling"
)
summary(dyn_model)
coef_dyn <- coef(dyn_model)

dyn_model_FE <- plm(
  SpotPriceDKK ~ lag1 + lag7 + ConsumptionkWh,
  data = pdata_spotprice,
  model = "within"
)
summary(dyn_model_FE)
coef_dyn_FE <- coef(dyn_model_FE)

# Split panels (per hour)
panels <- pdata_spotprice %>%
  as.data.frame() %>%
  group_by(Hour) %>%
  group_split()

# Weekday mapping (FIX)
weekday_map <- c(
  "Monday" = "ma",
  "Tuesday" = "ti",
  "Wednesday" = "on",
  "Thursday" = "to",
  "Friday" = "fr",
  "Saturday" = "lø",
  "Sunday" = "sø"
)

##################
#### FORECAST ####
##################
forecast_spot_panel <- function(panel_df, h, coef_dyn, avg_consumption) {
  
  panel_df <- panel_df[order(panel_df$Date), ]
  panel_df$Date <- as.Date(panel_df$Date)
  
  last_y  <- tail(panel_df$SpotPriceDKK, 1)
  
  last_date <- max(panel_df$Date)
  future_dates <- seq(from = last_date + 1, by = "day", length.out = h)
  
  forecast <- numeric(h)
  
  for (t in 1:h) {
    
    # lag1 (recursive)
    lag1_val <- if (t == 1) last_y else forecast[t - 1]
    
    # lag7
    if (t <= 7) {
      lag7_val <- panel_df$SpotPriceDKK[nrow(panel_df) - 7 + t]
    } else {
      lag7_val <- forecast[t - 7]
    }
    
    # consumption input (IMPORTANT)
    cons_val <- avg_consumption
    
    # forecast equation
    forecast[t] <- coef_dyn["(Intercept)"] +
      coef_dyn["lag1"] * lag1_val +
      coef_dyn["lag7"] * lag7_val +
      coef_dyn["ConsumptionkWh"] * cons_val
  }
  
  data.frame(
    Hour = unique(panel_df$Hour),
    Date = c(last_date, future_dates),
    Forecast = c(last_y, forecast)
  )
}

# Run forecast
h <- 5

avg_consumption <- mean(pdata_spotprice$ConsumptionkWh, na.rm = TRUE)

forecast_list <- lapply(
  panels,
  forecast_spot_panel,
  h = h,
  coef_dyn = coef_dyn,
  avg_consumption = avg_consumption
)

forecast_df <- do.call(rbind, forecast_list) %>%
  arrange(Hour, Date)

# Historical data
history_df <- pdata_spotprice %>%
  as.data.frame() %>%
  mutate(
    Date = as.Date(Date),
    Hour = as.factor(Hour),
    SpotPriceDKK = as.numeric(SpotPriceDKK)
  ) %>%
  select(Hour, Date, SpotPriceDKK)

forecast_df <- forecast_df %>%
  mutate(
    Date = as.Date(Date),
    Hour = as.factor(Hour),
    Forecast = as.numeric(Forecast)
  )

# Plot
cutoff_date <- max(history_df$Date) - 30
history_recent <- history_df %>%
  filter(Date >= cutoff_date)

forecast_recent <- forecast_df

ggplot() +
  geom_line(data = history_recent,
            aes(x = Date, y = SpotPriceDKK, group = Hour),
            alpha = 0.2) +
  geom_line(data = forecast_recent,
            aes(x = Date, y = Forecast, group = Hour),
            color = "blue",
            linewidth = 1.2) +
  geom_vline(xintercept = max(history_df$Date),
             linetype = "dashed") +
  labs(
    title = "Spot Price Forecast",
    subtitle = "Dashed line = forecast start",
    y = "Spot Price (DKK)",
    x = "Date"
  )


# combine ts
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


plot <- ggplot() +
  geom_line(data = history_recent_ts,
            aes(x = Datetime, y = SpotPriceDKK),
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
    x = "",
    y = "Spot Prices (DKK)"
  )
plot


