path <- setwd("~/Desktop/P10") 
source("Get_Data_Hourly.R")
#########################
#### MODEL SELECTION ####
#########################
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
re_model <- plm(ConsumptionkWh ~ SpotPriceDKK + Weekday + Month,
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
  ConsumptionkWh ~ lag1 + lag7 + SpotPriceDKK,
  data = pdata_consumption,
  model = "pooling"
)
summary(dyn_model)
coef_dyn <- coef(dyn_model)

dyn_model_FE <- plm(
  ConsumptionkWh ~ lag1 + lag7 + SpotPriceDKK,
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
      coef_dyn["SpotPriceDKK"] * avg_price 
    #+ sum(coef_dyn[grep("Weekday", names(coef_dyn))] * wd_mat[1, -1])
  }
  
  data.frame(
    Hour = unique(panel_df$Hour),
    Date = c(last_date, future_dates),
    Forecast = c(last_y, forecast)
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

forecast_df <- do.call(rbind, forecast_list) %>%
  arrange(Hour, Date)
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

plot1 <- ggplot() +
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
    x = ""
  )
plot1
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

