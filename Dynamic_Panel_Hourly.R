path <- setwd("C:/Users/P/OneDrive - Aalborg Universitet/Skrivebord/P10")
source("Get_Data_Hourly.R")

options(contrasts = c("contr.treatment", "contr.poly"))

# =========================================================
# Active transformation
# =========================================================

#price_var <- "YJPrice"
#price_var <- "LogPrice_asinh"
#price_var <- "LogPrice_100"
price_var <- "LogPrice"

# =========================================================
# 2. Prepare panel data
# =========================================================

panel_data <- train_data %>%
  mutate(
    Date = as.Date(Date),

    Weekday = factor(
      weekdays(Date),
      levels = c(
        "Monday",
        "Tuesday",
        "Wednesday",
        "Thursday",
        "Friday",
        "Saturday",
        "Sunday"
      )
    )
  )

pdata <- pdata.frame(
  panel_data,
  index = c("Hour", "Date")
)

# =========================================================
# 3. Create lags
# =========================================================

pdata$lag1 <- lag(pdata[[price_var]], 1)
pdata$lag7 <- lag(pdata[[price_var]], 7)

# =========================================================
# 4. Estimate dynamic panel model
# =========================================================

formula_dyn <- as.formula(
  paste(
    price_var,
    "~ lag1 + lag7 +",
    "Consumption_DK1 +",
    "OffshoreWindPower_DK1 +",
    "OnshoreWindPower_DK1 +",
    "SolarPower_DK1 +",
    "Weekday"
  )
)

dyn_model <- plm(
  formula_dyn,
  data = pdata,
  model = "within"
)

summary(dyn_model)

coef_dyn <- coef(dyn_model)

fe <- fixef(dyn_model)

# =========================================================
# 5. Split into quarter panels
# =========================================================

panels <- pdata %>%
  as.data.frame() %>%
  group_by(Hour) %>%
  group_split()

# =========================================================
# 6. Forecast function
# =========================================================

inverse_yj <- function(y, lambda) {

  x <- numeric(length(y))

  # Positive branch
  pos <- y >= 0

  if (lambda != 0) {
    x[pos] <- (y[pos] * lambda + 1)^(1 / lambda) - 1
  } else {
    x[pos] <- exp(y[pos]) - 1
  }

  # Negative branch
  neg <- !pos

  if (lambda != 2) {
    x[neg] <- 1 - (1 - (2 - lambda) * y[neg])^(1 / (2 - lambda))
  } else {
    x[neg] <- 1 - exp(-y[neg])
  }

  return(x)
}

yj <- yeojohnson(train_data$SpotPrice_DK1)

lambda_yj <- yj$lambda


forecast_panel <- function(panel_df,
                           h,
                           coef_dyn,
                           fe,
                           future_consumption,
                           future_offshore,
                           future_onshore,
                           future_solar) {

  panel_df <- panel_df[order(panel_df$Date), ]

  panel_df$Date <- as.Date(panel_df$Date)

  last_date <- max(panel_df$Date)

  future_dates <- seq(
    from = last_date + 1,
    by = "day",
    length.out = h
  )

  alpha_i <- fe[as.character(unique(panel_df$Hour))]

  forecast <- numeric(h)

  for (t in 1:h) {

    # -----------------------------------------
    # Lag 1
    # -----------------------------------------

    lag1_val <- if (t == 1) {
      tail(panel_df[[price_var]], 1)
    } else {
      forecast[t - 1]
    }

    # -----------------------------------------
    # Lag 7
    # -----------------------------------------

    if (t <= 7) {
      lag7_val <- tail(panel_df[[price_var]], 8)[t]
    } else {
      lag7_val <- forecast[t - 7]
    }

    # -----------------------------------------
    # Weekday effect
    # -----------------------------------------

    wd <- weekdays(future_dates[t])

    weekday_effect <- ifelse(
      paste0("Weekday", wd) %in% names(coef_dyn),
      coef_dyn[paste0("Weekday", wd)],
      0
    )

    # -----------------------------------------
    # Recursive forecast
    # -----------------------------------------

    forecast[t] <-
      alpha_i +
      coef_dyn["lag1"] * lag1_val +
      coef_dyn["lag7"] * lag7_val +
      coef_dyn["Consumption_DK1"]    * future_consumption[t] +
      coef_dyn["OffshoreWindPower_DK1"] * future_offshore[t] +
      coef_dyn["OnshoreWindPower_DK1"]  * future_onshore[t] +
      coef_dyn["SolarPower_DK1"]        * future_solar[t] +
      weekday_effect
  }

  data.frame(
    Hour = unique(panel_df$Hour),
    Date = future_dates,
    Forecast_transformed = forecast,

    ForecastPrice = if (price_var == "LogPrice_asinh") {

      sinh(forecast)

    } else if (price_var == "LogPrice_100"){

      exp(forecast) - (abs(min(train_data$SpotPrice_DK1, na.rm = TRUE)) + 100)

    } else if (price_var == "LogPrice") {

      exp(forecast) - (abs(min(train_data$SpotPrice_DK1, na.rm = TRUE)) + 1)

    } else {

      inverse_yj(forecast, lambda_yj)

    }
  )
}

# =========================================================
# 7. Forecast horizon
# =========================================================

h <- 5

# =========================================================
# 8. Run recursive forecasts
# =========================================================

forecast_list <- lapply(
  panels,
  forecast_panel,

  h = h,
  coef_dyn = coef_dyn,
  fe = fe,

  future_consumption = rep(mean(pdata$Consumption_DK1), h),
  future_offshore    = rep(mean(pdata$OffshoreWindPower_DK1), h),
  future_onshore     = rep(mean(pdata$OnshoreWindPower_DK1), h),
  future_solar       = rep(mean(pdata$SolarPower_DK1), h)
)

forecast_df <- do.call(rbind, forecast_list)

forecast_df$Hour <- as.numeric(
  as.character(forecast_df$Hour)
)

# =========================================================
# 10. Historical data
# =========================================================

history_df <- pdata %>%
  as.data.frame() %>%
  mutate(
    Date = as.Date(Date)
  ) %>%
  select(
    Date,
    Hour,
    SpotPrice_DK1
  )

# =========================================================
# 11. Datetime construction
# =========================================================

history_df <- history_df %>%
  mutate(
    DateTime = as.POSIXct(
      paste(Date, sprintf("%02d:00:00", Hour)),
      format = "%Y-%m-%d %H:%M:%S"
    )
  )

forecast_df <- forecast_df %>%
  mutate(
    DateTime = as.POSIXct(
      paste(Date, sprintf("%02d:00:00", Hour)),
      format = "%Y-%m-%d %H:%M:%S"
    )
  )

forecast_df <- forecast_df %>%
  arrange(Date, Hour)

history_df <- history_df %>%
  arrange(Date, Hour)

# =========================================================
# 12. Keep recent historical data
# =========================================================

forecast_start <- min(forecast_df$Date)

cutoff_date <- forecast_start - h

history_plot <- history_df %>%
  filter(Date >= cutoff_date) %>%
  arrange(DateTime)

forecast_plot <- forecast_df %>%
  arrange(DateTime)

# =========================================================
# 13. Actual realized future prices
# =========================================================

actual_plot <- test_data %>%
  mutate(
    Date = as.Date(Date),

    DateTime = as.POSIXct(
      paste(Date, sprintf("%02d:00:00", Hour)),
      format = "%Y-%m-%d %H:%M:%S"
    )
  ) %>%
  filter(
    Date >= min(forecast_df$Date),
    Date < min(forecast_df$Date) + h
  ) %>%
  arrange(DateTime)

# =========================================================
# 14. Connect series
# =========================================================

last_hist_point <- history_plot %>%
  slice_tail(n = 1) %>%
  transmute(
    DateTime,
    ForecastPrice = as.numeric(SpotPrice_DK1)
  )

forecast_plot_connected <- bind_rows(
  last_hist_point,
  forecast_plot %>%
    select(DateTime, ForecastPrice)
)

actual_plot_connected <- bind_rows(

  last_hist_point %>%
    transmute(
      DateTime,
      SpotPrice_DK1 = ForecastPrice
    ),

  actual_plot %>%
    select(DateTime, SpotPrice_DK1)
)

# =========================================================
# 15. Plot
# =========================================================
plot_dynamic_panel_forecast <- function(
    history_plot,
    forecast_plot_connected,
    actual_plot_connected,
    save = FALSE
) {

  # forecast start
  forecast_start <- min(forecast_plot_connected$DateTime)

  p <- ggplot() +

    # Historical
    geom_line(
      data = history_plot,
      aes(
        x = DateTime,
        y = SpotPrice_DK1,
        color = "Historical"
      ),
      linewidth = 0.4
    ) +

    # Forecast
    geom_line(
      data = forecast_plot_connected,
      aes(
        x = DateTime,
        y = ForecastPrice,
        color = "Forecast"
      ),
      linewidth = 0.5
    ) +

    # Actual future realizations
    geom_line(
      data = actual_plot_connected,
      aes(
        x = DateTime,
        y = SpotPrice_DK1,
        color = "Actual"
      ),
      linewidth = 0.4,
      linetype = "dotted"
    ) +

    # Forecast split
    geom_vline(
      xintercept = forecast_start,
      linetype = "dashed",
      color = "black",
      linewidth = 0.5
    ) +

    scale_color_manual(
      values = c(
        "Historical" = "black",
        "Forecast" = "red",
        "Actual" = "blue"
      )
    ) +

    scale_x_datetime(
      date_breaks = "1 day",
      date_labels = "%b %d"
    ) +

    coord_cartesian(
      ylim = c(-300, 1800)
    ) +

    labs(
      title = "Dynamic Panel Forecast vs Actual for Hourly Data",
      x = "Date",
      y = "Spot Price",
      color = NULL
    ) +

    theme_minimal() +

    theme(

      plot.title = element_text(
        hjust = 0.5,
        size = 16
      ),

      axis.title = element_text(
        size = 14
      ),

      axis.text.x = element_text(
        angle = 45,
        hjust = 1
      ),

      legend.position = "right",

      legend.text = element_text(
        size = 11
      )
    )

  if(save) {

    ggsave(
      filename = "hourly_DynamicPanelForecast_full.png",
      plot = p,
      width = 12,
      height = 6,
      dpi = 600
    )
  }

  return(p)
}

plot_dynamic_panel_forecast(
  history_plot,
  forecast_plot_connected,
  actual_plot_connected
)
# Save
# plot_dynamic_panel_forecast(
#   history_plot,
#   forecast_plot_connected,
#   actual_plot_connected,
#   save = TRUE
# )

##########################
# Tests
##########################
# =========================================================
# Transformations
# =========================================================

transformations <- c(
  "LogPrice",
  "LogPrice_100",
  "LogPrice_asinh",
  "YJPrice"
)

# =========================================================
# Storage objects
# =========================================================

results <- list()

qq_data <- list()

# =========================================================
# Loop over transformations
# =========================================================

for (price_var in transformations) {

  cat("\n=================================================\n")
  cat("Running model for:", price_var, "\n")
  cat("=================================================\n")

  # ------------------------------------------------------
  # Create lags
  # ------------------------------------------------------

  pdata$lag1 <- lag(pdata[[price_var]], 1)
  pdata$lag7 <- lag(pdata[[price_var]], 7)

  # ------------------------------------------------------
  # Dynamic panel formula
  # ------------------------------------------------------

  formula_dyn <- as.formula(
    paste(
      price_var,
      "~ lag1 + lag7 +",
      "Consumption_DK1 +",
      "OffshoreWindPower_DK1 +",
      "OnshoreWindPower_DK1 +",
      "SolarPower_DK1 +",
      "Weekday"
    )
  )

  # ------------------------------------------------------
  # Estimate model
  # ------------------------------------------------------

  dyn_model <- plm(
    formula_dyn,
    data = pdata,
    model = "within"
  )

  # ------------------------------------------------------
  # Diagnostic tests
  # ------------------------------------------------------

  lm_test <- plmtest(dyn_model)

  cd_test <- pcdtest(
    dyn_model,
    test = "cd"
  )

  bg_test <- pbgtest(dyn_model)

  # ------------------------------------------------------
  # Robust SE tables
  # ------------------------------------------------------

  hc1_coefs <- coeftest(
    dyn_model,

    vcov = vcovHC(
      dyn_model,
      type = "HC1",
      cluster = "group"
    )
  )

  scc_coefs <- coeftest(
    dyn_model,

    vcov = vcovSCC(
      dyn_model,
      type = "HC1"
    )
  )

  # ------------------------------------------------------
  # Store everything
  # ------------------------------------------------------

  results[[price_var]] <- list(

    model = dyn_model,

    diagnostics = list(

      plmtest = lm_test,
      cdtest = cd_test,
      bgtest = bg_test
    ),

    coefficients = list(

      hc1 = hc1_coefs,
      scc = scc_coefs
    ),

    residuals = residuals(dyn_model)
  )

  # ------------------------------------------------------
  # QQ-plot data
  # ------------------------------------------------------

  qq_data[[price_var]] <- data.frame(

    Residuals = residuals(dyn_model),

    Transformation = price_var
  )
}

qq_df <- bind_rows(qq_data)

ggplot(
  qq_df,
  aes(sample = Residuals)
) +

  stat_qq(color = "blue") +

  stat_qq_line(
    color = "red",
    linewidth = 0.8
  ) +

  facet_wrap(
    ~Transformation,
    scales = "free"
  ) +

  labs(
    title = "QQ-Plots of Dynamic Panel Residuals",
    x = "Theoretical Quantiles",
    y = "Sample Quantiles"
  ) +

  theme_minimal()


# =========================================================
# Print all model results
# =========================================================

for (price_var in names(results)) {

  cat("\n\n")
  cat("====================================================\n")
  cat("Transformation:", price_var, "\n")
  cat("====================================================\n")

  # -----------------------------------------------------
  # Model summary
  # -----------------------------------------------------

  cat("\n---------------- MODEL SUMMARY ----------------\n")

  print(
    summary(
      results[[price_var]]$model
    )
  )

  # -----------------------------------------------------
  # LM Test
  # -----------------------------------------------------

  cat("\n---------------- PLMTEST ----------------\n")

  print(
    results[[price_var]]$diagnostics$plmtest
  )

  # -----------------------------------------------------
  # Cross-sectional dependence
  # -----------------------------------------------------

  cat("\n---------------- CD TEST ----------------\n")

  print(
    results[[price_var]]$diagnostics$cdtest
  )

  # -----------------------------------------------------
  # Serial correlation
  # -----------------------------------------------------

  cat("\n---------------- BG TEST ----------------\n")

  print(
    results[[price_var]]$diagnostics$bgtest
  )

  # -----------------------------------------------------
  # HC1 robust coefficients
  # -----------------------------------------------------

  cat("\n---------------- HC1 COEFFICIENTS ----------------\n")

  print(
    results[[price_var]]$coefficients$hc1
  )

  # -----------------------------------------------------
  # Driscoll-Kraay coefficients
  # -----------------------------------------------------

  cat("\n---------------- SCC COEFFICIENTS ----------------\n")

  print(
    results[[price_var]]$coefficients$scc
  )

  cat("\n====================================================\n")
}
