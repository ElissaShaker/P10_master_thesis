path <- setwd("~/Desktop/P10") 
source("Get_Data_Hourly.R")
###########################
#### PANEL DATA STATIC ####
###########################
head(pdata)
# Pooled Regression (PR)
pr_model <- plm(
  LogPrice_asinh ~ ConsumptionkWh + OffshoreWindPower +
    OnshoreWindPower + SolarPower,
  data = pdata,
  model = "pooling"
)

summary(pr_model)

# Fixed Effects (FE)
fe_model <- plm(
  LogPrice_asinh ~ ConsumptionkWh + OffshoreWindPower +
    OnshoreWindPower + SolarPower,
  data = pdata,
  model = "within"
)

summary(fe_model)

# Random Effects (RE)
re_model <- plm(LogPrice_asinh ~ ConsumptionkWh + OffshoreWindPower+ OnshoreWindPower + SolarPower +
                  Weekday + Month,
  data = pdata,
  model = "random"
)

summary(re_model)


panel_latex_table <- function(models,
                              model_names = c("Pooled OLS", "Fixed Effects", "Random Effects"),
                              digits = 3,
                              include_intercept = TRUE,
                              weekday_effects = c("No", "No", "Yes"),
                              month_effects   = c("No", "No", "Yes"),
                              caption = "Comparison of Panel Data Models for Electricity Prices") {
  
  # ---------- tidy ----------
  tidy_list <- map2(models, model_names, ~{
    tidy(.x) %>% mutate(model = .y)
  })
  
  df <- bind_rows(tidy_list)
  
  # ---------- REMOVE WEEKDAY + MONTH DUMMIES ----------
  df <- df %>%
    filter(!grepl("^Weekday", term),
           !grepl("^Month", term))
  
  # ---------- intercept ----------
  intercept_df <- df %>% filter(term == "(Intercept)")
  df <- df %>% filter(term != "(Intercept)")
  
  # ---------- stars ----------
  df <- df %>%
    mutate(
      stars = case_when(
        p.value < 0.001 ~ "***",
        p.value < 0.01  ~ "**",
        p.value < 0.05  ~ "*",
        p.value < 0.1   ~ ".",
        TRUE ~ ""
      ),
      est = formatC(estimate, format = "e", digits = digits),
      se  = formatC(std.error, format = "e", digits = digits),
      value = paste0(est, stars),
      se_value = paste0("(", se, ")")
    )
  
  # ---------- nicer names ----------
  df$term <- recode(df$term,
                    ConsumptionkWh = "Consumption",
                    OffshoreWindPower = "Offshore Wind",
                    OnshoreWindPower  = "Onshore Wind",
                    SolarPower        = "Solar Power")
  
  # ---------- coefficient table ----------
  coef_tbl <- df %>%
    select(term, model, value) %>%
    pivot_wider(names_from = model, values_from = value)
  
  se_tbl <- df %>%
    select(term, model, se_value) %>%
    pivot_wider(names_from = model, values_from = se_value)
  
  # ---------- R2 ----------
  get_r2 <- function(m) {
    s <- summary(m)$r.squared
    if (is.null(s)) return(NA)
    if ("rsq" %in% names(s)) return(s["rsq"])
    if ("within" %in% names(s)) return(s["within"])
    as.numeric(s[1])
  }
  
  r2 <- map_dbl(models, get_r2)
  
  get_adj_r2 <- function(m) {
    s <- summary(m)$r.squared
    if (!is.null(s) && "adjrsq" %in% names(s)) return(s["adjrsq"])
    NA
  }
  
  adj_r2 <- map_dbl(models, get_adj_r2)
  
  # ---------- LaTeX ----------
  cols <- model_names
  
  # ---------- SAVE PATH ----------
  output_path <- "Tables/model_results.tex"
  
  # label derived from file name
  table_label <- tools::file_path_sans_ext(basename(output_path))
  table_label <- paste0("tab:", table_label)
  
  latex <- paste0(
    "\\begin{table}[h]\n\\centering\n",
    "\\caption{", caption, "}\n",
    "\\label{", table_label, "}\n",
    "\\begin{tabular}{lccc}\n",
    "\\toprule\n",
    " & \\textbf{", paste(cols, collapse = "} & \\textbf{"), "} \\\\\n",
    "\\midrule\n\n"
  )
  
  # coefficients
  for (i in 1:nrow(coef_tbl)) {
    latex <- paste0(
      latex,
      coef_tbl$term[i], " & ",
      paste(coef_tbl[i, -1], collapse = " & "),
      " \\\\\n",
      "                   & ",
      paste(se_tbl[i, -1], collapse = " & "),
      " \\\\\n\n"
    )
  }
  
  # intercept
  # ---------- intercept (ROBUST FIX) ----------
  if (include_intercept && any(df$term == "(Intercept)")) {
    
    intercept_df <- df %>%
      filter(term == "(Intercept)") %>%
      select(term, model, value, se_value)
    
    ic_vals <- intercept_df %>%
      select(model, value) %>%
      pivot_wider(names_from = model, values_from = value)
    
    ic_se <- intercept_df %>%
      select(model, se_value) %>%
      pivot_wider(names_from = model, values_from = se_value)
    
    # ensure correct column order
    ic_vals <- ic_vals[, model_names, drop = FALSE]
    ic_se   <- ic_se[, model_names, drop = FALSE]
    
    latex <- paste0(
      latex,
      "Intercept & ",
      paste(ic_vals[1, ], collapse = " & "),
      " \\\\\n",
      "          & ",
      paste(ic_se[1, ], collapse = " & "),
      " \\\\\n\n"
    )
  }
  
  # fixed effects summary lines only
  latex <- paste0(
    latex,
    "\\midrule\n",
    "Weekday Effects & ",
    paste(weekday_effects, collapse = " & "),
    " \\\\\n",
    "Month Effects & ",
    paste(month_effects, collapse = " & "),
    " \\\\\n\n"
  )
  
  # R2
  latex <- paste0(
    latex,
    "\\midrule\n",
    "$R^2$ & ",
    paste(round(r2, 3), collapse = " & "),
    " \\\\\n",
    "Adj. $R^2$ & ",
    paste(round(adj_r2, 3), collapse = " & "),
    " \\\\\n",
    "\\bottomrule\n",
    "\\end{tabular}\n",
    "\\end{table}\n"
  )
  
  # ---------- SAVE TO FILE ----------
  output_path <- "Tables/model_results.tex"
  
  # ensure folder exists
  dir.create(dirname(output_path), showWarnings = FALSE, recursive = TRUE)
  
  writeLines(latex, con = output_path)
  
  return(invisible(output_path))
  
  
}
models <- list(
  pr_model,
  fe_model,
  re_model
)

panel_latex_table(models)

#### TEST ####
# FE vs PR → F-test 
## if p-val<0.05 => FE
pFtest(fe_model, pr_model)

# RE vs PR → Breusch-Pagan Lagrange Multiplier test
## if p-val<0.05 => RE
plmtest(pr_model, type = "bp")

# FE vs RE → Hausman test
## if p-val<0.05 => FE
phtest(fe_model, re_model)

###############
#### Kilde ####
###############
# Select variables
X <- panel_data %>%
  select(ConsumptionkWh, OffshoreWindPower, OnshoreWindPower, SolarPower) %>%
  na.omit()

# Standardize + PCA
X_scaled <- scale(X)

pca_model <- prcomp(X_scaled, center = TRUE, scale. = TRUE)
summary(pca_model)
# This gives: Variance explained and Number of relevant factors
# A common rule in factor models is to capture ~70–85% of total variance, so keeping factor 1 and 2 gives us 51%+29%=80% explained
# Using too many factors can overfit and/or make economic interpretation weaker

# Choose number of factors
plot(pca_model, type = "l")   # Scree plot
# from the plot we use 2 factors

factors <- as.data.frame(pca_model$x[, 1:2])  # choose 2 factors
colnames(factors) <- c("F1", "F2")

panel_data <- panel_data %>%
  bind_cols(factors)

panel_data <- panel_data %>%
  arrange(Hour, Date) %>%
  # group_by(Hour) %>%
  mutate(
    F1_lag1 = lag(F1, 24),
    F1_lag2 = lag(F1, 48),
    F2_lag1 = lag(F2, 24),
    F2_lag2 = lag(F2, 48)
  ) %>%
  ungroup()

pdata <- pdata.frame(panel_data, index = c("Hour", "Date"))

# Estimate Factor Model (Panel Regression) 
#Fixed Effects Model: LogPrice_it=α_i+β_1 F1_it+β_2 F2_it +ϵ_it
factor_model <- plm(
  LogPrice_asinh ~ F1 + F2,
  data = pdata,
  model = "within"
)

summary(factor_model)

# Add original variables (hybrid model)
factor_model_full <- plm(
  LogPrice_asinh ~ F1 + F2 + ConsumptionkWh +
    OffshoreWindPower + OnshoreWindPower + SolarPower,
  data = pdata,
  model = "within"
)

summary(factor_model_full)

# Create Result Table
# library(stargazer)
# stargazer(factor_model, factor_model_full,
#           type = "latex",
#           title = "Factor Model Results",
#           column.labels = c("Factor Only", "Factor + Variables"),
#           digits = 4)

# Factor Interpretation Plot
loadings <- as.data.frame(pca_model$rotation)

print(loadings)

# Plot factors over time
panel_data %>%
  ggplot(aes(x = Date, y = F1)) +
  geom_line() +
  labs(title = "Factor 1 over time")

panel_data$pred <- predict(factor_model)

ggplot(panel_data, aes(x = Date)) +
  geom_line(aes(y = LogPrice_asinh, color = "Actual")) +
  geom_line(aes(y = pred, color = "Predicted")) +
  facet_wrap(~Hour) +
  labs(title = "Actual vs Predicted Log Prices")

rmse(panel_data$LogPrice_asinh, panel_data$pred)
mae(panel_data$LogPrice_asinh, panel_data$pred)




# Estimate Dynamic Factor Model
dfm_model <- plm(
  LogPrice_asinh ~ F1 + F1_lag1 + F1_lag2 +
    F2 + F2_lag1 + F2_lag2,
  data = pdata,
  model = "within"
)

summary(dfm_model)

# full DFM
dfm_full <- plm(
  LogPrice_asinh ~ F1 + F1_lag1 + F1_lag2 +
    F2 + F2_lag1 + F2_lag2 +
    ConsumptionkWh +
    OffshoreWindPower +
    OnshoreWindPower +
    SolarPower,
  data = pdata,
  model = "within"
)

summary(dfm_full)

panel_data$pred_dfm <- predict(dfm_full)

ggplot(panel_data, aes(x = Date)) +
  geom_line(aes(y = LogPrice_asinh, color = "Actual")) +
  geom_line(aes(y = pred_dfm, color = "DFM")) +
  facet_wrap(~Hour) +
  labs(title = "Dynamic Factor Model vs Actual Prices")


panel_data$pred_static <- predict(factor_model_full)

ggplot(panel_data, aes(x = Date)) +
  geom_line(aes(y = LogPrice_asinh, color = "Actual")) +
  geom_line(aes(y = pred_static, color = "Static")) +
  geom_line(aes(y = pred_dfm, color = "Dynamic")) +
  facet_wrap(~Hour) +
  labs(title = "Model Comparison")


rmse(panel_data$LogPrice_asinh, panel_data$pred_static)
rmse(panel_data$LogPrice_asinh, panel_data$pred_dfm)



###########################
#### DYAMIC PANEL DATA ####
###########################
# Dynamic fixed effects time-series cross-section model
dyn_fe_model <- plm(
  LogPrice ~ lag(LogPrice, 1) + lag(LogPrice, 7) + lag(LogPrice, 2) + 
    ConsumptionkWh + OffshoreWindPower+ OnshoreWindPower + SolarPower + factor(Hour),
  data = pdata,
  model = "within"
)
# hvorfor factor(Hour): Electricity prices are heavily driven by predictable intraday patterns, and failing to control for them would bias both consumption and lag effects.
# hvorfor kun within: Although the inclusion of a lagged dependent variable introduces endogeneity in short panels, the bias of the fixed effects estimator decreases at rate O(1/T). Given the large time dimension in the present dataset, the bias is expected to be negligible, and the within estimator is therefore used for both static and dynamic specifications.
summary(dyn_fe_model)
# Yit=αi+0.636Yi,t−1+0.240Yi,t−7+0.032Yi,t−2−0.00144Consumptionit+εit
# very strong persistence (0.64)
# weekly cycle (lag 7)
# short-term inertia (lag 2)
# consumption slightly lowers price

# Pesaran CD test on the fixed effects model
## is the residuals from the panel model correlated across cross-sectional units
cd_test <- pcdtest(dyn_fe_model, test = "cd")

cd_test


##################
#### FORECAST ####
##################
coef_model <- coef(dyn_fe_model)
coef_model

forecast_spot_panel <- function(panel_df, h, model, avg_consumption) {
  
  panel_df <- panel_df[order(panel_df$Date, panel_df$Hour), ]
  panel_df$Date <- as.Date(panel_df$Date)
  
  coef_model <- coef(model)
  hour_effects <- fixef(model)  # IMPORTANT for factor(Hour)
  
  last_block <- panel_df[panel_df$Date == max(panel_df$Date), ]
  
  forecast <- numeric(h * 24)
  dates <- rep(NA, h * 24)
  hours <- rep(0:23, h)
  
  y_history <- panel_df$SpotPriceDKK
  
  idx <- 1
  
  for (d in 1:h) {
    
    for (hr in 1:24) {
      
      # lag1 (recursive)
      lag1 <- tail(y_history, 1)
      
      # lag2
      lag2 <- tail(y_history, 2)[1]
      
      # lag7 (weekly pattern approximation)
      lag7 <- if (length(y_history) > 7) tail(y_history, 7)[1] else mean(y_history)
      
      # consumption assumption
      cons <- avg_consumption
      
      # hour effect
      hour_eff <- hour_effects[as.character(hr)]
      
      # prediction
      y_hat <- 
        coef_model["lag(SpotPriceDKK, 1)"] * lag1 +
        coef_model["lag(SpotPriceDKK, 2)"] * lag2 +
        coef_model["lag(SpotPriceDKK, 7)"] * lag7 +
        coef_model["ConsumptionkWh"] * cons +
        hour_eff
      
      forecast[idx] <- y_hat
      
      y_history <- c(y_history, y_hat)
      
      idx <- idx + 1
    }
    
    dates[((d-1)*24+1):(d*24)] <- max(panel_df$Date) + d
  }
  
  data.frame(
    Date = dates,
    Hour = hours,
    Forecast = forecast
  )
}
spot_price_panel <- panel_data %>% 
  mutate(
    Date = as.Date(Date),
    Weekday = factor(Weekday)
  )

pdata_spotprice <- pdata.frame(spot_price_panel, index = c("Hour", "Date"))
pdata_spotprice$Date <- as.Date(as.character(pdata_spotprice$Date))

avg_consumption <- mean(pdata$ConsumptionkWh, na.rm = TRUE)

forecast_df <- forecast_spot_panel(
  pdata_spotprice,
  h = 5,
  model = dyn_fe_model,
  avg_consumption = avg_consumption
)









#####

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

dyn_model_RE <- plm(
  SpotPriceDKK ~ lag1 + lag7 + ConsumptionkWh,
  data = pdata_spotprice,
  model = "random"
)

pFtest(dyn_model_FE, dyn_model)
plmtest(dyn_model, type = "bp")
phtest(dyn_model_FE, dyn_model_RE)


# Split panels (per hour)
panels <- pdata_spotprice %>%
  as.data.frame() %>%
  group_by(Hour) %>%
  group_split()


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
            linewidth = 1) +
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


