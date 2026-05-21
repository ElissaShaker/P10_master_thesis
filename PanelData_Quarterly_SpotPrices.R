path <- setwd("~/Desktop/P10") 
source("Get_Data_Quarterly.R")
head(train_pdata$Quarter)
names(train_data)

y <- train_pdata$YJPrice
###########################
#### PANEL DATA STATIC ####
###########################
# Pooled Regression (PR)
pr_model <- plm(y ~ lag(y, 1) + Consumption_DK1 + OffshoreWindPower_DK1 +
      OnshoreWindPower_DK1 + SolarPower_DK1 + Weekday + Month,
    data = train_pdata,
    model = "pooling",
    index = c("Quarter", "Date"))

pr_model <- plm(
  y ~ Consumption_DK1 + OffshoreWindPower_DK1 + OnshoreWindPower_DK1 + SolarPower_DK1 + Weekday + Month,
  data = train_pdata,
  model = "pooling",
  index = c("Quarter", "Date")
)

summary(pr_model)

# Fixed Effects (FE)
fe_model <- plm(
  y ~ Consumption_DK1 + OffshoreWindPower_DK1 + OnshoreWindPower_DK1 + SolarPower_DK1 + Weekday + Month,
  data = train_pdata,
  model = "within",
  index = c("Quarter", "Date")
)

summary(fe_model)

# Random Effects (RE)
re_model <- plm(y ~ Consumption_DK1 + OffshoreWindPower_DK1 + OnshoreWindPower_DK1 + 
                  SolarPower_DK1 + Weekday + Month,
                data = train_pdata,
                model = "random",
                index = c("Quarter", "Date")
)

summary(re_model)


panel_latex_table <- function(models,
                              model_names = c("Pooled OLS", "Fixed Effects", "Random Effects"),
                              digits = 3,
                              include_intercept = TRUE,
                              weekday_effects = c("No", "No", "Yes"),
                              month_effects   = c("No", "No", "Yes"),
                              caption = "Comparison of Panel Data Models for Spot Prices (Quarterly)") {
  
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
                    Consumption_DK1  = "Consumption (DK1)",
                    OffshoreWindPower_DK1 = "Offshore Wind (DK1)",
                    OnshoreWindPower_DK1 = "Onshore Wind (DK1)",
                    SolarPower_DK1 = "Solar Power (DK1)",
                    )
  
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
  output_path <- "Tables/model_results_quarterly.tex"
  
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


######################
#### FACTOR MODEL ####
######################
df <- train_pdata %>%
  select(
    Date, Quarter,
    YJPrice,
    SpotPrice_DK2, SpotPrice_DE, SpotPrice_NO2, SpotPrice_SE3, SpotPrice_SE4, #Weekday, Month,
    Consumption_HS, Consumption_MJ, Consumption_NJ, Consumption_SJ, Consumption_SJL,
    OffshoreWindPower_DK1, OffshoreWindPower_DK2,
    OnshoreWindPower_DK1, OnshoreWindPower_DK2,
    SolarPower_DK1, SolarPower_DK2,
    ProductionLt100MW, ProductionGe100MW,
    ExchangeGermany, ExchangeNetherlands,
    ExchangeGreatBritain, ExchangeNorway, ExchangeSweden,
    BornholmSE4
  ) %>%
  na.omit()

X_raw <- df %>% select(-Date, -Quarter, -YJPrice)

# STEP 1 — PCA on X_i (choose factors)
X_scaled <- scale(X_raw)

pca_X <- prcomp(X_scaled, center = TRUE, scale. = TRUE)

plot(pca_X, type = "l")   # scree plot
summary(pca_X)

r_x <- 4
F_X <- pca_X$x[, 1:r_x, drop = FALSE]

# STEP 2 — OLS estimation of Y_i − X_i β
df_stage1 <- data.frame(
  df[, c("Date", "Quarter")],
  y = y,
  F_X
)

ols_stage1 <- lm(y ~ F_X, data = df_stage1)

df_stage1$residuals_1 <- residuals(ols_stage1)

# STEP 3 — PCA on residual structure → \widehat{F}^e
res_matrix <- reshape2::acast(
  df_stage1,
  Date ~ Quarter,
  value.var = "residuals_1"
)

pca_res <- prcomp(res_matrix, center = TRUE, scale. = TRUE)

plot(pca_res, type = "l")
summary(pca_res)

r_e <- 3
F_e <- pca_res$x[, 1:r_e, drop = FALSE]

# STEP 4 — Construct moment condition
Z <- cbind(1, F_X)
colnames(Z)[1] <- "Intercept"

# Compute residuals (aligned) 
u_hat <- df_stage1$residuals_1

moment_part <- colMeans(Z * u_hat) # 1/N ∑Z_i' u_i

# STEP 4b — Build correction term S(F^e ⊗ Id)g
# correction <- F_e
# align dimensions: map F_e into same dimension as Z moments
g_hat <- colMeans(F_e)
correction <- rep(g_hat, length.out = length(moment_part))
names(correction) <- names(moment_part)

# FINAL STEP — estimator
mu_bar <- moment_part - correction
mu_bar

#########################
#### FORECAST FACTOR ####
#########################
h <- 5
T <- nrow(res_matrix)
N <- ncol(res_matrix)

# Forecast X-factors F_X
F_X_ts <- as.data.frame(F_X)

F_X_fcast <- apply(F_X_ts, 2, function(x) {
  forecast(auto.arima(x), h = h)$mean
})

# Forecast residual factors F_e
F_e_ts <- as.data.frame(F_e)

F_e_fcast <- apply(F_e_ts, 2, function(x) {
  forecast(auto.arima(x), h = h)$mean
})

# Reconstruct forecasted residual matrix
# dimensions: 3 days × r_e
F_e_fcast
# lambda_hat <- matrix(colMeans(F_e), nrow = N, ncol = r_e, byrow = TRUE)
lambda_hat <- solve(t(F_e) %*% F_e) %*% t(F_e) %*% res_matrix

# Forecast residual component:
residual_forecast <- F_e_fcast %*% lambda_hat

# Forecast common component
beta_hat <- coef(ols_stage1)

F_X_fcast_mat <- as.matrix(F_X_fcast)

common_forecast <- F_X_fcast_mat %*% beta_hat[-1] + beta_hat[1]
common_forecast_mat <- matrix(
  common_forecast,
  nrow = h,
  ncol = N
)

final_forecast <- common_forecast_mat + residual_forecast
colnames(final_forecast) <- colnames(res_matrix)
rownames(final_forecast) <- paste0(
  "Day_", 1:h
)
final_forecast

# start of forecast horizon
start_date <- as.Date("2026-05-01")

forecast_long <- as.data.frame(final_forecast) %>%
  mutate(Day = rownames(final_forecast)) %>%
  pivot_longer(
    cols = -Day,
    names_to = "Quarter",
    values_to = "Forecast_YJ"
  ) %>%
  group_by(Day) %>%
  mutate(
    Quarter = row_number() - 1   # ← HERE
  ) %>%
  ungroup() %>%
  mutate(
    Date = as.Date(start_date + as.integer(substr(Day, 5, 5)) - 1)
  ) %>%
  select(Date, Quarter, Forecast_YJ)

# panel_data$SpotPrice_back <- car::inverseTransform(yj, panel_data$YJPrice)
# forecast_long$Forecast_Price <- inverseTransform(yj, forecast_long$Forecast_YJ)

plot_data <- panel_data %>%
  select(Date, Quarter, YJPrice, QuarterLabel) %>% 
  left_join(
    forecast_long,
    by = c("Date", "Quarter")
  ) %>% 
  filter(Date <= as.Date("2026-05-05"),
         Date >= as.Date("2026-04-01") ) %>% 
  arrange(Date, Quarter)

plot_quarter_grid <- function(data, quarters) {
  
  # filter selected quarters
  df <- data %>%
    filter(Quarter %in% quarters) %>%
    arrange(Date, Quarter)
  
  # detect forecast start date
  forecast_start <- df %>%
    filter(!is.na(Forecast_YJ)) %>%
    summarise(start_date = min(Date)) %>%
    pull(start_date)
  
  # reshape to long format
  df_long <- df %>%
    pivot_longer(
      cols = c(YJPrice, Forecast_YJ),
      names_to = "Type",
      values_to = "Value"
    ) %>%
    mutate(
      Type = dplyr::recode(Type,
                    YJPrice = "Observed",
                    Forecast_YJ = "Forecast")
    )
  
  # plot
  p <- ggplot(df_long, aes(x = Date, y = Value, color = Type)) +
    
    # vertical dashed line at forecast start
    geom_vline(
      xintercept = forecast_start,
      linetype = "dashed",
      color = "black",
      linewidth = 0.6
    ) +
    
    geom_line(linewidth = 0.4, na.rm = TRUE) +
    
    facet_wrap(~QuarterLabel, ncol = 4, nrow = 4, scales = "free_y") +
    
    scale_color_manual(
      values = c(
        "Observed" = "black",
        "Forecast" = "blue"
      )
    ) +
    
    labs(
      x = NULL,
      y = NULL,
      color = NULL
    ) +
    
    theme_minimal() +
    theme(
      strip.text = element_text(size = 10),
      legend.position = "bottom"
    )
  
  filename <- paste0(
    "plots/Quarterly/Forecast/QuarterlyForecast_Q",
    min(quarters),
    "_to_",
    max(quarters),
    ".png"
  )
  
  ggsave(
    filename,
    p,
    width = 10,
    height = 6,
    dpi = 600
  )
  
  return(p)
}

# Example
plot_quarter_grid(plot_data, quarters = 0:15) # 0-4
plot_quarter_grid(plot_data, quarters = 16:31) # 4-8
plot_quarter_grid(plot_data, quarters = 32:47) # 8-12
plot_quarter_grid(plot_data, quarters = 48:63) # 12-16 
plot_quarter_grid(plot_data, quarters = 64:79) # 16 - 20
plot_quarter_grid(plot_data, quarters = 80:95) # 20-24




# ##########################
# #### DYNAMIC FE MODEL ####
# ##########################
# names(pdata)
# # fe_dyn_model <- plm(
# #   LogPrice_100 ~ ConsumptionkWh + OffshoreWindPower +
# #     OnshoreWindPower + SolarPower + lag(LogPrice_100, 96),
# #   data = pdata,
# #   model = "within",
# #   index = c("Quarter", "Date")
# # )
# # 
# # summary(fe_dyn_model)
# 
# model_data <- pdata %>%
#   as.data.frame() %>%
#   arrange(Date, Quarter) %>%
#   group_by(Quarter) %>%   # ensure correct panel structure
#   mutate(
#     LagLogPrice_96 = dplyr::lag(LogPrice_100, 96)
#   ) %>%
#   ungroup() %>%
#   filter(!is.na(LagLogPrice_96))
# 
# fe_dyn_model <- plm(
#   LogPrice_100 ~ ConsumptionkWh +
#     OffshoreWindPower +
#     OnshoreWindPower +
#     SolarPower +
#     LagLogPrice_96,
#   data = model_data,
#   model = "within",
#   index = c("Quarter", "Date")
# )
# 
# summary(fe_dyn_model)
# # Pesaran CD test on the fixed effects model
# ## is the residuals from the panel model correlated across cross-sectional units
# pcdtest(fe_dyn_model, test = "cd")
# 
# model_data$Predicted <- as.numeric(fitted(fe_dyn_model))
# model_data$Predicted <- as.numeric(
#   model.matrix(fe_dyn_model) %*% coef(fe_dyn_model)
# )
# 
# full_plot_data <- pdata %>%
#   as.data.frame() %>%
#   arrange(Date, Quarter) %>%
#   left_join(
#     model_data %>% select(Date, Quarter, Predicted),
#     by = c("Date", "Quarter")
#   )
# 
# plot_quarter_hours <- function(data, start_hour = 12) {
#   
#   selected_quarters <- (start_hour * 4):((start_hour + 4) * 4 - 1)
# 
#   plot_data <- data %>%
#     filter(Quarter %in% selected_quarters) %>%
#     filter(!is.na(LogPrice_100), !is.na(Predicted))
#   
#   ggplot(plot_data, aes(x = Date, group = Quarter)) +
#     geom_line(aes(y = LogPrice_100), colour = "black", linewidth = 0.3) +
#     geom_line(aes(y = Predicted), colour = "red", linewidth = 0.3) +
#     
#     facet_wrap(~ QuarterLabel, ncol = 4) +
#     labs(
#       title = paste0(
#         "Quarter-hour electricity prices (hours ",
#         start_hour, "–", start_hour + 3, ")"
#       ),
#       x = NULL,
#       y = NULL
#     ) +
#     
#     theme_bw() +
#     theme(
#       strip.text = element_text(size = 10),
#       axis.text.x = element_text(size = 6),
#       axis.text.y = element_text(size = 6),
#       panel.grid = element_blank(),
#       plot.title = element_text(face = "bold")
#     )
# }
# 
# plot_quarter_hours(full_plot_data, start_hour = 12)
# 
# 
# # Create predicted values from the dynamic FE model
# pdata$Predicted <- NA
# pdata$Predicted[as.numeric(rownames(model.frame(fe_dyn_model)))] <- predict(fe_dyn_model)
# # Remove rows with missing predictions
# # (the lagged variable creates missing values at the beginning)
# plot_data <- na.omit(data.frame(
#   Date      = pdata$Date,
#   Quarter   = pdata$Quarter,
#   Observed  = pdata$LogPrice_100,
#   Predicted = pdata$Predicted
# ))
# 
# plot_subset$QuarterLabel <- factor(
#   plot_subset$Quarter,
#   levels = c(0, 24, 48, 72),
#   labels = c("00:00", "06:00", "12:00", "18:00")
# )
# 
# ggplot(plot_subset, aes(x = Date)) +
#   geom_line(aes(y = Observed, color = "Observed")) +
#   geom_line(aes(y = Predicted, color = "Predicted")) +
#   facet_wrap(~ QuarterLabel, scales = "free_y") +
#   labs(
#     title = "Observed vs Predicted Log Prices",
#     x = "Date",
#     y = "LogPrice_100",
#     color = ""
#   ) +
#   theme_minimal()
# 
# # Plot observed vs predicted
# ggplot(plot_data, aes(x = Date)) +
#   geom_line(aes(y = Observed, color = "Observed")) +
#   geom_line(aes(y = Predicted, color = "Predicted")) +
#   facet_wrap(~ Quarter, scales = "free_y") +
#   labs(
#     title = "Observed vs Predicted Log Prices by Quarter",
#     x = "Date",
#     y = "LogPrice_100",
#     color = ""
#   ) +
#   theme_minimal()
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# plot_data <- model.frame(fe_dyn_model)
# 
# plot_data$Fitted <- fitted(fe_dyn_model)
# plot_data$Residuals <- residuals(fe_dyn_model)
# plot_data$Predicted <- as.numeric(predict(fe_dyn_model))
# 
# ggplot(plot_data, aes(x = 1:nrow(plot_data))) +
#   geom_line(aes(y = LogPrice_100, color = "LogPrice_100")) +
#   geom_line(aes(y = Predicted, color = "Predicted")) +
#   labs(
#     title = "LogPrice_100 vs Predicted",
#     x = "Observation",
#     y = "Log Price"
#   ) +
#   theme_minimal()
# # -------------------------------
# # 2. Residuals vs Fitted Values
# # -------------------------------
# 
# ggplot(plot_data, aes(x = Fitted, y = Residuals)) +
#   geom_point(alpha = 0.3) +
#   geom_hline(yintercept = 0, linetype = "dashed") +
#   labs(
#     title = "Residuals vs Fitted Values",
#     x = "Fitted Values",
#     y = "Residuals"
#   ) +
#   theme_minimal()
# 
# # -------------------------------
# # 3. Histogram of Residuals
# # -------------------------------
# plot_data <- data.frame(
#   LogPrice_100 = as.numeric(plot_data$LogPrice_100),
#   Fitted       = as.numeric(fitted(fe_dyn_model)),
#   Residuals    = as.numeric(residuals(fe_dyn_model))
# )
# ggplot(plot_data, aes(x = Residuals)) +
#   geom_histogram(bins = 50) +
#   labs(
#     title = "Distribution of Residuals",
#     x = "Residuals",
#     y = "Frequency"
#   ) +
#   theme_minimal()
# 
# # -------------------------------
# # 4. Residuals Over Time
# # -------------------------------
# 
# ggplot(plot_subset, aes(x = 1:nrow(plot_subset), y = Residuals)) +
#   geom_line() +
#   geom_hline(yintercept = 0, linetype = "dashed") +
#   labs(
#     title = "Residuals Over Time",
#     x = "Time",
#     y = "Residuals"
#   ) +
#   theme_minimal()
# 
# # -------------------------------
# # 5. ACF Plot of Residuals
# # -------------------------------
# 
# acf(plot_data$Residuals,
#     main = "ACF of Residuals")
# 
# 
# ###############
# #### Kilde ####
# ###############
# # log variables
# pdata <- pdata %>%
#   mutate(
#     log_consumption = log(ConsumptionkWh),
#     log_offshore = log(OffshoreWindPower + 1),
#     log_onshore  = log(OnshoreWindPower + 1),
#     log_solar    = log(SolarPower + 1)
#   )
# 
# #removes season like paper
# pdata <- pdata %>%
#   mutate(
#     trend = row_number(),
#     cos_year = cos(2*pi*trend/365),
#     cos_week = cos(2*pi*trend/7)
#   )
# 
# pdata <- pdata %>%
#   arrange(Date, Quarter) %>%
#   mutate(
#     lag_price_1 = lag(LogPrice, 1),
#     lag_price_2 = lag(LogPrice, 2),
#     lag_price_7 = lag(LogPrice, 7)
#   )
# 
# factor_data <- pdata %>%
#   select(log_consumption, log_offshore, log_onshore, log_solar) %>%  #,
#          #lag_price_1, lag_price_2, lag_price_7) %>%
#   drop_na()
# 
# # Standardize - so all variables have mean 0 and variance 1
# X_factor <- scale(factor_data)
# 
# # Extract factors using PCA
# pca_model <- prcomp(X_factor)
# 
# # choose how many factors to include in the model
# summary(pca_model)
# 
# pca_model$rotation
# 
# factors <- as.data.frame(pca_model$x[, 1:3])
# colnames(factors) <- c("F1", "F2", "F3")#, "F4", "F5")
# 
# pdata_model <- pdata %>%
#   drop_na() %>%
#   bind_cols(factors)
# 
# far_model <- lm(
#   LogPrice ~ log_consumption + log_offshore + log_onshore + log_solar + F1 + F2 + F3,
#   data = pdata_model
# )
# 
# summary(far_model)
# 
# 
# pdata_panel <- pdata_model %>%
#   mutate(id = Quarter) %>%
#   pdata.frame(index = c("id", "Date"))
# 
# far_fe <- plm(
#   LogPrice ~ log_consumption + log_offshore + log_onshore + log_solar + F1 + F2 +F3,
#   data = pdata_panel,
#   model = "within"
# )
# 
# summary(far_fe)
# 
# #
# summary(pca_model)$importance[2,]
# # multicollinearity
# cor(pdata_model %>% select(log_consumption, log_offshore, log_onshore, log_solar))
# 
# 
# 
# ###########################
# #### DYAMIC PANEL DATA ####
# ###########################
# # Dynamic fixed effects time-series cross-section model
# dyn_fe_model <- plm(
#   LogPrice ~ lag(LogPrice, 1) + lag(LogPrice, 7) + lag(LogPrice, 2) + 
#     ConsumptionkWh + OffshoreWindPower+ OnshoreWindPower + SolarPower + Quarter,
#   data = pdata,
#   model = "within"
# )
# 
# ab_model <- pgmm(
#   LogPrice_100 ~ lag(LogPrice_100, 1:2) +
#     ConsumptionkWh + OffshoreWindPower +
#     OnshoreWindPower + SolarPower |
#     lag(LogPrice_100, 2:99),
#   data = pdata,
#   effect = "individual",
#   model = "twosteps",
#   transformation = "d"
# )
# 
# summary(ab_model)
# # hvorfor factor(Hour): Electricity prices are heavily driven by predictable intraday patterns, and failing to control for them would bias both consumption and lag effects.
# # hvorfor kun within: Although the inclusion of a lagged dependent variable introduces endogeneity in short panels, the bias of the fixed effects estimator decreases at rate O(1/T). Given the large time dimension in the present dataset, the bias is expected to be negligible, and the within estimator is therefore used for both static and dynamic specifications.
# summary(dyn_fe_model)
# # Yit=αi+0.636Yi,t−1+0.240Yi,t−7+0.032Yi,t−2−0.00144Consumptionit+εit
# # very strong persistence (0.64)
# # weekly cycle (lag 7)
# # short-term inertia (lag 2)
# # consumption slightly lowers price
# 
# # Pesaran CD test on the fixed effects model
# ## is the residuals from the panel model correlated across cross-sectional units
# cd_test <- pcdtest(dyn_fe_model, test = "cd")
# 
# cd_test
