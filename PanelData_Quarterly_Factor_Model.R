path <- setwd("~/Desktop/P10") 
source("Get_Data_Quarterly.R")
head(train_pdata$Quarter)
names(train_data)

######################
#### FACTOR MODEL ####
######################
df <- train_pdata %>%
  select(
    Date, Quarter,
    LogPrice_100,
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
y <- df$LogPrice_100
X_raw <- df %>% select(-Date, -Quarter, -LogPrice_100)

# cor_matrix <- cor(X_raw)
# 
# round(cor_matrix, 2)
# 
# cor_matrix <- cor(X_raw)
# 
# png("Plots/Quarterly/correlation_plot.png", width = 2000, height = 2000, res = 300)
# 
# corrplot(
#   cor_matrix,
#   method = "color",
#   type = "upper",
#   # order = "hclust",
#   addCoef.col = "black",
#   number.cex = 0.4,
#   tl.cex = 0.5,
#   tl.col = "black",
#   tl.srt = 45,
#   col = colorRampPalette(c("#B2182B", "white", "#2166AC"))(200),
#   diag = FALSE
# )
# dev.off()

# STEP 1 — PCA on X_i (choose factors)
X_scaled <- scale(X_raw)

pca_X <- prcomp(X_scaled, center = TRUE, scale. = TRUE)

# png("Plots/Quarterly/scree_plot_X.png", width = 2000, height = 1400, res = 300)
plot(pca_X, type = "l")   # scree plot
# dev.off()
# 
# pca_sum <- summary(pca_X)
# pca_sum
# pca_table <- data.frame(
#   # PC = paste0("PC", 1:6),
#   Std_Dev = pca_sum$importance["Standard deviation", 1:6],
#   Prop_Var = pca_sum$importance["Proportion of Variance", 1:6],
#   Cum_Prop = pca_sum$importance["Cumulative Proportion", 1:6]
# )
# 
# 
# latex_code <- kable(
#   pca_table,
#   format = "latex",
#   booktabs = TRUE,
#   caption = "PCA Summary on X (First 6 Principal Components)",
#   label = "tab:quarterly_pca_table_X"
# )
# 
# writeLines(latex_code, "Tables/quarterly_pca_table_X.tex")


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

# png("Plots/Quarterly/scree_plot_residuals.png", width = 2000, height = 1400, res = 300)
plot(pca_res, type = "l")
# dev.off()
# 
# pca_res_sum <- summary(pca_res)
# 
# pca_table <- data.frame(
#   # PC = paste0("PC", 1:6),
#   Std_Dev = pca_res_sum$importance["Standard deviation", 1:6],
#   Prop_Var = pca_res_sum$importance["Proportion of Variance", 1:6],
#   Cum_Prop = pca_res_sum$importance["Cumulative Proportion", 1:6]
# )
# 
# 
# latex_code <- kable(
#   pca_table,
#   format = "latex",
#   booktabs = TRUE,
#   caption = "PCA Summary on the residuals (First 6 Principal Components)",
#   label = "tab:quarterly_pca_table_res"
# )
# 
# writeLines(latex_code, "Tables/quarterly_pca_table_res.tex")


r_e <- 2
F_e <- pca_res$x[, 1:r_e, drop = FALSE]

# STEP 4 — Construct moment condition
Z <- cbind(Intercept = 1, F_X)
Z <- as.matrix(Z)

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


gmm_moments <- function(theta, y, Z) {
  
  # Z = (Intercept, F_X)
  # theta = (alpha, beta_1, ..., beta_r)
  
  u <- y - Z %*% theta
  
  g <- colMeans(Z * as.numeric(u))
  
  return(g)
}

gmm_objective <- function(theta, y, Z, W) {
  
  g <- gmm_moments(theta, y, Z)
  
  as.numeric(t(g) %*% W %*% g)
}

init <- coef(lm(y ~ F_X))
init <- as.numeric(init)

names(init) <- colnames(Z)

W <- diag(ncol(Z))

gmm_fit <- optim(
  par = init,
  fn = gmm_objective,
  y = y,
  Z = Z,
  W = W,
  method = "BFGS"
)
theta_hat <- gmm_fit$par
theta_hat


#########################
#### FORECAST FACTOR ####
#########################
h <- 5
T <- nrow(res_matrix)
N <- ncol(res_matrix)

# Forecast X-factors F_X
F_X_ts <- as.data.frame(F_X)

F_X_fcast <- sapply(F_X_ts, function(x) {
  forecast(auto.arima(x), h = h)$mean
})

F_X_fcast <- as.matrix(F_X_fcast)   # h × r_x

# Forecast residual factors F_e
F_e_ts <- as.data.frame(F_e)

F_e_fcast <- sapply(F_e_ts, function(x) {
  forecast(auto.arima(x), h = h)$mean
})

F_e_fcast <- as.matrix(F_e_fcast)   # h × r_e

# lambda_hat <- matrix(colMeans(F_e), nrow = N, ncol = r_e, byrow = TRUE)
lambda_hat <- solve(t(F_e) %*% F_e) %*% t(F_e) %*% res_matrix
lambda_hat <- as.matrix(lambda_hat)   # r_e × N

beta_lambda <- lambda_hat

beta_hat <- coef(ols_stage1)
beta_hat <- as.numeric(beta_hat)

F_X_fcast_mat <- as.matrix(F_X_fcast)

common_forecast <- F_X_fcast_mat %*% beta_hat[-1] + beta_hat[1]
common_forecast_mat <- matrix(common_forecast, nrow = h, ncol = N)

# Forecast residual component:
residual_forecast <- F_e_fcast %*% lambda_hat

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

# forecast_long$Forecast_Price <- predict(yj, newdata = forecast_long$Forecast_YJ, 
#                                         inverse = TRUE)

forecast_long$Forecast_Price <- exp(forecast_long$Forecast_YJ) -
  (abs(min(train_data$SpotPrice_DK1, na.rm = TRUE)) + 100)

plot_data <- panel_data %>%
  select(Date, Quarter, SpotPrice_DK1, QuarterLabel) %>% 
  left_join(
    forecast_long %>% select(-Forecast_YJ),
    by = c("Date", "Quarter")
  ) %>% 
  filter(Date <= as.Date("2026-05-05"),
         Date >= as.Date("2026-04-20") ) %>% 
  arrange(Date, Quarter)

plot_quarter_grid <- function(data, quarters, save = FALSE) {
  
  # filter selected quarters
  df <- data %>%
    filter(Quarter %in% quarters) %>%
    arrange(Date, Quarter)
  
  # detect forecast start date
  forecast_start <- df %>%
    filter(!is.na(Forecast_Price)) %>%
    summarise(start_date = min(Date)) %>%
    pull(start_date)
  
  # reshape to long format
  df_long <- df %>%
    pivot_longer(
      cols = c(SpotPrice_DK1, Forecast_Price),
      names_to = "Type",
      values_to = "Value"
    ) %>%
    mutate(
      Type = dplyr::recode(Type,
                           SpotPrice_DK1 = "Observed",
                           Forecast_Price = "Forecast")
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
  
  if (save) {
    ggsave(
      filename = filename,
      plot = p,
      width = 10,
      height = 6,
      dpi = 600
    )
  }  
  return(p)
}

# Example
plot_quarter_grid(plot_data, quarters = 0:15) # 0-4
plot_quarter_grid(plot_data, quarters = 16:31) # 4-8
plot_quarter_grid(plot_data, quarters = 32:47) # 8-12
plot_quarter_grid(plot_data, quarters = 48:63) # 12-16 
plot_quarter_grid(plot_data, quarters = 64:79) # 16-20
plot_quarter_grid(plot_data, quarters = 80:95) # 20-24

quarter_lookup <- train_data %>%
  distinct(Quarter, Hour, Minute)

forecast_plot <- forecast_long %>%
  
  left_join(
    quarter_lookup,
    by = "Quarter"
  ) %>%
  
  mutate(
    DateTime = as.POSIXct(
      paste(Date, sprintf("%02d:%02d", Hour, Minute))
    )
  ) %>%
  
  arrange(DateTime)

history_plot <- panel_data %>%
  
  select(
    Date,
    Quarter,
    Hour,
    Minute,
    SpotPrice_DK1
  ) %>%
  
  filter(
    Date >= as.Date("2026-04-20"),
    Date <= as.Date("2026-05-01")
  ) %>%
  
  mutate(
    DateTime = as.POSIXct(
      paste(Date, sprintf("%02d:%02d", Hour, Minute))
    )
  ) %>%
  
  arrange(DateTime)

actual_plot <- test_data %>%
  
  filter(
    Date >= min(forecast_long$Date),
    Date <= max(forecast_long$Date)
  ) %>%
  
  select(
    Date,
    Quarter,
    Hour,
    Minute,
    SpotPrice_DK1
  ) %>%
  
  mutate(
    DateTime = as.POSIXct(
      paste(Date, sprintf("%02d:%02d", Hour, Minute))
    )
  ) %>%
  
  arrange(DateTime)

last_hist_point <- history_plot %>%
  
  slice_tail(n = 1) %>%
  
  transmute(
    DateTime,
    Forecast_Price = SpotPrice_DK1
  )

forecast_plot_connected <- bind_rows(
  
  last_hist_point,
  
  forecast_plot %>%
    select(DateTime, Forecast_Price)
)

actual_plot_connected <- bind_rows(
  
  last_hist_point %>%
    transmute(
      DateTime,
      SpotPrice_DK1 = Forecast_Price
    ),
  
  actual_plot %>%
    select(DateTime, SpotPrice_DK1)
)

p_forecast <- ggplot() +
  
  # Historical
  geom_line(
    data = history_plot,
    aes(DateTime, SpotPrice_DK1, color = "Historical"),
    linewidth = 0.35
  ) +
  
  # Forecast
  geom_line(
    data = forecast_plot_connected,
    aes(DateTime, Forecast_Price, color = "Forecast"),
    linewidth = 0.7
  ) +
  
  # Actual realized future
  geom_line(
    data = actual_plot_connected,
    aes(DateTime, SpotPrice_DK1, color = "Actual"),
    linewidth = 0.5,
    linetype = "22"
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
  
  labs(
    title = "Factor Model Forecast vs Actual",
    x = "Date",
    y = "Spot Price",
    color = ""
  ) +
  
  theme_minimal() +
  
  theme(
    legend.position = "right",
    
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  )
p_forecast
ggsave(
  filename = "plots/Quarterly/Forecast/forecast_all.png",
  plot = p_forecast,
  width = 12,
  height = 6,
  dpi = 300
)
