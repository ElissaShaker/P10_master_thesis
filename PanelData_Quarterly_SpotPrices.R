path <- setwd("~/Desktop/P10") 
source("Get_Data_Quarterly.R")
head(train_pdata$Quarter)
names(train_data)
###########################
#### PANEL DATA STATIC ####
###########################
# choose which transformation is best based on qqplot on the residuals
run_panel_models <- function(y_var, data) {
  # Formulas
  static_formula <- as.formula(
    paste(
      y_var,
      "~ Consumption_DK1 + OffshoreWindPower_DK1 +",
      "OnshoreWindPower_DK1 + SolarPower_DK1"
    )
  )
  
  re_formula <- as.formula(
    paste(
      y_var,
      "~ Consumption_DK1 + OffshoreWindPower_DK1 +",
      "OnshoreWindPower_DK1 + SolarPower_DK1 + Weekday + Month"
    )
  )
  
  # Pooled Regression (PR)
  pr_model <- plm(
    static_formula,
    data  = data,
    model = "pooling",
    index = c("Quarter", "Date")
  )
  
  # Fixed Effects (FE)
  fe_model <- plm(
    static_formula,
    data  = data,
    model = "within",
    index = c("Quarter", "Date")
  )
  
  # Random Effects (RE)
  re_model <- plm(
    re_formula,
    data  = data,
    model = "random",
    index = c("Quarter", "Date")
  )
  
  # Residuals
  pr_residuals <- residuals(pr_model)
  fe_residuals <- residuals(fe_model)
  re_residuals <- residuals(re_model)
  
  # Output
  results <- list(
    y_variable = y_var,
    # Models
    pooled_model = pr_model,
    fixed_effects_model = fe_model,
    random_effects_model = re_model,
    # Summaries
    pooled_summary = summary(pr_model),
    fixed_effects_summary = summary(fe_model),
    random_effects_summary = summary(re_model),
    # Residuals
    pooled_residuals = pr_residuals,
    fixed_effects_residuals = fe_residuals,
    random_effects_residuals = re_residuals
  )
  return(results)
}

results_spot <- run_panel_models("SpotPrice_DK1", train_pdata)
results_log <- run_panel_models("LogPrice", train_pdata)
results_log100 <- run_panel_models("LogPrice_100", train_pdata)
results_asinh <- run_panel_models("LogPrice_asinh", train_pdata)
results_yj <- run_panel_models("YJPrice", train_pdata)

plot_model_qq <- function(results_obj, y_name) {
  
  p1 <- ggplot(
    data.frame(residuals = results_obj$pooled_residuals),
    aes(sample = residuals)
  ) +
    stat_qq(color = "blue") +
    stat_qq_line(color = "red") +
    labs(title = paste(y_name, "- Pooled OLS")) +
    theme_minimal(base_size = 14)
  
  p2 <- ggplot(
    data.frame(residuals = results_obj$fixed_effects_residuals),
    aes(sample = residuals)
  ) +
    stat_qq(color = "blue") +
    stat_qq_line(color = "red") +
    labs(title = paste(y_name, "- Fixed Effects")) +
    theme_minimal(base_size = 14)
  
  p3 <- ggplot(
    data.frame(residuals = results_obj$random_effects_residuals),
    aes(sample = residuals)
  ) +
    stat_qq(color = "blue") +
    stat_qq_line(color = "red") +
    labs(title = paste(y_name, "- Random Effects")) +
    theme_minimal(base_size = 14)
  
  # Combine into one row
  (p1 | p2 | p3)
}

results_list <- list(
  SpotPrice_DK1 = results_spot,
  LogPrice = results_log,
  LogPrice_100 = results_log100,
  LogPrice_asinh = results_asinh,
  YJPrice = results_yj
)

qq_plots <- lapply(
  names(results_list),
  function(name) {
    p <- plot_model_qq(results_list[[name]], name)
    
    ggsave(
      filename = paste0("plots/Quarterly/QQplot_models_", name, ".png"),
      plot = p,
      width = 15,
      height = 5,
      dpi = 300
    )
    
    p  }
)

qq_plots[[1]]
qq_plots[[2]]
qq_plots[[3]]
qq_plots[[4]]
qq_plots[[5]]




all_results <- list(
  SpotPrice_DK1 = results_spot,
  LogPrice = results_log,
  LogPrice_100 = results_log100,
  LogPrice_asinh = results_asinh,
  YJPrice = results_yj
)

extract_residuals_df <- function(results_list, data) {
  
  bind_rows(lapply(names(results_list), function(name) {
    
    res <- results_list[[name]]
    
    tibble(
      Quarter = data$Quarter,
      Date = data$Date,
      Transformation = name,
      
      PR_res = as.numeric(res$pooled_residuals),
      FE_res = as.numeric(res$fixed_effects_residuals),
      RE_res = as.numeric(res$random_effects_residuals)
    )
  }))
}
residual_panel <- extract_residuals_df(all_results, train_pdata)


residual_long <- residual_panel %>%
  pivot_longer(
    cols = c(PR_res, FE_res, RE_res),
    names_to = "Model",
    values_to = "Residual"
  )


residual_long <- residual_long %>%
  mutate(
    Transformation = factor(
      Transformation,
      levels = c(
        "SpotPrice_DK1",
        "LogPrice",
        "LogPrice_100",
        "LogPrice_asinh",
        "YJPrice"
      )
    )
  )

residual_stats <- residual_long %>%
  group_by(Transformation, Model) %>%
  summarise(
    Skewness = e1071::skewness(Residual, na.rm = TRUE),
    Kurtosis = e1071::kurtosis(Residual, na.rm = TRUE),
    .groups = "drop"
  )

tex_residual_stats <- residual_stats %>%
  kable(
    format = "latex",
    booktabs = TRUE,
    digits = 3,
    caption = "Skewness and Kurtosis of Residuals (Panel Models)",
    label = "tab:quarterly_residual_skew_kurt_panel"
  ) %>%
  kable_styling(latex_options = "striped") %>%
  as.character()

writeLines(tex_residual_stats,
           "Tables/quarterly_residual_skew_kurt_panel.tex")

####################
#### PANEL DATA ####
####################
y <- train_pdata$LogPrice_100
# Pooled Regression (PR)
pr_model <- plm(
  y ~ Consumption_DK1 + OffshoreWindPower_DK1 + OnshoreWindPower_DK1 + SolarPower_DK1,
  data = train_pdata,
  model = "pooling",
  index = c("Quarter", "Date")
)

summary(pr_model)

# Fixed Effects (FE)
fe_model <- plm(
  y ~ Consumption_DK1 + OffshoreWindPower_DK1 + OnshoreWindPower_DK1 + SolarPower_DK1,
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
  df$term <- dplyr::recode(df$term,
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
  
  return((output_path))
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

train_pdata$LogPrice_100
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

# 
# 
# 
# 
# plot_data_ts <- plot_data %>%
#   mutate(
#     DateTime = as.POSIXct(Date) + Quarter * 15 * 60
#   ) %>%
#   arrange(DateTime)
# 
# p_forecast <-  ggplot(plot_data_ts, aes(x = DateTime)) +
#   geom_line(
#     aes(y = SpotPrice_DK1, color = "Observed"),
#     linewidth = 0.8
#   ) +
#   geom_line(
#     aes(y = Forecast_Price, color = "Forecast"),
#     linewidth = 0.8
#   ) +
#   labs(
#     x = "Time",
#     y = "Price",
#     color = "",
#     title = "Observed vs Forecasted Spot Prices"
#   ) +
#   theme_minimal()
# 


# =========================================================
# 1. Quarter lookup table
# =========================================================

quarter_lookup <- train_data %>%
  distinct(Quarter, Hour, Minute)

# =========================================================
# 2. Add Hour/Minute to forecasts
# =========================================================

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

# =========================================================
# 3. Historical data
# =========================================================

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

# =========================================================
# 4. Actual realized future prices
# =========================================================

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

# =========================================================
# 5. Connect forecast to historical series
# =========================================================

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

# =========================================================
# 6. Plot
# =========================================================

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

ggsave(
  filename = "plots/Quarterly/Forecast/forecast_all.png",
  plot = p_forecast,
  width = 12,
  height = 6,
  dpi = 300
)
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
