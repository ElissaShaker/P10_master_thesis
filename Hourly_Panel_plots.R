path <- setwd("C:/Users/P/OneDrive - Aalborg Universitet/Skrivebord/P10")
source("Get_Data_Hourly.R")

head(train_pdata$Hour)
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
    index = c("Hour", "Date")
  )

  # Fixed Effects (FE)
  fe_model <- plm(
    static_formula,
    data  = data,
    model = "within",
    index = c("Hour", "Date")
  )

  # Random Effects (RE)
  re_model <- plm(
    re_formula,
    data  = data,
    model = "random",
    index = c("Hour", "Date")
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
      filename = paste0("hourly_QQplot_models_", name, ".png"),
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
      Hour = data$Hour,
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
    label = "tab:hourly_residual_skew_kurt_panel"
  ) %>%
  kable_styling(latex_options = "striped") %>%
  as.character()

writeLines(tex_residual_stats,
           "Hourly_residual_skew_kurt_panel.tex")

####################
#### PANEL DATA ####
####################
y <- train_pdata$LogPrice
# Pooled Regression (PR)
pr_model <- plm(
  y ~ Consumption_DK1 + OffshoreWindPower_DK1 + OnshoreWindPower_DK1 + SolarPower_DK1,
  data = train_pdata,
  model = "pooling",
  index = c("Hour", "Date")
)

summary(pr_model)

# Fixed Effects (FE)
fe_model <- plm(
  y ~ Consumption_DK1 + OffshoreWindPower_DK1 + OnshoreWindPower_DK1 + SolarPower_DK1,
  data = train_pdata,
  model = "within",
  index = c("Hour", "Date")
)

summary(fe_model)

# Random Effects (RE)
re_model <- plm(y ~ Consumption_DK1 + OffshoreWindPower_DK1 + OnshoreWindPower_DK1 +
                  SolarPower_DK1 + Weekday + Month,
                data = train_pdata,
                model = "random",
                index = c("Hour", "Date")
)

summary(re_model)


panel_latex_table <- function(models,
                              model_names = c("Pooled OLS", "Fixed Effects", "Random Effects"),
                              digits = 3,
                              include_intercept = TRUE,
                              weekday_effects = c("No", "No", "Yes"),
                              month_effects   = c("No", "No", "Yes"),
                              caption = "Comparison of Panel Data Models for Spot Prices (Hourly)") {

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
  output_path <- "model_results_hourly.tex"

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

