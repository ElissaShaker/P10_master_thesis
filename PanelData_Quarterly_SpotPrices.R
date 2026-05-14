path <- setwd("~/Desktop/P10") 
source("Get_Data_Quarterly.R")
head(pdata)
###########################
#### PANEL DATA STATIC ####
###########################
# Pooled Regression (PR)
pr_model <- plm(
  LogPrice_100 ~ ConsumptionkWh + OffshoreWindPower +
    OnshoreWindPower + SolarPower,
  data = pdata,
  model = "pooling"
)

summary(pr_model)

# Fixed Effects (FE)
fe_model <- plm(
  LogPrice_100 ~ ConsumptionkWh + OffshoreWindPower +
    OnshoreWindPower + SolarPower,
  data = pdata,
  model = "within"
)

summary(fe_model)

# Random Effects (RE)
re_model <- plm(LogPrice_100 ~ ConsumptionkWh + OffshoreWindPower+ OnshoreWindPower + SolarPower +
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

###############
#### Kilde ####
###############
# log variables
pdata <- pdata %>%
  mutate(
    log_consumption = log(ConsumptionkWh),
    log_offshore = log(OffshoreWindPower + 1),
    log_onshore  = log(OnshoreWindPower + 1),
    log_solar    = log(SolarPower + 1)
  )

#removes season like paper
pdata <- pdata %>%
  mutate(
    trend = row_number(),
    cos_year = cos(2*pi*trend/365),
    cos_week = cos(2*pi*trend/7)
  )

pdata <- pdata %>%
  arrange(Date, Quarter) %>%
  mutate(
    lag_price_1 = lag(LogPrice, 1),
    lag_price_2 = lag(LogPrice, 2),
    lag_price_7 = lag(LogPrice, 7)
  )

factor_data <- pdata %>%
  select(log_consumption, log_offshore, log_onshore, log_solar) %>%  #,
         #lag_price_1, lag_price_2, lag_price_7) %>%
  drop_na()

# Standardize - so all variables have mean 0 and variance 1
X_factor <- scale(factor_data)

# Extract factors using PCA
pca_model <- prcomp(X_factor)

# choose how many factors to include in the model
summary(pca_model)

pca_model$rotation

factors <- as.data.frame(pca_model$x[, 1:3])
colnames(factors) <- c("F1", "F2", "F3")#, "F4", "F5")

pdata_model <- pdata %>%
  drop_na() %>%
  bind_cols(factors)

far_model <- lm(
  LogPrice ~ log_consumption + log_offshore + log_onshore + log_solar + F1 + F2 + F3,
  data = pdata_model
)

summary(far_model)


pdata_panel <- pdata_model %>%
  mutate(id = Quarter) %>%
  pdata.frame(index = c("id", "Date"))

far_fe <- plm(
  LogPrice ~ log_consumption + log_offshore + log_onshore + log_solar + F1 + F2 +F3,
  data = pdata_panel,
  model = "within"
)

summary(far_fe)

#
summary(pca_model)$importance[2,]
# multicollinearity
cor(pdata_model %>% select(log_consumption, log_offshore, log_onshore, log_solar))


###########################
#### PANEL DATA STATIC ####
###########################
# Pooled Regression (PR)
pr_model <- plm(LogPrice ~ ConsumptionkWh + OffshoreWindPower+ OnshoreWindPower + SolarPower + 
                  factor(Quarter) + Weekday + Month,
                data = pdata,
                model = "pooling"
)
summary(pr_model)
# Fixed Effects (FE)
fe_model <- plm(LogPrice ~ ConsumptionkWh + OffshoreWindPower+ OnshoreWindPower + SolarPower + 
                  factor(Quarter), 
                # You cannot include Weekday or Month in FE if they are constant within Date (they get absorbed).
                data = pdata,
                model = "within"
)
summary(fe_model)

# Random Effects (RE)
re_model <- plm(LogPrice ~ ConsumptionkWh + OffshoreWindPower+ OnshoreWindPower + SolarPower +
                  Weekday + Month,
                data = pdata,
                model = "random"
)
summary(re_model)

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

###########################
#### DYAMIC PANEL DATA ####
###########################
# Dynamic fixed effects time-series cross-section model
dyn_fe_model <- plm(
  LogPrice ~ lag(LogPrice, 1) + lag(LogPrice, 7) + lag(LogPrice, 2) + 
    ConsumptionkWh + OffshoreWindPower+ OnshoreWindPower + SolarPower + Quarter,
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

######################
#### FACTOR MODEL ####
######################
# residuals
model_data <- model.frame(dyn_fe_model)
model_data$res <- residuals(dyn_fe_model)
idx <- as.data.frame(index(dyn_fe_model))
colnames(idx) <- c("Date", "Quarter")

model_data <- cbind(idx, model_data)

# residual matrix
resid_matrix <- model_data %>%
  as.data.frame() %>%
  select(Date, Quarter, res) %>%
  pivot_wider(
    names_from = Quarter,
    values_from = res
  ) %>%
  arrange(Date)
