path <- setwd("~/Desktop/P10") 
source("Get_Data_Quarterly.R")
head(pdata)
##############
#### DATA ####
##############
# log variables
pdata <- pdata %>%
  mutate(
    log_consumption = log(ConsumptionkWh),
    log_offshore = log(OffshoreWindPower + 1),
    log_onshore  = log(OnshoreWindPower + 1),
    log_solar    = log(SolarPower + 1),
    lag_price_1 = lag(LogPrice, 1),
    lag_price_7 = lag(LogPrice, 7)
  )

price_matrix <- pdata %>%
  as.data.frame() %>%
  select(Date, Quarter, LogPrice) %>%
  pivot_wider(
    names_from = Quarter,
    values_from = LogPrice
  ) %>%
  arrange(Date)

price_mat <- price_matrix %>%
  select(-Date) %>%
  as.matrix()

price_mat_scaled <- scale(price_mat)

pca_price <- prcomp(price_mat_scaled)

summary(pca_price)

factors <- as.data.frame(pca_price$x[, 1:3])
colnames(factors) <- c("F1", "F2", "F3")

factor_df <- price_matrix %>%
  select(Date) %>%
  distinct() %>%
  bind_cols(factors)

pdata_model <- pdata %>%
  left_join(factor_df, by = "Date")


pdata_panel <- pdata_model %>%
  pdata.frame(index = c("Quarter", "Date"))

# model 1
factor_model <- plm(
  LogPrice ~ F1 + F2,
  data = pdata_panel,
  model = "within"
)

summary(factor_model)

# model 2
far_model <- plm(
  LogPrice ~ log_consumption + log_offshore + log_onshore + log_solar + F1 + F2+ F3,
  data = pdata_panel,
  model = "within"
)

summary(far_model)

loadings <- pca_price$rotation[, 1:3]

# plot - assuming loadings is a matrix (e.g. 96 x 3)
df <- as.data.frame(loadings)

# add quarter index
df$Quarter <- 0:(nrow(df) - 1)

# Estimate and interpret factor loadings
df_long <- df %>%
  pivot_longer(cols = -Quarter,
               names_to = "Factor",
               values_to = "Loading")

ggplot(df_long, aes(x = Quarter, y = Loading, color = Factor)) +
  geom_line() +
  labs(title = "Factor loadings across quarters",
       x = "Quarter (0–95)",
       y = "Loading") +
  theme_minimal()

# Factor 1
ggplot(data.frame(Quarter = 1:nrow(loadings),
                  Loading = loadings[,1]),
       aes(x = Quarter, y = Loading)) +
  geom_line() +
  labs(title = "Factor 1 loadings",
       x = "Quarter", y = "Loading") +
  theme_minimal() 

# Factor 2
ggplot(data.frame(Quarter = 1:nrow(loadings),
                  Loading = loadings[,2]),
       aes(x = Quarter, y = Loading)) +
  geom_line() +
  labs(title = "Factor 2 loadings",
       x = "Quarter", y = "Loading") +
  theme_minimal()

ggplot(data.frame(Quarter = 1:nrow(loadings),
                  Loading = loadings[,3]),
       aes(x = Quarter, y = Loading)) +
  geom_line() +
  labs(title = "Factor 3 loadings",
       x = "Quarter", y = "Loading") +
  theme_minimal()

# dynamic FAR model
dyn_model <- plm(
  LogPrice ~ lag_price_1+lag_price_7 + log_consumption + log_offshore + log_onshore + 
    log_solar + F1 + F2 + F3,
  data = pdata_panel,
  model = "within"
)

summary(dyn_model)



price_mat_scaled   # (T × N) matrix: days × quarters
pca_price          # prcomp result
pdata_panel        # plm panel (Quarter, Date)

################
#### BAI NG ####
################
#Choose k that minimizes an information criterion: IC(k)=log(σ^k2​)+k⋅g(N,T)
bai_ng_ic <- function(X, max_factors = 10) {
  T <- nrow(X)
  N <- ncol(X)
  
  pca <- prcomp(X)
  F_all <- pca$x
  Lambda_all <- pca$rotation
  
  IC1 <- numeric(max_factors)
  IC2 <- numeric(max_factors)
  IC3 <- numeric(max_factors)
  
  for (k in 1:max_factors) {
    F_k <- F_all[, 1:k]
    Lambda_k <- Lambda_all[, 1:k]
    
    X_hat <- F_k %*% t(Lambda_k)
    residuals <- X - X_hat
    
    sigma2 <- mean(residuals^2)
    
    # Bai & Ng penalties
    g1 <- (k * (N + T) / (N * T)) * log(N * T / (N + T))
    g2 <- (k * (N + T) / (N * T)) * log(min(N, T))
    g3 <- k * log(min(N, T)) / min(N, T)
    
    IC1[k] <- log(sigma2) + g1
    IC2[k] <- log(sigma2) + g2
    IC3[k] <- log(sigma2) + g3
  }
  
  data.frame(
    k = 1:max_factors,
    IC1 = IC1,
    IC2 = IC2,
    IC3 = IC3
  )
}

ic_results <- bai_ng_ic(price_mat_scaled, max_factors = 10)
ic_results

which.min(ic_results$IC1)
which.min(ic_results$IC2)
which.min(ic_results$IC3)

# plot IC
df_long <- ic_results %>%
  pivot_longer(cols = -k,
               names_to = "Criterion",
               values_to = "IC_value")

# plot
ggplot(df_long, aes(x = k, y = IC_value, color = Criterion)) +
  geom_line() +
  labs(title = "Bai & Ng Information Criteria",
       x = "Number of factors",
       y = "IC value") +
  theme_minimal()

####################
#### PESARAN CD ####
####################
cd_test <- pcdtest(
  LogPrice ~ log_consumption + log_offshore + log_onshore + log_solar,
  data = pdata_panel,
  test = "cd"
)

cd_test

# cd test on residuals
model_base <- plm(
  LogPrice ~ log_consumption + log_offshore + log_onshore + log_solar,
  data = pdata_panel,
  model = "within"
)

pcdtest(model_base, test = "cd")


##################
#### FORECAST ####
##################
pdata_model$F1
pdata_model$F2
pdata_model$F3

forecast_data <- pdata_model %>%
  as.data.frame() %>%
  arrange(Quarter, Date) %>%
  group_by(Quarter) %>%
  mutate(
    logprice_lead1 = dplyr::lead(LogPrice, 1),
    logprice_lag1  = dplyr::lag(LogPrice, 1)
  ) %>%
  ungroup()

forecast_data <- forecast_data %>%
  drop_na(logprice_lead1, F1, F2)

model_base <- lm(
  logprice_lead1 ~ LogPrice + log_consumption +
    log_offshore + log_onshore + log_solar,
  data = forecast_data
)
summary(model_base)

model_factor <- lm(
  logprice_lead1 ~ LogPrice +
    log_consumption + log_offshore + log_onshore + log_solar +
    F1 + F2 + F3,
  data = forecast_data
)

summary(model_factor)


forecast_data$pred_base <- predict(model_base, forecast_data)
forecast_data$pred_factor <- predict(model_factor, forecast_data)

rmse <- function(y, yhat) {
  sqrt(mean((y - yhat)^2))
}

rmse_base <- rmse(forecast_data$logprice_lead1, forecast_data$pred_base)
rmse_factor <- rmse(forecast_data$logprice_lead1, forecast_data$pred_factor)

rmse_base
rmse_factor


plot(forecast_data$logprice_lead1, type = "l", col = "black",
     main = "Forecast comparison",
     ylab = "Log Price", xlab = "Time")

lines(forecast_data$pred_base, col = "red")
lines(forecast_data$pred_factor, col = "blue")

legend("topright",
       legend = c("Actual", "Base", "Factor model"),
       col = c("black", "red", "blue"),
       lty = 1)



model_factor_ar <- lm(
  logprice_lead1 ~ LogPrice + lag(log_consumption, 1) +
    lag(log_offshore, 1) + lag(log_onshore, 1) +
    lag(log_solar, 1) + F1 + F2 + F3,
  data = forecast_data
)
