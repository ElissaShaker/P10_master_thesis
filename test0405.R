path <- setwd("~/Desktop/P10") 
source("Get_Data_Hourly.R")
head(pdata)


# Create lags (important: sort first!)
pdata <- pdata %>%
  arrange(Date, Hour) %>%
  group_by(Hour) %>%
  mutate(
    lag1_LogPrice = lag(LogPrice, 1),
    lag7_LogPrice = lag(LogPrice, 7)
  ) %>%
  ungroup() %>%
  na.omit()


# Wide format: rows = Date, columns = Hour
Y_mat <- pdata %>%
  select(Date, Hour, LogPrice) %>%
  pivot_wider(names_from = Hour, values_from = LogPrice) %>%
  arrange(Date) %>%
  drop_na()   # 🔥 removes rows with any NA

# Remove Date column
Y <- as.matrix(Y_mat[,-1])

# PCA
pca_model <- prcomp(Y, scale. = TRUE)

# Choose number of factors (e.g. 2–3 usually works)
r <- 2
F_hat <- pca_model$x[, 1:r]   # factors

factor_df <- data.frame(Date = Y_mat$Date, F_hat)

pdata <- pdata %>%
  left_join(factor_df, by = "Date")

model <- lm(
  LogPrice ~ lag1_LogPrice + lag7_LogPrice +
    ConsumptionkWh + OnshoreWindPower + OffshoreWindPower + SolarPower +
    PC1 + PC2,
  data = pdata
)

summary(model)

model_fe <- lm(
  LogPrice ~ lag1_LogPrice + lag7_LogPrice +
    ConsumptionkWh + OnshoreWindPower + OffshoreWindPower + SolarPower +
    PC1 + PC2 + factor(Hour),
  data = pdata
)


pdata$pred_log <- predict(model, newdata = pdata)

pdata$pred_price <- exp(pdata$pred_log) - (abs(min(pdata$SpotPriceDKK, na.rm = TRUE)) + 1)



#############################################

library(dplyr)
library(tidyr)

# Fix shift once
shift <- abs(min(pdata$SpotPriceDKK, na.rm = TRUE)) + 1

pdata <- pdata %>%
  filter(is.finite(LogPrice)) %>%
  arrange(Date, Hour)

# Balanced panel for factor extraction
Y_mat <- pdata %>%
  select(Date, Hour, LogPrice) %>%
  pivot_wider(names_from = Hour, values_from = LogPrice) %>%
  arrange(Date) %>%
  drop_na()

Y <- as.matrix(Y_mat[,-1])
