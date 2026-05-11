path <- setwd("~/Desktop/P10") 
source("Get_Data_Hourly.R")

spot_price_panel <- panel_data %>% 
  mutate(
    Date = as.Date(Date),
    Weekday = factor(Weekday)
  )

# lag variables
pdata_spotprice <- pdata.frame(spot_price_panel, index = c("Hour", "Date"))

# Lags of SpotPrice 1, 2 and 7 days before
pdata_spotprice$lag1  <- lag(pdata_spotprice$SpotPriceDKK, 1)
pdata_spotprice$lag2  <- lag(pdata_spotprice$SpotPriceDKK, 2)
pdata_spotprice$lag7 <- lag(pdata_spotprice$SpotPriceDKK, 7) 

# Lags of Consumption 1, 2 and 7 days before
pdata_spotprice$contemp <- pdata_spotprice$ConsumptionkWh
pdata_spotprice$con_lag1 <- lag(pdata_spotprice$ConsumptionkWh, 1)
pdata_spotprice$con_lag2 <- lag(pdata_spotprice$ConsumptionkWh, 2)
pdata_spotprice$con_lag7 <- lag(pdata_spotprice$ConsumptionkWh, 7)


# dynamic FE model (baseline) 
  # Good for interpretation
  # But biased (Nickell bias)
dyn_fe <- plm(
  SpotPriceDKK ~ lag1 + lag2 + lag7 + contemp + con_lag1 + con_lag2 + con_lag7,
  data = pdata_spotprice,
  model = "within")
summary(dyn_fe)
# dynamin model (GMM)
  # Uses internal instruments (lags)
  # Corrects for:
    # endogeneity
    # dynamic bias
gmm_model <- pgmm(
  SpotPriceDKK ~ lag(SpotPriceDKK, 1:2) + lag(SpotPriceDKK, 7) +
    ConsumptionkWh + lag(ConsumptionkWh, 1) |
    lag(SpotPriceDKK, 2:4) + lag(ConsumptionkWh, 2:4),
  data = pdata_spotprice,
  effect = "individual",
  model = "twosteps",
  transformation = "d"
)
summary(gmm_model)
