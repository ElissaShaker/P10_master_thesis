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
pr_model <- plm(SpotPriceDKK ~ ConsumptionkWh + OffshoreWindPower+ OnshoreWindPower + SolarPower + 
                  factor(Quarter) + Weekday + Month,
                data = pdata,
                model = "pooling"
)

# Fixed Effects (FE)
fe_model <- plm(SpotPriceDKK ~ ConsumptionkWh + OffshoreWindPower+ OnshoreWindPower + SolarPower + 
                  factor(Quarter), 
                # You cannot include Weekday or Month in FE if they are constant within Date (they get absorbed).
                data = pdata,
                model = "within"
)

# Random Effects (RE)
re_model <- plm(SpotPriceDKK ~ ConsumptionkWh + OffshoreWindPower+ OnshoreWindPower + SolarPower +
                  Weekday + Month,
                data = pdata,
                model = "random"
)

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
  SpotPriceDKK ~ lag(SpotPriceDKK, 1) + lag(SpotPriceDKK, 7) + lag(SpotPriceDKK, 2) + 
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
