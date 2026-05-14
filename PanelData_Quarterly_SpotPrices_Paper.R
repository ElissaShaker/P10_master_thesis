path <- setwd("~/Desktop/P10") 
source("Get_Data_Quarterly.R")
head(pdata)
names(pdata)
######################
#### FACTOR MODEL ####
######################
y_it <- pdata$LogPrice_100 # or LogPrice_asinh or LogPrice

# choose regressors
X_it = c(
  pdata$LagLogPrice_100_1,
  pdata$LagLogPrice_100_7,
  pdata$ConsumptionkWh,
  pdata$OffshoreWindPower,
  pdata$OnshoreWindPower,
  pdata$SolarPower
)

factor_proxies <- panel_data %>%
  group_by(Date) %>%
  summarise(
    y_bar  = mean(LogPrice_100),
    lag1_bar = mean(LagLogPrice_asinh_1),
    cons_bar = mean(ConsumptionkWh),
    off_bar  = mean(OffshoreWindPower),
    on_bar   = mean(OnshoreWindPower),
    sol_bar  = mean(SolarPower)
  )

panel_model <- left_join(panel_data, factor_proxies, by = "Date")

F_R <- as.matrix(factor_proxies[,-1])  # remove Date

# covariance
Sigma <- t(F_R) %*% F_R / nrow(F_R)

eig <- eigen(Sigma)$values

ER <- eig[-length(eig)] / eig[-1]

r_hat <- which.max(ER)
r_hat


pca <- prcomp(F_R, scale. = TRUE)
pca
summary(pca)

F_hat <- pca$x[, 1:r_hat]
colnames(F_hat) <- paste0("F", 1:r_hat)

factor_final <- data.frame(Date = factor_proxies$Date, F_hat)

panel_data <- left_join(panel_data, factor_final, by = "Date")


#######

model <- plm(
  LogPrice_100 ~ 
    #LagLogPrice_100_1 +
    #LagLogPrice_100_7 +
    ConsumptionkWh +
    OffshoreWindPower +
    OnshoreWindPower +
    SolarPower +
    y_bar + x1_bar + x2_bar + x3_bar + x4_bar,
  data = panel_model,
  model = "pooling"
)
summary(model)

F_matrix <- as.matrix(panel_model[, c("y_bar","x1_bar","x2_bar","x3_bar","x4_bar")])

pca <- prcomp(F_matrix, scale. = TRUE)
summary(pca)

panel_model$F1 <- pca$x[,1]
panel_model$F2 <- pca$x[,2]
panel_model$F3 <- pca$x[,3]
panel_model$F4 <- pca$x[,4]
panel_model$F5 <- pca$x[,5]


model_2 <- plm(
  LogPrice_100 ~ 
    #LagLogPrice_100_1 +
    #LagLogPrice_100_7 +
    ConsumptionkWh +
    OffshoreWindPower +
    OnshoreWindPower +
    SolarPower +
    F1 + F2 + F3,
  data = panel_model,
  model = "pooling"
)
summary(model_2)
summary(model)
