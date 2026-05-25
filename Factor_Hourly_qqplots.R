run_pca_gmm <- function(train_pdata,
                        response_var,
                        r_x = 4,
                        r_e = 2,
                        save_plots = FALSE
                        ) {

  library(dplyr)
  library(ggplot2)
  library(reshape2)

  # -----------------------------
  # STEP 0 — Prepare data
  # -----------------------------

  df <- train_pdata %>%
    select(
      Date, Hour,
      all_of(response_var),
      SpotPrice_DK2, SpotPrice_DE, SpotPrice_NO2,
      SpotPrice_SE3, SpotPrice_SE4,
      Consumption_HS, Consumption_MJ,
      Consumption_NJ, Consumption_SJ,
      Consumption_SJL,
      OffshoreWindPower_DK1,
      OffshoreWindPower_DK2,
      OnshoreWindPower_DK1,
      OnshoreWindPower_DK2,
      SolarPower_DK1,
      SolarPower_DK2,
      ProductionLt100MW,
      ProductionGe100MW,
      ExchangeGermany,
      ExchangeNetherlands,
      ExchangeGreatBritain,
      ExchangeNorway,
      ExchangeSweden,
      BornholmSE4
    ) %>%
    na.omit()

  y <- df[[response_var]]

  X_raw <- df %>%
    select(-Date, -Hour, -all_of(response_var))

  # -----------------------------
  # STEP 1 — PCA on X
  # -----------------------------

  X_scaled <- scale(X_raw)

  pca_X <- prcomp(
    X_scaled,
    center = TRUE,
    scale. = TRUE
  )

  if (save_plots) {
    png(
      paste0("scree_plot_X_", response_var, ".png"),
      width = 2000,
      height = 1400,
      res = 300
    )
  }

  plot(pca_X, type = "l")

  if (save_plots) dev.off()

  F_X <- pca_X$x[, 1:r_x, drop = FALSE]

  # -----------------------------
  # STEP 2 — OLS regression
  # -----------------------------

  df_stage1 <- data.frame(
    df[, c("Date", "Hour")],
    y = y,
    F_X
  )

  ols_stage1 <- lm(y ~ F_X, data = df_stage1)

  df_stage1$residuals_1 <- residuals(ols_stage1)

  # -----------------------------
  # STEP 3 — PCA on residuals
  # -----------------------------

  res_matrix <- reshape2::acast(
    df_stage1,
    Date ~ Hour,
    value.var = "residuals_1"
  )

  pca_res <- prcomp(
    res_matrix,
    center = TRUE,
    scale. = TRUE
  )

  if (save_plots) {
    png(
      paste0("scree_plot_residuals_", response_var, ".png"),
      width = 2000,
      height = 1400,
      res = 300
    )
  }

  plot(pca_res, type = "l")

  if (save_plots) dev.off()

  F_e <- pca_res$x[, 1:r_e, drop = FALSE]

  # -----------------------------
  # STEP 4 — Moment condition
  # -----------------------------

  Z <- cbind(Intercept = 1, F_X)
  Z <- as.matrix(Z)

  u_hat <- df_stage1$residuals_1

  moment_part <- colMeans(Z * u_hat)

  g_hat <- colMeans(F_e)

  correction <- rep(
    g_hat,
    length.out = length(moment_part)
  )

  names(correction) <- names(moment_part)

  mu_bar <- moment_part - correction

  # -----------------------------
  # STEP 5 — GMM estimation
  # -----------------------------

  gmm_moments <- function(theta, y, Z) {

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

  # -----------------------------
  # STEP 6 — Residuals
  # -----------------------------

  u_gmm <- as.numeric(y - Z %*% theta_hat)

  qqplot_gmm <- ggplot(
    data.frame(residuals = u_gmm),
    aes(sample = residuals)
  ) +
    stat_qq(color = "blue") +
    stat_qq_line(color = "red") +
    labs(
      title = paste(
        "QQ-Plot of Final GMM Residuals:",
        response_var
      )
    ) +
    theme_minimal()#base_size = 14)

  print(qqplot_gmm)

  if (save_plots) {
    ggsave(
      paste0(
        "qqplot_gmm_",
        response_var,
        ".png"
      ),
      qqplot_gmm,
      width = 10,
      height = 6,
      dpi = 300
    )
  }

  # -----------------------------
  # RETURN RESULTS
  # -----------------------------

  return(list(
    response_variable = response_var,
    data = df,
    pca_X = pca_X,
    pca_res = pca_res,
    ols_stage1 = ols_stage1,
    gmm_fit = gmm_fit,
    theta_hat = theta_hat,
    residuals_gmm = u_gmm,
    qqplot_gmm = qqplot_gmm,
    mu_bar = mu_bar,
    Z = Z,
    F_X = F_X,
    F_e = F_e
  ))
}
res_spot     <- run_pca_gmm(train_pdata, "SpotPrice_DK1")
res_log      <- run_pca_gmm(train_pdata, "LogPrice")
res_log100   <- run_pca_gmm(train_pdata, "LogPrice_100")
res_asinh    <- run_pca_gmm(train_pdata, "LogPrice_asinh")
res_yj       <- run_pca_gmm(train_pdata, "YJPrice")


p <- arrangeGrob(
  res_log$qqplot_gmm,
  res_log100$qqplot_gmm,
  res_asinh$qqplot_gmm,
  res_yj$qqplot_gmm,
  ncol = 2
)

ggsave(
  "qqplot_gmm.png",
  p,
  width = 10,
  height = 6,
  dpi = 300
)
