start_time <- Sys.time()
library(httr)
library(jsonlite)
library(dplyr)
library(tidyr)
library(lubridate)
library(plm)
library(ggplot2)
library(e1071)
library(knitr)
library(kableExtra)
library(Metrics)
library(xtable)
library(modelsummary)
library(moments)
library(broom)
library(purrr)
library(stringr)

setwd("~/Desktop/P10")
########################
#### GET SPOT PRICE ####
########################
# function til at hente spotpriser i DK1
get_elspot <- function(base, start = NULL, end = NULL, pricearea = "DK1") {
  query <- list()
  
  if (!is.null(start)) query$start <- start
  if (!is.null(end))   query$end   <- end
  
  if (!is.null(pricearea)) {
    query$filter <- sprintf('{"PriceArea":"%s"}', pricearea)
  }
  
  res <- GET(base, query = query)
  stop_for_status(res)
  
  data <- content(res, as = "text", encoding = "UTF-8")
  parsed <- fromJSON(data, flatten = TRUE)
  
  return(as_tibble(parsed$records))
}

# Today
today_end <- Sys.Date()

# One year ago
one_year_ago <- today_end - years(2)

# Format as "YYYY-MM-DDT00:00"
today_end_str <- paste0(format(today_end, "%Y-%m-%d"), "T00:00")
one_year_ago_str <- paste0(format(one_year_ago, "%Y-%m-%d"), "T00:00")

today_end_str
one_year_ago_str
#####
# #spot price util 2025-10-01
# spotprice1 <- get_elspot("https://api.energidataservice.dk/dataset/Elspotprices", 
#                          start = one_year_ago_str, #"2025-02-22T00:00",
#                          end   = "2025-10-01T00:00"
# )
# 
# # Ensure HourDK column are datetime
# spotprice1 <- spotprice1 %>%
#   mutate(HourDK = ymd_hms(HourDK))
# 
# # make spotprice1 to quaterly data 
# # create 15-min intervals for each hour
# spotprice1_quarter <- spotprice1 %>%
#   # for each row, create 4 rows
#   slice(rep(1:n(), each = 4)) %>%
#   group_by(HourDK) %>%
#   mutate(
#     # offset minutes by 0, 15, 30, 45
#     HourDK = HourDK + minutes(rep(c(0, 15, 30, 45), times = n()/4))
#   ) %>%
#   ungroup()

#####
oct1_start <- "2025-10-01T00:00"
oct1_start
#spot price from 2025-10-01
spotprice2 <- get_elspot("https://api.energidataservice.dk/dataset/DayAheadPrices", 
                         start = oct1_start,
                         end   = today_end_str 
) %>% 
  rename(HourUTC = TimeUTC,
         HourDK = TimeDK,
         SpotPriceDKK = DayAheadPriceDKK#, SpotPriceEUR = DayAheadPriceEUR
         )

# Ensure HourDK column are datetime
spotprice2 <- spotprice2 %>%
  mutate(HourDK = ymd_hms(HourDK))

# Combine datasets
spotprice_all <- spotprice2 #bind_rows(spotprice1_quarter, spotprice2) %>% arrange(HourDK)

# quartely panel, each quarter should be a separate “group”:
spotprice_panel <- spotprice_all %>%
  mutate(
    Date = as_date(HourDK),
    Hour = hour(HourDK),
    Minute = minute(HourDK),
    Quarter = Hour * 4 + Minute %/% 15,   # 0–95
    Weekday = wday(Date, label = TRUE, week_start = 1),
    Month = month(Date, label = TRUE, abbr = TRUE)
  ) %>%
  group_by(Date, Quarter) %>%
  summarise(
    SpotPriceDKK = mean(SpotPriceDKK, na.rm = TRUE),
    #SpotPriceEUR = mean(SpotPriceEUR, na.rm = TRUE),
    Weekday = first(Weekday),
    Month = first(Month),
    .groups = "drop"
  )

# dubletter eller missing (vinter til sommertid)
spotprice_panel %>%
  group_by(Date) %>%
  summarise(n_quarters = n()) %>%
  filter(n_quarters != 96)

spotprice_panel <- spotprice_panel %>%
  group_by(Date) %>%
  complete(Quarter = 0:95) %>%
  arrange(Date, Quarter) %>%
  fill(Weekday, Month, .direction = "downup") %>%
  ungroup()

spotprice_panel <- spotprice_panel %>%
  group_by(Date) %>%
  arrange(Quarter) %>%
  fill(SpotPriceDKK, .direction = "down") %>%
  ungroup()

# log spotprice
spotprice_panel <- spotprice_panel %>%
  mutate(
    Hour = Quarter %/% 4,
    Minute = (Quarter %% 4) * 15,
    QuarterLabel = sprintf("%02d:%02d", Hour, Minute),
    Period = if_else(Hour >= 8 & Hour < 16,
                     "Working hours",
                     "Non-working hours")
  )

spotprice_panel <- spotprice_panel %>%
  arrange(Hour, Date) %>%
  group_by(Hour) %>%
  mutate(
    LogPrice = log(SpotPriceDKK + abs(min(SpotPriceDKK, na.rm = TRUE)) + 1),
    LogPrice_100 = log(SpotPriceDKK + abs(min(SpotPriceDKK, na.rm = TRUE)) + 100),
    LogPrice_asinh = log(SpotPriceDKK + sqrt(SpotPriceDKK^2 + 1)),
    
    LagLogPrice_1 = lag(LogPrice, 1),
    LagLogPrice_7 = lag(LogPrice, 7),
    
    LagLogPrice_100_1 = lag(LogPrice_100, 1),
    LagLogPrice_100_7 = lag(LogPrice_100, 7),
    
    
    LagLogPrice_asinh_1 = lag(LogPrice_asinh, 1),
    LagLogPrice_asinh_7 = lag(LogPrice_asinh, 7)
  ) %>%
  ungroup()

head(spotprice_panel)
names(spotprice_panel)
####################################
#### SPOT PRICE ANALYSIS CHPT.2 ####
###################################
hour <- 10

spotprice_subset <- spotprice_panel %>%
  filter(Quarter %in% (hour*4):(hour*4 + 3),
         Date >= as.Date("2025-10-01")) %>%
  mutate(
    Quarter_in_hour = Quarter - hour*4,
    Quarter_label = case_when(
      Quarter_in_hour == 0 ~ paste0(sprintf("%02d", hour), ":00-", sprintf("%02d", hour), ":15"),
      Quarter_in_hour == 1 ~ paste0(sprintf("%02d", hour), ":15-", sprintf("%02d", hour), ":30"),
      Quarter_in_hour == 2 ~ paste0(sprintf("%02d", hour), ":30-", sprintf("%02d", hour), ":45"),
      Quarter_in_hour == 3 ~ paste0(sprintf("%02d", hour), ":45-", sprintf("%02d", hour + 1), ":00")
    )
  )

spotprice_subset <- spotprice_subset %>%
  mutate(Quarter_label = factor(Quarter_label))

ggplot(spotprice_subset, aes(x = Date, y = SpotPriceDKK)) +
  geom_line(color = "steelblue") +
  facet_wrap(~ Quarter_label, ncol = 1, scales = "free_y") +
  labs(
    title = paste("Spot Prices at", hour,"(4 quarters per day)"),
    x = "",
    y = "Spot Price (DKK)"
  ) +
  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%b\n%Y"
  ) +
  theme_minimal()
# ggsave("plots/Quarterly/spotprice_querterly_Time10.png", width = 10, height = 6, dpi = 300)

ggplot(spotprice_subset, aes(x = Date, y = SpotPriceDKK, color = Quarter_label)) +
  geom_line(linewidth = 0.5) +
  # scale_color_manual(values = c(
  #   "0" = "#d62728",
  #   "1" = "#ff7f0e",
  #   "2" = "#2ca02c",
  #   "3" = "#1f77b4"
  # )) +
  labs(
    title = paste("Spot Prices at", hour, "(4 quarters per day)"),
    # x = "Date",
    y = "Spot Price (DKK)",
    color = "Quarter"
  ) +
  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%b\n%Y"
  ) +
  theme_minimal(base_size = 20) 
#ggsave("plots/Quarterly/spotprice_querterly_Time10_oneplot.png", width = 10, height = 6, dpi = 300)


# Plot: one line per weekday
avg_quarterly <- spotprice_panel %>%
  group_by(Weekday, Quarter) %>%
  summarise(
    AvgPriceDKK = mean(SpotPriceDKK, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(avg_quarterly, aes(x = Quarter, y = AvgPriceDKK, color = Weekday)) +
  geom_line(linewidth = 1) +
  scale_x_continuous(breaks = seq(0, 95, by = 4)) +
  labs(
    title = "Average Quarter-Hour Spot Price by Weekday",
    x = "15 minutes",
    y = "Average Spot Price (DKK)",
    color = "Weekday"
  ) +
  theme_minimal(base_size = 20)  # <- key change
#ggsave("plots/Quarterly/spotprice_avg_quarterly_weekday.png", width = 10, height = 6, dpi = 600)


# Plot: one line per month
avg_quarterly_month <- spotprice_panel %>%
  group_by(Month, Quarter) %>%
  summarise(
    AvgPriceDKK = mean(SpotPriceDKK, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(avg_quarterly_month, aes(x = Quarter, y = AvgPriceDKK, color = Month)) +
  geom_line(linewidth = 1) +
  scale_x_continuous(breaks = seq(0, 95, by = 4)) +
  labs(
    title = "Average Quarter-Hour Spot Price by Month",
    x = "15 minutes",
    y = "Average Spot Price (DKK)",
    color = "Month"
  ) +
  theme_minimal(base_size = 20)  # <- key change
#ggsave("plots/Quarterly/spotprice_avg_quarterly_month.png", width = 10, height = 6, dpi = 600)

# plot of log price
plot_quarter_hours <- function(data, start_hour = 12) {

  # quarters for 4 consecutive hours
  selected_quarters <- (start_hour * 4):((start_hour + 4) * 4 - 1)

  plot_data <- data %>%
    filter(Quarter %in% selected_quarters)

  ggplot(plot_data, aes(x = Date, y = LogPrice)) +
    geom_line(size = 0.2, colour = "black") +

    facet_wrap(~ QuarterLabel, ncol = 4) +
    labs(
      title = paste0(
        "Quarter-hour electricity prices (hours ",
        start_hour, "–", start_hour + 3, ")"
      ),
      x = NULL,
      y = NULL
    ) +

    theme_bw() +
    theme(
      strip.text = element_text(size = 10),
      axis.text.x = element_text(size = 6),
      axis.text.y = element_text(size = 6),
      panel.grid = element_blank(),
      plot.title = element_text(face = "bold")
    )
}

plot_quarter_hours(spotprice_panel, start_hour = 12)
#ggsave("plots/Quarterly/Quarter_hour_spotprice_4_4_plot.png", width = 10, height = 6, dpi = 600)


head(spotprice_panel)
########
# tables sum
plot_qq <- function(data, var, title = NULL, save_path = NULL,
                    color_points = "blue", color_line = "red",
                    base_size = 20, width = 6, height = 6, dpi = 600) {
  
  p <- ggplot(data, aes(sample = {{ var }})) +
    stat_qq(color = color_points) +
    stat_qq_line(color = color_line) +
    ggtitle(ifelse(is.null(title), deparse(substitute(var)), title)) +
    theme_minimal(base_size = base_size)
  
  if (!is.null(save_path)) {
    ggsave(save_path, plot = p, width = width, height = height, dpi = dpi)
  }
  
  return(p)
}

plot_qq(spotprice_panel, SpotPriceDKK,
        save_path = "plots/Quarterly/qqplot_spotprice.png")

plot_qq(spotprice_panel, LogPrice,
        save_path = "plots/Quarterly/qqplot_logPrice.png")

plot_qq(spotprice_panel, LogPrice_100,
        save_path = "plots/Quarterly/qqplot_logPrice_100.png")

plot_qq(spotprice_panel, LogPrice_asinh,
        save_path = "plots/Quarterly/qqplot_logPrice_asinh.png")


stats_summary <- data.frame(
  Variable = c("SpotPriceDKK", "LogPrice", "LogPrice_100", "LogPrice_asinh"),
  Skewness = c(
    e1071::skewness(spotprice_panel$SpotPriceDKK, na.rm = TRUE),
    e1071::skewness(spotprice_panel$LogPrice, na.rm = TRUE),
    e1071::skewness(spotprice_panel$LogPrice_100, na.rm = TRUE),
    e1071::skewness(spotprice_panel$LogPrice_asinh, na.rm = TRUE)
  ),
  Kurtosis = c(
    e1071::kurtosis(spotprice_panel$SpotPriceDKK, na.rm = TRUE),
    e1071::kurtosis(spotprice_panel$LogPrice, na.rm = TRUE),
    e1071::kurtosis(spotprice_panel$LogPrice_100, na.rm = TRUE),
    e1071::kurtosis(spotprice_panel$LogPrice_asinh, na.rm = TRUE)
  )
)

stats_summary

tex_output <- stats_summary %>%
  kable(
    format = "latex",
    booktabs = TRUE,
    digits = 3,
    align = "c",
    caption = "Skewness and Kurtosis of Price Transformations",
    label = "skew_kurt_Quarterly"
  ) %>%
  kable_styling(
    position = "center",
    latex_options = c("striped")
  ) %>%
  as.character()

# writeLines(
#   tex_output,
#   "Tables/quarterly_skewness_kurtosis_table.tex"
# )


########
quarterly_desc_stats <- function(data, spotprice, start_hour = 00, n_hours = 24) {
  
  end_hour <- start_hour + n_hours - 1
  
  stats <- data %>%
    # keep only selected hours
    mutate(Hour = Quarter %/% 4) %>%
    filter(Hour >= start_hour, Hour <= end_hour) %>%
    group_by(Hour, QuarterLabel) %>%
    summarise(
      Min      = min(.data[[spotprice]], na.rm = TRUE),
      Mean     = mean(.data[[spotprice]], na.rm = TRUE),
      Median   = median(.data[[spotprice]], na.rm = TRUE),
      Max      = max(.data[[spotprice]], na.rm = TRUE),
      Sd       = sd(.data[[spotprice]], na.rm = TRUE),
      Skewness = e1071::skewness(.data[[spotprice]], type = 2, na.rm = TRUE),
      Kurtosis = e1071::kurtosis(.data[[spotprice]], type = 2, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      Min = round(Min, 2),
      Max = round(Max, 2),
      Mean = round(Mean, 3),
      Median = round(Median, 3),
      Sd = round(Sd, 3),
      Skewness = round(Skewness, 2),
      Kurtosis = round(Kurtosis, 2)
    )
  
  return(stats)
}
test <- quarterly_desc_stats(spotprice_panel, "SpotPriceDKK")
test_log_1 <- quarterly_desc_stats(spotprice_panel, "LogPrice")
test_log_100 <- quarterly_desc_stats(spotprice_panel, "LogPrice_100")
test_asinh <- quarterly_desc_stats(spotprice_panel, "LogPrice_asinh")

save_quarterly_latex_table_single <- function(data, spotprice,
                                              start_index = 21,
                                              n_quarters = 40,
                                              output_dir = "Tables") {
  
  table_stat <- quarterly_desc_stats(data, spotprice)
  
  unique_quarters <- unique(table_stat$QuarterLabel)
  selected_quarters <- unique_quarters[start_index:(start_index + n_quarters - 1)]
  
  table_stat <- table_stat %>%
    filter(QuarterLabel %in% selected_quarters)
  
  create_block <- function(df_subset) {
    df_subset %>%
      select(QuarterLabel, Min, Mean, Median, Max, Sd, Skewness, Kurtosis) %>%
      pivot_longer(
        cols = -QuarterLabel,
        names_to = "Statistic",
        values_to = "Value"
      ) %>%
      pivot_wider(
        names_from = QuarterLabel,
        values_from = Value
      ) %>%
      mutate(Statistic = factor(
        Statistic,
        levels = c("Min", "Mean", "Median", "Max", "Sd", "Skewness", "Kurtosis")
      )) %>%
      arrange(Statistic)
  }
  
  q <- selected_quarters
  
  block1 <- create_block(filter(table_stat, QuarterLabel %in% q[1:8]))
  block2 <- create_block(filter(table_stat, QuarterLabel %in% q[9:16]))
  block3 <- create_block(filter(table_stat, QuarterLabel %in% q[17:24]))
  block4 <- create_block(filter(table_stat, QuarterLabel %in% q[25:32]))
  block5 <- create_block(filter(table_stat, QuarterLabel %in% q[33:40]))
  
  get_rows <- function(block) {
    kable(block,
          format = "latex",
          booktabs = TRUE,
          digits = 3,
          align = "c") %>%
      as.character() %>%
      gsub(".*\\\\toprule", "", .) %>%
      gsub("\\\\bottomrule.*", "", .)
  }
  
  rows1 <- get_rows(block1)
  rows2 <- get_rows(block2)
  rows3 <- get_rows(block3)
  rows4 <- get_rows(block4)
  rows5 <- get_rows(block5)
  
  col_format <- paste0("l", paste(rep("c", 8), collapse = ""))
  
  price_name <- dplyr::case_when(
    spotprice == "LogPrice" ~ "shifted log prices (+1)",
    spotprice == "LogPrice_100" ~ "shifted log prices (+100)",
    spotprice == "LogPrice_asinh" ~ "asinh-transformed log prices",
    TRUE ~ spotprice
  )
  
  caption <- paste0("Descriptive statistics for ", tolower(price_name), " by quarter")
  label <- paste0("tab:quarterly_descriptive_statistics_", spotprice)
  
  latex_output <- paste0(
    "\\begin{table}[h]\n\\centering\n",
    "\\caption{", caption, "}\n",
    "\\label{", label, "}\n",
    "\\begin{tabular}{", col_format, "}\n",
    "\\toprule\n",
    
    rows1,
    "\\midrule\n",
    rows2,
    "\\midrule\n",
    rows3,
    "\\midrule\n",
    rows4,
    "\\midrule\n",
    rows5,
    
    "\\bottomrule\n",
    "\\end{tabular}\n",
    "\\end{table}"
  )
  
  writeLines(latex_output,
             paste0(output_dir, "/quarterly_descriptive_statistics_", spotprice, ".tex"))
  
  invisible(NULL)
}
save_quarterly_latex_table_single(spotprice_panel, "LogPrice")
save_quarterly_latex_table_single(spotprice_panel, "LogPrice_100")
save_quarterly_latex_table_single(spotprice_panel, "LogPrice_asinh")

###################
mean(spotprice_panel$SpotPriceDKK <= 0, na.rm = TRUE)

spotprice_panel %>%
  summarise(
    n_total = n(),
    n_non_positive = sum(SpotPriceDKK <= 0, na.rm = TRUE),
    share_non_positive = mean(SpotPriceDKK <= 0, na.rm = TRUE)
  )


#########################
#### GET CONSUMPTION ####
#########################
# https://www.energidataservice.dk/dso-electricity/ConsumptionConsumerCategoryHour
get_consumption <- function(base, start = NULL, region = "Region Nordjylland") {
  query <- list()
  
  if (!is.null(start)) query$start <- start
  
  if (!is.null(region)) {
    query$filter <- sprintf('{"RegionName":"%s"}', region)
  }
  
  res <- GET(base, query = query)
  stop_for_status(res)
  
  data <- content(res, as = "text", encoding = "UTF-8")
  parsed <- fromJSON(data, flatten = TRUE)
  
  return(as_tibble(parsed$records))
}

# Brug funktionen:
consumption <- get_consumption("https://api.energidataservice.dk/dataset/ConsumptionConsumerCategoryHour?",
                               start = oct1_start)%>% 
  rename(HourUTC = TimeUTC,
         HourDK = TimeDK) %>%
  mutate(HourDK = ymd_hms(HourDK)) %>%  
  select(HourDK, ConsumptionkWh)

consumption_q <- consumption %>%
  # for each row, create 4 rows
  slice(rep(1:n(), each = 4)) %>%
  group_by(HourDK) %>%
  mutate(
    # offset minutes by 0, 15, 30, 45
    HourDK = HourDK + minutes(rep(c(0, 15, 30, 45), times = n()/4))
  ) %>%
  ungroup()

consumption_q_panel <- consumption_q %>%
  mutate(
    Date = as_date(HourDK),
    Hour = hour(HourDK),
    Minute = minute(HourDK),
    Quarter = Hour * 4 + Minute %/% 15,   # 0–95
    Weekday = wday(Date, label = TRUE, week_start = 1),
    Month = month(Date, label = TRUE, abbr = TRUE)
  ) %>%
  group_by(Date, Quarter) %>%
  summarise(
    ConsumptionkWh = mean(ConsumptionkWh, na.rm = TRUE),
    Weekday = first(Weekday),
    Month = first(Month),
    .groups = "drop"
  )

# dubletter eller missing (vinter til sommertid)
consumption_q_panel %>%
  group_by(Date) %>%
  summarise(n_quarters = n()) %>%
  filter(n_quarters != 96)

consumption_q_panel <- consumption_q_panel %>%
  group_by(Date) %>%
  complete(Quarter = 0:95) %>%
  arrange(Date, Quarter) %>%
  fill(Weekday, Month, .direction = "downup") %>%
  ungroup()

consumption_q_panel <- consumption_q_panel %>%
  group_by(Date) %>%
  arrange(Quarter) %>%
  fill(ConsumptionkWh, .direction = "down") %>%
  ungroup()

#################
### GET WIND ####
#################
# https://energidataservice.dk/tso-electricity/Forecasts_5Min
# 3 variables for wind in 5 min interval agregate to 15 min interval with avg
get_wind <- function(base, start = NULL, end = NULL, pricearea = "DK1") {
  
  query <- list()
  
  if (!is.null(start)) query$start <- start
  if (!is.null(end))   query$end   <- end
  
  # force PriceArea filter (default DK1)
  if (!is.null(pricearea)) {
    query$filter <- sprintf('{"PriceArea":"%s"}', pricearea)
  }
  
  res <- GET(base, query = query)
  stop_for_status(res)
  
  parsed <- fromJSON(content(res, "text", encoding = "UTF-8"), flatten = TRUE)
  
  as_tibble(parsed$records)
}

wind <- get_wind(
  base = "https://api.energidataservice.dk/dataset/ElectricityProdex5MinRealtime?",
  start = oct1_start,
  end   = today_end_str
)

wind_15min_panel <- wind %>%
  mutate(
    Minutes5DK = ymd_hms(Minutes5DK),
    Date = as_date(Minutes5DK),
    Hour = hour(Minutes5DK),
    Minute = minute(Minutes5DK),
    Quarter = Hour * 4 + Minute %/% 15,
    Weekday = wday(Date, label = TRUE, week_start = 1),
    Month = month(Date, label = TRUE, abbr = TRUE)
  ) %>%
  group_by(Date, Quarter) %>%
  summarise(
    OffshoreWindPower = mean(OffshoreWindPower, na.rm = TRUE),
    OnshoreWindPower  = mean(OnshoreWindPower, na.rm = TRUE),
    SolarPower        = mean(SolarPower, na.rm = TRUE),
    Weekday = first(Weekday),
    Month = first(Month),
    .groups = "drop"
  ) 

# dubletter eller missing (vinter til sommertid)
wind_15min_panel %>%
  group_by(Date) %>%
  summarise(n_quarters = n()) %>%
  filter(n_quarters != 96)

wind_15min_panel <- wind_15min_panel %>%
  group_by(Date) %>%
  complete(Quarter = 0:95) %>%
  arrange(Date, Quarter) %>%
  fill(Weekday, Month, .direction = "downup") %>%
  ungroup()

wind_15min_panel <- wind_15min_panel %>%
  group_by(Date) %>%
  arrange(Quarter) %>%
  fill(OffshoreWindPower, OnshoreWindPower, SolarPower, .direction = "down") %>%
  ungroup()

# date 2025-11-22 is missing and therefore we copy the day before
wind_15min_panel %>% filter(Date == "2025-11-22")
bad_day <- as.Date("2025-11-22")
prev_day <- bad_day - 1

wind_15min_panel <- wind_15min_panel %>%
  filter(Date != bad_day) %>%
  bind_rows(
    wind_15min_panel %>%
      filter(Date == prev_day) %>%
      mutate(Date = bad_day)
  ) %>%
  mutate(
    Weekday = wday(Date, label = TRUE, week_start = 1)
  ) %>%
  arrange(Date, Quarter)

wind_15min_panel %>% filter(Date == "2025-11-22")

##########
# tecnical error from energinet 2026-03-29
bad_day <- as.Date("2026-03-29")
prev_day <- bad_day - 1

# quarters corresponding to 03:00–03:59
bad_quarters <- 12:15

wind_15min_panel <- wind_15min_panel %>%
  filter(!(Date == bad_day & Quarter %in% bad_quarters)) %>%
  bind_rows(
    wind_15min_panel %>%
      filter(Date == prev_day, Quarter %in% bad_quarters) %>%
      mutate(Date = bad_day)
  ) %>%
  mutate(
    Weekday = wday(Date, label = TRUE, week_start = 1)
  ) %>%
  arrange(Date, Quarter)

##################
#### ALL DATA ####
##################
panel_data <- spotprice_panel %>%
  left_join(
    consumption_q_panel %>%
      select(Date, Quarter, ConsumptionkWh),
    by = c("Date", "Quarter")
  ) %>%
  left_join(
    wind_15min_panel %>%
      select(Date, Quarter, OffshoreWindPower, OnshoreWindPower, SolarPower),
    by = c("Date", "Quarter")
  ) %>% 
  filter(!is.na(ConsumptionkWh))

panel_data %>%
  filter(if_any(everything(), is.na))

# tjekker om alt er rigtigt
check_panel <- function(df, value_col) {
  list(
    wrong_hours = df %>% count(Date) %>% filter(n != 96),
    missing_values = sum(is.na(df[[value_col]])),
    duplicates = df %>% count(Date, Quarter) %>% filter(n > 1)
  )
}
check_panel(panel_data, "SpotPriceDKK")
check_panel(panel_data, "ConsumptionkWh")
check_panel(panel_data, "SpotPriceDKK")
check_panel(panel_data, "ConsumptionkWh")
check_panel(panel_data, "OffshoreWindPower")
check_panel(panel_data, "OnshoreWindPower")
check_panel(panel_data, "SolarPower")


####################
#### PANEL DATA ####
####################
panel_data <- panel_data %>%
  mutate(Date = as.Date(Date))

pdata <- pdata.frame(panel_data, index = c("Quarter", "Date"))

end_time <- Sys.time()
cat(end_time - start_time)
