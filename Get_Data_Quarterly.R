library(httr)
library(jsonlite)
library(dplyr)
library(tidyr)
library(lubridate)
library(plm)
library(ggplot2)
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

#spot price util 2025-10-01
spotprice1 <- get_elspot("https://api.energidataservice.dk/dataset/Elspotprices", 
                         start = one_year_ago_str, #"2025-02-22T00:00",
                         end   = "2025-10-01T00:00"
)

# Ensure HourDK column are datetime
spotprice1 <- spotprice1 %>%
  mutate(HourDK = ymd_hms(HourDK))


#spot price from 2025-10-01
spotprice2 <- get_elspot("https://api.energidataservice.dk/dataset/DayAheadPrices", 
                         start = "2025-10-01T00:00",
                         end   = today_end_str # "2026-02-23T00:00"
) %>% 
  rename(HourUTC = TimeUTC,
         HourDK = TimeDK,
         SpotPriceDKK = DayAheadPriceDKK,
         SpotPriceEUR = DayAheadPriceEUR)

# Ensure HourDK column are datetime
spotprice2 <- spotprice2 %>%
  mutate(HourDK = ymd_hms(HourDK))

# make spotprice1 to quaterly data 
# create 15-min intervals for each hour
spotprice1_quarter <- spotprice1 %>%
  # for each row, create 4 rows
  slice(rep(1:n(), each = 4)) %>%
  group_by(HourDK) %>%
  mutate(
    # offset minutes by 0, 15, 30, 45
    HourDK = HourDK + minutes(rep(c(0, 15, 30, 45), times = n()/4))
  ) %>%
  ungroup()

# Combine datasets
spotprice_all <- bind_rows(spotprice1_quarter, spotprice2) %>%
  arrange(HourDK)

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
    SpotPriceEUR = mean(SpotPriceEUR, na.rm = TRUE),
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
  fill(SpotPriceDKK, SpotPriceEUR, .direction = "down") %>%
  ungroup()


####################################
#### SPOT PRICE ANALYSIS CHPT.2 ####
####################################
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
ggsave("plots/Quarterly/spotprice_querterly_Time10.png", width = 10, height = 6, dpi = 300)

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
  theme_minimal() 
ggsave("plots/Quarterly/spotprice_querterly_Time10_oneplot.png", width = 10, height = 6, dpi = 300)


# Plot: one line per weekday
avg_quarterly <- spotprice_panel %>%
  group_by(Weekday, Quarter) %>%
  summarise(
    AvgPriceDKK = mean(SpotPriceDKK, na.rm = TRUE),
    .groups = "drop"
  )
ggplot(avg_quarterly, aes(x = Quarter, y = AvgPriceDKK, color = Weekday)) +
  geom_line(linewidth = 1) +
  
  scale_x_continuous(breaks = seq(0, 95, by = 2)) +
  
  labs(
    title = "Average Quarter-Hour Spot Price by Weekday",
    x = "Quarter of Day (0–95)",
    y = "Average Spot Price (DKK)",
    color = "Weekday"
  ) +
  
  theme_minimal()
ggsave("plots/Quarterly/spotprice_avg_quarterly_weekday.png", width = 10, height = 6, dpi = 300)


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
    x = "Quarter of Day (0–95)",
    y = "Average Spot Price (DKK)",
    color = "Month"
  ) +
  
  theme_minimal()
ggsave("plots/Quarterly/spotprice_avg_quarterly_month.png", width = 10, height = 6, dpi = 300)


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
                               start = one_year_ago_str)%>% 
  rename(HourUTC = TimeUTC,
         HourDK = TimeDK)

consumption <- consumption %>%
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

##################
#### ALL DATA ####
##################
panel_data <- spotprice_panel %>%
  left_join(
    consumption_q_panel %>%
      select(Date, Quarter, ConsumptionkWh),
    by = c("Date", "Quarter")
  ) %>%
  filter(!is.na(ConsumptionkWh))


# tjekker om alt er rigtigt
check_panel <- function(df, value_col) {
  list(
    wrong_hours = df %>% count(Date) %>% filter(n != 96),
    missing_values = sum(is.na(df[[value_col]])),
    duplicates = df %>% count(Date, Quarter) %>% filter(n > 1)
  )
}

check_panel(spotprice_panel, "SpotPriceDKK")
check_panel(consumption_q_panel, "ConsumptionkWh")
check_panel(panel_data, "SpotPriceDKK")
check_panel(panel_data, "ConsumptionkWh")


####################
#### PANEL DATA ####
####################
panel_data <- panel_data %>%
  mutate(Date = as.factor(Date))  # important for FE

pdata <- pdata.frame(panel_data, index = c("Quarter", "Date"))
