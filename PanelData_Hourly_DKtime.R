library(httr)
library(jsonlite)
library(dplyr)
library(lubridate)
library(plm)
library(ggplot2)

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
  start = one_year_ago_str, # "2025-03-17T00:00",
  end   = "2025-10-01T00:00" # denne ædres ikke da dataet stopper her
)

# Ensure HourDK column are datetime
spotprice1 <- spotprice1 %>%
  mutate(HourDK = ymd_hms(HourDK))


#spot price from 2025-10-01
spotprice2 <- get_elspot("https://api.energidataservice.dk/dataset/DayAheadPrices", 
                         start = "2025-10-01T00:00", # denne ædres ikke da dataet starter her
                         end   = today_end_str # "2026-03-17T00:00"
) %>% 
  rename(HourUTC = TimeUTC,
         HourDK = TimeDK,
         SpotPriceDKK = DayAheadPriceDKK,
         SpotPriceEUR = DayAheadPriceEUR)

# make spotprice2 to hourly data
spotprice2_hourly <- spotprice2 %>%
  mutate(HourDK = ymd_hms(HourDK)) %>%   # convert to datetime
  filter(minute(HourDK) == 0)            # keep only rows where minutes = 0# 

# Combine datasets
spotprice_all <- bind_rows(spotprice1, spotprice2_hourly) %>%
  arrange(HourDK)

# hourly panel, each hour should be a separate “group”:
spotprice_panel <- spotprice_all %>%
  mutate(
    Date = as_date(HourDK),
    Hour = hour(HourDK),
    Weekday = wday(Date, label = TRUE, week_start = 1),  # Monday = 1
    Month = month(Date, label = TRUE, abbr = TRUE)       # Jan, Feb, ...
  ) %>%
  group_by(Date, Hour) %>%  # handle repeated hours (DST)
  summarise(
    SpotPriceDKK = mean(SpotPriceDKK, na.rm = TRUE),
    SpotPriceEUR = mean(SpotPriceEUR, na.rm = TRUE),
    Weekday = first(Weekday),  # keep Weekday
    Month = first(Month),      # keep Month
    .groups = "drop"
  )

# DEN ENDELIG PANEL DATASET ER spotprice_panel

# tjek for duplikater - hvis tom = ingen dublikater (hvis der er duplikater er det pga sommer/vinter tid)
spotprice_panel %>%
  group_by(Hour, Date) %>%
  filter(n() > 1)

# dubletter for dag med 23 timer elrpis (vinter til sommertid)

####################################
#### SPOT PRICE ANALYSIS CHPT.2 ####
####################################

spotprice_subset <- spotprice_panel %>%
  filter(Hour %in% c(0, 6, 12, 18)) #%>%
  # mutate(
  #   HourLabel = case_when(
  #     Hour == 0 ~ 24,       # recode midnight to 24
  #     TRUE ~ Hour
  #   )
  # )

# plot af time 8, 16 og 24
ggplot(spotprice_subset, aes(x = Date, y = SpotPriceDKK)) +
  geom_line(color = "steelblue") +
  facet_wrap(~ Hour, ncol = 1, scales = "free_y") +
   #facet_wrap(~ HourLabel, ncol = 1, scales = "free_y") +
  labs(
    title = "Spot Prices Over Time for Selected Hours (DK time)",
    x = "Date",
    y = "Spot Price (DKK)",# caption = "Hour 24 = midnight"
  ) +
  scale_x_date(
    date_breaks = "1 months",      # hver 2. måned
    date_labels = "%b \n %Y"          # fx "Jan 2025"
  ) +
  theme_minimal()


# Compute average price per Hour and Weekday
avg_hourly <- spotprice_panel %>%
  group_by(Weekday, Hour) %>%
  summarise(
    AvgPriceDKK = mean(SpotPriceDKK, na.rm = TRUE),
    .groups = "drop"
  )

# Plot: one line per weekday
ggplot(avg_hourly, aes(x = Hour, y = AvgPriceDKK, color = Weekday)) +
  geom_line(size = 1) +
  scale_x_continuous(breaks = 0:23) +
  labs(
    title = "Average Hourly Day-Ahead Spot Price by Weekday",
    x = "Hour of Day",
    y = "Average Spot Price (DKK)",
    color = "Weekday"
  ) +
  theme_minimal()

avg_hourly_month <- spotprice_panel %>%
  group_by(Month, Hour) %>%
  summarise(
    AvgPriceDKK = mean(SpotPriceDKK, na.rm = TRUE),
    .groups = "drop"
  )

# Plot: one line per month
ggplot(avg_hourly_month, aes(x = Hour, y = AvgPriceDKK, color = Month)) +
  geom_line(size = 1) +
  scale_x_continuous(breaks = 0:23) +
  labs(
    title = "Average Hourly Day-Ahead Spot Price by Month",
    x = "Hour of Day",
    y = "Average Spot Price (DKK)",
    color = "Month"
  ) +
  theme_minimal()


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

consumption_panel <- consumption %>%
  mutate(
    Date = as_date(HourDK),
    Hour = hour(HourDK)
  ) %>%
  group_by(Date, Hour) %>%   # hvis der også er dubletter her
  summarise(
    ConsumptionkWh = mean(ConsumptionkWh, na.rm = TRUE),
    .groups = "drop"
  )

panel_data <- spotprice_panel %>%
  left_join(consumption_panel, by = c("Date", "Hour"))

#########################
#### MODEL SELECTION ####
#########################
# Treat 'Hour' as the individual i, 'Date' as the time t
panel_data <- panel_data %>%
  dplyr::mutate(
    Weekday = as.numeric(Weekday),
    Month = as.numeric(Month)
    )

spotprice_panel_plm <- pdata.frame(panel_data, index = c("Hour","Date"))

# You want to include predictors like Weekday or Month. Common choices:
## Weekday effect: captures weekly patterns.
## Month effect: captures seasonal monthly patterns.
## Both: if you suspect both weekly and monthly seasonality.
formula <- SpotPriceDKK ~ ConsumptionkWh

# Pooled Regression (PR)
pr_model <- plm(formula, data = spotprice_panel_plm, model = "pooling")

# Fixed Effects (FE)
fe_model <- plm(formula, data = spotprice_panel_plm, model = "within")

# Random Effects (RE)
re_model <- plm(formula, data = spotprice_panel_plm, model = "random")

# library(lme4)
# re_model <- lmer(SpotPriceDKK ~ ConsumptionkWh + Weekday + Month + (1|Hour), data = panel_data)

##############
#### TEST ####
##############
# FE vs PR → F-test 
## if p-val<0.05 => FE
pFtest(fe_model, pr_model)

# RE vs PR → Breusch-Pagan Lagrange Multiplier test
## if p-val<0.05 => RE
plmtest(pr_model, type = "bp")

# FE vs RE → Hausman test
## if p-val<0.05 => RE
phtest(fe_model, re_model)


#### DYAMIC PANEL DATA ####
spotprice_panel_dynamic <- panel_data %>%
  arrange(Hour, Date) %>%
  group_by(Hour) %>%
  mutate(SpotPriceLag1 = lag(SpotPriceDKK, 1)) %>%
  ungroup()

spotprice_panel_dynamic$Weekday <- factor(spotprice_panel_dynamic$Weekday, ordered = FALSE)

spotprice_panel_dynamic_plm <- pdata.frame(
  spotprice_panel_dynamic,
  index = c("Hour", "Date")
)

dyn_re_model <- plm(
  SpotPriceDKK ~ SpotPriceLag1,
  data = spotprice_panel_dynamic_plm,
  model = "random"
)
summary(dyn_re_model)

# 
# dyn_model <- plm(SpotPriceDKK ~ SpotPriceLag1 + Weekday, 
#                  data = pdata.frame(spotprice_panel_dynamic, index = c("Hour","Date")),
#                  model = "pooling")
# summary(dyn_model)
# det ses at SpotPriceLag1 p-val<2e-16, hvilket betyder den er significant.
# Dette tyder på at vi har en Dynamic panel data


