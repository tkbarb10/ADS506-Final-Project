library(fpp3)

df <- read.csv("data/global_temp.csv")

# --------------- Change to tsibble ---------------
temp_data <- df |> mutate(
  Month = month.abb[Month],
  Date = yearmonth(paste(Month, Year)),
  Monthly_Anomaly = replace_na(Monthly_Anomaly, mean(Monthly_Anomaly, na.rm = T))) |>
  select(c(Date, Monthly_Anomaly)) |> 
  as_tsibble(index = Date)

# --------------- convert anomalies to actual temperatures ---------------

baseline_vector <- c(
  '1' = 12.30, '2' = 12.50, '3' = 13.13, '4' = 14.06, 
  '5' = 15.00, '6' = 15.66, '7' = 15.90, '8' = 15.75, 
  '9' = 15.18, '10' = 14.27, '11' = 13.28, '12' = 12.57
)
converted_df <- temp_data |> 
  mutate(
    month_char = as.character(month(Date)),
    baseline_temp = baseline_vector[month_char],
    actual_temp = baseline_temp + Monthly_Anomaly
    ) |> 
  select(-c(month_char, baseline_temp))

# --------------- initial eda ---------------

dim(df)

str(df)

sum(is.na(temp_data))

temp_data |> 
  autoplot()

temp_data |> 
  model(STL()) |> 
  components() |> 
  autoplot() + 
  labs(title = "STL Decomposition of Global Monthly Temperature Anomalies")

temp_data |> 
  filter(year(Date) >= 1930) |>
  model(STL()) |> 
  components() |> 
  autoplot()

gg_tsdisplay(temp_data, Monthly_Anomaly, plot_type = 'partial') + labs(title = "ACF and PACF of Global Monthly Temperature Anomalies")

features(temp_data, Monthly_Anomaly, feat_stl)

features(temp_data, Monthly_Anomaly, c(unitroot_kpss, unitroot_ndiffs, unitroot_nsdiffs))
# We'll need at least one difference

features(temp_data, Monthly_Anomaly, c(ljung_box, box_pierce))
# Looks like some sort of transformation is needed

features(temp_data, Monthly_Anomaly, guerrero)
# lambda is close to 1, so no power transformation needed


features(temp_data, Monthly_Anomaly, feat_spectral)

?components

# --------------- EDA with full temps ---------------

full_temp <- read.csv("data/converted_global_temp.csv")

head(full_temp)

full_temp_tsibble <- full_temp |> 
  mutate(
    Date = yearmonth(Date)
  ) |> 
  select(c(Date, actual_temp)) |> 
  as_tsibble(index = Date)

full_temp_tsibble |> 
  autoplot(actual_temp)

full_temp_tsibble |> 
  model(STL()) |> 
  components() |> 
  autoplot() +
  labs(title = "STL Decomposition of Global Monthly Actual Temperatures")

full_temp_tsibble |> 
  filter(year(Date) >= 1930) |>
  model(STL()) |> 
  components() |> 
  autoplot()

gg_tsdisplay(full_temp_tsibble, actual_temp, plot_type = 'partial')
# much more clearly see the seasonal auto correlation now

features(full_temp_tsibble, actual_temp, c(unitroot_kpss, unitroot_ndiffs, unitroot_nsdiffs))
# need at least one difference and one seasonal difference

features(full_temp_tsibble, actual_temp, guerrero)
# Lamdba estimation of 1.44, so a power transformation may be helpful
