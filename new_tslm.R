library(tidyverse)
library(fpp3)

# ch4 should be squared
# log for both Total_Co2 and ppm

lagged_data <- full_reg |> 
  mutate(
    lagged_emissions_five = lag(Total_CO2, 60),
    lagged_emissions_ten = lag(Total_CO2, 120),
    lagged_ppm_five = lag(co2_ppm, 60),
    lagged_ppm_ten = lag(co2_ppm, 120),
    lagged_ch4_five = lag(ch4, 60),
    lagged_ch4_ten = lag(ch4, 120)
  ) |> 
  filter(!is.na(lagged_ppm_ten))

cv_data <- lagged_data |> 
  stretch_tsibble(.init = 1089, .step = 60)

future_cv_data <- new_data(cv_data, n = 60)

future_cv_data <- future_cv_data |> 
  inner_join(lagged_data, by = "Date")

tslm_fit <- cv_data |> 
  model(
    tslm_ppm_ten = TSLM(actual_temp ~ trend() + season() + lagged_ppm_ten),
    tslm_ppm_five = TSLM(actual_temp ~ trend() + season() + log(lagged_ppm_five)),
    tslm_log_ppm_ten = TSLM(actual_temp ~ trend() + season() + log(lagged_ppm_ten) + TSI),
    tslm_log_and_ch4 = TSLM(actual_temp ~ trend() + season() + log(co2_ppm) + lagged_ch4_five + TSI)
  )

tslm_fit |> 
  accuracy() |> 
  group_by(.model) |>
  summarise(across(.cols = c(RMSE, MAE, MAPE), .fns = mean)) |> 
  arrange(RMSE)

tslm_fit |> 
  select(tslm_log_and_ch4) |> 
  tail(n = 1) |> 
  report()


tslm_fc <- tslm_fit |> 
  forecast(new_data = future_cv_data)

tslm_fc |> 
  accuracy(lagged_data) |> 
  arrange(RMSE)

tslm_fc |>
  ggplot(aes(x = .model, y = .mean, fill = .model)) +
  geom_boxplot() +
  geom_jitter(width = 0.2, alpha = 0.5) + # Shows the individual dots (folds)
  theme_minimal() +
  labs(
    title = "Model Stability: RMSE across CV Folds",
    subtitle = "Lower and tighter box is better",
    y = "RMSE (Degrees Celsius)"
  )

tslm_fc |>
  filter(.id %in% c(14, 15)) |>
  autoplot() +
  autolayer(lagged_data |> filter(year(Date) >= 2005), actual_temp) +
  facet_grid(.model ~ .id) +
  theme_minimal()
