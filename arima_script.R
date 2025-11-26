library(fpp3)
library(tidyverse)

arima_fit <- cv_data |> 
  model(
    arima_lag_ppm  = ARIMA(actual_temp ~ log(lagged_ppm_five) + pdq(p = 1, d = 0, q = 0:3) + PDQ(P = 0:2, D = 1, Q = 0:2, period = 12)),
    arima_auto = ARIMA(actual_temp ~ pdq(p = 1, d = 0, q = 0:3) + PDQ(P = 0:2, D = 1, Q = 0:2, period = 12) + co2_ppm + TSI + ch4),
    arima_trans = ARIMA(actual_temp ~ pdq(p = 1, d = 0, q = 0:3) + PDQ(P = 0:2, D = 1, Q = 0:2, period = 12) + log(co2_ppm) + TSI + box_cox(ch4, 2))
  )

arima_fit |> 
  accuracy() |> 
  group_by(.model) |> 
  summarise(across(.cols = c(RMSE, ME, MAE, MPE, MAPE), .fns = mean)) |> 
  arrange(RMSE)

arima_fit |> 
  tail(1) |> 
  report()

arima_fit |> 
  tidy() |> 
  group_by(term) |> 
  summarize(across(.cols = c(estimate:`p.value`), .fns = mean))

arima_fc <- arima_fit |> 
  forecast(new_data = future_cv_data)

arima_fc |> 
  accuracy(lagged_data) |> 
  arrange(RMSE)

arima_fc |>
  filter(.id %in% c(7, 8)) |>
  autoplot() +
  autolayer(lagged_data |> filter(year(Date) >= 2005), actual_temp) +
  facet_grid(.model ~ .id) +
  theme_minimal()
