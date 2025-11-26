library(fpp3)
library(tidyverse)

arima_fit <- cv_data |> 
  model(
    arima_lag_ppm  = ARIMA(actual_temp ~ lagged_log_ppm + pdq(p = 1, d = 0, q = 1) + PDQ(P = 3, D = 1, Q = 0, period = 12))
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
