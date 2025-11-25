library(fpp3)
library(tidyverse)
library(tictoc)

df <- read_rds("data/tslm_regressor_ts.rds")

full_temp <- df |> 
  mutate(Date = yearmonth(Date)) |> 
  select(c(Date, actual_temp)) |> 
  as_tsibble()

full_temp |> 
  ggtime::gg_tsdisplay(
    actual_temp |> difference() |> difference(12),
    plot_type = 'partial'
  )


cv_nino_data <- df |> 
  stretch_tsibble(.init = 397, .step = 58)

cv_trn_nino <- cv_nino_data |> 
  group_by(.id) |> 
  slice(1:(n() - 60)) |> 
  ungroup()

cv_valid <- cv_nino_data |> 
  group_by(.id) |> 
  slice_tail(n = 60) |> 
  ungroup()

tic("Arima modeling")

arima_auto <- cv_trn_nino |> 
  model(
    arima_auto = ARIMA(actual_temp ~ co2 + el_nino + la_nina + TSI + ch4 + pdq(p = 0:3, d = 0:1, q = 0:3) + PDQ(P = 0:3, D = 0:2, Q = 0:3, period = 12))
  )

"v=LM w/ ARIMA(0,0,2)(0,1,1)[12] errors"

ending <- toc()


arima_auto |> 
  accuracy() |> 
  group_by(.model) |> 
  summarise(across(.cols = c(RMSE, ME, MAE, MPE, MAPE), .fns = mean)) |> 
  arrange(RMSE)

arima_auto |> 
  tail(1) |> 
  report()

arima_fc <- arima_auto |> 
  forecast(new_data = cv_valid)

arima_fc |> 
  accuracy(cv_valid) |> 
  group_by(.model) |>
  summarise(across(.cols = c(RMSE, MAE, MAPE), .fns = mean)) |> 
  arrange(RMSE)

arima_fc |>
  filter(.id %in% c(20, 21)) |>
  autoplot() +
  autolayer(df |> filter(year(Date) >= 2000), actual_temp) +
  facet_grid(.model ~ .id) +
  theme_minimal()
