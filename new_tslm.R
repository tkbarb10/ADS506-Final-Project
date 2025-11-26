library(tidyverse)
library(fpp3)

# ch4 should be squared
# log for both Total_Co2 and ppm

cv_data <- full_temp_nino |> 
  stretch_tsibble(.init = 1077, .step = 60)

cv_trn <- cv_data |> 
  group_by(.id) |> 
  slice(1:(n() - 60)) |> 
  ungroup()

cv_valid <- cv_data |> 
  group_by(.id) |> 
  slice_tail(n = 60) |> 
  ungroup()

tslm_fit <- cv_trn |> 
  model(
    tslm_ppm_ten_nina = TSLM(actual_temp ~ trend() + season() + lagged_ppm_ten + el_nino + la_nina),
    tslm_ppm_five = TSLM(actual_temp ~ trend() + season() + log(lagged_ppm_five)),
    tslm_all_box = TSLM(box_cox(actual_temp, lambda = 1.5) ~ trend() + season() + lagged_ppm_ten + ch4 + TSI),
    tslm_all_box_five = TSLM(box_cox(actual_temp, lambda = 1.5) ~ trend() + season() + lagged_ppm_five + ch4 + TSI),
    tslm_all_box_five_nino = TSLM(box_cox(actual_temp, lambda = 1.5) ~ trend() + season() + lagged_ppm_five + ch4 + TSI + el_nino + la_nina)
  )

tslm_fit |> 
  accuracy() |> 
  group_by(.model) |>
  summarise(across(.cols = c(RMSE, MAE, MAPE), .fns = mean)) |> 
  arrange(RMSE)

tslm_fit |> 
  select(tslm_ppm_ten_nina) |> 
  tail(n = 1) |> 
  report()


tslm_fc <- tslm_fit |> 
  forecast(new_data = cv_valid)

tslm_fc |> 
  accuracy(cv_valid) |> 
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
  filter(.id %in% c(7, 8)) |>
  autoplot() +
  autolayer(lagged_data |> filter(year(Date) >= 2010), actual_temp) +
  facet_grid(.model ~ .id) +
  theme_minimal()

tail(tslm_fc)
