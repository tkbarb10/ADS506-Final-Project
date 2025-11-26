library(tidyverse)
library(fpp3)

legit <- read_rds("data/legit_monthly_co2.rds")
early <- read_rds("data/early_co2_monthly.rds")
full_pred <- read_rds("data/tslm_regressor_ts.rds")

combined <- bind_rows(early, legit)

new <- new_data(combined, n = 9, keep_all = TRUE)
new$Total_CO2 <- 3516666.67

combined <- bind_rows(early, legit, new)

full_data <- full_pred |> 
  left_join(combined)

lagged_data <- full_data |> 
  mutate(
    lagged_co2_ten = lag(Total_CO2, 120),
    lagged_log_ppm = lag(log(co2), 120),
    lagged_ppm = lag(co2, 120)
  ) |> 
  filter(!is.na(lagged_co2_ten))

cv_data <- lagged_data |> 
  stretch_tsibble(.init = 957, .step = 60)

future_cv_data <- new_data(cv_data, n = 60)

future_cv_data <- future_cv_data |> 
  inner_join(lagged_data, by = "Date")

tslm_fit <- cv_data |> 
  model(
    tslm_lag10 = TSLM(actual_temp ~ trend() + season() + el_nino + la_nina + TSI + lagged_co2_ten),
    tslm_ppm_log_lag10 = TSLM(actual_temp ~ trend() + season() + el_nino + la_nina + TSI + ch4 + lagged_log_ppm),
    tslm_ppm_lag10 = TSLM(actual_temp ~ trend() + season() + el_nino + la_nina + TSI + ch4 + lagged_ppm),
    tslm_all = TSLM(actual_temp ~ trend() + season() + el_nino + la_nina + ch4 + TSI + Total_CO2)
  )

tslm_fit |> 
  accuracy() |> 
  group_by(.model) |>
  summarise(across(.cols = c(RMSE, MAE, MAPE), .fns = mean)) |> 
  arrange(RMSE)

tslm_fit |> 
  select(tslm_ppm_lag10) |> 
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
  filter(.id %in% c(7, 8)) |>
  autoplot() +
  autolayer(lagged_data |> filter(year(Date) >= 2005), actual_temp) +
  facet_grid(.model ~ .id) +
  theme_minimal()
