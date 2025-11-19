library(fpp3)
library(tictoc)

df <- read.csv("data/converted_global_temp.csv")

full_temp <- df |> 
  mutate(
    Date = yearmonth(Date)
  ) |> 
  select(c(Date, actual_temp)) |> 
  as_tsibble(index = Date)

trn_data <- full_temp |>
  filter(year(Date) < 2020)

cv_trn <- trn_data |> 
  stretch_tsibble(.init = 960, .step = 60)

# ----------------- ETS Modeling -----------------

tic("ETS Model Fitting")
ets_fit <- cv_trn |> 
  model(
    ets_auto = ETS(),
    additive = ETS(actual_temp ~ error("A") + trend("A") + season("A")),
    multiplicative = ETS(actual_temp ~ error("M") + trend("A") + season("M")),
    damped = ETS(actual_temp ~ error("A") + trend("Ad") + season("A")),
    log_auto = ETS(log(actual_temp)),
    box_cox_auto = ETS(box_cox(actual_temp, lambda = 1.5))
  )

ets_time <- toc()

# Training Performance
ets_fit |> accuracy() |> group_by(.model) |> 
  summarise(across(.cols = c(RMSE, MAE, MAPE), .fns = mean)) |> 
  arrange(RMSE)

# Parameter Estimates
ets_fit |> 
  tidy() |> 
  group_by(.model, term) |> 
  summarise(across(.cols = estimate, .fns = mean)) |> 
  pivot_wider(names_from = term, values_from = estimate)

# ------------------ ETS Forecasting ------------------

# Number chosen by remaining months in dataset after training period
ets_fc <- ets_fit |> 
  select(c(box_cox_auto, ets_auto, damped)) |> 
  forecast(h = 69)

# Forecast Accuracy
ets_fc |> 
  accuracy(full_temp) |>
  arrange(RMSE)

# Visualize forecasts by splits
ets_fc |>
  filter(.id %in% c(18, 19)) |>
  autoplot() +
  autolayer(full_temp |> filter(year(Date) > 2000), actual_temp) +
  facet_grid(.model ~ .id) +
  theme_minimal()

ets_fc |>
  filter(.model == "box_cox_auto") |> 
  group_by(.model, Date) |> 
  summarise(across(.cols = .mean, .fns = mean)) |> 
  autoplot()

# Plot residuals by model
ets_fit |> 
  select(box_cox_auto) |>
  augment() |> 
  ggplot(aes(x = .fitted, y = .innov)) +
  geom_point(alpha = 0.3) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed")

