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

# ----------------- TSLM Modeling -----------------

tic("TSLM Model Fitting")
tslm_fit <- cv_trn |> 
  model(
    tslm_auto = TSLM(actual_temp),
    tslm_trend = TSLM(actual_temp ~ trend()),
    tslm_trend_season = TSLM(actual_temp ~ trend() + season()),
    tslm_fourier = TSLM(actual_temp ~ trend() + fourier(K = 2)),
    tslm_log = TSLM(log(actual_temp) ~ trend()),
    tslm_box_cox = TSLM(box_cox(actual_temp, lambda = 1.5) ~ trend()),
    tslm_piecewise = TSLM(actual_temp ~ trend(knots = c(1920, 1975)))
  )

tslm_time <- toc()

tslm_fit |> accuracy() |> group_by(.model) |> 
  summarise(across(.cols = c(RMSE, MAE, MAPE), .fns = mean)) |> 
  arrange(RMSE)

tslm_fit |> 
  tidy() |> 
  group_by(.model, term) |> 
  summarise(across(.cols = estimate, .fns = mean)) |> 
  pivot_wider(names_from = term, values_from = estimate) |> 
  View()

# ------------------ Second Fit -------------------

tslm_fit_two <- cv_trn |> 
  model(
    tslm_fourier = TSLM(actual_temp ~ trend() + fourier(K = 4)),
    tslm_log = TSLM(log(actual_temp) ~ trend() + season()),
    tslm_box_cox = TSLM(box_cox(actual_temp, lambda = 1.5) ~ trend() + season()),
    tslm_piecewise = TSLM(actual_temp ~ trend(knots = c(1920, 1975) ) + season())
  )

tslm_fit_two |> accuracy() |> group_by(.model) |> 
  summarise(across(.cols = c(RMSE, MAE, MAPE), .fns = mean)) |> 
  arrange(RMSE)

# ------------------ Final Fit -------------------

tslm_final <- cv_trn |> 
  model(
    tslm_log = TSLM(log(actual_temp) ~ trend() + season()),
    tslm_box_cox = TSLM(box_cox(actual_temp, lambda = 1.5) ~ trend() + season()),
    tslm_piecewise = TSLM(actual_temp ~ trend(knots = c(1920, 1975) ) + season()),
    tslm_trend_season = TSLM(actual_temp ~ trend() + season()),
    tslm_fourier = TSLM(actual_temp ~ trend() + fourier(K = 2))
  )

# ------------------ TSLM Forecasting ------------------

tslm_fc <- tslm_final |> 
  forecast(h = 69)

tslm_fc |> 
  accuracy(full_temp) |>
  arrange(RMSE)

tslm_fc |>
  filter(.id %in% c(18, 19)) |>
  autoplot() +
  autolayer(full_temp |> filter(year(Date) > 2000), actual_temp) +
  facet_grid(.model ~ .id) +
  theme_minimal()


# --------------- TSLM Adding El Nino Events ----------------

el_nino <- read.csv("data/el_nino_events.csv")

full_temp_nino <- full_temp |> 
  # 1. Extract the Year from your index (Date) to create a common key
  mutate(join_year = year(Date)) |> 
  
  # 2. Left join the yearly El Nino data onto the monthly structure
  #    (This automatically repeats the yearly status for all 12 months of that year)
  left_join(el_nino, by = c("join_year" = "Year")) |> 
  
  # 3. Create your 3 dummy variable columns
  #    We use 'missing = 0' to handle years where data might be NA
  mutate(
    el_nino       = if_else(Phase == "El Niño", 1, 0, missing = 0),
    la_nina       = if_else(Phase == "La Niña", 1, 0, missing = 0)
  ) |> 
  
  # 4. Cleanup: remove the temporary joining column and the raw Phase/Strength columns
  select(-join_year, -Phase, -Strength) |> 
  filter(year(Date) > 1895)

cv_trn_nino <- full_temp_nino |>  
  stretch_tsibble(.init = 300, .step = 60)

future_test_data <- new_data(cv_trn_nino, n = 63) |> 
  # 2. Join the known El Nino/La Nina data onto this scaffolding
  #    We join by Date to pull in the 'el_nino' and 'la_nina' columns 
  left_join(full_temp_nino, by = "Date")

tslm_nino_fit <- cv_trn_nino |> 
  model(
    tslm_piecewise = TSLM(actual_temp ~ trend(knots = c(1920, 1975) ) + season()),
    tslm_log = TSLM(log(actual_temp) ~ trend() + season()),
    tslm_nino = TSLM(actual_temp ~ trend() + season() + el_nino + la_nina)
  )

tslm_nino_fit |> 
  tidy() |> 
  group_by(.model, term) |> 
  summarise(across(.cols = c(estimate, std.error, p.value), .fns = mean))

tslm_nino_fc <- tslm_nino_fit |> 
  forecast(new_data = future_test_data)

tslm_nino_fc |> 
  accuracy(full_temp_nino) |> 
  arrange(RMSE)

tslm_nino_fc |>
  filter(.id %in% c(20, 21)) |>
  autoplot() +
  autolayer(full_temp |> filter(year(Date) > 2000), actual_temp) +
  facet_grid(.model ~ .id) +
  theme_minimal()
