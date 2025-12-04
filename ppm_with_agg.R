ppm <- read_rds("data/interpolated_co2_ppm.rds")

emissions <- read_rds("data/interpolated_co2_emissions.rds")

ppm_nino <- read_rds("data/agg_emissions_enso_index.rds")

ppm_agg <- ppm |> 
  filter(year(Date) < 2025) |> 
  left_join(emissions, by = "Date") |> 
  mutate(
    aggregate_emissions = cumsum(total_co2) / 1000000 # convert from gigagrams to gigatons for scaling purposes
  )

trn_data <- ppm_nino |>
  filter(year(Date) > 1957) |> 
  slice(1:(n() - 36))

valid_data <- ppm_nino |> 
  filter(year(Date) > 1957) |> 
  slice_tail(n = 36)


best_arima <- trn_data |> 
  model(
    arima_smoothed = ARIMA(co2_ppm ~ 0 + aggregate_emissions + enso_smooth + PDQ(0, 1, 1) + pdq(1, 1, 1), greedy = FALSE)
  )

best_arima |> report()

best_arima |> tidy()

best_arima_fc <- best_arima |> 
  forecast(new_data = valid_data)

best_arima_fc |> 
  accuracy(valid_data)

best_arima |> 
  gg_tsresiduals(plot_type = 'partial')

best_arima_fc |>
  autoplot() +
  autolayer(ppm_nino |> filter(year(Date) >= 2010), co2_ppm) +
  theme_minimal()



