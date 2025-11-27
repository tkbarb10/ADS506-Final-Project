monthly_co2 <- read_excel("C:/Users/tkbar/Downloads/IEA_EDGAR_CO2_m_1970_2024/IEA_EDGAR_CO2_m_1970_2024.xlsx", sheet = "TOTALS BY COUNTRY", skip = 9)

legit_monthly <- monthly_co2 |> 
  select(c(Year:Dec)) |> 
  group_by(Year) |> 
  summarize(across(.cols = c(Jan:Dec), .fns = sum)) |> 
  pivot_longer(cols = c(Jan:Dec), names_to = 'Month', values_to = "CO2_Emissions") |> 
  mutate(
    date_string = paste(Year, Month, "01", sep = "-"),
    Date = yearmonth(date_string)
  ) |> 
  select(c(Date, CO2_Emissions)) |> 
  as_tsibble()


land_ts <- new_land |> 
  mutate(Date = yearmonth(time)) |> 
  as_tibble() |> 
  select(Date, co2_emissions) |> 
  as_tsibble()

country_ts <- new_co2 |> 
  mutate(
    Date = yearmonth(time),
    co2_emissions = value * 3664
  ) |> 
  as_tibble() |> 
  select(Date, co2_emissions) |> 
  as_tsibble()

pre_1970_ts <- country_ts |> 
  left_join(land_ts, by = "Date") |> 
  mutate(
    co2_emissions = co2_emissions.x + co2_emissions.y
  ) |> 
  select(Date, co2_emissions) |> 
  filter(year(Date) < 1970)

# model land_ts out 1 year
# combine with legit_monthly
# model combined out 9 months


arima_fit <- land_ts |> 
  model(ARIMA())

arima_fc <- arima_fit |> 
  forecast(h = 12)

arima_fc

predicted_land <- arima_fc |> 
  as_tibble() |> 
  select(Date, .mean) |> 
  rename(co2_emissions = .mean) |> 
  select(Date, co2_emissions) |> 
  as_tsibble() |> 
  bind_rows(land_ts) |> 
  arrange(Date) |> 
  filter(year(Date) > 1969)

# Add together the predicted land with observed monthly

combined_monthly <- legit_monthly |> 
  left_join(predicted_land, by = "Date") |> 
  mutate(total_co2 = CO2_Emissions + co2_emissions) |> 
  select(Date, total_co2)


# forecast combined by 9 months


trn_combined <- combined_monthly |> 
  filter(year(Date) < 2022)

valid_combined <- combined_monthly |> 
  filter(year(Date) >= 2022)


arima_fit <- combined_monthly |> 
  model(ARIMA())

arima_fc <- arima_fit |> 
  forecast(h = 9)


pred_combined <- arima_fc |> 
  as_tibble() |> 
  select(Date, .mean) |> 
  rename(total_co2 = .mean) |> 
  select(Date, total_co2) |> 
  as_tsibble() |> 
  bind_rows(combined_monthly) |> 
  arrange(Date)

full_emissions <- pre_1970_ts |> 
  rename(total_co2 = co2_emissions) |> 
  bind_rows(pred_combined) |> 
  arrange(Date)














