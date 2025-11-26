co2_emissions <- read_rds("data/co2_emissions_1850.rds")

global_temps <- read_csv("data/converted_global_temp.csv")

co2_csv <- read_csv("data/full_co2_ppm_1850.csv")

tsi <- read_csv("C:/Users/tkbar/OneDrive/Desktop/ADS 506/tsi_monthly_combined_all.csv")

ch4 <- read_csv("C:/Users/tkbar/Downloads/data_2025-11-22 (1).csv", skip = 1)


full_ch4 <- ch4 |> 
  filter(year(`CH4 Date`) >= 1849) |> 
  mutate(Date = yearmonth(`CH4 Date`)) |> 
  rename(ch4 = `CH4 Value`) |> 
  select(Date, ch4) |> 
  as_tsibble() |> 
  fill_gaps(.start = yearmonth("1850-01"), .end = yearmonth('2025-09')) |> 
  fill(ch4, .direction = "down")

full_tsi <- tsi |> 
  mutate(Date = yearmonth(time)) |> 
  select(Date, TSI) |> 
  as_tsibble() |> 
  fill_gaps(.start = yearmonth("1850-01"), .end = yearmonth('2025-09')) |> 
  fill(TSI, .direction = "updown")


co2_ts <- co2_csv |> mutate(Date = yearmonth(Date)) |> rename(co2_ppm = `CO2 PPM`) |> select(Date, co2_ppm) |> as_tsibble()

full_reg <- global_temps |> 
  select(Date, actual_temp) |> 
  mutate(Date = yearmonth(Date)) |> 
  as_tsibble() |> 
  left_join(co2_emissions, by = 'Date') |> 
  left_join(full_ch4, by = "Date") |> 
  left_join(full_tsi, by = "Date") |> 
  left_join(co2_ts, by = "Date")

write_rds(full_reg, "data/full_data_regressors_1850.rds")


