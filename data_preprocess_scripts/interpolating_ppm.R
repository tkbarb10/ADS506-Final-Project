library(zoo)

mauna_loa <- read_csv("data/mauna_loa_co2.csv")

loa_ts <- mauna_loa |> 
  mutate(
    Date = make_yearmonth(year = year, month = month)
  ) |> 
  rename(co2_ppm = average) |> 
  select(Date, co2_ppm) |> 
  as_tsibble()

seasonal_trend <- mauna_loa |> 
  mutate(
    Date = make_yearmonth(year = year, month = month)
    ) |> 
  select(Date, average) |> 
  as_tsibble() |> 
  model(STL()) |> 
  components() |> 
  as_tibble() |> 
  group_by(month(Date)) |> 
  summarise(co2_season = mean(season_year)) |> 
  rename(month = `month(Date)`)

icecore <- read_csv("data/annual_co2_icecore.csv")

full_ts <- icecore |> 
  mutate(
    Date = yearmonth(`CO2 Date`)
  ) |> 
  rename(co2 = `CO2 PPM`) |> 
  select(Date, co2) |> 
  complete(Date = seq(min(Date), max(Date), by = 1)) |> 
  filter(between(year(Date), 1850, 1957)) |> 
  complete(Date = seq(min(Date), yearmonth("1958-02"), by = 1)) |> 
  as_tsibble() |> 
  mutate(
    co2_ppm = na.spline(co2)
  ) |> 
  mutate(month = month(Date)) |> 
  left_join(seasonal_trend, by = "month") |> 
  mutate(
    scale_factor = co2_ppm / mean(mauna_loa$average),
    final_co2 = co2_ppm + (co2_season * scale_factor)
  ) |> 
  select(Date, final_co2) |> 
  rename(co2_ppm = final_co2) |> 
  bind_rows(loa_ts)

full_ts |> 
  model(STL()) |> 
  components() |> 
  autoplot()

write_rds(full_ts, "data/interpolated_co2_ppm.rds")
