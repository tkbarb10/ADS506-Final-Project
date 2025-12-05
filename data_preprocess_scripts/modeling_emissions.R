library(tempdisagg)

co2_ppm <- read_csv("C:/Users/tkbar/OneDrive/Desktop/ADS 506/annual_co2.csv")
co2_land <- read_csv("C:/Users/tkbar/OneDrive/Desktop/ADS 506/average_co2_land.csv")

co2_subset <- co2_ppm |> 
  mutate(Date = yearmonth(as.Date(paste0(Year, "-01-01"), format = "%Y-%m-%d"))) |>
  select(Date, World) |> 
  as_tsibble()

co2_land_subset <- co2_land |> 
  mutate(Date = yearmonth(as.Date(paste0(Year, "-01-01"), format = "%Y-%m-%d"))) |>
  select(Date, Average) |> 
  as_tsibble()

# interpolating monthly estimates
# 
agg_model <- td(co2_subset ~ 1,
                to = 'monthly',
                method = 'denton-cholette')

land_model <- td(co2_land_subset ~ 1,
                 to = 'monthly',
                 method = 'denton-cholette')

new_co2 <- predict(agg_model)
new_land <- predict(land_model)

# converting to gigagrams

new_land$co2_emissions <- new_land$value * 3.664 * 1000

new_co2$total <- (new_co2$value + new_land$value) * 3.664 * 1000



