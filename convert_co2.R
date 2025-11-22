library(fpp3)
library(tidyverse)

df <- read_csv("C:/Users/tkbar/Downloads/data_2025-11-22.csv", skip = 1)

write_csv(df, "data/annual_co2_icecore.csv")

loa_df <- read_csv("data/mauna_loa_co2.csv")

head(df)

df |> 
  filter(year(`CO2 Date`) > 1895, year(`CO2 Date`) <= 1955) |> 
  add_row(
    `CO2 Date` = as.Date("1956-01-01"),
    `CO2 ppm` = 313
  ) |> 
  add_row(
    `CO2 Date` = as.Date("1957-01-01"),
    `CO2 ppm` = 313
  )


co2_ts <- df |> 
  mutate(
    Year = year(`CO2 Date`),
    Month = list(1:12)
  ) |> 
  unnest(Month) |> 
  mutate(
    Date = as.Date(glue::glue("{Year}-{Month}-01")),
    Date = yearmonth(Date)
  ) |> 
  select(c(Date, `CO2 PPM`)) |> 
  filter(year(Date) > 1895, year(Date) < 1958) |>
  as_tsibble() |> 
  fill_gaps(.start = yearmonth("1896-01"), .end = yearmonth('1958-02')) |> 
  fill(`CO2 PPM`, .direction = "down")


loa_ts <- loa_df |> 
  mutate(
    Date = as.Date(glue::glue("{year}-{month}-01")),
    Date = yearmonth(Date)
  ) |> 
  rename(`CO2 PPM` = average) |> 
  select(Date, `CO2 PPM`) |> 
  as_tibble()

co2_ppm <- bind_rows(co2_ts, loa_ts)

write_csv(co2_ppm, "data/full_co2_ppm.csv")
