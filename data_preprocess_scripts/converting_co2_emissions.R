library(fpp3)
library(tidyverse)
library(readxl)


# all values in annual are in million tons of carbon per year.  For CO2, multiply by 3.664
# starts at 1850 

annual_co2 <- read_csv("C:/Users/tkbar/OneDrive/Desktop/ADS 506/annual_co2.csv")

co2_land_use <- read_csv("C:/Users/tkbar/OneDrive/Desktop/ADS 506/average_co2_land.csv")

# all values are in gigagrams which is equal to 1000 metric tons
# 1970 to 2024

monthly_co2 <- read_excel("C:/Users/tkbar/Downloads/IEA_EDGAR_CO2_m_1970_2024/IEA_EDGAR_CO2_m_1970_2024.xlsx", sheet = "TOTALS BY COUNTRY", skip = 9)

monthly_co2_bio <- read_excel("C:/Users/tkbar/Downloads/EDGAR_CO2bio_m_1970_2024/EDGAR_CO2bio_m_1970_2024.xlsx", sheet = "TOTALS BY COUNTRY", skip = 9)

month_co2 <- monthly_co2 |> 
  select(c(Year:Dec)) |> 
  group_by(Year) |> 
  summarize(across(.cols = c(Jan:Dec), .fns = sum)) |> 
  pivot_longer(cols = c(Jan:Dec), names_to = 'Month', values_to = "CO2_Emissions") |> 
  mutate(
    date_string = paste(Year, Month, "01", sep = "-"),
    Date = yearmonth(date_string)
  ) |> 
  select(c(Date, CO2_Emissions))

month_co2_bio <- monthly_co2_bio |> 
  select(c(Year:Dec)) |> 
  group_by(Year) |> 
  summarize(across(.cols = c(Jan:Dec), .fns = sum)) |> 
  pivot_longer(cols = c(Jan:Dec), names_to = 'Month', values_to = "CO2_Bio_Emissions") |> 
  mutate(
    date_string = paste(Year, Month, "01", sep = "-"),
    Date = yearmonth(date_string)
  ) |> 
  select(c(Date, CO2_Bio_Emissions))

combined_monthly <- month_co2 |> 
  left_join(month_co2_bio)

final_monthly <- combined_monthly |> 
  mutate(Total_CO2 = CO2_Bio_Emissions + CO2_Emissions) |> 
  select(c(Date, Total_CO2))

write_rds(final_monthly, "data/legit_monthly_co2.rds")


annual_convert <- annual_co2 |> 
  filter(Year < 1970) |> 
  uncount(12, .id = "Month") |> 
  mutate(
    Date = make_yearmonth(year = Year, month = Month),
    monthly_co2 = (World * 3664) / 12
  ) |> 
  select(Date, monthly_co2) |> 
  as_tsibble(index = Date)

annual_bio_convert <- co2_land_use |> 
  filter(Year <= 1969) |> 
  uncount(12, .id = "Month") |> 
  mutate(
    Date = make_yearmonth(year = Year, month = Month),
    monthly_bio_co2 = (Average * 3664) / 12
  ) |> 
  select(Date, monthly_bio_co2) |> 
  as_tsibble(index = Date)


combined_monthly_convert <- annual_convert |> 
  left_join(annual_bio_convert)

earlier_co2_data <- combined_monthly_convert |> 
  mutate(Total_CO2 = (monthly_bio_co2 + monthly_co2)) |> 
  select(c(Date, Total_CO2))

write_rds(earlier_co2_data, "data/early_co2_monthly_1850.rds")
