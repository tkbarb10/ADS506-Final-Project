library(fpp3)
library(tidyverse)

new_df <- new_df |> 
  select(-enso_smooth) |> 
  mutate(
    enso_smooth_12 = slider::slide_dbl(ENSO, mean, .before = 11, complete = TRUE),
    enso_smooth_6 = slider::slide_dbl(ENSO, mean, .before = 5, complete = TRUE),
    enso_smooth_3 = slider::slide_dbl(ENSO, mean, .before = 2, complete = TRUE),
    enso_delay = lag(enso_smooth_3, 4),
    rf_co2 = 5.35 * log(co2_ppm / 278)
  )

df <- read_tsv("C:/Users/tkbar/Downloads/volcano-events-2025-12-03_09-46-47_-0800.tsv")

volcano <- df |> 
  select(Year, Mo, Name, VEI) |> 
  drop_na() |> 
  filter(VEI > 0) |> 
  mutate(
    Date = make_yearmonth(year = Year, month = Mo),
    
  ) |> 
  select(Date, VEI)

df |> 
  filter(Year == 2022)


# 1. CLEAN THE VOLCANO DATA
# Filter out small events (VEI < 4). 
# CRITICAL NOTE: If your data includes 2022, you might want to manually exclude 
# Hunga Tonga here to avoid predicting cooling that didn't happen.
volcano_clean <- volcano %>%
  filter(VEI >= 4)

# 2. DEFINE THE EXPONENTIAL DECAY FUNCTION
calculate_decay <- function(current_date, eruption_table, decay_months = 36, tau = 12) {
  
  # Ensure dates are proper Date objects
  curr_d <- as.Date(current_date)
  erup_d <- as.Date(eruption_table$Date)
  
  # Window Start (Look back 3 years)
  window_start <- curr_d - months(decay_months)
  
  # Find active eruptions (occurred within the last 36 months)
  active_indices <- which(erup_d <= curr_d & erup_d > window_start)
  
  if (length(active_indices) == 0) {
    return(0)
  }
  
  # Calculate time elapsed in months
  # We use the interval operator %--% from lubridate
  time_span <- lubridate::interval(erup_d[active_indices], curr_d)
  months_elapsed <- time_span %/% months(1)
  
  # --- THE FIX: EXPONENTIAL DECAY FORMULA ---
  # Formula: Strength = Peak * e^(-t / tau)
  # At t=0, strength is 1. At t=12, strength is 0.36. At t=36, strength is 0.04.
  strengths <- exp(-months_elapsed / tau)
  
  # Sum them up (in case multiple volcanoes overlap)
  return(sum(strengths))
}

# 3. APPLY TO TSIBBLE
climate_tsibble_updated <- lagged %>%
  mutate(
    volcano_forcing = map_dbl(Date, calculate_decay, eruption_table = volcano_clean)
  )




