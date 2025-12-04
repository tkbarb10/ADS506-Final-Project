library(readxl)

new_df <- read_excel("C:/Users/tkbar/Downloads/monthly_ens_oni_anomaly_jan_1850-feb_2024.xlsx", skip = 1, col_types = "numeric")

# 1. Get the row index where Year is 2024
row_index <- which(new_df$Year == 2024)

# 2. Assign the new values to the specific columns for that row
if (length(row_index) == 1) { # Ensure only one row is matched
  new_df[row_index, "MAR"] <- 1.13
  new_df[row_index, "APR"] <- 0.77
  new_df[row_index, "MAY"] <- 0.23
  new_df[row_index, "JUN"] <- 0.17
  new_df[row_index, "JUL"] <- 0.04
  new_df[row_index, "AUG"] <- -0.12
  new_df[row_index, "SEP"] <- -0.26
  new_df[row_index, "OCT"] <- -0.27
  new_df[row_index, "NOV"] <- -0.25
  new_df[row_index, "DEC"] <- -0.60
}


new_df_long <- new_df |>
  pivot_longer(
    cols = c(JAN:DEC),
    names_to = "Month",
    values_to = "ENSO"
  ) |>
  mutate(
    # Combine Year, Month, and day into a single string
    Date_String = glue::glue("{Year}-{Month}-01"),
    
    # Convert the string to a date, specifying the format
    # %Y = 4-digit year, %b = abbreviated month name (JAN, FEB), %d = day of the month
    Date = yearmonth(as.Date(Date_String, format = "%Y-%b-%d"))
  ) |>
  # (Optional) Remove the temporary Date_String column
  select(Date, ENSO) |> 
  as_tsibble()

ppm_nino <- ppm_agg |> 
  left_join(new_df_long, by = "Date")

ppm_nino$enso_smooth <- slider::slide_dbl(ppm_nino$ENSO, mean, .before = 12)
