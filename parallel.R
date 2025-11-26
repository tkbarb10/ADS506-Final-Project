library(future) # Required for parallel processing

# 1. Turn on Parallel Processing
# This uses all your CPU cores. 
# 'multisession' works on Windows, Mac, and Linux.
plan(multisession) 

# 2. Run the streamlined model
# Note: I removed the p=0:3 ranges. 
# 'stepwise = TRUE' is the default behavior when you don't specify ranges.
arima_fit <- cv_data |> 
  model(
    arima_lag_ppm = ARIMA(actual_temp ~ lagged_ppm + el_nino + la_nina + TSI),
    
    arima_lag_co2 = ARIMA(actual_temp ~ lagged_co2_ten + el_nino + la_nina + TSI),
    
    arima_ppm     = ARIMA(actual_temp ~ co2 + el_nino + la_nina + TSI)
  )

# 3. Turn off parallel processing when done (good practice)
plan(sequential)
