library(shiny)
library(fpp3)
library(tidyverse)
library(gt)
library(bslib)
library(bsicons)
library(here)

# --- 1. DATA LOADING (Load once) ---
# Ensure these files exist in your 'data/' folder
time_series <- read_rds(here("data/time_series_visual.rds"))
actual_temps <- read_rds(here("data/converted_global_temp.rds"))
modeling_data <- read_rds(here("data/lagged_nino_predictors.rds"))

# --- 2. PRE-COMPUTATION (Models & Baseline) ---

# Fit models once on startup to save time
# (Assuming modeling_data is a tsibble or suitable dataframe)
temp_model <- modeling_data |> 
  filter(year(Date) > 1957) |> 
  model(
    temp_arima = ARIMA(actual_temp ~ co2_ppm + ENSO + fourier(K = 3) + PDQ(0, 0, 0))
  )

ppm_model <- modeling_data |> 
  filter(year(Date) > 1957) |> 
  model(
    ppm_arima = ARIMA(co2_ppm ~ 0 + aggregate_emissions + ENSO + PDQ(0, 1, 1) + pdq(1, 1, 1))
  )

# Pre-calculate "Business as Usual" Baseline (10 Years, 0% Growth, Neutral ENSO)
# We do this here so we don't re-run it every time the user clicks a button.
last_historical_row <- modeling_data |> tail(1)
start_date <- last_historical_row$Date
base_co2_flow <- last_historical_row$Total_CO2
base_emissions_stock <- last_historical_row$aggregate_emissions

# Create 10-year baseline future data
baseline_months <- 10 * 12
baseline_future <- tibble(
  Date = start_date + (1:baseline_months),
  ENSO = 0 # Stable
) |>
  mutate(
    growth_factor = 1, # 0% growth
    Total_CO2 = base_co2_flow * (growth_factor ^ row_number()),
    cumulative_additions_Gt = cumsum(Total_CO2) * 1e-6,
    aggregate_emissions = base_emissions_stock + cumulative_additions_Gt
  )

# Trick to handle lags: bind last row of history
baseline_linked <- bind_rows(modeling_data |> tail(1), baseline_future) |> 
  filter(Date > start_date)

# Forecast Baseline PPM
baseline_ppm_fc <- ppm_model |> forecast(new_data = baseline_linked)

# Forecast Baseline Temp
baseline_final_data <- baseline_linked |> mutate(co2_ppm = baseline_ppm_fc$.mean)
baseline_temp_fc <- temp_model |> forecast(new_data = baseline_final_data)

# Cache this object for the server
cached_baseline <- as_tibble(baseline_temp_fc) |> 
  select(Date, Baseline_Temp = .mean)


# --- 3. UI DEFINITION ---
ui <- page_navbar(
  title = "Global Warming Trajectory Simulator",
  theme = bs_theme(bootswatch = "flatly", version = 5),
  
  # --- Tab 1: Explore ---
  nav_panel(
    title = "Historical Explorer",
    layout_sidebar(
      sidebar = sidebar(
        radioButtons("hist_var", "Select Variable:",
                     choices = c("Global Temperature" = "temp", 
                                 "CO2 Emissions" = "emissions", 
                                 "CO2 Concentration" = "ppm")),
        checkboxInput("show_stl", "Show Trend & Seasonality (STL)", value = FALSE),
        sliderInput("date_range", "Date Range:",
                    min = as.Date("1950-01-01"), max = Sys.Date(),
                    value = c(as.Date("1980-01-01"), Sys.Date()),
                    timeFormat = "%Y")
      ),
      card(
        card_header("Historical Trends Analysis"),
        plotOutput("hist_plot", height = "500px"),
        card_footer(
          "Note: For CO2 metrics, seasonality remains relatively constant. The primary driver of increase is the long-term trend."
        )
      )
    )
  ),
  
  # --- Tab 2: Forecast ---
  nav_panel(
    title = "Forecast & Scenarios",
    layout_sidebar(
      sidebar = sidebar(
        width = 350,
        h4("Scenario Builder"),
        
        # Scope Selector (Visual only for now)
        radioButtons("scope_select", "Scope:",
                     choices = c("Global View", "Country View")),
        
        # Conditional Panel for Country (Disabled/Coming Soon)
        conditionalPanel(
          condition = "input.scope_select == 'Country View'",
          selectInput("country_select", "Select Country:", choices = c("United States")),
          helpText(bs_icon("info-circle"), "Country-level granularity coming in v2.0. Switching to Global extrapolation...")
        ),
        
        hr(),
        
        # Time Horizon
        radioButtons("fc_years", "Forecast Horizon:",
                     choices = c("5 Years" = 5, "10 Years" = 10), inline = TRUE),
        
        # ENSO Selector
        selectInput("enso_scen", "Climate Pattern (ENSO):",
                    choices = c("Stable / Neutral" = 0,
                                "Strong El Niño (+2.0)" = 2,
                                "Strong La Niña (-2.0)" = -2)),
        
        # The Lever
        sliderInput("growth_slider", "Adjust Projected CO2 Emissions:",
                    min = -10, max = 10, value = 0, step = 0.5, post = "%"),
        helpText("0% = Maintain current monthly emission levels."),
        
        hr(),
        
        # Action Button
        actionButton("run_scenario", "Run Scenario", class = "btn-primary w-100", icon = icon("play"))
      ),
      
      # Main Forecast Layout
      card(
        card_header("Projected Global Temperature Anomalies"),
        plotOutput("forecast_plot", height = "400px"),
        layout_columns(
          value_box(
            title = "Temperature Impact",
            value = textOutput("impact_delta"),
            showcase = bs_icon("thermometer-half"),
            theme = "primary" # Dynamic color logic in server
          ),
          value_box(
            title = "Scenario Description",
            value = textOutput("scenario_desc"),
            showcase = bs_icon("graph-up"),
            theme = "light"
          )
        )
      )
    )
  ),
  
  # --- Tab 3: Impact Report ---
  nav_panel(
    title = "Impact Report",
    layout_sidebar(
      sidebar = sidebar(
        radioButtons("report_granularity", "Granularity:",
                     choices = c("Yearly Averages", "Monthly Data")),
        downloadButton("download_data", "Download .csv")
      ),
      card(
        card_header("Detailed Scenario Comparison"),
        gt_output("impact_table")
      )
    )
  )
)


# --- 4. SERVER LOGIC ---
server <- function(input, output, session) {
  
  # --- Tab 1: Historical Logic ---
  
  filtered_history <- reactive({
    req(input$date_range)
    time_series |> 
      filter(Date >= input$date_range[1], Date <= input$date_range[2])
  })
  
  output$hist_plot <- renderPlot({
    data <- filtered_history()
    
    # Select Variable
    if (input$hist_var == "temp") {
      # For temp, we might use actual_temps or time_series depending on your data structure
      # Assuming 'time_series' has 'Monthly_Anomaly' as per your template
      p_data <- data |> select(Date, Value = Monthly_Anomaly)
      title_txt <- "Global Temperature Anomalies"
    } else if (input$hist_var == "emissions") {
      p_data <- data |> select(Date, Value = total_co2) # Adjust column name if needed
      title_txt <- "Global CO2 Emissions"
    } else {
      p_data <- data |> select(Date, Value = co2_ppm)
      title_txt <- "CO2 Concentration (PPM)"
    }
    
    # STL Decomposition or Standard Plot
    if (input$show_stl) {
      # Check if enough data points exist for STL
      if(nrow(p_data) < 24) {
        validate("Need at least 2 years of data for Seasonality analysis.")
      }
      
      p_data |> 
        as_tsibble(index = Date) |> 
        model(STL(Value ~ season(window = "periodic"))) |> 
        components() |> 
        autoplot() + 
        theme_minimal() +
        labs(title = paste("STL Decomposition of", title_txt))
      
    } else {
      p_data |> 
        ggplot(aes(x = Date, y = Value)) +
        geom_line(color = "steelblue", size = 1) +
        theme_minimal() +
        labs(title = title_txt, y = "Value", x = "Year")
    }
  })
  
  
  # --- Tab 2: Forecast Logic ---
  
  # Main Scenario Calculator
  # Only runs when "Run Scenario" is clicked
  scenario_data <- eventReactive(input$run_scenario, {
    
    # 1. Setup inputs
    years <- as.numeric(input$fc_years)
    growth_pct <- as.numeric(input$growth_slider)
    enso_val <- as.numeric(input$enso_scen)
    n_months <- years * 12
    
    # 2. Build Future Data
    future_scenario <- tibble(
      Date = start_date + (1:n_months),
      ENSO = enso_val # User defined ENSO scenario
    ) |>
      mutate(
        growth_factor = 1 + (growth_pct / 100),
        # Compound growth based on last historical row
        Total_CO2 = base_co2_flow * (growth_factor ^ row_number()), 
        cumulative_additions_Gt = cumsum(Total_CO2) * 1e-6,
        aggregate_emissions = base_emissions_stock + cumulative_additions_Gt
      )
    
    # 3. Handle lags (bind history)
    # We need just enough history for lags, using tail(1) is usually enough for AR(1) but safer to bind more if needed
    scenario_linked <- bind_rows(modeling_data |> tail(3), future_scenario) |> 
      filter(Date > start_date)
    
    # 4. Forecast PPM (assuming emissions driver)
    # Note: If your PPM model relies on lags of aggregate_emissions, ensure data is sufficient
    ppm_fc <- ppm_model |> forecast(new_data = scenario_linked)
    
    # 5. Forecast Temp using forecasted PPM
    scenario_final <- scenario_linked |> 
      mutate(co2_ppm = ppm_fc$.mean)
    
    temp_fc <- temp_model |> forecast(new_data = scenario_final)
    
    # 6. Combine with Baseline
    # Filter cached baseline to match selected years
    relevant_baseline <- cached_baseline |> 
      filter(Date <= max(future_scenario$Date))
    
    user_results <- as_tibble(temp_fc) |> 
      select(Date, User_Temp = .mean)
    
    combined <- left_join(user_results, relevant_baseline, by = "Date") |> 
      mutate(Difference = User_Temp - Baseline_Temp)
    
    return(combined)
  })
  
  # Render Forecast Plot
  output$forecast_plot <- renderPlot({
    req(scenario_data())
    df <- scenario_data()
    
    # Reshape for plotting
    plot_df <- df |> 
      pivot_longer(cols = c("User_Temp", "Baseline_Temp"), 
                   names_to = "Scenario", values_to = "Temperature")
    
    ggplot(plot_df, aes(x = Date, y = Temperature, color = Scenario)) +
      geom_line(size = 1.2) +
      scale_color_manual(values = c("Baseline_Temp" = "grey50", "User_Temp" = "#d9534f")) +
      theme_minimal(base_size = 14) +
      labs(y = "Temp Anomaly (°C)", x = NULL, color = NULL) +
      theme(legend.position = "top")
  })
  
  # Calculate Impact Delta (Comparison at end of period)
  output$impact_delta <- renderText({
    req(scenario_data())
    df <- scenario_data()
    final_diff <- tail(df$Difference, 1)
    
    sign_char <- ifelse(final_diff > 0, "+", "")
    paste0(sign_char, round(final_diff, 3), " °C")
  })
  
  output$scenario_desc <- renderText({
    paste(input$fc_years, "Year Forecast |", input$growth_slider, "% Emissions Growth")
  })
  
  
  # --- Tab 3: Table Logic ---
  
  output$impact_table <- render_gt({
    req(scenario_data())
    df <- scenario_data()
    
    # Granularity Toggle
    if(input$report_granularity == "Yearly Averages") {
      table_data <- df |> 
        mutate(Year = year(Date)) |> 
        group_by(Year) |> 
        summarize(
          Baseline = mean(Baseline_Temp),
          Scenario = mean(User_Temp),
          Difference = mean(Difference)
        )
    } else {
      table_data <- df |> 
        select(Date, Baseline = Baseline_Temp, Scenario = User_Temp, Difference)
    }
    
    # Logic for Impact Rating
    table_data <- table_data |> 
      mutate(Impact = case_when(
        Difference > 0.05 ~ "Significant Increase",
        Difference > 0 ~ "Slight Increase",
        Difference < -0.05 ~ "Significant Decrease",
        Difference < 0 ~ "Slight Decrease",
        TRUE ~ "Negligible"
      ))
    
    # Create GT Table
    table_data |> 
      gt() |> 
      fmt_number(columns = c(Baseline, Scenario, Difference), decimals = 3) |> 
      tab_spanner(label = "Projected Temperature", columns = c(Baseline, Scenario)) |> 
      data_color(
        columns = Difference,
        fn = scales::col_numeric(
          palette = c("blue", "white", "red"),
          domain = c(-0.1, 0.1) # Adjust domain based on expected variance
        )
      ) |> 
      opt_interactive(use_search = TRUE, use_pagination = TRUE)
  })
  
  # Download Handler
  output$download_data <- downloadHandler(
    filename = function() { paste("impact_report_", Sys.Date(), ".csv", sep = "") },
    content = function(file) {
      write.csv(scenario_data(), file)
    }
  )
}

# --- 5. RUN APP ---
shinyApp(ui, server)