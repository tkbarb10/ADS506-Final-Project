library(shiny)
library(fpp3)
library(tidyverse)
library(gt)
library(bslib)
library(bsicons)
library(slider)
library(here)

# --- 1. CONSTANTS ---
# 1 Gt = 1,000,000 Gg
# 2024 Estimates (Gt/year)
GLOBAL_TERRITORIAL_2024_GT <- 38.68967
GLOBAL_LUC_2024_GT <- 4.583664
GLOBAL_ANNUAL_GT <- GLOBAL_TERRITORIAL_2024_GT + GLOBAL_LUC_2024_GT # ~43.27 Gt/yr

# Convert to Monthly Gg for the ARIMA Model Baseline
# (43.27 * 1,000,000) / 12 = ~3.6 Million Gg/month
GLOBAL_MONTHLY_GG <- (GLOBAL_ANNUAL_GT * 1e6) / 12


# --- 2. DATA LOADING ---
time_series <- read_rds(here("data/time_series_visual.rds"))
actual_temps <- read_rds(here("data/converted_global_temp.rds"))
modeling_data <- read_rds(here("data/lagged_nino_predictors.rds"))

# Load and Pivot Country Data
country_baseline <- tryCatch({
  raw_data <- read_rds(here("country_2024_baseline_Gt.rds"))
  if (nrow(raw_data) == 1 && !"Country" %in% names(raw_data)) {
    raw_data |> 
      pivot_longer(everything(), names_to = "Country", values_to = "Total_Territorial")
  } else {
    raw_data 
  }
}, error = function(e) {
  # Fallback for testing
  tibble(Country = c("United States", "China", "India", "EU"), Total_Territorial = c(5.2, 10.6, 2.8, 3.0))
})


# --- 3. MODEL FITTING (On Startup) ---
temp_model <- modeling_data |> 
  filter(year(Date) > 1957) |> 
  fill_gaps() |> 
  model(
    temp_arima = ARIMA(actual_temp ~ co2_ppm + ENSO + fourier(K = 3) + PDQ(0, 0, 0))
  )

ppm_model <- modeling_data |> 
  filter(year(Date) > 1957) |> 
  fill_gaps() |> 
  model(
    ppm_arima = ARIMA(co2_ppm ~ 0 + aggregate_emissions + ENSO + PDQ(0, 1, 1) + pdq(1, 1, 1))
  )

# --- Pre-calculate Standard Baseline ---
last_hist_row <- modeling_data |> filter(!is.na(Total_CO2)) |> tail(1)
start_date <- last_hist_row$Date
base_stock <- last_hist_row$aggregate_emissions

# Create 20-year Standard Baseline
baseline_20yr <- tibble(
  Date = start_date + (1:240),
  ENSO = 0, # Stable
  Total_CO2 = GLOBAL_MONTHLY_GG # Correct Unit: Monthly Gg
) |>
  mutate(
    # Model uses Gt for aggregate stock (1 Gg = 1e-6 Gt)
    aggregate_emissions = base_stock + (cumsum(Total_CO2) * 1e-6)
  ) |>
  as_tsibble(index = Date)

baseline_linked <- bind_rows(modeling_data |> tail(3), baseline_20yr) |> filter(Date > start_date)

# Forecast Baseline
base_ppm_fc <- ppm_model |> forecast(new_data = baseline_linked)
base_temp_fc <- temp_model |> forecast(new_data = baseline_linked |> mutate(co2_ppm = base_ppm_fc$.mean))

cached_baseline <- as_tibble(base_temp_fc) |> 
  select(Date, Baseline_Temp = .mean)


# --- 4. UI DEFINITION ---
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
        checkboxInput("show_stl", "Show Trend/Seasonality (STL)", value = FALSE),
        sliderInput("date_range", "Date Range:",
                    min = as.Date("1850-01-01"), max = Sys.Date(),
                    value = c(as.Date("1950-01-01"), Sys.Date()), timeFormat = "%Y")
      ),
      card(
        card_header("Historical Trends Analysis"),
        plotOutput("hist_plot", height = "500px", hover = hoverOpts(id = "hist_hover")),
        card_footer(uiOutput("hist_info_ui"))
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
        radioButtons("scope_select", "Scope:", choices = c("Global View", "Country View")),
        conditionalPanel(
          condition = "input.scope_select == 'Country View'",
          selectInput("country_select", "Select Country:", choices = NULL),
          uiOutput("country_snapshot_ui")
        ),
        hr(),
        radioButtons("fc_years", "Forecast Horizon:", choices = c("5 Years" = 5, "10 Years" = 10, "15 Years" = 15, "20 Years" = 20), inline = TRUE),
        selectInput("enso_scen", "Climate Pattern (ENSO):", 
                    choices = c("Stable / Neutral" = 0, "Strong El Niño (+2.0)" = 2, "Strong La Niña (-2.0)" = -2)),
        hr(),
        radioButtons("scenario_mode", "Emission Adjustment Mode:", 
                     choices = c("Percentage Growth (Annual)" = "pct", "Absolute Addition (Annual)" = "abs")),
        conditionalPanel(
          condition = "input.scenario_mode == 'pct'",
          sliderInput("growth_slider", "Annual Growth Rate:", min = -10, max = 10, value = 0, step = 0.5, post = "%"),
          helpText("Adjusts the selected scope's emissions flow.")
        ),
        conditionalPanel(
          condition = "input.scenario_mode == 'abs'",
          numericInput("abs_amount", "Annual Addition Amount:", value = 0),
          selectInput("abs_unit", "Unit:", choices = c("Gigagrams (Gg)" = "Gg", "Gigatons (Gt)" = "Gt", "Metric Tons" = "Tonne", "US Tons" = "Ton"))
        ),
        hr(),
        actionButton("run_scenario", "Run Scenario", class = "btn-primary w-100", icon = icon("play"))
      ),
      card(
        card_header("Projected Global Temperature Anomalies"),
        plotOutput("forecast_plot", height = "400px", hover = hoverOpts(id = "fc_hover")),
        div(style = "background-color: #f8f9fa; padding: 10px; border-radius: 5px; text-align: center;",
            strong("Nearest Forecast Point: "), textOutput("fc_tooltip_text", inline = TRUE)
        ),
        layout_columns(
          value_box(title = "Temp Difference (End of Period)", value = textOutput("impact_delta"), showcase = bs_icon("thermometer-half"), theme = "primary"),
          value_box(title = "Cumulative Emissions Change", value = textOutput("avoided_emissions"), showcase = bs_icon("cloud-slash"), theme = "light")
        )
      )
    )
  ),
  
  # --- Tab 3: Impact Report ---
  nav_panel(
    title = "Impact Report",
    layout_sidebar(
      sidebar = sidebar(
        radioButtons("report_granularity", "Granularity:", choices = c("Yearly Averages", "Monthly Data")),
        downloadButton("download_data", "Download .csv")
      ),
      card(card_header("Detailed Scenario Comparison"), gt_output("impact_table"))
    )
  )
)


# --- 5. SERVER LOGIC ---
server <- function(input, output, session) {
  
  # Update Country Choices
  observe({
    if(!is.null(country_baseline)) {
      updateSelectInput(session, "country_select", choices = country_baseline$Country)
    }
  })
  
  # Snapshot UI
  output$country_snapshot_ui <- renderUI({
    req(input$country_select)
    row <- country_baseline |> filter(Country == input$country_select)
    if(nrow(row) == 0) return(NULL)
    pct_share <- (row$Total_Territorial / GLOBAL_TERRITORIAL_2024_GT) * 100
    div(style = "margin-top: 10px; padding: 10px; background-color: #e9ecef; border-radius: 5px; font-size: 0.9em;",
        strong(input$country_select), " contributes ", strong(sprintf("%.4f%%", pct_share)),
        " of global territorial emissions,", br(), em("according to 2024 emission levels.")
    )
  })
  
  # --- Tab 1: Historical Logic ---
  output$hist_plot <- renderPlot({
    req(input$date_range)
    min_year <- year(input$date_range[1]); max_year <- year(input$date_range[2])
    d <- time_series |> filter(year(Date) >= min_year, year(Date) <= max_year)
    
    if (input$show_stl) {
      if(input$hist_var == "emissions") stl_d <- d |> filter(year(Date) > 1969) |> select(Date, Value = total_co2)
      else if(input$hist_var == "ppm") stl_d <- d |> filter(year(Date) > 1957) |> select(Date, Value = co2_ppm)
      else stl_d <- actual_temps |> filter(year(Date) >= min_year, year(Date) <= max_year) |> select(Date, Value = actual_temp)
      stl_d |> fill_gaps() |> model(STL(Value)) |> components() |> autoplot() + theme_minimal() + labs(title = "STL Decomposition")
    } else {
      if (input$hist_var == "emissions") {
        d |> as_tibble() |> group_by(Year = year(Date)) |> summarize(sum = sum(total_co2, na.rm = TRUE)) |> 
          mutate(Date = make_date(Year)) |> ggplot(aes(x = Date, y = sum)) + geom_line(color = "black") +
          labs(title = "CO2 Emissions", x = "Date", subtitle = "Annual Total (Gg)") + theme_minimal()
      } else if (input$hist_var == "ppm") {
        d |> mutate(ppm_slide = slider::slide_dbl(co2_ppm, mean, .before = 12, .complete = TRUE)) |> 
          autoplot(ppm_slide) + labs(title = "CO2 Concentration (Rolling Annual)", y = "CO2 PPM", x = "Date") + theme_minimal()
      } else {
        d |> ggplot(aes(x = Date, y = Monthly_Anomaly)) + geom_col(fill = "steelblue4") +
          labs(title = "Global Temperature", subtitle = "Anomaly vs Baseline", x = 'Date', y = "Difference") + theme_minimal()
      }
    }
  })
  
  # Explore Tooltip
  output$hist_info_ui <- renderUI({
    req(input$hist_hover)
    if(input$show_stl) return(NULL)
    min_year <- year(input$date_range[1]); max_year <- year(input$date_range[2])
    d <- time_series |> filter(year(Date) >= min_year, year(Date) <= max_year)
    
    if (input$hist_var == "emissions") {
      plot_df <- d |> as_tibble() |> group_by(Year = year(Date)) |> summarize(Value = sum(total_co2, na.rm=TRUE)) |> mutate(Date = make_date(Year))
    } else if (input$hist_var == "ppm") {
      plot_df <- d |> mutate(Value = slider::slide_dbl(co2_ppm, mean, .before = 12, .complete = TRUE)) |> as_tibble() |> select(Date, Value)
    } else {
      plot_df <- d |> as_tibble() |> select(Date, Value = Monthly_Anomaly) |> mutate(Date = as.Date(Date))
    }
    near <- nearPoints(plot_df, input$hist_hover, xvar = "Date", yvar = "Value", maxpoints = 1)
    if(nrow(near) == 0) return(div("Hover over data..."))
    div(strong("Date: "), format(near$Date, "%Y-%m"), " | ", strong("Value: "), round(near$Value, 2))
  })
  
  
  # --- Tab 2: Forecast Logic ---
  
  scenario_data <- eventReactive(input$run_scenario, {
    req(input$fc_years, input$enso_scen)
    
    years <- as.numeric(input$fc_years)
    months_horizon <- years * 12
    enso_val <- as.numeric(input$enso_scen)
    
    # 1. Identify Target Flow (Annual Gt)
    #    This is the amount of emissions we are modifying
    if (input$scope_select == "Country View") {
      req(input$country_select)
      c_row <- country_baseline |> filter(Country == input$country_select)
      target_annual_gt <- if(nrow(c_row) > 0) c_row$Total_Territorial else 0
      rest_annual_gt <- GLOBAL_ANNUAL_GT - target_annual_gt
    } else {
      target_annual_gt <- GLOBAL_ANNUAL_GT
      rest_annual_gt <- 0
    }
    
    # 2. Apply Scenario to Target
    #    Create a vector of "Target Annual Gt" for each future month
    
    baseline_target_vec <- rep(target_annual_gt, months_horizon)
    
    if (input$scenario_mode == "pct") {
      req(input$growth_slider)
      annual_pct <- as.numeric(input$growth_slider) / 100
      # Compound Growth: Target * (1+r)^(t_years)
      # monthly multiplier = (1+r)^(1/12)
      monthly_multiplier <- (1 + annual_pct)^(1/12)
      modified_target_vec <- target_annual_gt * (monthly_multiplier ^ (1:months_horizon))
      
    } else {
      req(input$abs_amount, input$abs_unit)
      amt <- as.numeric(input$abs_amount); unit <- input$abs_unit
      # Convert Absolute Input to Gt
      addition_gt <- case_when(
        unit == "Gg" ~ amt * 1e-6, unit == "Gt" ~ amt,
        unit == "Tonne" ~ amt * 1e-9, unit == "Ton" ~ amt * 9.07185e-10
      )
      # Constant absolute addition to the annual flow
      modified_target_vec <- target_annual_gt + addition_gt
    }
    
    # 3. Reassemble and Convert to Monthly Gg for Model
    #    New Annual Gt = Rest + Modified Target
    final_global_annual_gt_vec <- rest_annual_gt + modified_target_vec
    
    #    Convert to Monthly Gg: (Gt * 1e6) / 12
    final_global_monthly_gg_vec <- (final_global_annual_gt_vec * 1e6) / 12
    
    # 4. Construct Future Data
    future_scenario <- tibble(
      Date = start_date + (1:months_horizon),
      ENSO = enso_val,
      Total_CO2 = final_global_monthly_gg_vec # Now in correct unit (Gg)
    ) |>
      mutate(
        # Aggregate uses Gt (1 Gg = 1e-6 Gt)
        aggregate_emissions = base_stock + (cumsum(Total_CO2) * 1e-6)
      ) |>
      as_tsibble(index = Date)
    
    # 5. Forecast
    scenario_linked <- bind_rows(modeling_data |> tail(3), future_scenario) |> filter(Date > start_date)
    ppm_fc <- ppm_model |> forecast(new_data = scenario_linked)
    temp_fc <- temp_model |> forecast(new_data = scenario_linked |> mutate(co2_ppm = ppm_fc$.mean))
    
    # 6. Calculate Deltas
    user_results <- as_tibble(temp_fc) |> select(Date, User_Temp = .mean)
    
    # Calculate Gt Difference (Cumulative)
    # Difference between Modified Target and Baseline Target (Annual Gt)
    annual_diff_gt <- modified_target_vec - baseline_target_vec
    # Monthly accumulation (Annual Rate / 12)
    cumulative_gt_diff <- cumsum(annual_diff_gt / 12)
    
    combined <- left_join(user_results, cached_baseline, by = "Date") |> 
      mutate(
        Difference = User_Temp - Baseline_Temp,
        Cumulative_Gt_Change = cumulative_gt_diff
      )
    
    return(combined)
  })
  
  output$forecast_plot <- renderPlot({
    req(scenario_data())
    plot_df <- scenario_data() |> mutate(Date = as.Date(Date)) |> 
      pivot_longer(cols = c("User_Temp", "Baseline_Temp"), names_to = "Scenario", values_to = "Temperature")
    
    ggplot(plot_df, aes(x = Date, y = Temperature, color = Scenario)) +
      geom_line(linewidth = 1.2) +
      scale_color_manual(values = c("Baseline_Temp" = "grey50", "User_Temp" = "#d9534f")) +
      theme_minimal(base_size = 14) +
      labs(y = "Temp Anomaly (°C)", x = NULL, color = NULL) +
      theme(legend.position = "top")
  })
  
  output$fc_tooltip_text <- renderText({
    req(input$fc_hover, scenario_data())
    plot_df <- scenario_data() |> mutate(Date = as.Date(Date)) |> 
      pivot_longer(cols = c("User_Temp", "Baseline_Temp"), names_to = "Scenario", values_to = "Temperature")
    near <- nearPoints(plot_df, input$fc_hover, xvar = "Date", yvar = "Temperature", maxpoints = 1)
    if (nrow(near) == 0) return("Hover over lines...")
    val <- round(as.numeric(near$Temperature), 3)
    lbl <- ifelse(near$Scenario == "User_Temp", "User", "Base")
    paste0(near$Date, " | ", lbl, ": ", val, " °C")
  })
  
  output$impact_delta <- renderText({
    req(scenario_data())
    final_diff <- as.numeric(tail(scenario_data()$Difference, 1))
    if (length(final_diff) == 0) return("Calculating...")
    paste0(ifelse(final_diff > 0, "+", ""), sprintf("%.5f", final_diff), " °C")
  })
  
  output$avoided_emissions <- renderText({
    req(scenario_data())
    total_change <- as.numeric(tail(scenario_data()$Cumulative_Gt_Change, 1))
    if (length(total_change) == 0) return("Calculating...")
    if (total_change < 0) paste0(round(abs(total_change), 2), " Gt Avoided") else paste0(round(abs(total_change), 2), " Gt Added")
  })
  
  output$impact_table <- render_gt({
    req(scenario_data())
    df <- scenario_data()
    
    if(input$report_granularity == "Yearly Averages") {
      table_data <- df |> mutate(Year = year(Date)) |> group_by(Year) |> 
        summarize(Baseline = mean(Baseline_Temp), Scenario = mean(User_Temp), Difference = mean(Difference), Cumulative_Gt_Change = last(Cumulative_Gt_Change))
    } else {
      table_data <- df |> select(Date, Baseline = Baseline_Temp, Scenario = User_Temp, Difference, Cumulative_Gt_Change)
    }
    
    table_data |> gt() |> 
      fmt_number(columns = c(Baseline, Scenario), decimals = 3) |> 
      fmt_number(columns = c(Difference), decimals = 5) |> 
      fmt_number(columns = c(Cumulative_Gt_Change), decimals = 2) |> 
      data_color(columns = Difference, fn = scales::col_numeric(palette = c("blue", "white", "red"), domain = c(-0.1, 0.1)))
  })
  
  output$download_data <- downloadHandler(
    filename = function() { paste("impact_report_", Sys.Date(), ".csv", sep = "") },
    content = function(file) { write.csv(scenario_data(), file) }
  )
}

shinyApp(ui, server)