library(shiny)
library(fpp3)
library(tidyverse)
library(gt)
library(bslib)
library(bsicons)
library(slider)
library(here)

# --- 1. DATA LOADING ---
# Load data AS IS (preserving yearmonth index)
time_series <- read_rds(here("data/time_series_visual.rds"))
actual_temps <- read_rds(here("data/converted_global_temp.rds"))
modeling_data <- read_rds(here("data/lagged_external_predictors.rds"))

# --- 2. PRE-COMPUTATION (Models & Baseline) ---

# Fit models using the native tsibble (yearmonth)
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

# Pre-calculate Baseline Constants
last_historical_row <- modeling_data |> 
  filter(!is.na(Total_CO2)) |> 
  tail(1)

start_date <- last_historical_row$Date
base_co2_flow <- last_historical_row$Total_CO2
base_emissions_stock <- last_historical_row$aggregate_emissions

# Create 10-year baseline future data
baseline_months <- 10 * 12
baseline_future <- tibble(
  Date = start_date + (1:baseline_months),
  ENSO = 0
) |>
  mutate(
    growth_factor = 1, 
    Total_CO2 = base_co2_flow, 
    cumulative_additions_Gt = cumsum(Total_CO2) * 1e-6,
    aggregate_emissions = base_emissions_stock + cumulative_additions_Gt
  ) |>
  as_tsibble(index = Date)

# Bind history & Forecast Baseline
baseline_linked <- bind_rows(modeling_data |> tail(3), baseline_future) |> 
  filter(Date > start_date)

baseline_ppm_fc <- ppm_model |> forecast(new_data = baseline_linked)
baseline_final_data <- baseline_linked |> mutate(co2_ppm = baseline_ppm_fc$.mean)
baseline_temp_fc <- temp_model |> forecast(new_data = baseline_final_data)

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
        
        checkboxInput("show_stl", "Show Trend/Seasonality (STL)", value = FALSE),
        
        sliderInput("date_range", "Date Range:",
                    min = as.Date("1850-01-01"), 
                    max = Sys.Date(),
                    value = c(as.Date("1950-01-01"), Sys.Date()),
                    timeFormat = "%Y")
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
        radioButtons("scope_select", "Scope:", choices = c("Global View", "Country View (Coming Soon)")),
        hr(),
        radioButtons("fc_years", "Forecast Horizon:", choices = c("5 Years" = 5, "10 Years" = 10), inline = TRUE),
        selectInput("enso_scen", "Climate Pattern (ENSO):", 
                    choices = c("Stable / Neutral" = 0, "Strong El Niño (+2.0)" = 2, "Strong La Niña (-2.0)" = -2)),
        hr(),
        radioButtons("scenario_mode", "Emission Adjustment Mode:", 
                     choices = c("Percentage Growth (Annual)" = "pct", "Absolute Addition (Annual)" = "abs")),
        conditionalPanel(
          condition = "input.scenario_mode == 'pct'",
          sliderInput("growth_slider", "Annual Growth Rate:", min = -10, max = 10, value = 0, step = 0.5, post = "%")
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
          value_box(title = "Temperature Impact", value = textOutput("impact_delta"), showcase = bs_icon("thermometer-half"), theme = "primary"),
          value_box(title = "Scenario Description", value = textOutput("scenario_desc"), showcase = bs_icon("graph-up"), theme = "light")
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


# --- 4. SERVER LOGIC ---
server <- function(input, output, session) {
  
  # --- Tab 1: Historical Logic ---
  
  output$hist_plot <- renderPlot({
    req(input$date_range)
    
    min_year <- year(input$date_range[1])
    max_year <- year(input$date_range[2])
    
    # Base filter for time_series (used for Emissions/PPM/Visuals)
    d <- time_series |> 
      filter(year(Date) >= min_year, year(Date) <= max_year)
    
    # --- STL MODE ---
    if (input$show_stl) {
      
      if(input$hist_var == "emissions") {
        stl_d <- d |> filter(year(Date) > 1969) |> select(Date, Value = total_co2)
      } else if(input$hist_var == "ppm") {
        stl_d <- d |> filter(year(Date) > 1957) |> select(Date, Value = co2_ppm)
      } else {
        # FIX: Use actual_temps for Temperature STL to capture seasonality
        # And ensure we select the 'actual_temp' column, NOT 'Monthly_Anomaly'
        stl_d <- actual_temps |> 
          filter(year(Date) >= min_year, year(Date) <= max_year) |> 
          select(Date, Value = actual_temp)
      }
      
      # Run STL
      stl_d |> 
        fill_gaps() |> 
        model(STL(Value)) |> 
        components() |> 
        autoplot() +
        theme_minimal() +
        labs(title = "STL Decomposition")
      
    } else {
      # --- VISUAL MODE ---
      
      if (input$hist_var == "emissions") {
        d |> 
          as_tibble() |> 
          group_by(Year = year(Date)) |> 
          summarize(sum = sum(total_co2, na.rm = TRUE)) |> 
          mutate(Date = make_date(Year)) |> 
          ggplot(aes(x = Date, y = sum)) +
          geom_line(color = "black") +
          labs(title = "CO2 Emissions in Gigagrams", x = "Date", subtitle = "Annual Total") +
          theme_minimal()
        
      } else if (input$hist_var == "ppm") {
        d |> 
          mutate(ppm_slide = slider::slide_dbl(co2_ppm, mean, .before = 12, .complete = TRUE)) |> 
          autoplot(ppm_slide) +
          labs(title = "CO2 Concentration on a Rolling Annual Basis", subtitle = "Parts Per Million (ppm)", y = "CO2 PPM", x = "Date") +
          theme_minimal()
        
      } else {
        # Visual Mode for Temp still uses Time Series Anomaly (Visual preference)
        d |> 
          ggplot(aes(x = Date, y = Monthly_Anomaly)) + 
          geom_col(fill = "steelblue4") +
          labs(
            title = "Global Temperature", 
            subtitle = "Departure from 1950-1980 baseline",
            x = 'Date',
            y = "Monthly Difference"
          ) +
          theme_minimal()
      }
    }
  })
  
  # Hover Info (Explore)
  output$hist_info_ui <- renderUI({
    req(input$hist_hover)
    if(input$show_stl) return(NULL) 
    
    min_year <- year(input$date_range[1])
    max_year <- year(input$date_range[2])
    d <- time_series |> filter(year(Date) >= min_year, year(Date) <= max_year)
    
    if (input$hist_var == "emissions") {
      plot_df <- d |> as_tibble() |> group_by(Year = year(Date)) |> 
        summarize(Value = sum(total_co2, na.rm=TRUE)) |> mutate(Date = make_date(Year))
    } else if (input$hist_var == "ppm") {
      plot_df <- d |> mutate(Value = slider::slide_dbl(co2_ppm, mean, .before = 12, .complete = TRUE)) |> 
        as_tibble() |> select(Date, Value)
    } else {
      plot_df <- d |> as_tibble() |> select(Date, Value = Monthly_Anomaly) |> mutate(Date = as.Date(Date))
    }
    
    near <- nearPoints(plot_df, input$hist_hover, xvar = "Date", yvar = "Value", maxpoints = 1)
    if(nrow(near) == 0) return(div("Hover over data..."))
    div(strong("Date: "), format(near$Date, "%Y-%m"), " | ", strong("Value: "), round(near$Value, 2))
  })
  
  
  # --- Tab 2: Forecast Logic ---
  
  scenario_data <- eventReactive(input$run_scenario, {
    
    years <- as.numeric(input$fc_years)
    enso_val <- as.numeric(input$enso_scen)
    n_months <- years * 12
    
    future_dates <- start_date + (1:n_months)
    baseline_flow_vec <- rep(base_co2_flow, n_months)
    
    if (input$scenario_mode == "pct") {
      annual_pct <- as.numeric(input$growth_slider)
      monthly_multiplier <- (1 + annual_pct/100)^(1/12)
      scenario_flow <- baseline_flow_vec * (monthly_multiplier ^ (1:n_months))
    } else {
      amount_val <- as.numeric(input$abs_amount)
      unit <- input$abs_unit
      amount_gg <- case_when(
        unit == "Gg" ~ amount_val,             
        unit == "Gt" ~ amount_val * 1e6,       
        unit == "Tonne" ~ amount_val / 1000,   
        unit == "Ton" ~ amount_val * 0.0009072 
      )
      scenario_flow <- baseline_flow_vec + (amount_gg / 12)
    }
    
    future_scenario <- tibble(
      Date = future_dates,
      ENSO = enso_val,
      Total_CO2 = scenario_flow
    ) |>
      mutate(
        cumulative_additions_Gt = cumsum(Total_CO2) * 1e-6,
        aggregate_emissions = base_emissions_stock + cumulative_additions_Gt
      ) |>
      as_tsibble(index = Date)
    
    scenario_linked <- bind_rows(modeling_data |> tail(3), future_scenario) |> 
      filter(Date > start_date)
    
    ppm_fc <- ppm_model |> forecast(new_data = scenario_linked)
    
    scenario_final <- scenario_linked |> 
      mutate(co2_ppm = ppm_fc$.mean)
    
    temp_fc <- temp_model |> forecast(new_data = scenario_final)
    
    relevant_baseline <- cached_baseline |> 
      filter(Date <= max(future_scenario$Date))
    
    user_results <- as_tibble(temp_fc) |> 
      select(Date, User_Temp = .mean)
    
    combined <- left_join(user_results, relevant_baseline, by = "Date") |> 
      mutate(Difference = User_Temp - Baseline_Temp)
    
    return(combined)
  })
  
  output$forecast_plot <- renderPlot({
    req(scenario_data())
    plot_df <- scenario_data() |> 
      mutate(Date = as.Date(Date)) |> 
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
    plot_df <- scenario_data() |> 
      mutate(Date = as.Date(Date)) |> 
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
    paste0(ifelse(final_diff > 0, "+", ""), round(final_diff, 3), " °C")
  })
  
  output$scenario_desc <- renderText({
    paste(input$fc_years, "Year Forecast |", 
          ifelse(input$scenario_mode == 'pct', paste(input$growth_slider, "% Growth"), "Absolute Addition"))
  })
  
  output$impact_table <- render_gt({
    req(scenario_data())
    df <- scenario_data()
    
    if(input$report_granularity == "Yearly Averages") {
      table_data <- df |> mutate(Year = year(Date)) |> group_by(Year) |> 
        summarize(Baseline = mean(Baseline_Temp), Scenario = mean(User_Temp), Difference = mean(Difference))
    } else {
      table_data <- df |> as_tibble() |> select(Date, Baseline = Baseline_Temp, Scenario = User_Temp, Difference)
    }
    
    table_data |> 
      mutate(Impact = case_when(Difference > 0.05 ~ "Significant Increase", Difference > 0 ~ "Slight Increase", TRUE ~ "Decrease/Neutral")) |> 
      gt() |> fmt_number(columns = c(Baseline, Scenario, Difference), decimals = 3) |> 
      data_color(columns = Difference, fn = scales::col_numeric(palette = c("blue", "white", "red"), domain = c(-0.1, 0.1)))
  })
  
  output$download_data <- downloadHandler(
    filename = function() { paste("impact_report_", Sys.Date(), ".csv", sep = "") },
    content = function(file) { write.csv(scenario_data(), file) }
  )
}

shinyApp(ui, server)