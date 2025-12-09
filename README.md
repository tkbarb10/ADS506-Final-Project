# ADS506 Final Project: Global Warming Trajectories

## Purpose and Overview
This project explores the link between anthropogenic CO₂ emissions and global temperature change. It combines historical climate data, statistical time-series models, and an interactive Shiny application that lets users test emission scenarios and see their projected temperature impacts. The repository also contains a Quarto document outlining the exploratory data analysis (EDA) and modeling workflow.

## Key Findings
- Berkeley Earth monthly anomaly data show a pronounced shift from long term stable temperature conditions in the early record to persistent warming over the last ~40 years, reinforcing the acceleration of climate change.
- STL decomposition of the anomaly series highlights a strong long-term trend layered on top of seasonal variation, underscoring the need to model both components when attributing changes to human activity.
- ARIMA models combined with CO₂ concentration (ppm) and ENSO signals, provide the backbone for temperature projections in the app, enabling scenario comparisons between baseline emissions and user-adjusted trajectories.

## Repository Structure
- `shiny_app/`: Shiny UI/server code for the Global Warming Trajectory Simulator (`app.R`, legacy prototype `app_main.R`).
- `data/`: Preprocessed RDS files used by the app and modeling pipeline (e.g., temperature conversions, ENSO-lagged predictors, country baselines).
- `appendix.qmd` and `appendix.pdf`: Quarto analysis notebook and rendered PDF with the EDA and modeling narrative.
- `data_preprocess_scripts/`: Scripts for transforming raw inputs (not run automatically by the app).

## How to Reproduce the Analysis
1. **Restore the R environment**
   ```r
   install.packages("renv")  # if needed
   renv::restore()
   ```
2. **Regenerate the appendix (optional)**
   Knit `appendix.qmd` to reproduce the EDA/modeling figures and tables:
   ```r
   quarto::quarto_render("appendix.qmd")
   ```
   The compiled report is available as `appendix.pdf` for quick reference.

## Running the Shiny App Locally
1. Ensure the data RDS files in `data/` are present (they are tracked in the repo).
2. From the project root, start the app:
   ```r
   shiny::runApp("shiny_app")
   ```
3. A browser window will open with three main tabs:
   - **Historical Explorer** – Plot historical global temperature anomalies, total CO₂ emissions, or CO₂ concentration; optionally view STL decompositions; hover for exact values.
   - **Forecast & Scenarios** – Configure forecast horizon (5–20 years), ENSO conditions, and emission adjustments (percentage growth or absolute changes). Compare user scenarios to the precomputed baseline temperature path.
   - **Impact Report** – View monthly or yearly averages of baseline vs. scenario temperatures, cumulative emission deltas, and download the results as CSV.

## Data and Modeling Notes
- Temperature inputs come from Berkeley Earth global temperature anomalies converted to absolute temperatures for seasonality-aware modeling.
- Emission baselines use 2024 territorial and land-use change estimates (~43.27 Gt/year) to anchor forward scenarios; country-level starting points are read from `country_2024_baseline_Gt.rds` when available.
- Forecasts leverage ARIMA models that couple CO₂ concentration, ENSO, and seasonal Fourier components to project temperature anomalies and highlight differences between baseline and user-defined emission pathways.

## Building/Deployment Tips
- The Shiny app runs entirely from precomputed RDS datasets; no external API calls are required.
- For deployment to a Shiny server or Posit Connect, bundle the `shiny_app/` directory and ensure the `data/` folder is accessible at the project root (or adjust paths accordingly via `here()`).
- The project uses `renv` for dependency management; deploying with an `renv.lock`-aware service will reproduce the tested package set.

## Navigation Cheatsheet
- **Explore tab**: Choose variable → optional STL toggle → adjust date slider → hover to see values.
- **Forecast tab**: Pick scope (global or country), horizon, ENSO state, and adjustment mode → click **Run Scenario** → hover lines to inspect exact temperatures; summary value boxes show temperature change and cumulative emission difference.
- **Impact Report tab**: Switch granularity to view monthly or yearly aggregates and download the scenario comparison.
