# US Weather Analysis Dashboard

An interactive **Shiny** dashboard to explore, analyze, and forecast historical weather data across the United States.

[![View the Live Application](https://img.shields.io/badge/View%20the%20Live%20Application-blue?style=for-the-badge&logo=shiny)](https://peiyuliu.shinyapps.io/us-weather-analysis/)

> **Note:** Because this app runs on a free server, switching years can briefly disconnect the session. Therefore, **2023 is used as the primary example year**. Analyses for other years follow **exactly the same methods**.  
> **Model simplification:** To make the app lighter on Shiny, the original LSTM forecasting model was **replaced with XGBoost** for training and visualization.

---

## Description

This application provides a practical platform for exploring daily U.S. weather data from the Global Surface Summary of the Day (GSOD). You can interact with the data to spot trends, compare patterns, and generate forecasts.

The dashboard is organized into three modules:

1. **Nationwide Visualization** – high-level summaries and quick exploration.
2. **Unsupervised Clustering** – group states by climate using K-Means.
3. **Time-Series Forecasting** – predict Florida temperatures with **XGBoost**.

Adjust parameters in each panel and see results update immediately.

---

## Features

### 1) Nationwide Visualization
- Explore nationwide patterns for a selected year.
- **Controls:** year, month range for correlation, a specific date for the map, and the primary variable for time series.
- **Visuals (4 plots):**
  - **Time Series Plot:** daily averages of the selected variable (temperature, humidity, etc.).
  - **Correlation Heatmap:** Pearson correlations for a chosen month range.
  - **Scatter Plot Matrix:** pairwise relationships among temperature, humidity, precipitation, and wind speed.
  - **Interactive Map:** Leaflet map of station-level observations for a chosen day.

### 2) Unsupervised Clustering of States
- **K-Means** clustering with 15 engineered annual weather features.
- **Tuning:** choose K, random seed, and whether to scale features.
- **Diagnostics & geography:** within/between-cluster sums of squares, silhouette plot, and a map of cluster assignments.

### 3) Machine-Learning Time-Series Forecasting (XGBoost)
- **XGBoost regression** to forecast Florida temperature trends.
- **Sliding-window features:** convert the series into supervised learning with a configurable look-back window.
- **Tunable hyperparameters:** number of rounds, max depth, learning rate (eta), and window size.
- **Performance plot:** compares actual vs predicted values on training and test sets.

### 4) Scenario Recording
- Add/remove parameter configurations in a table to **track and compare** different analysis runs.

---

## Tech Stack

- **R / Shiny** with **shinydashboard**
- **Data wrangling:** `dplyr`, **tidyverse**
- **Visualization:** `ggplot2`, **leaflet**
- **Clustering:** base `stats`, `cluster`
- **Forecasting:** **`xgboost`**
- **Data source:** GSOD via the **`GSODR`** package

> Data for each year is stored as separate `.RData` files and loaded reactively only when that year is selected to keep the app responsive.


