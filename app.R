library(shiny)
library(shinydashboard)
library(tidyverse)
library(climatol)
library(corrplot)
library(GGally)
library(remap)
library(leaflet)
library(viridis)
library(cluster)
library(fpc)
library(gridExtra)
library(readxl)
library(stringr)
library(zoo)
library(RColorBrewer)
library(forecast)
library(ggfortify)
library(tseries)
library(lubridate)
library(purrr)
library(maps)
library(reshape2)
library(ggthemes)
library(scales)
library(xgboost)

library(shinyBS)
Sys.setlocale("LC_TIME","English")
ui <- dashboardPage(
  dashboardHeader(title = "Weather Analysis",
                  tags$li(class = "dropdown",
                          tags$style(HTML(".main-header { background-color: gray !important; }")))),
  dashboardSidebar(
    sidebarMenu(
      
      menuItem("Dashboard", tabName = "analysis", icon = icon("globe")),
      menuItem("Github", icon = icon("github"), 
               href = "https://github.com/peiyuliu-biostats/anpp-trial-comparison", newtab = TRUE),
      menuItem("Model Description", tabName = "model_desc", icon = icon("circle-info")),
      menuItem("shinyapps.io", icon = icon("cloud"), 
               href = "https://peiyuliu.shinyapps.io/us-weather-analysis/", newtab = TRUE),
      menuItem("Author", tabName = "about", icon = icon("user"))
      
    )
  ),
  dashboardBody(
    tags$style(HTML("
      .main-header, .main-header .navbar {
        background-color: gray !important;
      }
      .main-header > .logo{
        background-color: gray !important;
      }
    ")),
    tabItems(
      # model description
      tabItem(tabName = "model_desc",
              fluidRow(
                box(
                  title = "Model & Methods Description", status = "primary", solidHeader = TRUE, width = 12,
                  
                  withMathJax(), # Enable LaTeX/MathJax rendering
                  
                  # -------------------------
                  # 0) Overview
                  # -------------------------
                  h2("Overview"),
                  p("This application provides an interactive platform for the exploration, analysis, and forecasting of weather data across the United States. The app is built upon data sourced from the Global Surface Summary of the Day (GSOD) via the `GSODR` package in R."),
                  p("The application is divided into three main analytical components, each controlled by a dedicated parameter panel:"),
                  tags$ol(
                    tags$li(strong("National Data Visualization:"), " An interactive suite of tools to explore nationwide weather patterns through various plots and maps."),
                    tags$li(strong("K-Means Cluster Analysis:"), " An unsupervised machine learning model to identify and group states with similar annual weather profiles."),
                    tags$li(strong("Time Series Forecasting:"), " A machine learning model to predict future temperature trends based on historical data.")
                  ),
                  p("By adjusting the parameters in each panel, you can dynamically control the analyses and visualize the results."),
                  
                  hr(),
                  
                  # -------------------------
                  # 1) National Data and Visualization
                  # -------------------------
                  h2("1. National Data and Visualization"),
                  p("This section allows for a broad overview of the weather data for a selected year."),
                  h3("How to Use the 'National Parameters' Panel"),
                  tags$ul(
                    tags$li(strong("Select Year:"), " Choose the year of GSOD data to be analyzed throughout the application."),
                    tags$li(strong("Select Month for Correlation Analysis:"), " Filter the data for a specific range of months before computing the correlation heatmap."),
                    tags$li(strong("Select Date for Map:"), " Pick a single day to visualize the geographic distribution of weather variables on the interactive map."),
                    tags$li(strong("Select Variable for Time Series:"), " Choose the primary weather variable (e.g., Temperature, Rainfall) to display in the Time Series tab.")
                  ),
                  h3("Visualization Tabs"),
                  tags$ul(
                    tags$li(strong("Time Series:"), " Daily average of the selected variable across all US stations for the chosen year."),
                    tags$li(strong("Correlation Heatmap:"), " Pearson correlation among Temperature, Relative Humidity, Rainfall, and Wind Speed."),
                    tags$li(strong("Scatter Plot:"), " Pairwise relationships between the key weather variables."),
                    tags$li(strong("Map:"), " An interactive Leaflet map with station-level data and variable switching.")
                  ),
                  
                  hr(),
                  
                  # -------------------------
                  # 2) K-Means Cluster Analysis (with formulas + collapsible parameter glossary)
                  # -------------------------
                  h2("2. K-Means Cluster Analysis"),
                  p("We evaluate whether US states can be grouped into distinct climatic zones based on annual weather characteristics using the K-Means algorithm."),
                  
                  h3("Algorithmic Objective & Updates"),
                  HTML("
      <p><strong>Objective (sum of squared distances):</strong></p>
      <div>\\[
        \\min_{\\{S_1,\\dots,S_K\\},\\{\\mu_1,\\dots,\\mu_K\\}}
        J = \\sum_{j=1}^{K} \\sum_{x_i \\in S_j} \\lVert x_i - \\mu_j \\rVert_2^2,
      \\]</div>
      <p><strong>Assignment step:</strong></p>
      <div>\\[
        S_j \\leftarrow \\big\\{ x_i : \\lVert x_i - \\mu_j \\rVert_2^2 \\le
        \\lVert x_i - \\mu_\\ell \\rVert_2^2,\\; \\forall\\, \\ell \\in \\{1,\\dots,K\\} \\big\\}.
      \\]</div>
      <p><strong>Update step:</strong></p>
      <div>\\[
        \\mu_j \\leftarrow \\frac{1}{|S_j|} \\sum_{x_i \\in S_j} x_i.
      \\]</div>
      <p>Iterate assignment and update until assignments stabilize or the reduction in \\(J\\) falls below a tolerance.</p>
      "),
                  
                  # Collapsible parameter glossary for K-Means
                  tags$details(
                    tags$summary("K-Means: Parameter Glossary (click to expand)"),
                    tags$ul(
                      tags$li(HTML("<strong>\\(K\\)</strong>: Number of clusters; defines how many climatic zones to partition.")),
                      tags$li(HTML("<strong>\\(x_i \\in \\mathbb{R}^p\\)</strong>: Feature vector for state <em>i</em> (engineered annual weather statistics).")),
                      tags$li(HTML("<strong>\\(\\mu_j\\)</strong>: Centroid (mean vector) of cluster <em>j</em>.")),
                      tags$li(HTML("<strong>Distance metric</strong>: Default Euclidean; determines similarity measure in assignment.")),
                      tags$li(HTML("<strong>Maximum iterations</strong>: Upper bound on assignment–update cycles to ensure termination.")),
                      tags$li(HTML("<strong>Random seed</strong>: Fixes centroid initialization for reproducibility.")),
                      tags$li(HTML("<strong>Scaling</strong>: Standardization (e.g., \\(x'=(x-\\mu)/\\sigma\\)) to prevent large-scale variables from dominating."))
                    )
                  ),
                  
                  h3("Feature Engineering & User Controls"),
                  p("Each state is represented by 15 engineered features (e.g., mean/min/max/standard deviation of temperature, humidity). The user can set: number of clusters (K), distance metric, maximum iterations, random seed, and whether to scale features."),
                  
                  hr(),
                  
                  # -------------------------
                  # 3) Time Series Forecasting (LSTM + XGBoost with formulas + collapsible parameter glossaries)
                  # -------------------------
                  h2("3. Time Series Forecasting"),
                  p("This module forecasts temperature for Florida. The original LSTM approach was replaced by XGBoost for faster, robust deployment."),
                  
                  h3("The Original Approach: LSTM (RNN)"),
                  p("Long Short-Term Memory (LSTM) networks capture long-range dependencies in sequences. The standard per-step updates are:"),
                  HTML("
      <div>\\[
        \\begin{aligned}
          i_t &= \\sigma(W_i x_t + U_i h_{t-1} + b_i),\\\\
          f_t &= \\sigma(W_f x_t + U_f h_{t-1} + b_f),\\\\
          o_t &= \\sigma(W_o x_t + U_o h_{t-1} + b_o),\\\\
          \\tilde{c}_t &= \\tanh(W_c x_t + U_c h_{t-1} + b_c),\\\\
          c_t &= f_t \\odot c_{t-1} + i_t \\odot \\tilde{c}_t,\\\\
          h_t &= o_t \\odot \\tanh(c_t).
        \\end{aligned}
      \\]</div>
      <p><strong>MSE loss:</strong> \\(\\mathcal{L} = \\tfrac{1}{n}\\sum_{t=1}^n (y_t - \\hat{y}_t)^2\\).</p>
      "),
                  
                  # Collapsible parameter glossary for LSTM
                  tags$details(
                    tags$summary("LSTM: Parameter & Notation Glossary (click to expand)"),
                    tags$ul(
                      tags$li(HTML("<strong>\\(x_t\\)</strong>: Input at time <em>t</em> (e.g., daily temperature).")),
                      tags$li(HTML("<strong>\\(h_t\\)</strong>: Hidden state capturing short- to mid-range temporal patterns.")),
                      tags$li(HTML("<strong>\\(c_t\\)</strong>: Cell state (long-term memory).")),
                      tags$li(HTML("<strong>\\(i_t, f_t, o_t\\)</strong>: Input, forget, and output gates controlling information flow.")),
                      tags$li(HTML("<strong>\\(\\sigma\\)</strong>: Logistic sigmoid; <strong>tanh</strong>: hyperbolic tangent activation.")),
                      tags$li(HTML("<strong>Weights</strong>: \\(W_\\*, U_\\*\\) and <strong>biases</strong> \\(b_\\*\\) are trainable parameters.")),
                      tags$li(HTML("<strong>Loss \\(\\mathcal{L}\\)</strong>: Mean squared error between observed and predicted temperature."))
                    )
                  ),
                  
                  h3("The Deployed Solution: XGBoost (Gradient-Boosted Trees)"),
                  p("For responsiveness, we adopt XGBoost with a sliding-window featureization that converts forecasting into regression."),
                  
                  h4("Sliding-Window Featureization"),
                  HTML("
      <p>With a look-back window of size \\(k\\), construct predictors \\(x_t = [T_{t-k},\\, T_{t-k+1},\\,\\dots,\\, T_{t-1}]\\) to predict \\(y_t = T_t\\).</p>
      "),
                  
                  h4("Additive Tree Ensemble & Objective"),
                  HTML("
      <p><strong>Additive model:</strong></p>
      <div>\\[
        \\hat{y}_i^{(t)} = \\hat{y}_i^{(t-1)} + f_t(x_i), \\quad f_t \\in \\mathcal{F},
      \\]</div>
      <p><strong>Regularized objective (up to round \\(t\\)):</strong></p>
      <div>\\[
        \\mathcal{J}^{(t)} = \\sum_{i=1}^{n} \\ell\\big(y_i,\\, \\hat{y}_i^{(t)}\\big)
        + \\sum_{k=1}^{t} \\Omega\\big(f_k\\big), \\quad
        \\Omega(f) = \\gamma T + \\tfrac{1}{2}\\lambda \\sum_{j=1}^{T} w_j^2,
      \\]
      where \\(T\\) is the number of leaves and \\(w_j\\) are leaf weights.</div>
      <p><strong>Second-order approximation at round \\(t\\):</strong></p>
      <div>\\[
        \\mathcal{J}^{(t)} \\approx
        \\sum_{i=1}^{n}\\Big[g_i f_t(x_i) + \\tfrac{1}{2} h_i f_t(x_i)^2\\Big] + \\Omega(f_t) + \\text{const},
      \\]
      with gradients \\(g_i = \\partial_{\\hat{y}}\\ell(y_i, \\hat{y}_i^{(t-1)})\\) and Hessians \\(h_i = \\partial^2_{\\hat{y}}\\ell(y_i, \\hat{y}_i^{(t-1)})\\).</div>
      <p><strong>Optimal leaf weight:</strong></p>
      <div>\\[
        w_j^{\\ast} = -\\frac{\\sum_{i \\in I_j} g_i}{\\lambda + \\sum_{i \\in I_j} h_i},
      \\]
      where \\(I_j\\) indexes samples in leaf \\(j\\).</div>
      <p><strong>Split gain (left/right child \\(L,R\\)):</strong></p>
      <div>\\[
        \\text{Gain} =
        \\tfrac{1}{2}\\Bigg(
          \\frac{\\big(\\sum_{i\\in I_L} g_i\\big)^2}{\\lambda + \\sum_{i\\in I_L} h_i}
          + \\frac{\\big(\\sum_{i\\in I_R} g_i\\big)^2}{\\lambda + \\sum_{i\\in I_R} h_i}
          - \\frac{\\big(\\sum_{i\\in I} g_i\\big)^2}{\\lambda + \\sum_{i\\in I} h_i}
        \\Bigg) - \\gamma.
      \\]</div>
      "),
                  
                  # Collapsible parameter glossary for XGBoost
                  tags$details(
                    tags$summary("XGBoost: Parameter & Notation Glossary (click to expand)"),
                    tags$ul(
                      tags$li(HTML("<strong>Look-back window (k)</strong>: Number of past days used as predictors.")),
                      tags$li(HTML("<strong>\\(f_t\\)</strong>: The tree added at boosting round <em>t</em>.")),
                      tags$li(HTML("<strong>\\(T\\)</strong>: Number of leaves per tree (tree complexity).")),
                      tags$li(HTML("<strong>\\(w_j\\)</strong>: Leaf weight (prediction value assigned to samples in leaf <em>j</em>).")),
                      tags$li(HTML("<strong>\\(\\gamma\\)</strong>: Complexity penalty per split; larger values yield shallower trees.")),
                      tags$li(HTML("<strong>\\(\\lambda\\)</strong>: L2 regularization on leaf weights; controls overfitting.")),
                      tags$li(HTML("<strong>\\(g_i, h_i\\)</strong>: First- and second-order derivatives of the loss w.r.t. predictions.")),
                      tags$li(HTML("<strong>Learning rate (eta)</strong>: Shrinks each tree’s contribution to stabilize training.")),
                      tags$li(HTML("<strong>Max depth</strong>: Upper bound on tree depth; interacts with \\(\\gamma\\) and \\(T\\).")),
                      tags$li(HTML("<strong>Subsample/colsample</strong>: Row/feature subsampling to reduce variance and improve generalization."))
                    )
                  ),
                  
                  hr(),
                  
                  # -------------------------
                  # 4) Full Analysis Report
                  # -------------------------
                  h2("4. Full Analysis Report"),
                  p("For a comprehensive, non-interactive report containing all visualizations and code from the initial exploratory data analysis of 2023, please see the following file. This report was generated from the R Markdown script that formed the basis of this application."),
                  tags$ul(
                    tags$li(tags$a(strong("US Weather Analysis Full Report"), href="US-Weather.html", target="_blank", rel="noopener noreferrer"))
                  )
                )
              )
      ),
      tabItem(tabName = "analysis",
              fluidRow(
                # Visulization Parameters
                box(
                  title = "National Parameters", status = "primary", solidHeader = TRUE,
                  tags$style(HTML(".box.box-primary > .box-header { background-color: purple !important; }")),
                  selectInput("year", "Select Year:", choices = 2022:2024, selected = 2023),
                  sliderInput("month", "Select Month For Correlation Analysis:", value = c(1, 12), 
                              min=1, max=12, step=1),
                  dateInput("map_date", "Select Date For Map:", value = "2023-12-31"),
                  selectInput("variable", "Select Variable For Time Series", 
                              choices = c("Temperature" = "Temp", 
                                          "Humidity" = "Humidity", 
                                          "Precipitation" = "Precip", 
                                          "Wind Speed" = "WindSpeed"), 
                              selected = "Temp"),
                  width = 4
                ),
                
                # K-Mean clustering Parameters
                box(
                  title = "K-Mean Analysis Parameters", status = "primary", solidHeader = TRUE,
                  sliderInput("k_clusters", "Select Number of Clusters (k):", 
                              min = 2, max = 10, value = 5, step = 1),
                  selectInput("distance_metric", "Distance Metric:",
                              choices = c("Euclidean" = "euclidean", "Manhattan" = "manhattan"),
                              selected = "euclidean"),
                  numericInput("max_iter", "Maximum Iterations:", value = 100, min = 10, step = 10),
                  numericInput("random_seed", "Random Seed:", value = 1234),
                  checkboxInput("scale_data", "Scale Data Before Clustering", value = TRUE),
                  width = 4
                ),
                
                box(
                  title = "XGBoost Parameters", status = "warning", solidHeader = TRUE,
                  numericInput("nrounds", "Number of Boosting Rounds:", value = 50, min = 10, max = 100, step = 10),
                  numericInput("max_depth", "Max Tree Depth:", value = 6, min = 3, max = 10, step = 1),
                  numericInput("eta", "Learning Rate:", value = 0.1, min = 0.01, max = 0.3, step = 0.01),
                  numericInput("look_back", "Look-back Window (days):", value = 7, min = 1, max = 14, step = 1),
                  width = 4
                )
              ),
              fluidRow(
                column(width = 12,
                       actionButton("add_record", "Add Record"),actionButton("remove_record", "Remove Record")
                ),
                box(title="Table", 
                    tableOutput("record_table"),
                    width=12)
              ),
              fluidRow(
                tabBox(
                  title = "National Visualization Result", width =12,
                  tabPanel("Map",
                           leafletOutput("plot4", height = 500)),
                  tabPanel("Correlation Heatmap",
                           plotOutput("plot2")),
                  tabPanel("Scatter Plot",
                           plotOutput("plot3")),
                  tabPanel("Time series",
                           plotOutput("plot1"))
                )
              ),
              fluidRow(
                tabBox(
                  title = "National Results", width = 6,
                  tabPanel("Within Clusters SS",
                           plotOutput("cluster_plot1")),
                  tabPanel("Between Clusters SS",
                           plotOutput("cluster_plot2")),
                  tabPanel("K-Means",
                           plotOutput("cluster_plot3")),
                  tabPanel("Silhouette",
                           plotOutput("cluster_plot4")),
                  tabPanel("Map",
                           leafletOutput("cluster_plot5", height = 250))
                ),
                tabBox(
                  title = "Florida Results", width = 6,
                  tabPanel("Variations",
                           plotOutput("florida_plot1")),
                  tabPanel("Average Temperature",
                           plotOutput("florida_plot2")),
                  tabPanel("Daily Temperature",
                           plotOutput("florida_plot3")),
                  tabPanel("XGBoot",
                           plotOutput("florida_plot4"))
                )
              )
      ),
      
      # Author information tab content
      tabItem(tabName = "about",
              fluidRow(
                box(
                  title = "About This Application",
                  solidHeader = TRUE,
                  status = "primary",
                  width = 12,
                  h3("Author Information"),
                  p("This application is designed to analyze and visualize US weather data for the year 2023."),
                  tags$ul(
                    tags$li(HTML("<b>Author:</b> Peiyu Liu")),
                    tags$li(HTML("<b>Affiliation:</b> Department of Biostatistics, University of Florida")),
                    tags$li(HTML("<b>Contact:</b> <a href='mailto:pyliu0620@outlook.com'>pyliu0620@outlook.com</a>"))
                  ),
                  
                  hr(), # A horizontal line for separation
                  
                )
              )
      )
    )
  )
)

compute_kmeans_metrics <- function(data, max_k = 20) {
  tot_withinss <- numeric(max_k)
  betweenss <- numeric(max_k)
  
  for (k in 1:max_k) {
    model <- kmeans(data, centers = k, iter.max = 100, algorithm = "MacQueen")
    tot_withinss[k] <- model$tot.withinss
    betweenss[k] <- model$betweenss
  }
  
  data.frame(kk = 1:max_k, tot_withinss, betweenss)
}

server <- function(input, output, session) {
  
  weather_data <- reactiveVal(NULL)
  records <- reactiveVal(data.frame())
  
  observeEvent(input$add_record, {
    new_record <- data.frame(
      Year = input$year,
      Month_Range = paste(input$month[1], "-", input$month[2]),
      Map_Date = input$map_date,
      Variable = input$variable,
      K_Clusters = input$k_clusters,
      Distance_Metric = input$distance_metric,
      Max_Iter = input$max_iter,
      Random_Seed = input$random_seed,
      Scale_Data = input$scale_data,
      Nrounds = input$nrounds,
      Max_Depth = input$max_depth,
      Eta = input$eta,
      Look_Back = input$look_back
    )
    current_records <- records()
    df <- rbind(current_records, new_record)
    df <- unique(df)
    records(df)
  })
  
  observeEvent(input$remove_record, {
    current_records <- records()
    if (nrow(current_records) > 0) {
      records(current_records[-nrow(current_records), ])
    }
  })
  output$record_table <- renderTable({
    records()
  })
  
  observeEvent(input$year, {
    req(input$year)
    showModal(modalDialog("Loading data for year ", input$year, "...", footer = NULL))
    
    # Dynamically construct the file name
    data_file <- paste0("data", as.integer(input$year), ".RData")
    
    # Check if the file exists before attempting to load
    if (file.exists(data_file)) {
      # Use a temporary environment to load, to avoid polluting global env
      temp_env <- new.env()
      load(data_file, envir = temp_env)
      
      # Assuming the RData file contains a variable named 'dataYYYY'
      # where YYYY is the year. We need to get that variable's name dynamically.
      # A safer way is to ensure your RData files consistently save to one name
      # e.g., 'USweather_data'. If not, we have to infer.
      
      # For your current setup (data2022, data2023, data2024 variables):
      # Get the name of the object that was loaded
      loaded_obj_name <- paste0("data", as.integer(input$year))
      
      # Assign the loaded data from the temporary environment to weather_data reactiveVal
      if (exists(loaded_obj_name, envir = temp_env)) {
        weather_data(get(loaded_obj_name, envir = temp_env))
      } else {
        warning(paste("Variable", loaded_obj_name, "not found in", data_file))
        weather_data(NULL) # Set to NULL if data not found
      }
      
    } else {
      warning(paste("Data file not found:", data_file))
      weather_data(NULL) # Set to NULL if file not found
    }
    
    # update the map_date input
    current_weather_data <- weather_data()
    if (!is.null(current_weather_data)) {
      min_date <- min(current_weather_data$YEARMODA, na.rm = TRUE)
      max_date <- max(current_weather_data$YEARMODA, na.rm = TRUE)
      
      updateDateInput(session, "map_date",
                      min = min_date,
                      max = max_date,
                      value = max_date)
    } else {
      # If data is NULL, reset map_date or handle gracefully
      updateDateInput(session, "map_date", value = NULL, min = NULL, max = NULL)
    }
    
    removeModal()
  }, ignoreInit = FALSE) # ignoreInit = FALSE ensures it runs on app start
  
  
  output$plot1 <- renderPlot({
    req(weather_data())  
    USweather <- weather_data()
    
    # Map selected variable to data column and label
    variable_map <- list(
      Temp = "TEMP",
      Humidity = "RH",
      Precip = "PRCP",
      WindSpeed = "WDSP"
    )
    selected_var <- variable_map[[input$variable]]
    var_label <- switch(input$variable,
                        Temp = "Mean Temperature (℃)",
                        Humidity = "Mean Relative Humidity (%)",
                        Precip = "Mean Rainfall (mm)",
                        WindSpeed = "Mean Wind Speed (m/s)")
    
    # Aggregate data by month for the selected variable
    USweather_month <- USweather %>%
      dplyr::group_by(MONTH) %>%
      summarise(MeanValue = mean(get(selected_var), na.rm = TRUE))
    # Create data frame for plotting
    US_month <- data.frame(
      month = USweather_month$MONTH,
      values = USweather_month$MeanValue
    )
    
    ggplot(US_month) +
      theme_classic() +
      geom_line(aes(month, values), colour = "red") +
      scale_x_continuous(breaks = seq(1, 12, 4), labels = paste(seq(1, 12, 4), "", sep = "")) +
      theme(plot.title = element_text(hjust = 0.5)) +
      labs(y = var_label, title = paste("US", var_label, "in 2023")) +
      xlab("Month")
  })
  
  output$plot2 <- renderPlot({
    req(weather_data())  
    USweather <- weather_data()
    USweather_month <- USweather %>%
      dplyr::filter(MONTH >= input$month[1] & MONTH <= input$month[2]) %>%
      dplyr::group_by(MONTH)  %>% 
      summarise(MeanTemperature = mean(TEMP),  #average temperature
                MeanRelative_humidity = mean(RH,na.rm = T),  #average relative humidity
                MeanRainfall = mean(PRCP,na.rm = T),  #average daily rainfall
                MeanWind_strong = mean(WDSP,na.rm = T)) #average wind force
    weacor <- cor(USweather_month[,2:5])
    rownames(weacor) <- c("Temperature", "Relative Humidity", "Rainfall", "Wind Speed")
    colnames(weacor) <- c("Temperature", "Relative Humidity", "Rainfall", "Wind Speed") 
    
    # Convert the correlation matrix to a tidy format
    weacor.tidy <- melt(weacor)
    
    # Create the correlation plot using ggplot2
    ggplot(weacor.tidy, aes(x = Var1, y = Var2, fill = value)) +
      geom_tile(color = "white") +
      geom_text(aes(label = round(value, 2)), size = 4, color = "black") +
      scale_fill_gradientn(colors = c("blue", "white", "red"), limits = c(-1, 1),
                           breaks = seq(-1, 1, by = 0.2))+
      labs(title = "Weather Correlation Coefficient", x = "", y = "",fill="Coefficient") +
      theme_minimal()
  })
  
  output$plot3 <- renderPlot({
    req(weather_data())  
    USweather <- weather_data()
    USweather_month <- USweather %>%
      dplyr::filter(MONTH >= input$month[1] & MONTH <= input$month[2]) %>%
      dplyr::group_by(MONTH)  %>% 
      summarise(MeanTemperature = mean(TEMP),  #average temperature
                MeanRelative_humidity = mean(RH,na.rm = T),  #average relative humidity
                MeanRainfall = mean(PRCP,na.rm = T),  #average daily rainfall
                MeanWind_strong = mean(WDSP,na.rm = T)) #average wind force
    smdata <- data.frame((USweather_month[,2:5]))
    names(smdata) <- c("Temperature", "Relative Humidity", "Rainfall", "Wind Speed")
    ggscatmat(smdata) + theme_bw() +
      theme(plot.title = element_text(hjust = 0.5)) +
      labs(x = "",y = "",title = "Weather Data")
  })
  
  output$plot4 <- renderLeaflet({
    req(weather_data())  
    USweather <- weather_data()
    
    ## Extract one day's data
    selected_date <- as.character(input$map_date)  
    newcitys_day <- USweather[USweather$YEARMODA == selected_date, ]
    
    ## Define color
    tempcolor <- colorNumeric(palette = viridis(6, option = "plasma"), domain = na.omit(newcitys_day$TEMP))
    rehucolor <- colorNumeric(palette = viridis(6, option = "plasma"), domain = na.omit(newcitys_day$RH))
    raincolor <- colorNumeric(palette = viridis(6, option = "viridis"), domain = na.omit(newcitys_day$PRCP))
    windcolor <- colorNumeric(palette = viridis(6, option = "viridis"), domain = na.omit(newcitys_day$WDSP))
    
    map <- leaflet(data = newcitys_day) %>%
      setView(lng = -95, lat = 37, zoom = 4) %>% # Set view center to United States
      addProviderTiles(providers$CartoDB.Positron) %>% # Use the map style provided by CartoDB.Positron
      addCircleMarkers(lng = ~LONGITUDE, lat = ~LATITUDE,
                       stroke = FALSE, group = "Temperature",
                       fillOpacity = 0.8, radius = ~ (8 + TEMP/4),
                       popup = ~paste(STATE, round(TEMP, 2), sep = "-Temperature:"),
                       color = ~tempcolor(TEMP)) %>%
      addCircleMarkers(lng = ~LONGITUDE, lat = ~LATITUDE,
                       stroke = FALSE, group = "Relative Humidity",
                       fillOpacity = 0.8, radius = ~ (2 + RH/10),
                       popup = ~paste(STATE, round(RH, 2), sep = "-Relative Humidity:"),
                       color = ~rehucolor(RH)) %>%
      addCircleMarkers(lng = ~LONGITUDE, lat = ~LATITUDE,
                       stroke = FALSE, group = "Rainfall",
                       fillOpacity = 0.8, radius = ~ (4.5 + PRCP),
                       popup = ~paste(STATE, round(PRCP, 2), sep = "-Rainfall:"),
                       color = ~raincolor(PRCP)) %>%
      addCircleMarkers(lng = ~LONGITUDE, lat = ~LATITUDE,
                       stroke = FALSE, group = "Wind Speed",
                       fillOpacity = 0.8, radius = ~ (2 + WDSP),
                       popup = ~paste(STATE, round(WDSP, 2), sep = "-Wind Speed:"),
                       color = ~windcolor(WDSP)) %>%
      addLayersControl(baseGroups = c("Temperature", "Relative Humidity", "Rainfall", "Wind Speed"),
                       options = layersControlOptions(collapsed = FALSE))%>%
      addLegend("topleft",pal = tempcolor, values = ~na.omit(TEMP),
                title = "Temperature",opacity = 0.5) %>%
      addLegend("topleft",pal = rehucolor, values = ~na.omit(RH),
                title = "Relative Humidity",opacity = 0.5) %>%
      addLayersControl(baseGroups = c("Temperature", "Relative Humidity", "Rainfall", "Wind Speed"),
                       options = layersControlOptions(collapsed = FALSE))%>%
      addLegend("topright",pal = raincolor, values = ~na.omit(PRCP),
                title = "Rainfall(mm)",opacity = 0.5) %>%
      addLegend("topright",pal = windcolor, values = ~na.omit(WDSP),
                title = "Wind Speed(m/s)",opacity = 0.5) 
    
    map
  })
  
  states_weather_clean <- reactive({
    req(weather_data())
    USweather <- weather_data()
    
    States <- USweather %>%
      group_by(STATE, MONTH) %>%
      summarise(
        MeanTemperature = mean(TEMP),
        MeanRelative_humidity = mean(RH, na.rm = TRUE),
        MeanRainfall = mean(PRCP, na.rm = TRUE),
        MeanWind_strong = mean(WDSP, na.rm = TRUE)
      )
    
    States_weather <- States %>%
      group_by(STATE) %>%
      summarise(
        MeanTemperature2 = mean(MeanTemperature, na.rm = TRUE),
        MinTemperature = min(MeanTemperature, na.rm = TRUE),
        MaxTemperature = max(MeanTemperature, na.rm = TRUE),
        SdTemperature = sd(MeanTemperature, na.rm = TRUE),
        MeanRelative_humidity2 = mean(MeanRelative_humidity, na.rm = TRUE),
        MinRelative_humidity = min(MeanRelative_humidity, na.rm = TRUE),
        MaxRelative_humidity = max(MeanRelative_humidity, na.rm = TRUE),
        SdRelative_humidity = sd(MeanRelative_humidity, na.rm = TRUE),
        MeanRainfall2 = mean(MeanRainfall, na.rm = TRUE),
        MaxRainfall = max(MeanRainfall, na.rm = TRUE),
        SdRainfall = sd(MeanRainfall, na.rm = TRUE),
        MeanWind_strong2 = mean(MeanWind_strong, na.rm = TRUE),
        MinWind_strong = min(MeanWind_strong, na.rm = TRUE),
        MaxWind_strong = max(MeanWind_strong, na.rm = TRUE),
        SdWind_strong = sd(MeanWind_strong, na.rm = TRUE)
      )
    
    States_weather %>%
      filter(STATE != "DC" | is.na(STATE))
  })
  
  
  # reactive function to get scaled and clustered data
  kmeans_cluster <- reactive({
    df <- states_weather_clean()
    states_cluster <- df[, 2:16]
    
    if(input$scale_data) {
      states_cluster <- scale(states_cluster)
    }
    
    set.seed(input$random_seed)
    kmeans(states_cluster, centers = input$k_clusters, iter.max = input$max_iter, algorithm = "MacQueen")
  })
  
  output$cluster_plot1 <- renderPlot({
    data_scaled <- states_weather_clean()[, 2:16]
    if(input$scale_data) data_scaled <- scale(data_scaled)
    
    kmeanvalue <- compute_kmeans_metrics(data_scaled, max_k = 20)
    
    ggplot(kmeanvalue, aes(x = kk, y = tot_withinss)) +
      theme_bw() +
      geom_point(color = "red") +
      geom_line() +
      labs(x = "Number of k-means Clusters", y = " ") +
      ggtitle("Sum of Squares Within Clusters") +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  output$cluster_plot2 <- renderPlot({
    data_scaled <- states_weather_clean()[, 2:16]
    if(input$scale_data) data_scaled <- scale(data_scaled)
    ## ==============================================================
    ## ===============k-means clustering algorithm======================================
    ## ==============================================================
    ## clustering using k-means clustering algorithm
    ## explore the changes caused by the number of clusters k
    ## calculate the sum of squares of residuals for clustering
    
    ## Calculate the sum of squares within a group and the sum of squares between groups
    tot_withinss <- vector()
    betweenss <- vector()
    kk = 20
    for(ii in 1:kk){
      set.seed(input$random_seed)
      km <- kmeans(data_scaled, ii, iter.max = input$max_iter, algorithm = "MacQueen")
      tot_withinss[ii] <- km$tot.withinss
      betweenss[ii] <- km$betweenss
    }
    
    kmeanvalue <- data.frame(kk = 1:kk,
                             tot_withinss = tot_withinss,
                             betweenss = betweenss)
    
    
    ggplot(kmeanvalue,aes(x = kk,y = betweenss))+
      theme_bw(base_family = "STKaiti") +
      geom_point(colour = "red") +
      geom_line() +
      labs(x = "Number of kmean Clusters",y = " ") +
      ggtitle("Sum of squares between clusters")+
      theme(plot.title = element_text(hjust = 0.5))
    
  })
  
  output$cluster_plot3 <- renderPlot({
    states_cluster <- states_weather_clean()[,2:16]
    if(input$scale_data) states_cluster <- scale(states_cluster)
    km <- kmeans_cluster()
    clusplot(states_cluster, km$cluster, main = paste("k-means Cluster Number =", input$k_clusters))
  })
  
  output$cluster_plot4 <- renderPlot({
    states_cluster <- states_weather_clean()[,2:16]
    if(input$scale_data) states_cluster <- scale(states_cluster)
    
    km <- kmeans_cluster()
    si1 <- silhouette(km$cluster, dist(states_cluster, method = input$distance_metric))
    plot(si1, main = "k-means Silhouette", col = "red")
  })
  
  output$cluster_plot5 <- renderLeaflet({
    df <- states_weather_clean()
    states_cluster <- df[, 2:16]
    if(input$scale_data) states_cluster <- scale(states_cluster)
    
    km <- kmeans_cluster()
    
    states_clu <- data.frame(state= df$STATE, cluster = km$cluster)
    
    state_geo <- map_data("state")
    state_bounds <- data.frame(state = unique(state_geo$region),
                               long = sapply(unique(state_geo$region), function(x) mean(state_geo$lon[state_geo$region == x])),
                               lat = sapply(unique(state_geo$region), function(x) mean(state_geo$lat[state_geo$region == x])))
    state_bounds$state<-state.abb[match(state_bounds$state, tolower(state.name))]
    state_bounds$state[is.na(state_bounds$state)]<-"DC"
    
    state_typeinfo<-merge(states_clu, state_bounds, by="state", all.x=TRUE)%>%
      na.omit()
    
    clucolor <- colorFactor(viridis(input$k_clusters,option = "viridis"),state_typeinfo$cluster)
    ## 绘制地图--------------------------------
    map_2 <- leaflet(data = state_typeinfo,width = 800, height = 600) %>%
      setView(lng = -95, lat = 37, zoom = 4) %>% # 设定视图中心到美国
      addProviderTiles(providers$CartoDB.Positron) %>% # 使用CartoDB.Positron 提供的地图样式 
      addCircleMarkers(lng = state_typeinfo$long, lat = state_typeinfo$lat,
                       stroke = FALSE,group = "Cluster",
                       fillOpacity = 0.8,radius = 8.5,
                       popup = ~paste(state_typeinfo$state,state_typeinfo$cluster,sep = "-cluster is:"),
                       color = ~clucolor(state_typeinfo$cluster)) %>%
      addLegend("topleft",pal = clucolor, values = state_typeinfo$cluster,
                title ="Cluster",opacity = 0.5) %>%
      addLayersControl(baseGroups = c("K means Clustering Result"),
                       options = layersControlOptions(collapsed = FALSE),
                       position = "topright")
    map_2
  })
  
  output$florida_plot1 <- renderPlot({
    req(weather_data())
    USweather <- weather_data()
    FL <- USweather[USweather$STATE=="FL",]%>%
      select(YEARMODA,TEMP,MONTH,NAME)
    
    mymonths <- c("January","February","March","April","May","June","July",
                  "August","September","October","November","December")
    
    ## Layer
    month.name <- sort(unique(FL$MONTH))
    FL$month2 <- factor(FL$MONTH, levels = month.name,labels = mymonths)
    FL$YEARMODA<-as.Date(FL$YEARMODA)
    FL$weekday = weekdays(FL$YEARMODA, abbreviate = FALSE)
    
    FL<-na.omit(FL)
    
    FL_sum<-FL%>%
      group_by(month2)%>%
      summarise(MeanTemp=mean(TEMP))
    
    FL_sum1<-FL%>%
      group_by(month2,NAME)%>%
      summarise(MeanTemp=mean(TEMP))
    
    FL_sum2<-FL%>%
      group_by(month2,weekday)%>%
      summarise(MeanTemp=mean(TEMP))
    ggplot(data=FL_sum2, aes(x=month2,y=weekday)) + 
      theme_bw(base_family = "STKaiti") +
      geom_tile(aes(fill = MeanTemp),colour = "white") + 
      geom_text(aes(label = round(MeanTemp,1))) +
      scale_fill_gradientn(colours=rev(brewer.pal(10,'Spectral'))) + 
      theme(legend.title=element_blank(),
            axis.title.y=element_blank(),
            axis.title.x=element_blank(),
            legend.position="top") + 
      ggtitle(paste0("Temperature Variations in Florida by Month and Weekday in ", input$year)) +
      labs(x="Month",y = "Weekday") +
      theme(plot.title = element_text(hjust = 0.5),
            axis.text.x = element_text(angle=45, hjust=1))
  })
  
  output$florida_plot2 <- renderPlot({
    req(weather_data())
    USweather <- weather_data()
    FL <- USweather[USweather$STATE=="FL",]%>%
      select(YEARMODA,TEMP,MONTH,NAME)
    
    mymonths <- c("January","February","March","April","May","June","July",
                  "August","September","October","November","December")
    
    ## Layer
    month.name <- sort(unique(FL$MONTH))
    FL$month2 <- factor(FL$MONTH, levels = month.name,labels = mymonths)
    FL$YEARMODA<-as.Date(FL$YEARMODA)
    FL$weekday = weekdays(FL$YEARMODA, abbreviate = FALSE)
    
    FL<-na.omit(FL)
    
    FL_sum<-FL%>%
      group_by(month2)%>%
      summarise(MeanTemp=mean(TEMP))
    
    FL_sum1<-FL%>%
      group_by(month2,NAME)%>%
      summarise(MeanTemp=mean(TEMP))
    
    FL_sum2<-FL%>%
      group_by(month2,weekday)%>%
      summarise(MeanTemp=mean(TEMP))
    
    ggplot(data =FL_sum1,aes(x=month2,y=MeanTemp,color=MeanTemp)) +
      theme_bw(base_family = "STKaiti") +
      scale_color_gradientn(colours=rev(brewer.pal(10,'Spectral'))) + 
      geom_boxplot(colour='black',size=.4,alpha=.5) + 
      geom_jitter(shape=10,width=.2,size=1) + 
      theme(legend.title=element_blank(),
            legend.position='top',
            axis.text.x = element_text(angle=45, hjust=1),
            plot.title = element_text(hjust = 0.5)) + 
      scale_y_continuous(breaks = seq(-10,30,5),labels = seq(-10,30,5)) +
      ggtitle(paste0("Monthly Average Temperature at Different Stations of Florida in ", input$year)) + 
      xlab('') + ylab('Temperature(℃)') 
  })
  
  
  output$florida_plot3 <- renderPlot({
    req(weather_data())
    USweather <- weather_data()
    FL <- USweather[USweather$STATE=="FL",]%>%
      select(YEARMODA,TEMP,MONTH,NAME)
    
    mymonths <- c("January","February","March","April","May","June","July",
                  "August","September","October","November","December")
    
    ## Layer
    month.name <- sort(unique(FL$MONTH))
    FL$month2 <- factor(FL$MONTH, levels = month.name,labels = mymonths)
    FL$YEARMODA<-as.Date(FL$YEARMODA)
    FL$weekday = weekdays(FL$YEARMODA, abbreviate = FALSE)
    
    FL<-na.omit(FL)
    
    FL_sum<-FL%>%
      group_by(month2)%>%
      summarise(MeanTemp=mean(TEMP))
    
    FL_sum1<-FL%>%
      group_by(month2,NAME)%>%
      summarise(MeanTemp=mean(TEMP))
    
    FL_sum2<-FL%>%
      group_by(month2,weekday)%>%
      summarise
    
    FL_sum3<-FL%>%
      group_by(YEARMODA)%>%
      summarise(MeanTemp=mean(TEMP))
    
    
    ## average temperature graph
    ggplot(data =FL_sum3,aes(x=YEARMODA,y=MeanTemp)) +
      theme_bw(base_family = "STKaiti") +
      geom_line()  + 
      xlab('Date') + ylab('Temperature(℃)') +
      ggtitle(paste0("Daily Temperature Changes in Florida in ", input$year)) + 
      theme(plot.title = element_text(hjust = 0.5)) +
      scale_x_date(
        labels = date_format("%Y %b")) +
      theme(axis.text.x = element_text(angle = 30, hjust = 1))
  })
  
  output$florida_plot4 <- renderPlot({
    req(weather_data())
    USweather <- weather_data()
    showModal(modalDialog("Building model...", footer = NULL))
    # Filter Florida data and aggregate by date
    FL <- USweather[USweather$STATE == "FL", ] %>%
      select(YEARMODA, TEMP) %>%
      na.omit()
    
    FL_sum3 <- FL %>%
      group_by(YEARMODA) %>%
      summarise(MeanTemp = mean(TEMP)) %>%
      arrange(YEARMODA)
    
    # Normalize data
    max_value <- max(FL_sum3$MeanTemp)
    min_value <- min(FL_sum3$MeanTemp)
    spread <- max_value - min_value
    dataset <- (FL_sum3$MeanTemp - min_value) / spread
    
    # Create dataset for XGBoost (look-back window)
    look_back <- input$look_back
    create_dataset <- function(dataset, look_back) {
      l <- length(dataset)
      dataX <- matrix(NA, nrow = l - look_back, ncol = look_back)
      for (i in 1:look_back) {
        dataX[, i] <- dataset[i:(l - look_back + i - 1)]
      }
      dataY <- dataset[(look_back + 1):l]
      return(list(dataX = dataX, dataY = dataY))
    }
    
    # Split into train and test
    train_size <- floor(0.67 * length(dataset))
    test_size <- length(dataset) - train_size
    train <- dataset[1:train_size]
    test <- dataset[(train_size + 1):length(dataset)]
    
    trainXY <- create_dataset(train, look_back)
    testXY <- create_dataset(test, look_back)
    
    # Prepare XGBoost data
    dtrain <- xgb.DMatrix(data = trainXY$dataX, label = trainXY$dataY)
    dtest <- xgb.DMatrix(data = testXY$dataX, label = testXY$dataY)
    
    # Train XGBoost model with user-specified parameters
    params <- list(
      objective = "reg:squarederror",
      max_depth = input$max_depth,
      eta = input$eta,
      subsample = 0.8,
      colsample_bytree = 0.8
    )
    model <- xgb.train(
      params = params,
      data = dtrain,
      nrounds = input$nrounds,
      verbose = 0
    )
    
    # Generate predictions
    train_predict <- predict(model, trainXY$dataX)
    test_predict <- predict(model, testXY$dataX)
    
    # Denormalize predictions
    train_predict <- train_predict * spread + min_value
    test_predict <- test_predict * spread + min_value
    
    # Create data frame for plotting
    df <- data.frame(
      index = 1:length(dataset),
      value = dataset * spread + min_value,
      type = "Actual"
    ) %>%
      rbind(
        data.frame(
          index = (look_back + 1):(look_back + length(train_predict)),
          value = train_predict,
          type = "Train"
        )
      ) %>%
      rbind(
        data.frame(
          index = (train_size + look_back + 1):(train_size + look_back + length(test_predict)),
          value = test_predict,
          type = "Test"
        )
      )
    removeModal()
    # Plot using ggplot2
    ggplot(data = df) +
      geom_line(aes(x = index, y = value, color = type)) +
      geom_point(aes(x = index, y = value, color = type), size = 0.5) +
      geom_vline(xintercept = train_size + 0.5, linetype = "dashed") +
      theme_classic() +
      scale_color_manual(values = c("Actual" = "#000000", "Train" = "#E41A1C", "Test" = "#377EB8")) +
      labs(
        title = paste0("Predicted vs Actual Temperature in Florida (", input$year, ")"),
        subtitle = "Vertical dashed line indicates train-test split",
        x = "Observation Index",
        y = "Temperature (℃)",
        color = "Type",
        caption = "Model: XGBoost"
      ) +
      theme(plot.title = element_text(hjust = 0.5))
  })
}

shinyApp(ui, server)
