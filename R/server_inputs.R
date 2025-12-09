library(tidyverse)
library(readxl)
library(stringr)
library(zoo)
library(lubridate)
library(purrr)
library(cluster)
library(fpc)

# 1. functions for computations & data processing

# compute K-Means metrics (elbow method)
compute_kmeans_metrics <- function(data, max_k = 20, seed = 123) {
  tot_withinss <- numeric(max_k)
  betweenss <- numeric(max_k)
  
  for (k in 1:max_k) {
    set.seed(seed)
    model <- kmeans(data, centers = k, iter.max = 100, algorithm = "MacQueen")
    tot_withinss[k] <- model$tot.withinss
    betweenss[k] <- model$betweenss
  }
  data.frame(kk = 1:max_k, tot_withinss, betweenss)
}

# aggregate state weather data
aggregate_state_weather <- function(weather_data) {
  req(weather_data)
  
  # monthly aggregation
  States <- weather_data %>%
    group_by(STATE, MONTH) %>%
    summarise(
      MeanTemperature = mean(TEMP, na.rm = TRUE),
      MeanRelative_humidity = mean(RH, na.rm = TRUE),
      MeanRainfall = mean(PRCP, na.rm = TRUE),
      MeanWind_strong = mean(WDSP, na.rm = TRUE),
      .groups = 'drop'
    )
  
  # state-level aggregation
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
      SdWind_strong = sd(MeanWind_strong, na.rm = TRUE),
      .groups = 'drop'
    )
  
  States_weather %>%
    filter(STATE != "DC" | is.na(STATE))
}


# 2. server input/computing module
servermod_inputs_computing <- function(input, session) {
  
  # 1. reactive values for data Storage
  weather_data <- reactiveVal(NULL)
  records <- reactiveVal(data.frame())
  
  # 2. event: Data loading
  observeEvent(input$year, {
    req(input$year)
    showModal(modalDialog("Loading data for year ", input$year, "...", footer = NULL))
    
    data_file <- file.path("data", paste0("data", as.integer(input$year), ".RData"))
    
    if (file.exists(data_file)) {
      temp_env <- new.env()
      load(data_file, envir = temp_env)
      loaded_obj_name <- paste0("data", as.integer(input$year))
      
      if (exists(loaded_obj_name, envir = temp_env)) {
        weather_data(get(loaded_obj_name, envir = temp_env))
      } else {
        warning(paste("Variable", loaded_obj_name, "not found in", data_file))
        weather_data(NULL)
      }
    } else {
      warning(paste("Data file not found:", data_file))
      weather_data(NULL)
    }
    removeModal()
  }, ignoreInit = FALSE)
  
  # 3. event: Record management
  observeEvent(input$add_record, {
    new_record <- data.frame(
      Year = input$year,
      Month_Range = paste(input$month[1], "-", input$month[2]),
      Map_Date = as.character(input$map_date),
      Variable = input$variable,
      K_Clusters = input$k_clusters,
      Distance_Metric = input$distance_metric,
      Max_Iter = input$max_iter,
      Random_Seed = input$random_seed,
      Scale_Data = input$scale_data,
      Nrounds = input$nrounds,
      Max_Depth = input$max_depth,
      Eta = input$eta,
      Look_Back = input$look_back,
      stringsAsFactors = FALSE
    )
    
    current_records <- records()
    df <- rbind(current_records, new_record)
    records(unique(df))
  })
  
  observeEvent(input$remove_record, {
    current_records <- records()
    if (nrow(current_records) > 0) {
      records(current_records[-nrow(current_records), ])
    }
  })
  
  # 4. computed reactives (core Logic)
  
  # cleaned/aggregated State Data
  states_weather_clean <- reactive({
    aggregate_state_weather(weather_data())
  })
  
  # K-Means clustering model
  kmeans_cluster <- reactive({
    req(states_weather_clean())
    df <- states_weather_clean()
    # Columns 2:16 are the numeric metrics based on previous logic
    states_cluster <- df[, 2:16]
    
    if(input$scale_data) {
      states_cluster <- scale(states_cluster)
    }
    
    set.seed(input$random_seed)
    kmeans(states_cluster, centers = input$k_clusters, iter.max = input$max_iter, algorithm = "MacQueen")
  })
  
  # list of reactives to be used by Output module and App.R
  list(
    weather_data = weather_data,
    records = records,
    states_weather_clean = states_weather_clean,
    kmeans_cluster = kmeans_cluster
  )
}