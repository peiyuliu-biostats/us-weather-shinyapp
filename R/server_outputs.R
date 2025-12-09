library(ggplot2)
library(leaflet)
library(corrplot)
library(GGally)
library(remap)
library(viridis)
library(gridExtra)
library(RColorBrewer)
library(forecast)
library(ggfortify)
library(tseries)
library(reshape2)
library(ggthemes)
library(scales)
library(xgboost)
library(maps)
library(cluster)
library(fpc)
library(future)
library(promises)

# 1. functions for visualization & plotting logic
# --- xgboost modelling and plotting ---
calculate_xgboost_data <- function(weather_data, year, params) {
  library(dplyr)
  library(xgboost)
  # data prep
  FL <- weather_data[weather_data$STATE == "FL", ] %>%
    dplyr::select(YEARMODA, TEMP) %>%
    na.omit()
  if(nrow(FL) == 0) return(NULL)
  FL_sum3 <- FL %>%
    dplyr::group_by(YEARMODA) %>%
    dplyr::summarise(MeanTemp = mean(TEMP), .groups='drop') %>%
    dplyr::arrange(YEARMODA)
  # normalization
  max_value <- max(FL_sum3$MeanTemp)
  min_value <- min(FL_sum3$MeanTemp)
  spread <- max_value - min_value
  if(spread == 0) return(NULL)
  dataset <- (FL_sum3$MeanTemp - min_value) / spread
  # look-back dataset creation
  look_back <- params$look_back
  l <- length(dataset)
  if(l <= look_back) return(NULL)
  dataX <- matrix(NA, nrow = l - look_back, ncol = look_back)
  for (i in 1:look_back) {
    dataX[, i] <- dataset[i:(l - look_back + i - 1)]
  }
  dataY <- dataset[(look_back + 1):l]
  # split train/test
  train_size <- floor(0.67 * length(dataset))
  if(train_size < 1) return(NULL)
  xy_len <- length(dataY)
  train_xy_size <- floor(0.67 * xy_len)
  if (train_xy_size < 1 || (xy_len - train_xy_size) < 1) return(NULL) # Check if train/test has data
  
  dtrain <- xgb.DMatrix(data = dataX[1:train_xy_size, , drop=FALSE], label = dataY[1:train_xy_size])
  # train model (time-consuming step)
  xgb_params <- list(
    objective = "reg:squarederror",
    max_depth = params$max_depth,
    eta = params$eta,
    subsample = 0.8,
    colsample_bytree = 0.8
  )
  model <- xgb.train(params = xgb_params, data = dtrain, nrounds = params$nrounds, verbose = 0)
  # predict
  train_predict <- predict(model, dataX[1:train_xy_size, , drop=FALSE])
  test_predict  <- predict(model, dataX[(train_xy_size + 1):xy_len, , drop=FALSE])
  # denormalize
  train_predict <- train_predict * spread + min_value
  test_predict <- test_predict * spread + min_value
  actual_values <- dataset * spread + min_value
  # construct plot data structure
  train_end   <- look_back + length(train_predict)
  test_start <- train_end + 1
  test_end   <- train_end + length(test_predict)
  # returns a list containing all metadata required for plotting
  list(
    df = rbind(
      data.frame(index = 1:l, value = actual_values, type = "Actual"),
      data.frame(index = (look_back + 1):train_end, value = train_predict, type = "Train"),
      data.frame(index = test_start:test_end, value = test_predict, type = "Test")
    ),
    train_end = train_size, # Use train_size for the vline for consistency
    year = year
  )
}
# --- helper: pure plotting part (xgboost visualization) ---
# this function runs on the main thread and receives the result of calculate_xgboost_data
plot_xgboost_chart <- function(calc_result) {
  if(is.null(calc_result)) return(ggplot() + labs(title="Not enough data to create plot."))
  df_plot <- calc_result$df
  train_end <- calc_result$train_end
  year <- calc_result$year
  ggplot(data = df_plot) +
    geom_line(aes(x = index, y = value, color = type)) +
    geom_point(aes(x = index, y = value, color = type), size = 0.5) +
    geom_vline(xintercept = train_end + 0.5, linetype = "dashed") +
    theme_classic() +
    scale_color_manual(values = c("Actual" = "#000000", "Train" = "#E41A1C", "Test" = "#377EB8")) +
    labs(
      title = paste0("Predicted vs Actual Temperature in Florida (", year, ")"),
      subtitle = "Vertical dashed line indicates train-test split",
      x = "Observation Index",
      y = "Temperature (℃)",
      color = "Type",
      caption = "Model: XGBoost (Async)"
    ) +
    theme(plot.title = element_text(hjust = 0.5))
}
# --- monthly trend plot ---
plot_monthly_trend <- function(weather_data, variable, year) {
  variable_map <- list(Temp = "TEMP", Humidity = "RH", Precip = "PRCP", WindSpeed = "WDSP")
  selected_var <- variable_map[[variable]]
  var_label <- switch(variable,
                      Temp = "Mean Temperature (℃)",
                      Humidity = "Mean Relative Humidity (%)",
                      Precip = "Mean Rainfall (mm)",
                      WindSpeed = "Mean Wind Speed (m/s)")
  USweather_month <- weather_data %>%
    dplyr::group_by(MONTH) %>%
    dplyr::summarise(MeanValue = mean(get(selected_var), na.rm = TRUE))
  ggplot(data.frame(month = USweather_month$MONTH, values = USweather_month$MeanValue)) +
    theme_classic() +
    geom_line(aes(month, values), colour = "red") +
    scale_x_continuous(breaks = seq(1, 12, 4), labels = paste(seq(1, 12, 4), "", sep = "")) +
    theme(plot.title = element_text(hjust = 0.5)) +
    labs(y = var_label, title = paste("US", var_label, "in", year)) +
    xlab("Month")
}
# --- correlation heatmap ---
plot_correlation_heatmap <- function(weather_data, month_range) {
  USweather_month <- weather_data %>%
    dplyr::filter(MONTH >= month_range[1] & MONTH <= month_range[2]) %>%
    dplyr::group_by(MONTH) %>%
    dplyr::summarise(MeanTemperature = mean(TEMP),
                     MeanRelative_humidity = mean(RH, na.rm = T),
                     MeanRainfall = mean(PRCP, na.rm = T),
                     MeanWind_strong = mean(WDSP, na.rm = T))
  weacor <- cor(USweather_month[,2:5], use = "complete.obs")
  rownames(weacor) <- colnames(weacor) <- c("Temperature", "Relative Humidity", "Rainfall", "Wind Speed")
  ggplot(melt(weacor), aes(x = Var1, y = Var2, fill = value)) +
    geom_tile(color = "white") +
    geom_text(aes(label = round(value, 2)), size = 4, color = "black") +
    scale_fill_gradientn(colors = c("blue", "white", "red"), limits = c(-1, 1), breaks = seq(-1, 1, by = 0.2)) +
    labs(title = "Weather Correlation Coefficient", x = "", y = "", fill="Coefficient") +
    theme_minimal()
}
# --- scatter matrix ---
plot_scatter_matrix <- function(weather_data, month_range) {
  USweather_month <- weather_data %>%
    dplyr::filter(MONTH >= month_range[1] & MONTH <= month_range[2]) %>%
    dplyr::group_by(MONTH) %>%
    dplyr::summarise(MeanTemperature = mean(TEMP),
                     MeanRelative_humidity = mean(RH, na.rm = T),
                     MeanRainfall = mean(PRCP, na.rm = T),
                     MeanWind_strong = mean(WDSP, na.rm = T))
  smdata <- data.frame(USweather_month[,2:5])
  names(smdata) <- c("Temperature", "Relative Humidity", "Rainfall", "Wind Speed")
  ggscatmat(smdata) + theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) +
    labs(x = "", y = "", title = "Weather Data")
}
# --- daily leaflet map ---
plot_daily_weather_map <- function(weather_data, selected_date) {
  newcitys_day <- weather_data[weather_data$YEARMODA == as.character(selected_date), ]
  if (nrow(newcitys_day) == 0) return(NULL)
  tempcolor <- colorNumeric(palette = viridis(6, option = "plasma"), domain = na.omit(newcitys_day$TEMP))
  rehucolor <- colorNumeric(palette = viridis(6, option = "plasma"), domain = na.omit(newcitys_day$RH))
  raincolor <- colorNumeric(palette = viridis(6, option = "viridis"), domain = na.omit(newcitys_day$PRCP))
  windcolor <- colorNumeric(palette = viridis(6, option = "viridis"), domain = na.omit(newcitys_day$WDSP))
  leaflet(data = newcitys_day) %>%
    setView(lng = -95, lat = 37, zoom = 4) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(lng = ~LONGITUDE, lat = ~LATITUDE, stroke = FALSE, group = "Temperature",
                     fillOpacity = 0.8, radius = ~ (8 + TEMP/4),
                     popup = ~paste(STATE, round(TEMP, 2), sep = "-Temperature:"), color = ~tempcolor(TEMP)) %>%
    addCircleMarkers(lng = ~LONGITUDE, lat = ~LATITUDE, stroke = FALSE, group = "Relative Humidity",
                     fillOpacity = 0.8, radius = ~ (2 + RH/10),
                     popup = ~paste(STATE, round(RH, 2), sep = "-Relative Humidity:"), color = ~rehucolor(RH)) %>%
    addCircleMarkers(lng = ~LONGITUDE, lat = ~LATITUDE, stroke = FALSE, group = "Rainfall",
                     fillOpacity = 0.8, radius = ~ (4.5 + PRCP),
                     popup = ~paste(STATE, round(PRCP, 2), sep = "-Rainfall:"), color = ~raincolor(PRCP)) %>%
    addCircleMarkers(lng = ~LONGITUDE, lat = ~LATITUDE, stroke = FALSE, group = "Wind Speed",
                     fillOpacity = 0.8, radius = ~ (2 + WDSP),
                     popup = ~paste(STATE, round(WDSP, 2), sep = "-Wind Speed:"), color = ~windcolor(WDSP)) %>%
    addLayersControl(baseGroups = c("Temperature", "Relative Humidity", "Rainfall", "Wind Speed"),
                     options = layersControlOptions(collapsed = FALSE)) %>%
    addLegend("topleft", pal = tempcolor, values = ~na.omit(TEMP), title = "Temperature", opacity = 0.5) %>%
    addLegend("topleft", pal = rehucolor, values = ~na.omit(RH), title = "Relative Humidity", opacity = 0.5) %>%
    addLegend("topright", pal = raincolor, values = ~na.omit(PRCP), title = "Rainfall(mm)", opacity = 0.5) %>%
    addLegend("topright", pal = windcolor, values = ~na.omit(WDSP), title = "Wind Speed(m/s)", opacity = 0.5)
}
# --- elbow plot (internal calculation for display) ---
plot_kmeans_elbow_within <- function(states_weather, scale_data, seed) {
  data_scaled <- states_weather[, 2:16]
  if(scale_data) data_scaled <- scale(data_scaled)
  # compute metrics (duplicated logic here for visualization containment as requested)
  tot_withinss <- numeric(20)
  for (k in 1:20) {
    set.seed(seed)
    model <- kmeans(data_scaled, centers = k, iter.max = 100, algorithm = "MacQueen")
    tot_withinss[k] <- model$tot.withinss
  }
  kmeanvalue <- data.frame(kk = 1:20, tot_withinss = tot_withinss)
  ggplot(kmeanvalue, aes(x = kk, y = tot_withinss)) +
    theme_bw() + geom_point(color = "red") + geom_line() +
    labs(x = "Number of k-means Clusters", y = " ") +
    ggtitle("Sum of Squares Within Clusters") +
    theme(plot.title = element_text(hjust = 0.5))
}
plot_kmeans_elbow_between <- function(states_weather, scale_data, seed) {
  data_scaled <- states_weather[, 2:16]
  if(scale_data) data_scaled <- scale(data_scaled)
  betweenss <- numeric(20)
  for (k in 1:20) {
    set.seed(seed)
    model <- kmeans(data_scaled, centers = k, iter.max = 100, algorithm = "MacQueen")
    betweenss[k] <- model$betweenss
  }
  kmeanvalue <- data.frame(kk = 1:20, betweenss = betweenss)
  ggplot(kmeanvalue, aes(x = kk, y = betweenss)) +
    theme_bw() + geom_point(colour = "red") + geom_line() +
    labs(x = "Number of kmean Clusters", y = " ") +
    ggtitle("Sum of squares between clusters") +
    theme(plot.title = element_text(hjust = 0.5))
}
# --- cluster plot (clusplot) ---
plot_kmeans_clusters <- function(states_weather, kmeans_model, scale_data, k_clusters) {
  states_cluster <- states_weather[,2:16]
  if(scale_data) states_cluster <- scale(states_cluster)
  clusplot(states_cluster, kmeans_model$cluster, main = paste("k-means Cluster Number =", k_clusters))
}
# --- silhouette plot ---
plot_kmeans_silhouette <- function(states_weather, kmeans_model, scale_data, dist_metric) {
  states_cluster <- states_weather[,2:16]
  if(scale_data) states_cluster <- scale(states_cluster)
  si1 <- silhouette(kmeans_model$cluster, dist(states_cluster, method = dist_metric))
  plot(si1, main = "k-means Silhouette", col = "red")
}
# --- daily cluster map (leaflet) ---
plot_cluster_map <- function(states_weather, kmeans_model, k_clusters) {
  states_clu <- data.frame(state = states_weather$STATE, cluster = kmeans_model$cluster)
  state_geo <- map_data("state")
  state_bounds <- data.frame(state = unique(state_geo$region),
                             long = sapply(unique(state_geo$region), function(x) mean(state_geo$lon[state_geo$region == x])),
                             lat = sapply(unique(state_geo$region), function(x) mean(state_geo$lat[state_geo$region == x])))
  state_bounds$state <- state.abb[match(state_bounds$state, tolower(state.name))]
  state_bounds$state[is.na(state_bounds$state)] <- "DC"
  state_typeinfo <- merge(states_clu, state_bounds, by="state", all.x=TRUE) %>% na.omit()
  clucolor <- colorFactor(viridis(k_clusters, option = "viridis"), state_typeinfo$cluster)
  leaflet(data = state_typeinfo, width = 800, height = 600) %>%
    setView(lng = -95, lat = 37, zoom = 4) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(lng = state_typeinfo$long, lat = state_typeinfo$lat,
                     stroke = FALSE, group = "Cluster",
                     fillOpacity = 0.8, radius = 8.5,
                     popup = ~paste(state_typeinfo$state, state_typeinfo$cluster, sep = "-cluster is:"),
                     color = ~clucolor(state_typeinfo$cluster)) %>%
    addLegend("topleft", pal = clucolor, values = state_typeinfo$cluster, title ="Cluster", opacity = 0.5) %>%
    addLayersControl(baseGroups = c("K means Clustering Result"),
                     options = layersControlOptions(collapsed = FALSE), position = "topright")
}
# --- florida weekday heatmap ---
plot_florida_heatmap <- function(weather_data, year) {
  FL <- weather_data[weather_data$STATE=="FL",] %>%
    dplyr::select(YEARMODA, TEMP, MONTH, NAME)
  mymonths <- c("January","February","March","April","May","June","July", "August","September","October","November","December")
  month.name <- sort(unique(FL$MONTH))
  FL$month2 <- factor(FL$MONTH, levels = month.name, labels = mymonths)
  FL$YEARMODA <- as.Date(FL$YEARMODA)
  FL$weekday <- weekdays(FL$YEARMODA, abbreviate = FALSE)
  FL <- na.omit(FL)
  FL_sum2 <- FL %>%
    dplyr::group_by(month2, weekday) %>%
    dplyr::summarise(MeanTemp=mean(TEMP), .groups='drop')
  ggplot(data=FL_sum2, aes(x=month2, y=weekday)) +
    theme_bw() +
    geom_tile(aes(fill = MeanTemp), colour = "white") +
    geom_text(aes(label = round(MeanTemp,1))) +
    scale_fill_gradientn(colours=rev(brewer.pal(10,'Spectral'))) +
    theme(legend.title=element_blank(), axis.title.y=element_blank(), axis.title.x=element_blank(), legend.position="top") +
    ggtitle(paste0("Temperature Variations in Florida by Month and Weekday in ", year)) +
    labs(x="Month", y = "Weekday") +
    theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle=45, hjust=1))
}
# --- florida station boxplots ---
plot_florida_stations <- function(weather_data, year) {
  FL <- weather_data[weather_data$STATE=="FL",] %>%
    dplyr::select(YEARMODA, TEMP, MONTH, NAME) %>%
    na.omit()
  mymonths <- c("January","February","March","April","May","June","July", "August","September","October","November","December")
  month.name <- sort(unique(FL$MONTH))
  FL$month2 <- factor(FL$MONTH, levels = month.name, labels = mymonths)
  FL_sum1 <- FL %>%
    dplyr::group_by(month2, NAME) %>%
    dplyr::summarise(MeanTemp=mean(TEMP), .groups='drop')
  ggplot(data = FL_sum1, aes(x=month2, y=MeanTemp, color=MeanTemp)) +
    theme_bw() +
    scale_color_gradientn(colours=rev(brewer.pal(10,'Spectral'))) +
    geom_boxplot(colour='black', size=.4, alpha=.5) +
    geom_jitter(shape=10, width=.2, size=1) +
    theme(legend.title=element_blank(), legend.position='top', axis.text.x = element_text(angle=45, hjust=1), plot.title = element_text(hjust = 0.5)) +
    scale_y_continuous(breaks = seq(-10,30,5), labels = seq(-10,30,5)) +
    ggtitle(paste0("Monthly Average Temperature at Different Stations of Florida in ", year)) +
    xlab('') + ylab('Temperature(℃)')
}
# --- florida daily trend ---
plot_florida_daily <- function(weather_data, year) {
  FL <- weather_data[weather_data$STATE=="FL",] %>%
    dplyr::select(YEARMODA, TEMP) %>%
    na.omit()
  FL$YEARMODA <- as.Date(FL$YEARMODA)
  FL_sum3 <- FL %>%
    dplyr::group_by(YEARMODA) %>%
    dplyr::summarise(MeanTemp=mean(TEMP), .groups='drop')
  ggplot(data = FL_sum3, aes(x=YEARMODA, y=MeanTemp)) +
    theme_bw() + geom_line() +
    xlab('Date') + ylab('Temperature(℃)') +
    ggtitle(paste0("Daily Temperature Changes in Florida in ", year)) +
    theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 30, hjust = 1)) +
    scale_x_date(labels = date_format("%Y %b"))
}


# 2. module: outputs & rendering
servermod_output_render <- function(input, output, reactives) {
  # --- 1. record table ---
  output$record_table <- renderTable({
    reactives$records()
  })
  # --- 2. national visualization plots ---
  output$plot1 <- renderPlot({
    req(reactives$weather_data())
    plot_monthly_trend(
      weather_data = reactives$weather_data(),
      variable = input$variable,
      year = input$year
    )
  }) %>% bindCache(input$year, input$variable)
  output$plot2 <- renderPlot({
    req(reactives$weather_data())
    plot_correlation_heatmap(
      weather_data = reactives$weather_data(),
      month_range = input$month
    )
  }) %>% bindCache(input$year, input$month)
  output$plot3 <- renderPlot({
    req(reactives$weather_data())
    plot_scatter_matrix(
      weather_data = reactives$weather_data(),
      month_range = input$month
    )
  }) %>% bindCache(input$year, input$month)
  output$plot4 <- renderLeaflet({
    req(reactives$weather_data(), input$map_date)
    # check if data exists for selected date
    selected_data <- reactives$weather_data()[reactives$weather_data()$YEARMODA == as.character(input$map_date), ]
    validate(need(nrow(selected_data) > 0, "No data available for selected date."))
    plot_daily_weather_map(
      weather_data = reactives$weather_data(),
      selected_date = input$map_date
    )
  })
  # --- 3. national cluster plots ---
  output$cluster_plot1 <- renderPlot({
    req(reactives$states_weather_clean())
    plot_kmeans_elbow_within(
      states_weather = reactives$states_weather_clean(),
      scale_data = input$scale_data,
      seed = input$random_seed
    )
  }) %>% bindCache(input$year, input$scale_data, input$random_seed)
  output$cluster_plot2 <- renderPlot({
    req(reactives$states_weather_clean())
    plot_kmeans_elbow_between(
      states_weather = reactives$states_weather_clean(),
      scale_data = input$scale_data,
      seed = input$random_seed
    )
  }) %>% bindCache(input$year, input$scale_data, input$random_seed)
  output$cluster_plot3 <- renderPlot({
    req(reactives$states_weather_clean(), reactives$kmeans_cluster())
    plot_kmeans_clusters(
      states_weather = reactives$states_weather_clean(),
      kmeans_model = reactives$kmeans_cluster(),
      scale_data = input$scale_data,
      k_clusters = input$k_clusters
    )
  })
  output$cluster_plot4 <- renderPlot({
    req(reactives$states_weather_clean(), reactives$kmeans_cluster())
    plot_kmeans_silhouette(
      states_weather = reactives$states_weather_clean(),
      kmeans_model = reactives$kmeans_cluster(),
      scale_data = input$scale_data,
      dist_metric = input$distance_metric
    )
  })
  output$cluster_plot5 <- renderLeaflet({
    req(reactives$states_weather_clean(), reactives$kmeans_cluster())
    plot_cluster_map(
      states_weather = reactives$states_weather_clean(),
      kmeans_model = reactives$kmeans_cluster(),
      k_clusters = input$k_clusters
    )
  })
  # --- 4. florida plots ---
  output$florida_plot1 <- renderPlot({
    req(reactives$weather_data())
    plot_florida_heatmap(
      weather_data = reactives$weather_data(),
      year = input$year
    )
  })
  output$florida_plot2 <- renderPlot({
    req(reactives$weather_data())
    plot_florida_stations(
      weather_data = reactives$weather_data(),
      year = input$year
    )
  })
  output$florida_plot3 <- renderPlot({
    req(reactives$weather_data())
    plot_florida_daily(
      weather_data = reactives$weather_data(),
      year = input$year
    )
  })
  # --- xgboost plot (async implementation) ---
  output$florida_plot4 <- renderPlot({
    # 1. basic check: ensure data is loaded
    req(reactives$weather_data())
    # 2. [critical step] grab data and parameters on the main thread
    # the background process started by future() cannot directly access shiny's input or reactive objects
    # therefore, they must be "snapshotted" into normal variables here first
    current_weather_data <- reactives$weather_data()
    current_year <- input$year
    current_params <- list(
      look_back = input$look_back,
      max_depth = input$max_depth,
      eta       = input$eta,
      nrounds   = input$nrounds
    )
    # 3. asynchronous computation pipeline
    future({
      # --- background process area ---
      # this block of code runs in a new R process, so the time-consuming calculation will not block the main app's ui
      # the calculate_xgboost_data function must load necessary packages (e.g., xgboost) internally
      calculate_xgboost_data(current_weather_data, current_year, current_params)
    }) %...>%
      # 4. result handling (promise)
      # --- main thread area ---
      # when the background calculation is complete, the result is passed back here for plotting
      (function(result) {
        validate(need(!is.null(result), "Insufficient data or model training failed."))
        plot_xgboost_chart(result)
      })
  })
  # --- 5. downloads ---
  output$downloadRmd <- downloadHandler(
    filename = function() { "US-Weather-Analysis-Report.Rmd" },
    content = function(file) { file.copy("US Weather_Peiyu.Rmd", file) },
    contentType = "text/plain"
  )
}