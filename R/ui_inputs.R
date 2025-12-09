# R/ui_inputs.R
# ui-2: contains all input parameter boxes and the record-keeping ui.

uimod_content_inputs <- function() {
  tagList(
    # -- Row 1: parameter Inputs --
    fluidRow(
      # national Visualization Parameters
      box(
        title = "National Parameters", status = "primary", solidHeader = TRUE,
        tags$style(HTML(".box.box-primary > .box-header { background-color: purple !important; }")),
        selectInput("year", "Select Year:", choices = 2022:2024, selected = 2023),
        sliderInput("month", "Select Month For Correlation Analysis:", value = c(1, 12),
                    min = 1, max = 12, step = 1),
        dateInput("map_date", "Select Date For Map:", value = "2023-12-31"),
        selectInput("variable", "Select Variable For Time Series",
                    choices = c("Temperature" = "Temp",
                                "Humidity" = "Humidity",
                                "Precipitation" = "Precip",
                                "Wind Speed" = "WindSpeed"),
                    selected = "Temp"),
        width = 4
      ),
      # K-Means clustering parameters
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
      # XGBoost parameters
      box(
        title = "XGBoost Parameters", status = "warning", solidHeader = TRUE,
        numericInput("nrounds", "Number of Boosting Rounds:", value = 50, min = 10, max = 100, step = 10),
        numericInput("max_depth", "Max Tree Depth:", value = 6, min = 3, max = 10, step = 1),
        numericInput("eta", "Learning Rate:", value = 0.1, min = 0.01, max = 0.3, step = 0.01),
        numericInput("look_back", "Look-back Window (days):", value = 7, min = 1, max = 14, step = 1),
        width = 4
      )
    ),
    
    # -- Row 2: record Management --
    fluidRow(
      column(width = 12,
             actionButton("add_record", "Add Record"),
             actionButton("remove_record", "Remove Record")
      ),
      box(title = "Parameter Records",
          tableOutput("record_table"),
          width = 12)
    )
  )
}