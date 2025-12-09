# R/ui_outputs.R
# ui module3: contains all output elements (plots, maps, etc.).

uimod_content_outputs <- function() {
  tagList(
    # -- Row 3: national visualization results --
    fluidRow(
      tabBox(
        title = "National Visualization Result", width = 12,
        tabPanel("Map", leafletOutput("plot4", height = 500)),
        tabPanel("Correlation Heatmap", plotOutput("plot2")),
        tabPanel("Scatter Plot", plotOutput("plot3")),
        tabPanel("Time series", plotOutput("plot1"))
      )
    ),
    
    # -- Row 4: cluster and forecast Results --
    fluidRow(
      tabBox(
        title = "National Results", width = 6,
        tabPanel("Within Clusters SS", plotOutput("cluster_plot1")),
        tabPanel("Between Clusters SS", plotOutput("cluster_plot2")),
        tabPanel("K-Means", plotOutput("cluster_plot3")),
        tabPanel("Silhouette", plotOutput("cluster_plot4")),
        tabPanel("Map", leafletOutput("cluster_plot5", height = 250))
      ),
      tabBox(
        title = "Florida Results", width = 6,
        tabPanel("Variations", plotOutput("florida_plot1")),
        tabPanel("Average Temperature", plotOutput("florida_plot2")),
        tabPanel("Daily Temperature", plotOutput("florida_plot3")),
        tabPanel("XGBoost", plotOutput("florida_plot4")) 
      )
    )
  )
}
