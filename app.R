library(shiny)
library(shinydashboard)
library(shinyBS)
library(promises)
library(future)
plan(multisession)  #asynchronous strategy  

# locale
Sys.setlocale("LC_TIME", "English")

# source all ui and server modules
source("R/ui_inputs.R")
source("R/ui_outputs.R")
source("R/ui_description.R")

source("R/server_inputs.R")
source("R/server_outputs.R")

# ============  
# 1. UI
# ============  
ui <- dashboardPage(
  dashboardHeader(title = "Weather Analysis",
                  tags$li(class = "dropdown",
                          tags$style(HTML(".main-header { background-color: gray !important; }")))
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "analysis", icon = icon("globe")),
      menuItem("Github", icon = icon("github"),
               href = "https://github.com/peiyuliu-biostats/us-weather-shinyapp", newtab = TRUE),
      menuItem("Model Description", tabName = "model_desc", icon = icon("circle-info")),
      menuItem("shinyapps.io", icon = icon("cloud"),
               href = "https://peiyuliu.shinyapps.io/us-weather-analysis/", newtab = TRUE),
      menuItem("Author", tabName = "about", icon = icon("user"))
    )
  ),
  dashboardBody(
    #theme custom CSS
    tags$style(HTML("
      .main-header, .main-header .navbar, .main-header > .logo {
        background-color: gray !important;
      }
    ")),
    # assemble the body using the UI modules
    tabItems(
      tabItem(tabName = "analysis",
              uimod_content_inputs(),
              uimod_content_outputs()
      ),
      uimod_content_doc(which = "model_desc"),
      uimod_content_doc(which = "about")
    )
  )
)


# ================= 
# 2. Server
# ================= 
server <- function(input, output, session) {
  
  # 1. inputs (reactive state & core computings)
  # a list of reactives: weather_data, records, states_weather_clean, kmeans_cluster
  reactives_list <- servermod_inputs_computing(input, session)
  
  # 2. outputs (Rendering, Visulization)
  # consumes the reactives from step 1
  servermod_output_render(input, output, reactives_list)
  
  # 3. update UI dynamics
  # update the map date input range based on loaded data
  observe({
    req(reactives_list$weather_data())
    current_weather_data <- reactives_list$weather_data()
    
    if (!is.null(current_weather_data)) {
      min_date <- min(current_weather_data$YEARMODA, na.rm = TRUE)
      max_date <- max(current_weather_data$YEARMODA, na.rm = TRUE)
      
      updateDateInput(session, "map_date",
                      min = min_date,
                      max = max_date,
                      value = max_date)
    } else {
      updateDateInput(session, "map_date", value = NULL, min = NULL, max = NULL)
    }
  })
  
}

shinyApp(ui, server)