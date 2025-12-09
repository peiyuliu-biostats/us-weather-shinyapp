# R/ui_description.R
# ui-1: a specific static content page (tabItem).

uimod_content_doc <- function(which = c("model_desc", "about")) {
  which <- match.arg(which)
  
  if (which == "model_desc") {
    # --- Model Description Tab ---
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
                
                # collapsible parameter glossary for LSTM
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
                p("For a copy of the report, including all code from the initial exploratory data analysis, you can download the R Markdown file directly. This is the script that formed the basis of this application."),
                tags$ul(
                  tags$li(downloadLink("downloadRmd", strong("Download Full Analysis Report (.Rmd)")))
                )
              )
            )
    )
  } else if (which == "about") {
    # --- Author Information Tab ---
    tabItem(tabName = "about",
            fluidRow(
              box(
                title = "About This Application",
                solidHeader = TRUE,
                status = "primary",
                width = 12,
                h3("Author Information"),
                p("This application is designed to analyze and visualize US weather data."),
                tags$ul(
                  tags$li(HTML("<b>Author:</b> Peiyu Liu")),
                  tags$li(HTML("<b>Affiliation:</b> Department of Biostatistics, University of Florida")),
                  tags$li(HTML("<b>Contact:</b> <a href='mailto:pyliu0620@outlook.com'>pyliu0620@outlook.com</a>"))
                ),
                hr()
              )
            )
    )
  }
}