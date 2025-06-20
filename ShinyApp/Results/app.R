# app.R
# Shiny app displaying ggplot metrics from a single RDS file

library(shiny)
library(ggplot2)
library(patchwork)

# Load pre-generated list of ggplot objects
# Data_plot_winds.rds should contain a named list of ggplot objects
# If it is nested (e.g., list(Raw = list(...), Relative = list(...))),
# this code will detect and handle it.
plots <- readRDS("Data_plot_winds.rds")

# Determine available metrics and plot extractor function
if (all(sapply(plots, function(x) inherits(x, "ggplot")))) {
  # Flat list of ggplot objects
  metric_names <- names(plots)
  extract_plots <- function(metric) plots[[metric]]
} else if (is.list(plots) && length(plots) > 0 &&
           all(sapply(plots, function(x) is.list(x) && all(sapply(x, inherits, "ggplot"))))) {
  # Nested list: list(<region or scale> = list(<metric> = ggplot, ...), ...)
  metric_names <- names(plots[[1]])
  extract_plots <- function(metric) {
    # collect ggplots for this metric across outer list
    lapply(plots, `[[`, metric)
  }
} else {
  stop("Data_plot_winds.rds has unsupported structure")
}

# UI ------------------------------------------------------------------
ui <- fillPage(
  tags$head(
    tags$style(HTML(
      "
      body, html { margin: 0; }
      #plot-box, #plot-box > .shiny-plot-output {
        height: 100vh; width: 100vw;
      }
      #controls {
        position: absolute;
        bottom: 1rem; right: 1rem;
        width: 230px;
        background: rgba(255,255,255,.92);
        border-radius: .5rem;
        box-shadow: 0 0.5rem 1rem rgba(0,0,0,.15);
        padding: .8rem 1rem;
        z-index: 1000;
      }
      #controls .shiny-input-container { margin-bottom: .8rem; }
      "
    ))
  ),
  div(id = "plot-box",
      plotOutput("displayPlot", height = "100%", width = "100%"),
      div(id = "controls",
          radioButtons(
            inputId = "metric",
            label   = "Select metric:",
            choices = metric_names,
            selected = metric_names[1]
          )
      )
  )
)

# Server ---------------------------------------------------------------
server <- function(input, output, session) {
  output$displayPlot <- renderPlot(res = 120, {
    # Extract the plot(s) for the selected metric
    p_objs <- extract_plots(input$metric)
    
    # If we get a single ggplot, render directly
    if (inherits(p_objs, "ggplot")) {
      p_objs
    } else if (is.list(p_objs) && all(sapply(p_objs, inherits, "ggplot"))) {
      # Combine multiple ggplots via patchwork
      wrap_plots(p_objs, ncol = 2) +
        plot_layout(guides = "collect") &
        theme(legend.position = "top")
    } else {
      stop("Unsupported plot object for metric: ", input$metric)
    }
  })
}

# Run the app ----------------------------------------------------------
shinyApp(ui, server)

# rsconnect::deployApp(appDir = "ShinyApp/Results")