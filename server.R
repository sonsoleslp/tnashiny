library(shiny)
library(DT)
library(tna)
library(rio)
library(shinyjs)
library(shinyBS)
library(colourpicker)
library(sortable)

# For arranging multiple plots
set.seed(19)

source("utils.R")
source("sections/input.R")
source("sections/intro.R")
source("sections/summary.R")
source("sections/visualization.R")
source("sections/sequences.R")
source("sections/centralities.R")
source("sections/edge-betweenness.R")
source("sections/communities.R")
source("sections/cliques.R")
source("sections/comparison.R")
source("sections/validation.R")
source("ui.R")


# Server
server <- function(input, output, session) {
  # Reactive values to store the analysis results
  rv <- reactiveValues(
    original = NULL,
    data = NULL,
    tna_result = NULL,
    centrality_result = NULL,
    cliques_result = NULL,
    clique_plots = list(),
    community_result = NULL,
    bootstrap_result = NULL,
    cs_result = NULL,
    states = NULL,
    meta_data = NULL
  )
  observeEvent(input$inputType, {
    rv$original <- NULL
  })
  
  # Read and process input data
  analyzeInputData(rv, input, output, session)

  # Data Preview
  previewData(rv, input, output, session)
  
  renderSummaryResults(rv, input, output, session)
  
  renderSequencePlots(rv, input, output, session)

  renderCentralityResults(rv, input, output, session)
  
  renderEdgeBetweenness(rv, input, output, session)
  
  renderCommunityResults(rv, input, output, session)
  
  renderCliqueResults(rv, input, output, session)
  
  renderComparison(rv, input, output, session)
  
  renderValidation(rv, input, output, session)
  
}
# Run the application
# shinyApp(ui = ui, server = server)
