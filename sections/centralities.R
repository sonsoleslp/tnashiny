centralitiesList <- c(
  "OutStrength",
  "InStrength",
  "ClosenessIn",
  "ClosenessOut",
  "Closeness",
  "BetweennessRSP",
  "Betweenness",
  "Diffusion",
  "Clustering"
)

getCentralities <- function() {
  fluidRow(
    box(
      fluidRow(
        column(
          width = 6,
          selectInput("centralitiesChoice", "Centralities", multiple = TRUE, 
                      choices = centralitiesList, selected = centralitiesList)
        ),
        column(
          width = 2,
          tags$label("Properties"),
          checkboxInput("loops", "Loops?", value = FALSE),
          checkboxInput("normalize", "Normalize?", value = FALSE),
          class = "checkboxcentralities"
        ),
        column(
          width = 2, 
          selectInput("paletteCentralities", "Palette", choices = getPalettes(9), selected = "Set3"),
          numericInput("nColsCentralities", 
                       "Columns", 3, min = 1, max = 9, step = 1),
          
        )
      ),
      width = 12
    ),
    box(
    fluidRow(
        column(
          width = 12, 
          div(
            title = "Centrality Measures",
            width = 12,
            fluidRow(
            div(
              tableOutput("centralityPrint"),
              align = "center",
              width = 12
            ),
            div(
              jqui_resizable(
                plotOutput(
                  "centralityPlot",
                  width = "800px",
                  height = "800px"
                ),
                # Render the TNA plot here
                options = list(ghost = TRUE, helper = "resizable-helper")
              ),
              align = "center",
              width = 12
            )
          )
        )
      )
    ),  
    width = 12
  )
)
}

renderCentralityResults <- function(rv, input, output, session) {
  # Centrality Measures
  output$centralityPlot <- renderPlot({
    req(rv$tna_result)
    # Calculate centrality measures
    centrality_result <- centralities(
      rv$tna_result,
      measures = input$centralitiesChoice,
      normalize = input$normalize,
      loops = input$loops
    )
    rv$centrality_result <- centrality_result
    # Plot centrality measures
    tryCatch({
      plot(centrality_result, 
           colors = getPalette(input$paletteCentralities, length(rv$tna_result$labels)),
           ncol = input$nColsCentralities)
    }, warning = function(w) {
      logjs(w)
    }, error = function(e) {
      showGenericError()
      logjs(e)
    }, silent = TRUE)
  }, res = 100)
  output$centralityPrint <- renderTable({
    req(rv$centrality_result)
    # Print centrality measures
    (data.frame(rv$centrality_result))
  })
}