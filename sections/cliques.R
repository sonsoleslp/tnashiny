getCliques <- function() {
  fluidRow(
    column(
      width = 3,
      fluidRow(
       box(
         title = "Clique Settings",
         width = 12,
         numericInput("cliqueSize", "Clique Size (n):", value = 3, min = 2, max = 10),
         numericInput("cliqueThreshold", "Threshold:", value = 0, min = 0, max = 1, step = 0.05),
         actionButton("findCliques", "Find Cliques", class = "btn-primary"),
         selectInput("cliqueSelect", "Choose Clique:", choices = NULL)
       ),
       getVisualizationSettings("Clique", 12)
      )
    ),
    getVisualizationPlot("Clique")
  )
}

renderCliqueResults <- function(rv, input, output, session) {
  # Clique Finding Logic
  observeEvent(input$findCliques, {
    req(rv$tna_result)
    req(input$cliqueSize)
    req(input$cliqueThreshold)
    # Identify cliques based on user input
    rv$cliques_result <- tna::cliques(
      rv$tna_result,
      size = input$cliqueSize,
      threshold = input$cliqueThreshold,
      n = 1000
    )
    if (length(rv$cliques_result$inits) > 0) {
      choices <- seq_along(rv$cliques_result$inits)
      names(choices) <- lapply(rv$cliques_result$inits,
                               \(x) names(x) |> paste(collapse = " - "))
      names(choices) <- paste0("Clique ", choices, ": ", names(choices))
      updateSelectInput(session, "cliqueSelect", choices = choices, selected = 1)  
    } else {
      updateSelectInput(session, "cliqueSelect", selected = character(0), choices = character(0))
    }
  })
  # Plot Cliques
  output$CliquePlot <- renderPlot({
    req(rv$cliques_result)
    if (is.null(input$cliqueSelect) | (input$cliqueSelect == "")) {
      return(NULL)
    } else {
      tryCatch({
        if (length(rv$cliques_result$inits) > 0) {
          sapply(rv$tna_result$labels, \(x) which(x == names(rv$cliques_result$inits), arr.ind = T))
          plot(
            rv$cliques_result,
            first = as.integer(input$cliqueSelect),
            n = 1,
            ask = FALSE,
            cut = input$cutClique,
            minimum = input$minimumClique,
            label.cex = input$node.labelClique,
            edge.color = input$edgeColorClique,
            edge.label.cex = input$edge.labelClique,
            curveAll = input$curveAllClique,
            vsize = input$vsizeClique,
            colors = getPalette(input$paletteClique, length(rv$tna_result$labels)),
            layout = getLayout(input$layoutClique),
            mar = mar
          )
        }
      }, warning = function(w) {
        logjs(w)
        print(w)
      }, error = function(e) {
        showGenericError()
        logjs(e)
        print(e)
      }, silent = TRUE)
    }
  }, res = 600)
}