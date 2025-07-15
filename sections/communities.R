getCommunities <- function() {
  fluidRow(# Left Sidebar (3-column width)
    column(width = 3,
           fluidRow(
             box(
               title = "Community Detection Settings",
               width = 12,
               selectInput("communityAlgorithm",
                           "Choose Algorithm:",
                           choices = "spinglass"),
               numericInput(
                 "gamma",
                 "Gamma (for certain algorithms):",
                 value = 1,
                 min = 0,
                 max = 100
               )
             ),
             getVisualizationSettings("Com", 12),
             width = 12
           )),
    # Right Main Content (9-column width)
    # Second Block
    getVisualizationPlot("Com"))
}

renderCommunityResults <- function(rv, input, output, session) {
  # Display Community Counts
  output$communityCounts <- renderPrint({
    req(rv$community_result)
    # Print the counts of communities found by each algorithm
    rv$community_result$counts
  })
  # Plot the selected community algorithm
  output$ComPlot <- renderPlot({
    req(rv$tna_result)
    # Detect communities in the transition networks
    rv$community_result <-
      tna::communities(rv$tna_result, gamma = input$gamma)
    # Populate the dropdown with algorithm names and community counts
    algorithm_choices <-
      sapply(names(rv$community_result$counts), function(alg) {
        paste0(alg, " (", rv$community_result$counts[[alg]], " communities)")
      })
    names(algorithm_choices) <-
      names(rv$community_result$counts)  # Set algorithm names as values
    choices <- names(algorithm_choices)
    names(choices) <-
      paste0(names(rv$community_result$counts),
             " (",
             rv$community_result$counts,
             ")")
    selected_algorithm <- input$communityAlgorithm
    updateSelectInput(
      session,
      "communityAlgorithm",
      choices = choices,
      selected = input$communityAlgorithm
    )  # Select the first algorithm by default
    # Plot the selected community detection algorithm's results
    tryCatch({
      plot(
        rv$community_result,
        method = selected_algorithm,
        mar = mar,
        cut = input$cutCom,
        minimum = input$minimumCom,
        label.cex = input$node.labelCom,
        edge.color = input$edgeColorCom,
        edge.label.cex = input$edge.labelCom,
        curveAll = input$curveAllCom,
        vsize = input$vsizeCom,
        shape = input$shapeCom,
        colors = getPalette(input$paletteCom, rv$community_result$counts[[selected_algorithm]]),
        layout = getLayout(input$layoutCom)
      )
    }, warning = function(w) {
      logjs(w)
      print(w)
    }, error = function(e) {
      showGenericError()
      logjs(e)
      print(e)
    }, silent = TRUE)
  }, res = 600)
}