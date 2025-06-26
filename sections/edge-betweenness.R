# Plot Edge betwenness
renderEdgeBetweenness <- function(rv, input, output, session) {
  output$EbetPlot <- renderPlot({
    req(rv$tna_result)
    # Plot the TNA results directly
    tryCatch({
      plot(
        betweenness_network(rv$tna_result),
        cut = input$cut,
        minimum = input$minimumEbet,
        label.cex = input$node.labelEbet,
        edge.label.cex = input$edge.labelEbet,
        edge.color = input$edgeColorEbet,
        vsize = input$vsizeEbet,
        shape = input$shapeEbet,
        layout = getLayout(input$layoutEbet),
        colors = getPalette(input$paletteEbet, length(rv$tna_result$labels)),
        mar = mar
      )
    }, warning = function(w) {
      logjs(w)
    }, error = function(e) {
      showGenericError()
      logjs(e)
      print(e)
    }, silent = TRUE)
  }, res = 600)
}