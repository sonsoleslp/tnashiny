getSummary <- function(x) {
  div(
    fluidRow(
       box(
        title = "Summary Statistics",
        width = 3,
        tableOutput("summaryStats")
      ),
      box(
        title = "Initial Probabilities",
        width = 4,
        DTOutput("initialProbs")
      ), 
      box(
        title = "Transition Matrix",
        width = 5,
        div(class = "responsive-table", DTOutput("transitionMatrix"))
      )
    ),
    fluidRow(
      box(
        title = "Histogram",
        width = 6,
        div(jqui_resizable(
          plotOutput("histogram", width = "700px", height = "400px"),
          # Render the TNA plot here
          options = list(ghost = TRUE, helper = "resizable-helper")
        ), align = "center")
      ),
      box(
        title = "Frequencies",
        width = 6,
        div(jqui_resizable(
          plotOutput("frequencies", width = "700px", height = "400px"),
          # Render the TNA plot here
          options = list(ghost = TRUE, helper = "resizable-helper")
        ), align = "center")
      ),
      conditionalPanel(
        condition = "input.type == 'absolute'",
        box(
          title = "Mosaic plot",
          width = 8,
          div(jqui_resizable(
            plotOutput("mosaic", width = "900px", height = "700px"),
            # Render the TNA plot here
            options = list(ghost = TRUE, helper = "resizable-helper")
          ), align = "center")
        )
      )
    )
  ) 
}


renderSummaryResults <- function(rv, input, output, session) {
  
  output$summary_model <- renderPrint({
    toPrint <- rv$tna_result
    toPrint$weights <- round(rv$tna_result$weights, 3)
    if(!is.null(rv$tna_result$inits)) {
      toPrint$inits <- round(rv$tna_result$inits, 3)
    }
    toPrint
  })
  
  output$tnaModel <- renderUI({
    if (is.null(rv$tna_result)) {
      NULL
    } else {
      verbatimTextOutput("summary_model")
    }
  })
  
  # Plot TNA results
  output$Plot <- renderPlot({
    req(rv$tna_result)
    tryCatch({
      plot(
        rv$tna_result,
        cut = input$cut,
        shape = input$shape,
        minimum = input$minimum,
        label.cex = input$node.label,
        edge.label.cex = input$edge.label,
        edge.color = input$edgeColor,
        vsize = input$vsize,
        layout = getLayout(input$layout),
        colors = getPalette(input$palette, length(rv$tna_result$labels)),
        mar = mar
      )
    }, warning = function(w) {
      logjs(w)
    }, error = function(e) {
      showGenericError()
      logjs(e)
    }, silent = TRUE)
  }, res = 600)
  
  # Transition Matrix Display
  output$transitionMatrix <- renderDT({
    req(rv$tna_result)
    datatable(round(rv$tna_result$weights, 3),
              options = list(pageLength = 10, scrollX = TRUE))
  })
  # Initial Probabilities Display
  output$initialProbs <- renderDT({
    req(rv$tna_result)
    inits <- rv$tna_result$inits
    if(!is.null(inits)) {
      init_probs <- data.frame(Probability = round(inits, 3))
      datatable(init_probs, options = list(pageLength = 10, scrollX = TRUE))
    }
  })
  # Summary Statistics
  output$summaryStats <- renderTable({
    req(rv$tna_result)
    summary(rv$tna_result)
  })
  
  # Histogram Plot
  output$histogram <- renderPlot({
    req(rv$tna_result)
    
    tryCatch({
      hist(rv$tna_result)
    }, warning = function(w) {
      logjs(w)
    }, error = function(e) {
      showGenericError()
      logjs(e)
    }, silent = TRUE)
  }, res = 100)
  
  # Frequencies Plot
  output$frequencies <- renderPlot({
    req(rv$tna_result)
    
    tryCatch({
      plot_frequencies(rv$tna_result, colors = "lightblue")
    }, warning = function(w) {
      logjs(w)
    }, error = function(e) {
      showGenericError()
      logjs(e)
    }, silent = TRUE)
  }, res = 100)
  
  
  output$mosaic <- renderPlot({
    req(rv$tna_result)
    
    tryCatch({
      plot_mosaic(rv$tna_result)
    }, warning = function(w) {
      logjs(w)
    }, error = function(e) {
      showGenericError()
      logjs(e)
      print(e)
    }, silent = TRUE)
  }, res = 100)
}