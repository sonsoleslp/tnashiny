getValidationAllowed <- function() {
  conditionalPanel(condition = "input.inputType & (input.inputType != 'matrix') & (input.inputType != 'one-hot')",
  fluidRow(
    tabBox(
       id = "tabset2", 
       width = 12,
       tabPanel("Bootstrapping", 
         fluidRow(
         column(fluidRow(
           box(
             title = "Bootstrapping",
             numericInput("iterBoot", "Iteration:", min = 0, max = 10000, value = 1000, step = 100),
             numericInput("levelBoot", "Level:", min = 0, max = 1, value = 0.05, step = 0.01),
             selectInput("methodBoot", "Method", choices = c("stability", "threshold"), selected = "stability"),
             conditionalPanel(
               condition = "input.methodBoot == 'threshold'",
               numericInput("thresBoot", "Threshold:", min = 0, max = 1, value = 0.1, step = 0.01)
             ),
             conditionalPanel(
               condition = "input.methodBoot == 'stability'",
               h4("Consistency Range"),
               numericInput("constLowerBoot", "Lower:", min = 0, max = 10, value = 0.75, step = 0.01),
               numericInput("constUpperBoot", "Upper:", min = 0, max = 10, value = 1.25, step = 0.01)
             ),
             actionButton("bootstrapButton", "Bootstrap", class = "btn-primary"),
             width = 12
           ),
          getVisualizationSettings("Boot", 12),
          width = 12
         ),
         width = 3),
         column(fluidRow(
           getVisualizationPlot("Boot", 12),
           width = 12
         ),
         width = 9)
       )
    ),
     tabPanel(
       "Case dropping", 
       fluidRow(
         box(
           fluidRow(
             column(
               width = 6,
               selectInput("centralitiesChoiceCase", "Centralities", 
                           multiple = TRUE, choices = centralitiesList, 
                           selected = c("OutStrength","InStrength"))
             ),
             column(
               width = 2,
               tags$label("Properties"),
               checkboxInput("loopsCase", "Loops?", value = FALSE),
               checkboxInput("normalizeCase", "Normalize?", value = FALSE),
               class = "checkboxcentralities"
             ),
             column(
               width = 2, 
               numericInput("thresholdCase", "Threshold", 0.7, 
                            min = 0, max = 1, step = 0.01),
               numericInput("iterCase", "Iteration:", 
                            min = 0, max = 10000, value = 1000, step = 100),
               
             ),
             column(
               width = 2, 
               actionButton("csButton", "Bootstrap", class = "btn-primary"),
             )
           ),
           width = 12
         ),
         box(
           fluidRow(
             column(div(jqui_resizable(
               plotOutput("caseDropping", width = "1200px", height = "600px"),
               # Render the TNA plot here
               options = list(ghost = TRUE, helper = "resizable-helper")
             ), align = "center"), width = 12)), width = 12)
     )
   ))))
}
getValidationNotAllowed <- function() {
  conditionalPanel(condition = "(input.inputType == 'matrix') | (input.inputType == 'one-hot')",
       box(
         span(
           icon("circle-info", class = "text-danger"),
           "Validation operations are only supported when the full data is provided"
         ),
         width = 7
       )
    )
}



renderValidation <- function(rv, input, output, session) {
  
  output$summary_boot_model <- renderPrint({
    rv$bootstrap_result
  })
  
  observeEvent(input$bootstrapButton, {
    req(rv$tna_result)
    tryCatch({
      if (!is.null(rv$tna_result)) {
        boot <- tna::bootstrap(
          rv$tna_result,
          iter = input$iterBoot,
          level = input$levelBoot,
          method  = input$methodBoot,
          threshold = input$thresBoot,
          consistency_range = sort(c(
            input$constLowerBoot,
            input$constUpperBoot
          ))
        )
        rv$bootstrap_result <- boot
      }
    }, warning = function(w) {
      print(w)
      logjs(w)
    }, error = function(e) {
      showGenericError()
      logjs(e)
      print(e)
    }, silent = TRUE)
  })
  
  output$BootPlot <- renderPlot({
    req(rv$bootstrap_result)
    tryCatch({
      plot(
        rv$bootstrap_result,
        cut = input$cutBoot,
        minimum = input$minimumBoot,
        shape = input$shapeBoot,
        label.cex = input$node.labelBoot,
        edge.label.cex = input$edge.labelBoot,
        edge.color = input$edgeColorBoot,
        vsize = input$vsizeBoot,
        layout = getLayout(input$layoutBoot),
        mar = mar
      )
    }, warning = function(w) {
      print(w)
      logjs(w)
    }, error = function(e) {
      showGenericError()
      logjs(e)
      print(e)
    }, silent = TRUE)
  }, res = 600)
  
  output$bootstrappedtnaModel <- renderUI({
    if (is.null(rv$bootstrap_result)) {
      NULL
    } else {
      verbatimTextOutput("summary_boot_model")
    }
  })
  
  observeEvent(input$csButton, {
    req(rv$tna_result)
    tryCatch({
      rv$cs_result <- estimate_cs(
        rv$tna_result,
        loops = input$loopsCase,
        normalize = input$normalizeCase,
        measures = input$centralitiesChoiceCase,
        iter = input$iterCase,
        method = "pearson",
        drop_prop = seq(0.1, 0.9, by = 0.1),
        threshold = input$thresholdCase,
        certainty = 0.95,
        progressbar = FALSE
      )
    }, warning = function(w) {
      print(w)
      logjs(w)
    }, error = function(e) {
      showGenericError()
      logjs(e)
      print(e)
    }, silent = TRUE)
  })
  
  output$caseDropping <- renderPlot({
    tryCatch({
      plot(rv$cs_result)
    }, warning = function(w) {
      print(w)
      logjs(w)
    }, error = function(e) {
      showGenericError()
      logjs(e)
      print(e)
    }, silent = TRUE)
  }, res = 100)
  
}