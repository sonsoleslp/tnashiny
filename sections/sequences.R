getSAAllowed <- function() {
  conditionalPanel(condition = "(input.inputType != 'matrix') | !input.inputType",
                   fluidRow(
                      
                       box(title = "Index plot", width = 6,
                           selectInput("paletteIndex", "Palette", choices = getPalettes(9), selected = "Set3"),
                           div(jqui_resizable(
                             plotOutput("sequencePlotIndex", width = "600px", height = "600px"),
                             # Render the TNA plot here
                             options = list(ghost = TRUE, helper = "resizable-helper")
                           ), align = "center")
                       ),
                       box(title = "Distribution plot", width = 6,
                           selectInput("paletteDist", "Palette", choices = getPalettes(9), selected = "Set3"),
                           div(jqui_resizable(
                             plotOutput("sequencePlotDist", width = "600px", height = "600px"),
                             # Render the TNA plot here
                             options = list(ghost = TRUE, helper = "resizable-helper")
                           ), align = "center")
                       )
                     
                   ))
}

getSANotAllowed <- function() {
  conditionalPanel(condition = "input.inputType == 'matrix'",
     box(
       span(
         icon("circle-info", class = "text-danger"),
         "Sequence analysis is only supported when the full data is provided"
       ),
       width = 7
     )
  )
}

renderSequencePlots <- function(rv, input, output, session) {
  output$sequencePlotIndex  <- renderPlot({
    req(rv$tna_result)
    
    # Plot centrality measures
    tryCatch({
      plot_sequences(rv$tna_result,
                     colors = input$paletteDist) + 
        ggplot2::theme(legend.position = "bottom", 
                       title = ggplot2::element_blank())
    }, warning = function(w) {
      logjs(w)
    }, error = function(e) {
      showGenericError()
      logjs(e)
      print(e)
    }, silent = TRUE)
  }, res = 100)
  
  output$sequencePlotDist  <- renderPlot({
    req(rv$tna_result)
    
    # Plot centrality measures
    tryCatch({
      plot_sequences(rv$tna_result, type = "dist", 
                     colors = input$paletteDist,
                     include_na = FALSE)  + 
        ggplot2::theme(legend.position = "bottom",
                       title = ggplot2::element_blank())
    }, warning = function(w) {
      logjs(w)
    }, error = function(e) {
      showGenericError()
      logjs(e)
      print(e)
    }, silent = TRUE)
  }, res = 100)
}