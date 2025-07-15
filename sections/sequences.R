getSAAllowed <- function() {
  conditionalPanel(condition = "(input.inputType != 'matrix') & (input.inputType != 'one-hot')",
                   fluidRow(
                       box(title = "Settings", width = 3,
                               selectInput("typeSequence", "Type", choices = c("index","distribution"), selected = "index"),
                               selectInput("paletteSequence", "Palette", choices = getPalettes(9), selected = "Set3"),
                               selectInput("scaleSequence", "Scale", choices = c("proportion","count"), selected = "proportion"),
                               selectInput("groupSequence", "Grouping column:", choices = NULL),
                               # textInput("titleSequence", "Plot Title" ),
                               textInput("xAxisSequence", "Title x-axis", value = "Time"),
                               textInput("yAxisSequence", "Title y-axis" ),
                               # textInput("legendSequence", "Legend title" ),
                               # checkboxInput("showNSequence", "No. of observations", value = FALSE),
                               numericInput("tickSequence", "Tick frequency", value = 5),
                               numericInput("ncolSequence", "Columns", value = 2, min = 0, max = 100),
                               # checkboxInput("borderSequence", "Border", value = FALSE),
                               checkboxInput("include_naSequence", "Include empty values", value = FALSE),
                       ),
                       box(title = "Sequence plot", width = 9,
                           div(jqui_resizable(
                             plotOutput("sequencePlot", width = "100%"),
                             # Render the TNA plot here
                             options = list(ghost = TRUE, helper = "resizable-helper")
                           ), align = "center")
                       )
                   )
                )
}

getSANotAllowed <- function() {
  conditionalPanel(condition = "(input.inputType == 'matrix') | (input.inputType == 'one-hot')",
     box(
       span(
         icon("circle-info", class = "text-danger"),
         "Sequence analysis is only supported when wide or long data are provided"
       ),
       width = 7
     )
  )
}

renderSequencePlots <- function(rv, input, output, session) {
  output$sequencePlot  <- renderPlot({
    req(rv$tna_result)
    
    # Plot centrality measures
    tryCatch({
      theModel <- rv$tna_result
      group <- rlang::missing_arg()
      if (!is.null(input$groupSequence) & (input$groupSequence != " ")) {
        theModel <-  buildGroupModel(rv, input, input$groupSequence) 
        group <- input$groupSequence
      }
      p <- plot_sequences(theModel,
                          type = input$typeSequence,
                          # show_n = input$showNSequence,
                          # border = input$borderSequence,
                          scale = input$scaleSequence,
                          xlab = input$xAxisSequence,
                          ylab = input$yAxisSequence,
                          # legend_title = input$legendSequence,
                          # title = input$titleSequence,
                          tick = input$tickSequence,
                          ncol = input$ncolSequence,
                          include_na = input$include_naSequence) + 
        ggplot2::theme(legend.position = "bottom", 
                       legend.ticks = ggplot2::element_blank(),
                       plot.title = ggplot2::element_blank()) 
      if (input$paletteSequence != "Default") {
        p <- p + ggplot2::scale_fill_brewer(palette = input$paletteSequence)
      }
      p
    }, warning = function(w) {
      logjs(w)
    }, error = function(e) {
      showGenericError()
      logjs(e)
      print(e)
    }, silent = TRUE)
  }, res = 120)
  
}