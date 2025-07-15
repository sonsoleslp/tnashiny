
buildGroupModel <- function(rv, input, group) {
  tryCatch({
    groupm <- NULL
    if (input$inputType == "sequence") {
      groupm <- group_model(
        req(rv$original),
        type = input$type,
        scaling = input$scaling,
        cols = seq(as.integer(input$seqFrom), as.integer(input$seqTo)),
        group =  group,
        na.rm	= T
      )
    } else {
      groupm <- group_model(
        req(rv$data),
        scaling = input$scaling,
        type = input$type,
        group = group,
        na.rm	= T
      )
    }
    return(groupm)
  }, error = function(e) {
    print(e)
    return(NULL)
    
  })
}

getComparisonAllowed <- function() {
  conditionalPanel(condition = "(input.inputType != 'matrix') & (input.inputType != 'one-hot')",
                   fluidRow(
                         box(
                           title = "Comparison Settings",
                           width = 12,
                           fluidRow(
                             column(selectInput("compareSelect", "Choose grouping column:", choices = NULL), width = 3),
                             column(selectInput("group1", "Choose group 1:", choices = NULL), width = 3),
                             column(selectInput("group2", "Choose group 2:", choices = NULL), width = 3),
                             column(
                               fluidRow(input_switch("compare_sig", "Permutation test")),
                               fluidRow(conditionalPanel(
                                 condition = "input.compare_sig",
                                 column(numericInput("iterPerm", "Iteration:", min = 0, max = 10000, value = 1000, step = 100 ), width = 6),
                                 column(numericInput("levelPerm", "Level:", min = 0, max = 1, value = 0.05, step = 0.01 ), width = 6),
                                 column(input_switch("pairedPerm", "Paired test"), width = 6)
                               )
                            ), width = 3)
                        ),
                      )
                  ),
                   fluidRow(
                    
                            tabBox(
                              id = "tabset1",
                              width = 12,
                              tabPanel("Plot",
                                 fluidRow(
                                   getVisualizationSettings("Group", width = 3, TRUE),
                                   column(
                                     jqui_resizable(
                                       plotOutput("comparisonPlot", 
                                                  width = "600px", 
                                                  height = "600px"),
                                       # Render the TNA plot here
                                       options = list(ghost = TRUE, 
                                                      helper = "resizable-helper")
                                     ),
                                     align = "center", width = 9
                                   )
                                )
                              ),
                              tabPanel(
                                "Table",
                                conditionalPanel(condition = "input.compare_sig",
                                                 DTOutput("comparisonDiffs")),
                                conditionalPanel(condition = "!input.compare_sig",
                                                 fluidRow(box(
                                                   span(
                                                     icon("circle-info", class = "text-danger"),
                                                     "Table is shown for permutation test results. Check the 'Permutation test' checkbox to see it"
                                                   ),
                                                   width = 7
                                                 )))
                              ),
                              tabPanel("Frequencies",
                                       fluidRow(
                                         column(fluidRow(
                                           column(width = 12, checkboxInput("showLabelsFrequenciesGroup", "Show labels", value = FALSE)),
                                           column(width = 12, selectInput("positionFrequenciesGroup", "Position", choices = c(Stacked = "stack", `Side by side`= "dodge",  `Side by side (no placeholder)`= "dodge2", Fill= "Fill"), selected = "Set3")),
                                           column(width = 12, sliderInput("widthFrequenciesGroup", "Bar width", min = 0, max = 1, value = 0.7, step = 0.1, ticks = FALSE)),
                                           column(width = 12, selectInput("paletteFrequenciesGroup", "Palette", choices = getPalettes(2), selected = "Set3")),
                                           column(width = 12, checkboxInput("paletteFrequenciesGroupReverse", "Reverse Palette")
                                          )), width = 3),
                                         column(div(
                                           jqui_resizable(
                                             plotOutput(
                                               "comparisonFrequencies",
                                               width = "900px",
                                               height = "500px"
                                             ),
                                             # Render the TNA plot here
                                             options = list(ghost = TRUE, helper = "resizable-helper")
                                           ),
                                           align = "center"
                                         ), width = 9)
                              )),
                              tabPanel("Mosaic",
                                       div(
                                         jqui_resizable(
                                           plotOutput("mosaicPlot", width = "600px", height = "500px"),
                                           # Render the TNA plot here
                                           options = list(ghost = TRUE, helper = "resizable-helper")
                                         ),
                                         align = "center"
                                       )),
                              tabPanel(
                                "Centralities",
                                fluidRow(
                                  column(
                                    fluidRow(
                                      column(width = 12, selectInput("centralitiesChoiceGroup", "Centralities", multiple = TRUE, choices = centralitiesList, selected = centralitiesList)),
                                      column(
                                        width = 12,
                                        tags$label("Properties"),
                                        checkboxInput("loopsGroup", "Loops?", value = FALSE),
                                        checkboxInput("normalizeGroup", "Normalize?", value = FALSE),
                                        class = "checkboxcentralities"
                                      ),
                                      column(width = 12, numericInput("nColsCentralitiesGroup", "Columns", 3, min = 1, max = 9, step = 1)),
                                      column(width = 12, selectInput("paletteCentralitiesGroup", "Palette", choices = getPalettes(2), selected = "Set3")),
                                      column(width = 12, checkboxInput("paletteCentralitiesGroupReverse", "Reverse Palette"))
                                      
                                    ),
                                    width = 2
                                  ),
                                  column(
                                    div(jqui_resizable(
                                  plotOutput(
                                    "groupCentralitiesPlot",
                                    width = "900px",
                                    height = "600px"
                                  ),
                                  # Render the TNA plot here
                                  options = list(ghost = TRUE, helper = "resizable-helper")
                                ),
                                align = "center"),
                                conditionalPanel(condition = "input.compare_sig",
                                                 DTOutput("groupCentralitiesTable")), width = 10)
                              
                            )
                          ))))
}

getComparisonNotAllowed <- function(x) {
  conditionalPanel(condition = "(input.inputType == 'matrix') | (input.inputType == 'one-hot')",
                   box(
                     span(
                       icon("circle-info", class = "text-danger"),
                       "Comparison operations are not supported for this type of data"
                     ),
                     width = 7
                   ))
}


renderComparison <- function(rv, input, output, session) {
  observeEvent(input$compareSelect, {
    if (is.null(rv$meta_data)) {
      return()
    }
    choices <- unique(data.frame(rv$meta_data)[, input$compareSelect])
    updateSelectInput(
      session,
      "group1",
      choices = choices,
      selected = ifelse(
        !is.null(choices) | (length(choices) > 0),
        choices[1],
        rlang::missing_arg()
      )
    )
    updateSelectInput(
      session,
      "group2",
      choices = choices,
      selected = ifelse(
        !is.null(choices) | (length(choices) > 1),
        choices[2],
        rlang::missing_arg()
      )
    )
  })
  
  output$comparisonDiffs <- renderDT({
    req(rv$data)
    tryCatch({
      group_tnad <- buildGroupModel(rv, input, input$compareSelect)
      
      if (input$compare_sig) {
        differentrows <-
          nrow(group_tnad[[req(input$group1)]]$data) != nrow(group_tnad[[req(input$group2)]]$data)
        
        permtest <- permutation_test(
          group_tnad[[req(input$group1)]],
          group_tnad[[req(input$group2)]],
          iter = input$iterPerm,
          paired = ifelse(differentrows, FALSE, input$pairedPerm),
          level = input$levelPerm
        )
        if (differentrows & input$pairedPerm) {
          showGenericError(
            "Paired test cannot be applied because groups have different number of observations"
          )
        }
        
        permedges <- data.frame(permtest$edges$stats) |>
          tidyr::separate(edge_name, sep = " -> ", into = c("From", "To")) |>
          dplyr::mutate_at(3:5, \(x) round(as.double(x), 3))
        
        datatable(permedges, options = list(pageLength = 10, scrollX = TRUE))
        
      } else {
        return (NULL)
        showGenericError("Choose permutation test first")
      }
    }, warning = function(w) {
      logjs(w)
      print(w)
    }, error = function(e) {
      showGenericError()
      logjs(e)
      print(e)
    }, silent = TRUE)
    
  })
  
  output$comparisonPlot <- renderPlot({
    req(rv$data)
    tryCatch({
      group_tnad <- buildGroupModel(rv, input, input$compareSelect)
      if (input$compare_sig) {
        differentrows <-
          nrow(group_tnad[[req(input$group1)]]$data) != nrow(group_tnad[[req(input$group2)]]$data)
        permtest <- permutation_test(
          group_tnad[[req(input$group1)]],
          group_tnad[[req(input$group2)]],
          iter = input$iterPerm,
          paired = ifelse(differentrows, FALSE, input$pairedPerm),
          level = input$levelPerm
        )
        if (differentrows & input$pairedPerm) {
          showGenericError(
            "Paired test cannot be applied because groups have different number of observations"
          )
        }
        plot(
          permtest,
          cut = input$cutGroup,
          minimum = input$minimumGroup,
          label.cex = input$node.labelGroup,
          edge.label.cex = input$edge.labelGroup,
          colors = getPalette(input$paletteGroup, length(rv$tna_result$labels)),
          vsize = input$vsizeGroup,
          layout = getLayout(input$layoutGroup),
          posCol = input$posEdgeColorGroup,
          negCol  = input$negEdgeColorGroup,
          mar = mar
        )
      } else {
        plot_compare(
          group_tnad[[req(input$group1)]],
          group_tnad[[req(input$group2)]],
          cut = input$cutGroup,
          minimum = input$minimumGroup,
          label.cex = input$node.labelGroup,
          edge.label.cex = input$edge.labelGroup,
          curveAll = input$curveAllGroup,
          colors = getPalette(input$paletteGroup, length(rv$tna_result$labels)),
          vsize = input$vsizeGroup,
          shape = input$shapeGroup,
          layout = getLayout(input$layoutGroup),
          posCol = input$posEdgeColorGroup,
          negCol  = input$negEdgeColorGroup,
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
  }, res = 600)
  
  output$mosaicPlot <- renderPlot({
    req(rv$tna_result)
    tryCatch({
      group_tnad <- buildGroupModel(rv, input, input$compareSelect)
      plot_mosaic(group_tnad)
      
    }, warning = function(w) {
      print(w)
      logjs(w)
    }, error = function(e) {
      showGenericError()
      logjs(e)
      print(e)
    }, silent = TRUE)
  }, res = 100)
  
  output$comparisonFrequencies <- renderPlot({
    req(rv$tna_result)
    tryCatch({
      group_tnad <- buildGroupModel(rv, input, input$compareSelect)
      palette <- getPalette(input$paletteFrequenciesGroup, length(group_tnad))
      if (input$paletteFrequenciesGroupReverse) {
        palette <- rev(palette)
      }
      plot_frequencies(group_tnad, 
                       colors = palette, 
                       width = input$widthFrequenciesGroup,
                       show_label = input$showLabelsFrequenciesGroup,
                       position = input$positionFrequenciesGroup)
      
    }, warning = function(w) {
      print(w)
      logjs(w)
    }, error = function(e) {
      showGenericError()
      logjs(e)
      print(e)
    }, silent = TRUE)
  }, res = 100)
  
  output$groupCentralitiesPlot <- renderPlot({
    req(rv$tna_result)
     
    tryCatch({
      group_tnad <- buildGroupModel(rv, input, input$compareSelect)
      palette <- getPalette(input$paletteCentralitiesGroup, length(group_tnad))
      if (input$paletteCentralitiesGroupReverse) {
        palette <- rev(palette)
      }
      plot(
        centralities(
          group_tnad,
          measures = input$centralitiesChoiceGroup,
          normalize = input$normalizeGroup,
          loops =  input$loopsGroup
        ),
        ncol = input$nColsCentralitiesGroup,
        colors = palette
      )
    }, warning = function(w) {
      print(w)
      logjs(w)
    }, error = function(e) {
      showGenericError()
      logjs(e)
      print(e)
    }, silent = TRUE)
  }, res = 100)
  
  output$groupCentralitiesTable <- renderDT({
    req(rv$data)
    tryCatch({
      group_tnad <- buildGroupModel(rv, input, input$compareSelect)
      if (input$compare_sig) {
        differentrows <-
          nrow(group_tnad[[req(input$group1)]]$data) != nrow(group_tnad[[req(input$group2)]]$data)
        permtest <- permutation_test(
          group_tnad[[req(input$group1)]],
          group_tnad[[req(input$group2)]],
          iter = input$iterPerm,
          paired = ifelse(differentrows,
                          FALSE, input$pairedPerm),
          level = input$levelPerm,
          measures = input$centralitiesChoiceGroup
        )
        if (differentrows & input$pairedPerm) {
          showGenericError(
            "Paired test cannot be applied because groups have different number of observations"
          )
        }
        
        permcents <- data.frame(permtest$centralities$stats) |>
          dplyr::mutate_at(3:5, \(x) round(as.double(x), 3))
        datatable(permcents, options = list(pageLength = 10, scrollX = TRUE))
      } else {
        return (NULL)
        showGenericError("Choose permutation test first")
      }
    }, warning = function(w) {
      logjs(w)
      print(w)
    }, error = function(e) {
      showGenericError()
      logjs(e)
      print(e)
    }, silent = TRUE)
  }, res = 100)
  
}
