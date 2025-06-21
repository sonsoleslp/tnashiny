
buildGroupModel <- function(rv, input) {
  tryCatch({
    groupm <- NULL
    if (input$inputType == "sequence") {
      groupm <- group_model(
        req(rv$data),
        type = input$type,
        scaling = input$scaling,
        cols = seq(as.integer(input$seqFrom),as.integer(input$seqTo)),
        group = rv$meta_data[,input$compareSelect]
      )
    } else {
      groupm <- group_model(
        req(rv$data),
        scaling =  input$scaling,
        type = input$type,
        group = input$compareSelect
      )
    }
    return(groupm)
  },error = function(e){
    return(NULL)
    print(e)
  })
}

getComparisonAllowed <- function() {
  conditionalPanel(
    condition = "(input.inputType != 'matrix')",
    fluidRow(
      column(
        width = 3,
        fluidRow(
          box(
            title = "Comparison Settings",
            width = 12,
            selectInput("compareSelect", "Choose grouping column:", choices = NULL),
            selectInput("group1", "Choose group 1:", choices = NULL),
            selectInput("group2", "Choose group 2:", choices = NULL),
            input_switch("compare_sig", "Permutation test"),
            conditionalPanel(
              condition = "input.compare_sig",
              numericInput("iterPerm", "Iteration:", min = 0, max = 10000, value = 1000, step = 100),
              numericInput("levelPerm", "Level:", min = 0, max = 1, value = 0.05, step = 0.01),
              input_switch("pairedPerm", "Paired test")
            ),
          ),
          getVisualizationSettings("Group", 12)
        )
      ),
      column(width = 9,
             fluidRow(
               tabBox(
                 id = "tabset1", 
                 width = 12,
                 tabPanel(
                   "Plot", 
                   div(
                     jqui_resizable(
                       plotOutput("comparisonPlot", width = "600px", height = "600px"),
                       # Render the TNA plot here
                       options = list(ghost = TRUE, helper = "resizable-helper")
                     ),
                     align = "center")
                 ),
                 tabPanel(
                   "Table", 
                   conditionalPanel(
                     condition = "input.compare_sig",
                     DTOutput("comparisonDiffs")
                   ),
                   conditionalPanel(
                     condition = "!input.compare_sig",
                     fluidRow(box(
                       span(
                         icon("circle-info", class = "text-danger"),
                         "Table is shown for permutation test results. Check the 'Permutation test' checkbox to see it"
                       ),
                       width = 7
                     )))
                 ),
                 tabPanel(
                   "Frequencies", 
                   div(
                     jqui_resizable(
                       plotOutput("comparisonFrequencies", width = "900px", height = "500px"),
                       # Render the TNA plot here
                       options = list(ghost = TRUE, helper = "resizable-helper")
                     ),
                     align = "center")
                 ),
                 tabPanel(
                   "Mosaic", 
                   div(
                     jqui_resizable(
                       plotOutput("mosaicPlot", width = "600px", height = "500px"),
                       # Render the TNA plot here
                       options = list(ghost = TRUE, helper = "resizable-helper")
                     ),
                     align = "center")
                 ),
                 tabPanel(
                   "Centralities", 
                   fluidRow(box(
                     fluidRow(
                       column(
                         width = 6,
                         selectInput("centralitiesChoiceGroup", "Centralities", multiple = TRUE, choices = centralitiesList, selected = centralitiesList)
                       ),
                       column(
                         width = 2,
                         tags$label("Properties"),
                         checkboxInput("loopsGroup", "Loops?", value = FALSE),
                         checkboxInput("normalizeGroup", "Normalize?", value = FALSE),
                         class = "checkboxcentralities"
                       ),
                       column(
                         width = 2,
                         numericInput("nColsCentralitiesGroup", "Columns", 3, min = 1, max = 9, step = 1)
                       )
                     ),
                     width = 12
                   )),
                   div(
                     jqui_resizable(
                       plotOutput("groupCentralitiesPlot", width = "900px", height = "600px"),
                       # Render the TNA plot here
                       options = list(ghost = TRUE, helper = "resizable-helper")
                     ),
                     align = "center"),
                   conditionalPanel(
                     condition = "input.compare_sig",
                     DTOutput("groupCentralitiesTable")
                   )
                 ),
                 
               )
             ))
    ))
}

getComparisonNotAllowed <- function(x) {
  conditionalPanel(
    condition = "(input.inputType == 'matrix')",
    box(
      span(
        icon("circle-info", class = "text-danger"),
        "Comparison operations are not supported for this type of data"
      ),
      width = 7
    )
  )
}


renderComparison <- function(rv, input, output, session) {
  
  observeEvent(input$compareSelect, {
    if (is.null(rv$meta_data)) {
      return()
    }
    choices <-
      unique(data.frame(rv$meta_data)[, input$compareSelect])
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
      group_tnad <- buildGroupModel(rv, input)
      
      
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
          showGenericError("Paired test cannot be applied because groups have different number of observations")
        }
        
        permedges <- data.frame(permtest$edges$stats) |> 
          tidyr::separate(edge_name, sep = " -> ", into = c("From", "To")) |>
          dplyr::mutate_at(3:5,\(x) round(as.double(x),3))
        
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
      group_tnad <- buildGroupModel(rv, input)
      if (input$compare_sig) {
        differentrows <-
          nrow(group_tnad[[req(input$group1)]]$data) != nrow(group_tnad[[req(input$group2)]]$data)
        permtest <- permutation_test(
          group_tnad[[req(input$group1)]],
          group_tnad[[req(input$group2)]],
          iter = input$iterPerm,
          paired = ifelse(differentrows,
                          FALSE, input$pairedPerm),
          level = input$levelPerm
        )
        if (differentrows & input$pairedPerm) {
          showGenericError("Paired test cannot be applied because groups have different number of observations")
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
          posCol = "darkblue",
          negCol = "red",
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
          colors = getPalette(input$paletteGroup, length(rv$tna_result$labels)),
          vsize = input$vsizeGroup,
          layout = getLayout(input$layoutGroup),
          posCol = "darkblue",
          negCol  = "red",
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
      group_tnad <- buildGroupModel(rv, input)
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
      group_tnad <- buildGroupModel(rv, input)
      plot_frequencies(group_tnad)
      
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
      group_tnad <- buildGroupModel(rv, input)
      plot(centralities(group_tnad,
                        measures = input$centralitiesChoiceGroup,
                        normalize = input$normalizeGroup,
                        loops =  input$loopsGroup), ncol = input$nColsCentralitiesGroup)
      
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
      group_tnad <- buildGroupModel(rv, input)
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
          showGenericError("Paired test cannot be applied because groups have different number of observations")
        }
        
        permcents <- data.frame(permtest$centralities$stats) |> 
          dplyr::mutate_at(3:5,\(x) round(as.double(x),3))
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


