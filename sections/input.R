library(sortable)


getInputData <- function() {
  fluidRow(
    column(width = 3, fluidRow(
      box(
        title = "Data Input",
        width = 12,
        radioButtons(
          "inputType",
          "Input Type:",
          selected = character(0),
          choices = c(
            "Sample data" = "sample",
            "Sequence Data" = "sequence",
            "Long Data" = "long",
            "Transition Matrix" = "matrix"
          )
        ),
        conditionalPanel(
          condition = "input.inputType == 'sequence'",
          fileInput("fileInput", "Upload data file (sequence or wide data)"),
          selectInput("seqFrom", "From:", choices = NULL, selectize = F),
          selectInput("seqTo", "To:", choices = NULL, selectize = F)
        ),
        conditionalPanel(
          condition = "input.inputType == 'long'",
          fileInput("longInput", "Upload long data"),
          selectInput("longAction", "Action:", choices = NULL, selectize = F),
          selectInput("longActor", "Actor:", choices = NULL, selectize = F),
          selectInput("longTime", "Time:", choices = NULL, selectize = F),
          selectInput("longOrder", "Order:", choices = NULL, selectize = F),
          numericInput("longThreshold", "Threshold:", min = 0, value = 900, step = 1),
          textInput("longDate", "Date format:", placeholder = "Not mandatory if your timestamp is in an established format")
        ),
        conditionalPanel(condition = "input.inputType == 'matrix'", fileInput("matrixInput", "Upload transition matrix")),
        selectInput("type", "Analysis Type:", choices = c("relative", "frequency","co-occurrence")),
        selectInput("scaling", "Scaling (discouraged):", multiple = TRUE, choices = c("minmax", "max", "rank")),
        conditionalPanel(condition = "(input.inputType)", uiOutput("sortable_ui")),
        conditionalPanel(condition = "(input.inputType)", actionButton("analyze", "Analyze", class = "btn-primary"))
      )
    )),
    column(width = 9, fluidRow(
      conditionalPanel(
        condition = "!(input.inputType)",
        fluidRow(
          box(
            width = 12,
            title = "Welcome to TNA!",
            fluidRow(column(12, p("The following are the data formats supported by  Transition Network Analysis (TNA). Select the format of your data on the left panel or use our example data for  demonstration purposes. After that click on 'Analyze' to begin!"))),
            fluidRow(
              column(4, 
                     span("Sequence Data", class = "datatype"),
                     img(src = "wide.png", width = "100%", class = "thumb"),
                     p("Wide-format data stores each time point in a  separate column. It can be a tabular file (csv, xlsx) or an R sequence stslist object (e.g., from TraMineR)"
                     )
              ),
              column(4,
                     span("Long Data", class = "datatype"),
                     img(src = "long.png", width = "100%", class = "thumb"),
                     p("Long-format data stacks repeated measurements in rows, and the columns specify the actor, action and timestamp or order, as well as additional metadata.")
              ),
              column(4,
                     span("Transition Matrix", class = "datatype"),
                     img(src = "matrix.png", width = "100%", class = "thumb"),
                     p("You can also upload directly a transition probability matrix.")
              )
            )
          )
        )
      ),
      conditionalPanel(
        condition = "input.inputType",
        box(
          title = "Data Preview",
          width = 12,
          DTOutput("dataPreview"),
          conditionalPanel(
            condition = "input.inputType != 'sample' & !input.dataPreview_state",
            span(icon("circle-info", class = "text-info"), "No data selected yet")),
          tags$br(),
          uiOutput("tnaModel")
        )
      )
    )
    ))
}


analyzeInputData <- function(rv, input, output, session) {
  
  observeEvent(input$analyze, {
    req(input$inputType)
    req(input$type)
    scaling <- NULL
    tryCatch({
      if(!is.null(input$scaling)) {
        scaling <- req(input$scaling)
      }
    })
    if (input$inputType == "sequence") {
      
      # Perform TNA analysis
      tryCatch({
        # Use tna(data) directly
        if(inherits(rv$original, "stslist")) {
          rv$data <- rv$original
          alphabet(rv$data) <- rv$states
        } else {
          meta_data <- data.frame()
          seqFrom = as.integer(input$seqFrom)
          seqTo = as.integer(input$seqTo)
          if ((seqFrom == 1) & (seqTo == ncol(rv$original))) {
            meta_data <- data.frame()
          } else {
            meta_data <- data.frame(rv$original[, -(seqFrom:seqTo)])
            if (ncol(meta_data) == 1) {
              if(seqFrom == 1) {
                names(meta_data)[1] <- names(rv$original)[ncol(rv$original)]
              } else {
                names(meta_data)[1] <- names(rv$original)[1]
              }
            } 
          }
          sequence_data <- dplyr::mutate_all(rv$original, \(x) factor(x, levels = rv$states))
          rv$data <- sequence_data
          rv$meta_data <- meta_data
          groupchoices <- names(meta_data)
          updateSelectInput(session, "compareSelect", choices = groupchoices)
        }
        
        rv$tna_result <-
          build_model(rv$data, 
                      type = req(input$type), 
                      scaling = scaling, 
                      cols = input$seqFrom:input$seqTo)
      }, warning = function(w) {
        logjs(w)
      }, error = function(e) {
        showGenericError()
        logjs(e)
        print(e)
      }, silent = TRUE)
    } else  if (input$inputType == "long") {
      tryCatch({
        action <- rlang::missing_arg()
        actor <- rlang::missing_arg()
        time <- rlang::missing_arg()
        order <- rlang::missing_arg()
        dateformat <- NULL
        thresh <- Inf
        whitelist <-
          c(".session_id", ".standardized_time", ".session_nr")
        if ((input$longAction != "") & !is.null(input$longAction)) {
          action <- input$longAction
          rv$data[,action] <- factor(rv$data[,action], levels = rv$states)
          whitelist <- c(whitelist, action)
        }
        if ((input$longActor != "") & !is.null(input$longActor)) {
          actor <- input$longActor
          whitelist <- c(whitelist, actor)
        }
        if ((input$longTime != "") & !is.null(input$longTime)) {
          time <- input$longTime
          whitelist <- c(whitelist, time)
        }
        if ((input$longOrder != "") & !is.null(input$longOrder)) {
          order <- input$longOrder
          whitelist <- c(whitelist, order)
        }
        if ((input$longDate != "") & !is.null(input$longDate)) {
          dateformat <- input$longDate
        }
        if ((input$longThreshold != "") & !is.null(input$longThreshold)) {
          thresh <- input$longThreshold
        }
        rv$data <-
          prepare_data(
            rv$original,
            action = action,
            actor = actor,
            time_threshold = thresh,
            time = time,
            order = order,
            custom_format = dateformat
          )
        rv$tna_result <-
          build_model(rv$data, type = req(input$type), scaling = scaling)
        
        rv$meta_data <- rv$data$meta_data
        groupchoices <- names(rv$meta_data)
        groupchoices <-
          groupchoices[sapply(groupchoices, \(x) ! (x %in% whitelist))]
        updateSelectInput(session, "compareSelect", choices = groupchoices)
      }, warning = function(w) {
        logjs(w)
      }, error = function(e) {
        print(e)
        showGenericError()
        logjs(e)
      }, silent = TRUE)
      # Perform TNA analysis
    } else if (input$inputType == "matrix") {
      tryCatch({
        matrix_data <- as.matrix(rv$original)
        rv$data <- matrix_data
        # Perform TNA analysis with matrix input
        rv$tna_result <-
          tna(matrix_data)  # Use tna(matrix_data) directly
      }, warning = function(w) {
        logjs(w)
      }, error = function(e) {
        showGenericError()
        logjs(e)
        print(e)
      }, silent = TRUE)
    } else if (input$inputType == "sample") {
      tryCatch({
        rv$data <- structure(
          list(
            long_data = NULL,
            sequence_data =  dplyr::mutate_all(rv$original, \(x) factor(x, levels = rv$states)),
            meta_data = data.frame(
              Achiever = c(rep("High", 1000), rep("Low", 1000)
              )),
            statistics = NULL
          ),
          class = "tna_data"
        )
        rv$meta_data <- rv$data$meta_data
        groupchoices <- names(rv$meta_data)
        updateSelectInput(session, "compareSelect", choices = groupchoices)
        rv$tna_result <-
          build_model(rv$data, type = req(input$type), scaling = scaling)
        rv$tna_result$data$Achiever = c(rep("High", 1000), rep("Low", 1000))
        rv$meta_data <- data.frame(Achiever = c(rep("High", 1000), rep("Low", 1000)))
      }, warning = function(w) {
        logjs(w)
      }, error = function(e) {
        showGenericError()
        logjs(e)
        print(e)
      }, silent = TRUE)
    }
    if ((req(input$type) == "frequency") | (req(input$type) == "co-occurrence")) {
      updateSliderInput(session, "minimum", max = max(rv$tna_result$weights))
      updateSliderInput(session, "minimumCom", max = max(rv$tna_result$weights))
      updateSliderInput(session, "minimumClique", max = nrow(rv$tna_result$weights))
      updateSliderInput(session, "minimumEbet", max = nrow(rv$tna_result$weights))
      updateSliderInput(session, "minimumGroup", max = nrow(rv$tna_result$weights))
      updateSliderInput(session, "minimumBoot", max = nrow(rv$tna_result$weights))
      
      updateSliderInput(session, "cut", max = max(rv$tna_result$weights))
      updateSliderInput(session, "cutCom", max = max(rv$tna_result$weights))
      updateSliderInput(session, "cutClique", max = max(rv$tna_result$weights))
      updateSliderInput(session, "cutEbet", max = nrow(rv$tna_result$weights))
      updateSliderInput(session, "cutGroup", max = nrow(rv$tna_result$weights))
      updateSliderInput(session, "cutBoot", max = nrow(rv$tna_result$weights))
    } else {
      updateSliderInput(session, "minimum", max = 1)
      updateSliderInput(session, "minimumCom", max = 1)
      updateSliderInput(session, "minimumClique", max = 1)
      updateSliderInput(session, "minimumEbet", max = 1)
      updateSliderInput(session, "minimumGroup", max = 1)
      updateSliderInput(session, "minimumBoot", max = 1)
      
      updateSliderInput(session, "cut", max = 1)
      updateSliderInput(session, "cutCom", max = 1)
      updateSliderInput(session, "cutClique", max = 1)
      updateSliderInput(session, "cutEbet", max = 1)
      updateSliderInput(session, "cutGroup", max = 1)
      updateSliderInput(session, "cutBoot", max = 1)
    }
    vsize <- 8 * exp(-1 * nrow(rv$tna_result$weights) / 80) + 1
    updateSliderInput(session, "vsize", value = vsize)
    updateSliderInput(session, "vsizeClique", value = 9)
    updateSliderInput(session, "vsizeCom", value = vsize)
    updateSliderInput(session, "vsizeEbet", value = vsize)
    updateSliderInput(session, "vsizeGroup", value = vsize)
    updateSliderInput(session, "vsizeBoot", value = vsize)
    
    nn <- length(rv$tna_result$labels)
    palettes <- getPalettes(nn)
    
    updateSelectInput(session, "palette", choices = palettes, selected = "Default")
    updateSelectInput(session, "paletteCom", choices = palettes, selected = "Default")
    updateSelectInput(session, "paletteClique", choices = palettes, selected = "Default")
    updateSelectInput(session, "paletteEbet", choices = palettes, selected = "Default")
    updateSelectInput(session, "paletteGroup", choices = palettes, selected = "Default")
    updateSelectInput(session, "paletteBoot", choices = palettes, selected = "Default")
    updateSelectInput(session, "paletteIndex", choices = palettes, selected = "Default")
    updateSelectInput(session, "paletteDist", choices = palettes, selected = "Default")
    
    
  })
}

previewData <- function(rv, input, output, session) {
  
  observeEvent(input$states, {
    rv$states <- input$states
  })
  
  observeEvent(input$seqFrom, {
    
    if(inherits(rv$original, "stslist")) {
      subset <-  TraMineR::seqdef(rv$original, input$seqFrom:input$seqTo)
      rv$states <- TraMineR::alphabet(subset)
    } else {
      subset <- rv$original[,input$seqFrom:input$seqTo]
      rv$states <- sort(na.omit(unique(unlist(subset[subset != ""]))))
    }
  })
  
  observeEvent(input$seqTo, {
    subset <- rv$original[,input$seqFrom:input$seqTo]
    if(inherits(rv$original, "stslist")) {
      subset <-  TraMineR::seqdef(rv$original, input$seqFrom:input$seqTo)
      rv$states <- TraMineR::alphabet(subset)
    } else {
      subset <-  rv$original[,input$seqFrom:input$seqTo]
      rv$states <- sort(na.omit(unique(unlist(subset[subset != ""]))))
    }
  })
  
  observeEvent(input$longAction, {
    if (!is.null(input$longAction) & input$longAction != "") {
      rv$states <- sort(unique(rv$original[,input$longAction]))
    }
  })
  
  output$dataPreview <- renderDT({
    rv$original <- NULL
    if (is.null(input$inputType)) {
      return(NULL)
    }
    if (!is.null(input$longInput) & input$inputType == "long") {
      rv$original <- import(input$longInput$datapath)
      theoptions <- c(Empty = "", names(rv$original))
      updateSelectInput(session, "longAction", choices = theoptions)
      updateSelectInput(session, "longActor", choices = theoptions)
      updateSelectInput(session, "longOrder", choices = theoptions)
      updateSelectInput(session, "longTime", choices = theoptions)
      
    } else if (!is.null(input$matrixInput) & input$inputType == "matrix") {
      rv$original <- import(input$matrixInput$datapath, row.names = 1)
      rv$states <- names(rv$original)
    } else if (!is.null(input$fileInput) & input$inputType == "sequence") {
      rv$original <- import(input$fileInput$datapath)
      
      sequencePos = names(rv$original)
      sequencePostList = seq(1,length(sequencePos))
      names(sequencePostList) <- sequencePos
      updateSelectInput(session, "seqFrom",  choices = sequencePostList, selected = 1)
      updateSelectInput(session, "seqTo", choices = sequencePostList, selected = length(sequencePos))
      if(inherits(rv$original, "stslist")) {
        rv$states <- TraMineR::alphabet(rv$original)
      } else {
        subset <-  na.omit(unique(unlist(rv$original)))
        rv$states <- sort(subset[subset != ""])
      }
      
    } else if (input$inputType == "sample") {
      rv$original <- group_regulation
      rv$states <- sort(na.omit(unique(unlist(rv$original))))
    }
    rv$tna_result <- NULL
    rv$centrality_result <- NULL
    rv$cliques_result <- NULL
    rv$clique_plots <- list()
    rv$community_result <- NULL
    rv$bootstrap_result <- NULL
    datatable(rv$original, options = list(pageLength = 5, scrollX = TRUE))
  })
  
  output$sortable_ui <- renderUI({
    bucket_list(
      header = "Order states/events:",
      group_name = "dynamic_group",
      orientation = "vertical",
      add_rank_list(
        labels = req(rv$states),
        text = "",
        input_id = "states"
      )
    )
  })
}



