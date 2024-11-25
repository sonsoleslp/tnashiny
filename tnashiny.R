# Install required packages if not already installed
# install.packages(c("shiny", "shinydashboard", "DT", "igraph", "visNetwork"))

library(shiny)
library(shinydashboard)
library(DT)
# library(igraph)
library(visNetwork)
library(tna)
library(rio)
library(grid)  # For arranging multiple plots
library(gridExtra)  # For arranging multiple plots

# UI
ui <- dashboardPage(skin = "red",
  dashboardHeader(title = "TNA"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("About TNA", tabName = "about", icon = icon("circle-info")),
      menuItem("Input Data", tabName = "input", icon = icon("table")),
      menuItem("Summary results", tabName = "results", icon = icon("chart-bar")),
      menuItem("Visualization", tabName = "tna_plot", icon = icon("circle-nodes")),
      menuItem("Centrality Measures", tabName = "centrality", icon = icon("chart-line")),
      menuItem("Community Detection", tabName = "communities", icon = icon("users")),  # New Community Detection tab
      menuItem("Cliques", tabName = "cliques", icon = icon("sitemap"))  # New tab for Cliques
      
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "about",
        h2("Transition Network Analysis (TNA)"),
        
        # Title and Acronym
        p(" Transition Network Analysis (TNA)  is designed for analyzing transition networks, providing methods for examining sequences, identifying communities, calculating centrality measures, and visualizing network dynamics."),
        
        # Description
        h3("Usage"),
        p("TNA offers a set of tools for researchers and analysts working with transition networks. It allows users to analyze sequences in data, detect community structures, compute various centrality measures, and visualize transitions within networks. It can be used from the R package `tna` or through the Shiny interface."),
        
        tags$ul(
          tags$li(tags$b("Transition Analysis"),":  Understand transitions and connections in sequential data through various analytical methods."),
          tags$li(tags$b("Community Detection"),": Apply multiple algorithms to find community structures within transition networks, supporting comparisons across algorithms."),
          tags$li(tags$b("Centrality Measures"),": Calculate centrality measures to identify key nodes and relationships in the network."),
          tags$li(tags$b("Visualization"),": Generate interactive and static plots to visually explore network dynamics.")
        ),
        img(src = "TNA.png",style="width: 500px; max-width: 100%;"),
        # # Links
        # h3("Resources"),
        # tags$ul(
        #   tags$li(tags$a(href = "https://github.com/your-repository/TNA", "TNA Package Repository", target = "_blank")),
        #   tags$li(tags$a(href = "https://tna-package-tutorial.com", "TNA Package Tutorial", target = "_blank")),
        #   tags$li(tags$a(href = "https://doi.org/10.1234/tna-paper", "Research Paper on TNA Methods", target = "_blank"))
        # ),
        # 
        # Citation
        h3("Citation"),
        p("Please cite the tna package if you use it in your research:"),
        tags$blockquote("López-Pernas S, Saqr M, Tikka S (2024). tna : An R package for Transition Network Analysis. R package
  version 0.1.0, <https://github.com/sonsoleslp/tna>.")
      ),
      # Input Data Tab
      tabItem(
        tabName = "input",
        fluidRow(
          box(
            title = "Data Input",
            width = 12,
            radioButtons("inputType", "Input Type:",
                         choices = c("Sequence Data" = "sequence",
                                     "Transition Matrix" = "matrix",
                                     "Sample data" = "sample")),
            conditionalPanel(
              condition = "input.inputType == 'sequence'",
              fileInput("fileInput", "Upload data file (sequence or wide data)"),
            ),
            conditionalPanel(
              condition = "input.inputType == 'matrix'",
              fileInput("matrixInput", "Upload transition matrix")
            ),
            selectInput("type", "Analysis Type:",
                        choices = c("relative", "scaled", "ranked", "absolute")),
            actionButton("analyze", "Analyze Data", class = "btn-danger")
          ),
          box(
            title = "Data Preview",
            width = 12,
            DTOutput("dataPreview")
          )
        )
      ),
      # Results Tab
      tabItem(
        tabName = "results",
        fluidRow(
          box(
            title = "Summary Statistics",
            width = 3,
            tableOutput("summaryStats")
          ),
          box(
            title = "Initial Probabilities",
            width = 3,
            DTOutput("initialProbs")
          ),
          box(
            title = "Transition Matrix",
            width = 6,
            div(class = "responsive-table", DTOutput("transitionMatrix"))
          )
        )
      ),
      
      # Centrality Measures Tab
      tabItem(
        tabName = "centrality",
        fluidRow(
          box(
            title = "Centrality Measures",
            width = 12,
            tableOutput("centralityPrint"),
            plotOutput("centralityPlot")
          )
        )
      ),
      # TNA Results Plot Tab
      tabItem(
        tabName = "tna_plot",
        
        fluidRow(
          box(
            fluidRow(
              column(
                width = 4,
                sliderInput("cut", "Cut Value:", min = 0, max = 1, value = 0.1, step = 0.01)
              ),
              column(
                width = 4,
                sliderInput("minimum", "Minimum Value:", min = 0, max = 1, value = 0.05, step = 0.01)
              ),
              column(
                width = 4,
                selectInput("layout", "Layout", choices = c("circle","spring"), selected = "circle")
              )
              
            ), 
            width = 12,
          ),
          box(
            title = "Visualization",
            width = 12,
            plotOutput("tnaPlot", width="700px", height = "700px")  # Render the TNA plot here
          )
        )
      ),
        # Community Detection Tab
        tabItem(
          tabName = "communities",
          fluidRow(
            box(
              title = "Community Detection Settings",
              width = 4,
              numericInput("gamma", "Gamma (for certain algorithms):", value = 1, min = 0, max = 5),
              actionButton("detectCommunities", "Detect Communities", class = "btn-danger")
            ),
            box(
              title = "Community Detection Results",
              width = 8,
              DTOutput("communityAssignments"),
              selectInput("communityAlgorithm", "Choose Algorithm:", choices = NULL,width = "30%"),  # Empty initially, populated later
              plotOutput("communityPlot")
            )
          )
        ),
      # New Cliques Tab
      tabItem(
        tabName = "cliques",
        fluidRow(
          box(
            title = "Clique Settings",
            width = 4,
            numericInput("cliqueSize", "Clique Size (n):", value = 3, min = 2, max = 10),
            numericInput("cliqueThreshold", "Threshold:", value = 0, min = 0, max = 1),
            actionButton("findCliques", "Find Cliques", class = "btn-danger")
          ),
          box(
            title = "Cliques Found",
            width = 8,
            selectInput("cliqueSelect", "Choose Clique:", choices = NULL, width = "30%"),  # Empty initially, populated later
            plotOutput("cliquesPlot")
          )
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  # Reactive values to store the analysis results
  rv <- reactiveValues(
    data = NULL,
    tna_result = NULL,
    centrality_result = NULL,
    cliques_result = NULL,
    clique_plots = list(),
    community_result = NULL
    
  )
  
  # Read and process input data
  observeEvent(input$analyze, {
    req(input$inputType)
    req(input$type)
    if (input$inputType == "sequence") {
      req(input$fileInput)
      
      data <- import(input$fileInput$datapath)
      rv$data <- data
      
      # Perform TNA analysis
      rv$tna_result <- tna(data,type = req(input$type))  # Use tna(data) directly
      
    } else if (input$inputType == "matrix") {
      req(input$matrixInput)
      matrix_data <- import(input$matrixInput$datapath, row.names = 1)
      matrix_data <- as.matrix(matrix_data)
      rv$data <- matrix_data
      
      # Perform TNA analysis with matrix input
      rv$tna_result <- tna(matrix_data, type = req(input$type))  # Use tna(matrix_data) directly
    } else if (input$inputType == "sample") {
      rv$tna_result <- tna(group_regulation, type = req(input$type)) 
      rv$data <- group_regulation
    }
  })
  
  # Data Preview
  output$dataPreview <- renderDT({
    req(rv$data)
    datatable(rv$data, options = list(scrollX = TRUE))
  })
  
 
  # Transition Matrix Display
  output$transitionMatrix <- renderDT({
    req(rv$tna_result)
    datatable(round(rv$tna_result$weights, 3),
              options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # Initial Probabilities Display
  output$initialProbs <- renderDT({
    req(rv$tna_result)
    init_probs <- data.frame(
      Probability = round(rv$tna_result$inits, 3)
    )
    datatable(init_probs, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # Summary Statistics
  output$summaryStats <- renderTable({
    req(rv$tna_result)
    
    # Calculate some basic network metrics
    # trans_matrix <- rv$tna_result$weights
    # cat("Network Summary:\n")
    # cat("Number of States:", ncol(trans_matrix), "\n")
    # cat("Average Transition Probability:", round(mean(trans_matrix[trans_matrix > 0]), 3), "\n")
    # cat("Maximum Transition Probability:", round(max(trans_matrix), 3), "\n")
    # cat("Number of Non-zero Transitions:", sum(trans_matrix > 0), "\n")
    (summary(rv$tna_result))
  })
  
  # Centrality Measures
  output$centralityPlot <- renderPlot({
    req(rv$tna_result)
    
    # Calculate centrality measures
    centrality_result <- centralities(rv$tna_result)
    rv$centrality_result <- centrality_result
    
    # Plot centrality measures
    plot(centrality_result)
  })
  
  output$centralityPrint <- renderTable({
    req(rv$centrality_result)
    
    # Print centrality measures
    (data.frame(rv$centrality_result))
  })
  
  # Plot TNA results
  output$tnaPlot <- renderPlot({
    req(rv$tna_result)
    
    # Plot the TNA results directly
    # plot(rv$tna_result)  # Use the plot method for the tna result
    
    plot(rv$tna_result, cut = input$cut, minimum = input$minimum, layout = input$layout)
    
  })
  
  # Detect Communities
  observeEvent(input$detectCommunities, {
    req(rv$tna_result)
    
    # Detect communities in the transition networks
    rv$community_result <- tna::communities(rv$tna_result, gamma = input$gamma)
  })
  
  # Display Community Counts
  output$communityCounts <- renderPrint({
    req(rv$community_result)
    # Print the counts of communities found by each algorithm
    rv$community_result$counts
  })
  
  # Display Community Assignments
  output$communityAssignments <- renderDT({
    req(rv$community_result)
    # Display the assignments as a data frame with color-coded community assignments
    datatable(rv$community_result$assignments, options = list(scrollX = TRUE))
  })
  
  observeEvent(input$detectCommunities, {
    req(rv$tna_result)
    
    # Detect communities in the transition networks
    rv$community_result <- tna::communities(rv$tna_result, gamma = input$gamma)
    
    # Populate the dropdown with algorithm names and community counts
    algorithm_choices <- sapply(names(rv$community_result$counts), function(alg) {
      paste0(alg, " (", rv$community_result$counts[[alg]], " communities)")
    })
    names(algorithm_choices) <- names(rv$community_result$counts)  # Set algorithm names as values
    choices <- names(algorithm_choices) 
    names(choices) <- paste0(names(rv$community_result$counts), " (",rv$community_result$counts,")")
    
    updateSelectInput(session, "communityAlgorithm",
                      choices = choices,
                      selected = "spinglass")  # Select the first algorithm by default
  })
  
  # Plot the selected community algorithm
  output$communityPlot <- renderPlot({
    req(rv$community_result, input$communityAlgorithm)
    
    # Plot the selected community detection algorithm's results
    selected_algorithm <- input$communityAlgorithm
    print(selected_algorithm)
    plot(rv$community_result, method = selected_algorithm)
  })
  
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
    
    if(length(rv$cliques_result$inits) > 0){
      choices <- seq_along(rv$cliques_result$inits)
      names(choices) <- lapply(rv$cliques_result$inits,\(x) names(x) |> paste(collapse = " - "))
      names(choices) <- paste0("Clique ", choices,": ", names(choices))
      updateSelectInput(session, "cliqueSelect",
                        choices = choices, 
                        selected = 1)  # Default to first clique
    } else {
      updateSelectInput(session, "cliqueSelect",
                        selected = 0, 
                        choices = NULL)  # Default to first clique
    }
    
  })
  
  # Display Cliques
  output$cliquesOutput <- renderPrint({
    req(rv$cliques_result)
    rv$cliques_result  # Display the cliques information
  })
  
  # Plot Cliques
  output$cliquesPlot <- renderPlot({
    req(rv$cliques_result)
    if (as.integer(input$cliqueSelect) == 0){
      NULL
    } else {
      plot(rv$cliques_result, 
           first = as.integer(input$cliqueSelect), 
           n = 1, ask = FALSE)
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)

