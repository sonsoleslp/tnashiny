library(shiny)
library(shinydashboard)
library(DT)
library(tna)
library(bslib)
library(rio)
library(shinyjs)
library(shinyjqui)  # For arranging multiple plots
set.seed(19)
mar <- c(2.5, 2.5, 2.5, 2.5)
db_header <- dashboardHeader(title = "TNA")

# Change this to en or comment this line

logo <- tags$span(tags$a(href = "https://sonsoles.me/tna",
                         tags$img(
                           src = "logo.png",
                           height = "44",
                           width = "40"
                         )),
                  "TNA")
db_header$children[[2]]$children <- logo

# UI
ui <- dashboardPage(
  skin = "purple",
  title = "TNA",
  db_header,
  dashboardSidebar(
    sidebarMenu(
      menuItem(
        "About TNA",
        tabName = "about",
        icon = icon("circle-info"),
        selected = FALSE
      ),
      menuItem(
        "Input Data",
        tabName = "input",
        icon = icon("table"),
        selected = TRUE
      ),
      menuItem(
        "Summary results",
        tabName = "results",
        icon = icon("chart-bar")
      ),
      menuItem(
        "Visualization",
        tabName = "tna_plot",
        icon = icon("circle-nodes")
      ),
      menuItem(
        "Centrality Measures",
        tabName = "centrality",
        icon = icon("chart-line")
      ),
      menuItem(
        "Community Detection",
        tabName = "communities",
        icon = icon("users")
      ),
      menuItem(
        "Edge Betweenness",
        tabName = "edgebet",
        icon = icon("people-arrows")
      ),
      menuItem("Cliques", tabName = "cliques",
               icon = icon("sitemap")),
      menuItem(
        "Comparison",
        tabName = "comparison",
        icon = icon("balance-scale")
      ),
      menuItem(
        "Validation",
        tabName = "bootstrap",
        icon = icon("check-circle")
      )
    )
  ),
  dashboardBody(
    tags$html(lang="en"),
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
    tabItems(
      tabItem(
        tabName = "about",
        h2("Transition Network Analysis (TNA)"),
        # Title and Acronym
        p(
          " Transition Network Analysis (TNA) is designed for analyzing 
          transition networks, providing methods for examining sequences, 
          identifying communities, calculating centrality measures, and
          visualizing network dynamics. TNA was presented for the first 
          time at the Learning Analytics & Knowledge conference (2025).",
          tags$a(
            "Check out our paper",
            href = "https://dl.acm.org/doi/10.1145/3706468.3706513"),
          "."
        ),
        # Description
        h3("Usage"),
        p(
          "TNA offers a set of tools for researchers and analysts working with 
          transition networks. It allows users to analyze sequences in data, 
          detect community structures, compute various centrality measures, 
          and visualize transitions within networks. It can be used from the R 
          package tna or through the Shiny interface",
          tags$a(
            "Check the package documentation",
            href = "https://sonsoles.me/tna/"),
          "."
        ),
        tags$ul(
          tags$li(
            tags$b("Transition Analysis"),
            ":  Understand transitions and connections in sequential data 
            through various analytical methods."
          ),
          tags$li(
            tags$b("Community Detection"),
            ": Apply multiple algorithms to find community structures within 
            transition networks, supporting comparisons across algorithms."
          ),
          tags$li(
            tags$b("Centrality Measures"),
            ": Calculate centrality measures to identify key nodes and 
            relationships in the network."
          ),
          tags$li(
            tags$b("Visualization"),
            ": Generate interactive and static plots to visually explore 
            network dynamics."
          )
        ),
        img(src = "TNA.png", style = "width: 500px; max-width: 100%;"),
        # # Links
        h3("Tutorials"),
        tags$ul(
          tags$li(
            tags$a(
              href = "https://lamethods.org/book2/chapters/ch15-tna/ch15-tna.html",
              "Basic TNA tutorial",
              target = "_blank")
          ),
          tags$li(
            tags$a(
              href = "https://lamethods.org/book2/chapters/ch16-ftna/ch16-ftna.html",
              "Frequency-based TNA tutorial",
              target = "_blank"
            )
          ),
          tags$li(
            tags$a(
              href = "https://lamethods.org/book2/chapters/ch17-tna-clusters/ch17-tna-clusters.html",
              "Clustering tutorial",
              target = "_blank"
            )
          )
        ),
        # Citation
        h3("Citation"),
        p("Please cite the tna package if you use it in your research:"),
        tags$blockquote(
          "López-Pernas S, Saqr M, Tikka S (2024). tna : An R package for
          Transition Network Analysis. R package version 0.1.0, 
          <https://github.com/sonsoleslp/tna>."
        )
      ),
      # Input Data Tab
      tabItem(tabName = "input",
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
                      fileInput("fileInput",
                                "Upload data file (sequence or wide data)"),
                    ),
                    conditionalPanel(
                      condition = "input.inputType == 'long'",
                      fileInput("longInput", "Upload long data"),
                      selectInput("longAction", "Action:", choices = NULL),
                      selectInput("longActor", "Actor:", choices = NULL),
                      selectInput("longTime", "Time:", choices = NULL),
                      selectInput("longOrder", "Order:", choices = NULL),
                      numericInput(
                        "longThreshold",
                        "Threshold:",
                        min = 0,
                        value = 900,
                        step = 1
                      ),
                      textInput("longDate", "Date format:")
                    ),
                    conditionalPanel(
                      condition = "input.inputType == 'matrix'",
                      fileInput("matrixInput", "Upload transition matrix")
                    ),
                    selectInput(
                      "type",
                      "Analysis Type:",
                      choices = c("relative", "frequency")),
                    actionButton("analyze", "Analyze", class = "btn-primary")
                  )
                )),
                column(width = 9,
                       fluidRow(
                         conditionalPanel(condition = "!(input.inputType)",
                                          fluidRow(
                                            box(
                                              width = 12,
                                              title = "Welcome to TNA!",
                                              fluidRow(column(
                                                12,
                                                p(
                                                  "The following are the data 
                                                  formats supported by 
                                                  Transition Network Analysis 
                                                  (TNA). Select the format of 
                                                  your data on the left panel 
                                                  or use our example data for 
                                                  demonstration purposes. 
                                                  After that click on 'Analyze' 
                                                  to begin!"
                                                )
                                              )),
                                              fluidRow(
                                                column(
                                                  4,
                                                  span("Sequence Data",
                                                       class = "datatype"),
                                                  img(src = "wide.png",
                                                      width = "100%",
                                                      class = "thumb"),
                                                  p(
                                                    "Wide-format data stores 
                                                    each time point in a 
                                                    separate column. It can be 
                                                    a tabular file (csv, xlsx) 
                                                    or an R sequence stslist 
                                                    object (e.g., from 
                                                    TraMineR)"
                                                  )
                                                ),
                                                column(
                                                  4,
                                                  span(
                                                    "Long Data",
                                                    class = "datatype"
                                                  ),
                                                  img(
                                                    src = "long.png",
                                                    width = "100%",
                                                    class = "thumb"
                                                  ),
                                                  p(
                                                    "Long-format data stacks 
                                                    repeated measurements in 
                                                    rows, and the columns 
                                                    specify the actor, action 
                                                    and timestamp or order, as 
                                                    well as additional 
                                                    metadata."
                                                  )
                                                ),
                                                column(
                                                  4,
                                                  span(
                                                    "Transition Matrix",
                                                    class = "datatype"),
                                                  img(
                                                    src = "matrix.png",
                                                    width = "100%",
                                                    class = "thumb"
                                                  ),
                                                  p("You can also upload 
                                                    directly a transition 
                                                    probability matrix.")
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
                             conditionalPanel(condition = "input.inputType != 'sample' & !input.dataPreview_state",
                                              span(
                                                icon(
                                                  "circle-info",
                                                  class = "text-info"
                                                ),
                                                "No data selected yet"
                                              )),
                             tags$br(),
                             uiOutput("tnaModel")
                           )
                         )
                       ))
              )),
      # Results Tab
      tabItem(tabName = "results",
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
              )),
      # Centrality Measures Tab
      tabItem(tabName = "centrality",
              fluidRow(box(
                fluidRow(
                  column(
                    width = 6,
                    selectInput(
                      "centralitiesChoice",
                      "Centralities",
                      multiple = TRUE,
                      choices = c(
                        "OutStrength",
                        "InStrength",
                        "ClosenessIn",
                        "ClosenessOut",
                        "Closeness",
                        "BetweennessRSP",
                        "Betweenness",
                        "Diffusion",
                        "Clustering"
                      ),
                      selected = c(
                        "OutStrength",
                        "InStrength",
                        "ClosenessIn",
                        "ClosenessOut",
                        "Closeness",
                        "BetweennessRSP",
                        "Betweenness",
                        "Diffusion",
                        "Clustering"
                      )
                    )
                  ),
                  column(
                    width = 2,
                    tags$label("Properties"),
                    checkboxInput("loops", "Loops?", value = FALSE),
                    checkboxInput("normalize", "Normalize?", value = FALSE),
                    class = "checkboxcentralities"
                  ),
                  column(
                    width = 2,
                    numericInput(
                      "nColsCentralities",
                      "Columns",
                      3,
                      min = 1,
                      max = 9,
                      step = 1
                    )
                  )
                ),
                width = 12
              )),
              fluidRow(
                box(
                  title = "Centrality Measures",
                  width = 12,
                  div(
                    tableOutput("centralityPrint"),
                    align = "center",
                    width = 12
                  ),
                  div(
                    jqui_resizable(
                      plotOutput(
                        "centralityPlot",
                        width = "800px",
                        height = "800px"
                      ),
                      # Render the TNA plot here
                      options = list(ghost = TRUE, helper = "resizable-helper")
                    ),
                    align = "center",
                    width = 12
                  )
                )
              )),
      # TNA Results Plot Tab
      tabItem(tabName = "tna_plot",
              fluidRow(
                column(fluidRow(
                  box(
                    title = "Settings",
                    width = 12,
                    sliderInput(
                      "cut",
                      "Cut Value",
                      min = 0,
                      max = 1,
                      value = 0.1,
                      step = 0.01
                    ),
                    sliderInput(
                      "minimum",
                      "Minimum Value",
                      min = 0,
                      max = 1,
                      value = 0.05,
                      step = 0.01
                    ),
                    sliderInput(
                      "edge.label",
                      "Edge label size",
                      min = 0,
                      max = 10,
                      value = 1,
                      step = 0.1
                    ),
                    sliderInput(
                      "vsize",
                      "Node  size",
                      min = 0,
                      max = 30,
                      value = 8,
                      step = 0.1
                    ),
                    sliderInput(
                      "node.label",
                      "Node label size",
                      min = 0,
                      max = 10,
                      value = 1,
                      step = 0.1
                    ),
                    selectInput(
                      "layout",
                      "Layout",
                      choices = c("circle", "spring"),
                      selected = "circle"
                    )
                  ),
                  width = 12
                ),
                width = 3),
                column(fluidRow(
                  box(
                    title = "Visualization",
                    width = 12,
                    div(jqui_resizable(
                      plotOutput("tnaPlot", width = "600px", height = "600px"),
                      # Render the TNA plot here
                      options = list(ghost = TRUE, helper = "resizable-helper")
                    ), align = "center")
                  ),
                  width = 12
                ),
                width = 9)
              )),
      tabItem(tabName = "edgebet",
              fluidRow(
                column(fluidRow(
                  box(
                    title = "Settings",
                    width = 12,
                    sliderInput(
                      "cutEbet",
                      "Cut Value",
                      min = 0,
                      max = 1,
                      value = 0.1,
                      step = 0.01
                    ),
                    sliderInput(
                      "minimumEbet",
                      "Minimum Value",
                      min = 0,
                      max = 1,
                      value = 0.05,
                      step = 0.01
                    ),
                    sliderInput(
                      "edge.labelEbet",
                      "Edge label size",
                      min = 0,
                      max = 10,
                      value = 1,
                      step = 0.1
                    ),
                    sliderInput(
                      "vsizeEbet",
                      "Node  size",
                      min = 0,
                      max = 30,
                      value = 8,
                      step = 0.1
                    ),
                    sliderInput(
                      "node.labelEbet",
                      "Node label size",
                      min = 0,
                      max = 10,
                      value = 1,
                      step = 0.1
                    ),
                    selectInput(
                      "layoutEbet",
                      "Layout",
                      choices = c("circle", "spring"),
                      selected = "circle"
                    )
                  ),
                  width = 12
                ),
                width = 3),
                column(fluidRow(
                  box(
                    title = "Visualization",
                    width = 12,
                    div(
                      jqui_resizable(
                        plotOutput(
                          "edgeBetPlot",
                          width = "600px",
                          height = "600px"
                        ),
                        # Render the TNA plot here
                        options = list(ghost = TRUE,
                                       helper = "resizable-helper")
                      ),
                      align = "center"
                    )
                  ),
                  width = 12
                ),
                width = 9)
              )),
      # Community Detection Tab
      tabItem(tabName = "communities",
              fluidRow(
                # Left Sidebar (3-column width)
                column(width = 3,
                       fluidRow(
                         box(
                           title = "Community Detection Settings",
                           width = 12,
                           selectInput(
                             "communityAlgorithm",
                             "Choose Algorithm:",
                             choices = "spinglass"
                           ),
                           numericInput(
                             "gamma",
                             "Gamma (for certain algorithms):",
                             value = 1,
                             min = 0,
                             max = 100
                           )
                         ),
                         box(
                           title = "Plotting Settings",
                           width = 12,
                           sliderInput(
                             "cutCom",
                             "Cut Value",
                             min = 0,
                             max = 1,
                             value = 0.1,
                             step = 0.01
                           ),
                           sliderInput(
                             "minimumCom",
                             "Minimum Value",
                             min = 0,
                             max = 1,
                             value = 0.05,
                             step = 0.01
                           ),
                           sliderInput(
                             "edge.labelCom",
                             "Edge label size",
                             min = 0,
                             max = 10,
                             value = 1,
                             step = 0.1
                           ),
                           sliderInput(
                             "vsizeCom",
                             "Node  size",
                             min = 0,
                             max = 30,
                             value = 8,
                             step = 0.1
                           ),
                           sliderInput(
                             "node.labelCom",
                             "Node label size",
                             min = 0,
                             max = 10,
                             value = 1,
                             step = 0.1
                           ),
                           selectInput(
                             "layoutCom",
                             "Layout",
                             choices = c("circle", "spring"),
                             selected = "circle"
                           )
                         ),
                         width = 12
                       )),
                # Right Main Content (9-column width)
                # Second Block
                box(
                  title = "Community Detection Results",
                  width = 9,
                  div(
                    jqui_resizable(
                      plotOutput("communityPlot", width = "600px", height = "600px"),
                      options = list(ghost = TRUE, helper = "resizable-helper")
                    ),
                    align = "center",
                    width = 12
                  )
                )
              )),
      # New Cliques Tab
      tabItem(tabName = "cliques",
              fluidRow(
                column(width = 3,
                       fluidRow(
                         box(
                           title = "Clique Settings",
                           width = 12,
                           numericInput(
                             "cliqueSize",
                             "Clique Size (n):",
                             value = 3,
                             min = 2,
                             max = 10
                           ),
                           numericInput(
                             "cliqueThreshold",
                             "Threshold:",
                             value = 0,
                             min = 0,
                             max = 1,
                             step = 0.05
                           ),
                           actionButton("findCliques", "Find Cliques", class = "btn-primary")
                         ),
                         box(
                           title = "Plotting Settings",
                           width = 12,
                           sliderInput(
                             "cutClique",
                             "Cut Value",
                             min = 0,
                             max = 1,
                             value = 0.1,
                             step = 0.01
                           ),
                           sliderInput(
                             "minimumClique",
                             "Minimum Value",
                             min = 0,
                             max = 1,
                             value = 0.05,
                             step = 0.01
                           ),
                           sliderInput(
                             "edge.labelClique",
                             "Edge label size",
                             min = 0,
                             max = 10,
                             value = 1,
                             step = 0.1
                           ),
                           sliderInput(
                             "vsizeClique",
                             "Node  size",
                             min = 0,
                             max = 30,
                             value = 8,
                             step = 0.1
                           ),
                           sliderInput(
                             "node.labelClique",
                             "Node label size",
                             min = 0,
                             max = 10,
                             value = 1,
                             step = 0.1
                           ),
                           selectInput(
                             "layoutClique",
                             "Layout",
                             choices = c("circle", "spring"),
                             selected = "circle"
                           )
                         )
                       )),
                column(width = 9,
                       fluidRow(
                         box(
                           title = "Cliques Found",
                           width = 12,
                           selectInput(
                             "cliqueSelect",
                             "Choose Clique:",
                             choices = NULL,
                             width = "30%"
                           ),
                           # Empty initially, populated later
                           div(
                             jqui_resizable(
                               plotOutput("cliquesPlot"),
                               options = list(ghost = TRUE, helper = "resizable-helper")
                             ),
                             align = "center",
                             width = 12
                           )
                         )
                       ))
              )),
      tabItem(
        tabName = "comparison",
        conditionalPanel(condition = "(input.inputType == 'long') | (input.inputType == 'sample') ",
                         fluidRow(
                           column(width = 3,
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
                                        numericInput(
                                          "iterPerm",
                                          "Iteration:",
                                          min = 0,
                                          max = 10000,
                                          value = 1000,
                                          step = 100
                                        ),
                                        numericInput(
                                          "levelPerm",
                                          "Level:",
                                          min = 0,
                                          max = 1,
                                          value = 0.05,
                                          step = 0.01
                                        ),
                                        input_switch("pairedPerm", "Paired test")
                                      ),
                                    ),
                                    box(
                                      title = "Plotting Settings",
                                      width = 12,
                                      sliderInput(
                                        "cutGroup",
                                        "Cut Value",
                                        min = 0,
                                        max = 1,
                                        value = 0.1,
                                        step = 0.01
                                      ),
                                      sliderInput(
                                        "minimumGroup",
                                        "Minimum Value",
                                        min = 0,
                                        max = 1,
                                        value = 0,
                                        step = 0.01
                                      ),
                                      sliderInput(
                                        "edge.labelGroup",
                                        "Edge label size",
                                        min = 0,
                                        max = 10,
                                        value = 1,
                                        step = 0.1
                                      ),
                                      sliderInput(
                                        "vsizeGroup",
                                        "Node  size",
                                        min = 0,
                                        max = 30,
                                        value = 8,
                                        step = 0.1
                                      ),
                                      sliderInput(
                                        "node.labelGroup",
                                        "Node label size",
                                        min = 0,
                                        max = 10,
                                        value = 1,
                                        step = 0.1
                                      ),
                                      selectInput(
                                        "layoutGroup",
                                        "Layout",
                                        choices = c("circle", "spring"),
                                        selected = "circle"
                                      )
                                    )
                                  )),
                           column(width = 9,
                                  fluidRow(
                                    tabBox(
                                      id = "tabset1", width = 12,
                                      tabPanel("Difference", div(
                                        jqui_resizable(
                                          plotOutput("comparisonPlot", width = "600px", height = "600px"),
                                          # Render the TNA plot here
                                          options = list(ghost = TRUE, helper = "resizable-helper")
                                        ),
                                        align = "center")),
                                      tabPanel("Mosaic", div(
                                        jqui_resizable(
                                          plotOutput("mosaicPlot", width = "1400px", height = "900px"),
                                          # Render the TNA plot here
                                          options = list(ghost = TRUE, helper = "resizable-helper")
                                        ),
                                        align = "center")),
                                      tabPanel("Centralities", 
                                               fluidRow(box(
                                                 fluidRow(
                                                   column(
                                                     width = 6,
                                                     selectInput(
                                                       "centralitiesChoiceGroup",
                                                       "Centralities",
                                                       multiple = TRUE,
                                                       choices = c(
                                                         "OutStrength",
                                                         "InStrength",
                                                         "ClosenessIn",
                                                         "ClosenessOut",
                                                         "Closeness",
                                                         "BetweennessRSP",
                                                         "Betweenness",
                                                         "Diffusion",
                                                         "Clustering"
                                                       ),
                                                       selected = c(
                                                         "OutStrength",
                                                         "InStrength",
                                                         "ClosenessIn",
                                                         "ClosenessOut",
                                                         "Closeness",
                                                         "BetweennessRSP",
                                                         "Betweenness",
                                                         "Diffusion",
                                                         "Clustering"
                                                       )
                                                     )
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
                                                     numericInput(
                                                       "nColsCentralitiesGroup",
                                                       "Columns",
                                                       3,
                                                       min = 1,
                                                       max = 9,
                                                       step = 1
                                                     )
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
                                        align = "center")
                                      )
                                    )
                                  ))
                         )),
        conditionalPanel(condition = "input.inputType != 'long' & input.inputType != 'sample' ",
                         box(
                           span(
                             icon("circle-info", class = "text-danger"),
                             "Comparison operations are only supported in long data"
                           ),
                           width = 7
                         ))
      ),
      tabItem(
        tabName = "bootstrap",
        conditionalPanel(condition = "input.inputType != 'matrix'",
                         fluidRow(
                           column(fluidRow(
                             box(
                               title = "Bootstrapping",
                               numericInput(
                                 "iterBoot",
                                 "Iteration:",
                                 min = 0,
                                 max = 10000,
                                 value = 1000,
                                 step = 100
                               ),
                               numericInput(
                                 "levelBoot",
                                 "Level:",
                                 min = 0,
                                 max = 1,
                                 value = 0.05,
                                 step = 0.01
                               ),
                               selectInput(
                                 "methodBoot",
                                 "Method",
                                 choices = c("stability", "threshold"),
                                 selected = "stability"
                               ),
                               conditionalPanel(
                                 condition = "input.methodBoot == 'threshold'",
                                 numericInput(
                                   "thresBoot",
                                   "Threshold:",
                                   min = 0,
                                   max = 1,
                                   value = 0.1,
                                   step = 0.01
                                 )
                               ),
                               conditionalPanel(
                                 condition = "input.methodBoot == 'stability'",
                                 h4("Consistency Range"),
                                 numericInput(
                                   "constLowerBoot",
                                   "Lower:",
                                   min = 0,
                                   max = 10,
                                   value = 0.75,
                                   step = 0.01
                                 ),
                                 numericInput(
                                   "constUpperBoot",
                                   "Upper:",
                                   min = 0,
                                   max = 10,
                                   value = 1.25,
                                   step = 0.01
                                 )
                               ),
                               actionButton("bootstrapButton", "Bootstrap", class = "btn-primary"),
                               width = 12
                             ),
                             box(
                               title = "Settings",
                               width = 12,
                               sliderInput(
                                 "cutBoot",
                                 "Cut Value",
                                 min = 0,
                                 max = 1,
                                 value = 0.1,
                                 step = 0.01
                               ),
                               sliderInput(
                                 "minimumBoot",
                                 "Minimum Value",
                                 min = 0,
                                 max = 1,
                                 value = 0.05,
                                 step = 0.01
                               ),
                               sliderInput(
                                 "edge.labelBoot",
                                 "Edge label size",
                                 min = 0,
                                 max = 10,
                                 value = 1,
                                 step = 0.1
                               ),
                               sliderInput(
                                 "vsizeBoot",
                                 "Node  size",
                                 min = 0,
                                 max = 30,
                                 value = 8,
                                 step = 0.1
                               ),
                               sliderInput(
                                 "node.labelBoot",
                                 "Node label size",
                                 min = 0,
                                 max = 10,
                                 value = 1,
                                 step = 0.1
                               ),
                               selectInput(
                                 "layoutBoot",
                                 "Layout",
                                 choices = c("circle", "spring"),
                                 selected = "circle"
                               )
                             ),
                             width = 12
                           ),
                           width = 3),
                           column(fluidRow(
                             box(
                               title = "Visualization",
                               width = 12,
                               div(jqui_resizable(
                                 plotOutput("tnaPlotBoot", width = "600px", height = "600px"),
                                 # Render the TNA plot here
                                 options = list(ghost = TRUE, helper = "resizable-helper")
                               ), align = "center")
                             ),
                             width = 12
                           ),
                           width = 9)
                         )),
        conditionalPanel(condition = "input.inputType == 'matrix'",
                         box(
                           span(
                             icon("circle-info", class = "text-danger"),
                             "Validation operations are only supported when the full data is provided"
                           ),
                           width = 7
                         ))
      )
    )
  )
)
# Server
server <- function(input, output, session) {
  # Reactive values to store the analysis results
  rv <- reactiveValues(
    original = NULL,
    data = NULL,
    tna_result = NULL,
    centrality_result = NULL,
    cliques_result = NULL,
    clique_plots = list(),
    community_result = NULL,
    bootstrap_result = NULL
  )
  observeEvent(input$inputType, {
    rv$original <- NULL
  })
  # Read and process input data
  observeEvent(input$analyze, {
    req(input$inputType)
    req(input$type)
    if (input$inputType == "sequence") {
      rv$data <- rv$original
      # Perform TNA analysis
      tryCatch({
        # Use tna(data) directly
        rv$tna_result <- build_model(data, type = req(input$type))
      }, warning = function(w) {
        logjs(w)
      }, error = function(e) {
        showNotification("There was an error",
                         "",
                         type = "error",
                         duration = 3)
        logjs(e)
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
          build_model(rv$data, type = req(input$type))
        groupchoices <- names(rv$data$meta_data)
        groupchoices <-
          groupchoices[sapply(groupchoices, \(x) ! (x %in% whitelist))]
        updateSelectInput(session, "compareSelect", choices = groupchoices)
      }, warning = function(w) {
        logjs(w)
      }, error = function(e) {
        showNotification("There was an error",
                         "",
                         type = "error",
                         duration = 3)
        logjs(e)
      }, silent = TRUE)
      # Perform TNA analysis
    } else if (input$inputType == "matrix") {
      tryCatch({
        matrix_data <- as.matrix(rv$original)
        rv$data <- matrix_data
        # Perform TNA analysis with matrix input
        rv$tna_result <-
          tna(matrix_data, type = req(input$type))  # Use tna(matrix_data) directly
      }, warning = function(w) {
        logjs(w)
      }, error = function(e) {
        showNotification("There was an error",
                         "",
                         type = "error",
                         duration = 3)
        logjs(e)
      }, silent = TRUE)
    } else if (input$inputType == "sample") {
      tryCatch({
        rv$data <- structure(
          list(
            long_data = NULL,
            sequence_data =  rv$original,
            meta_data = data.frame(
              Achiever = c(rep("High", 1000), rep("Low", 1000)
            )),
            statistics = NULL
          ),
          class = "tna_data"
        )
        groupchoices <- names(rv$data$meta_data)
        updateSelectInput(session, "compareSelect", choices = groupchoices)
        rv$tna_result <-
          build_model(rv$data, type = req(input$type))
        rv$tna_result$data$Achiever = c(rep("High", 1000), rep("Low", 1000))
      }, warning = function(w) {
        logjs(w)
      }, error = function(e) {
        showNotification("There was an error",
                         "",
                         type = "error",
                         duration = 3)
        logjs(e)
        print(e)
      }, silent = TRUE)
    }
    if (req(input$type) == "frequency") {
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
  })
  # Data Preview
  output$dataPreview <- renderDT({
    rv$original <- NULL
    if (is.null(input$inputType)) {
      return(NULL)
    }
    if (!is.null(input$longInput) & input$inputType == "long") {
      rv$original <- import(input$longInput$datapath)
      theoptions <- c("", names(rv$original))
      updateSelectInput(session, "longAction", choices = theoptions)
      updateSelectInput(session, "longActor", choices = theoptions)
      updateSelectInput(session, "longOrder", choices = theoptions)
      updateSelectInput(session, "longTime", choices = theoptions)
    } else if (!is.null(input$matrixInput) & input$inputType == "matrix") {
      rv$original <- import(input$matrixInput$datapath, row.names = 1)
    } else if (!is.null(input$fileInput) & input$inputType == "long") {
      rv$original <- import(input$fileInput$datapath)
    } else if (input$inputType == "sample") {
      rv$original <- group_regulation
    }
    rv$tna_result <- NULL
    rv$centrality_result <- NULL
    rv$cliques_result <- NULL
    rv$clique_plots <- list()
    rv$community_result <- NULL
    rv$bootstrap_result <- NULL
    datatable(rv$original, options = list(scrollX = TRUE))
  })
  output$summary_model <- renderPrint({
    rv$tna_result
  })
  output$summary_boot_model <- renderPrint({
    rv$bootstrap_result
  })
  output$tnaModel <- renderUI({
    if (is.null(rv$tna_result)) {
      NULL
    } else {
      verbatimTextOutput("summary_model")
    }
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
    init_probs <- data.frame(Probability = round(rv$tna_result$inits, 3))
    datatable(init_probs, options = list(pageLength = 10, scrollX = TRUE))
  })
  # Summary Statistics
  output$summaryStats <- renderTable({
    req(rv$tna_result)
    summary(rv$tna_result)
  })
  # Centrality Measures
  output$centralityPlot <- renderPlot({
    req(rv$tna_result)
    # Calculate centrality measures
    centrality_result <- centralities(
      rv$tna_result,
      measures = input$centralitiesChoice,
      normalize = input$normalize,
      loops =  input$loops
    )
    rv$centrality_result <- centrality_result
    # Plot centrality measures
    tryCatch({
      
      plot(centrality_result, ncol = input$nColsCentralities)
    }, warning = function(w) {
      logjs(w)
    }, error = function(e) {
      showNotification("There was an error",
                       "",
                       type = "error",
                       duration = 3)
      logjs(e)
    }, silent = TRUE)
  }, res = 100)
  output$centralityPrint <- renderTable({
    req(rv$centrality_result)
    # Print centrality measures
    (data.frame(rv$centrality_result))
  })
  # Plot TNA results
  output$tnaPlot <- renderPlot({
    req(rv$tna_result)
    tryCatch({
      plot(
        rv$tna_result,
        cut = input$cut,
        minimum = input$minimum,
        label.cex = input$node.label,
        edge.label.cex = input$edge.label,
        vsize = input$vsize,
        layout = input$layout,
        mar = mar
      )
    }, warning = function(w) {
      logjs(w)
    }, error = function(e) {
      showNotification("There was an error",
                       "",
                       type = "error",
                       duration = 3)
      logjs(e)
    }, silent = TRUE)
  }, res = 600)
  # Plot Edge betwenness
  output$edgeBetPlot <- renderPlot({
    req(rv$tna_result)
    # Plot the TNA results directly
    tryCatch({
      plot(
        betweenness_network(rv$tna_result),
        cut = input$cut,
        minimum = input$minimumEbet,
        label.cex = input$node.labelEbet,
        edge.label.cex = input$edge.labelEbet,
        vsize = input$vsizeEbet,
        layout = input$layoutEbet,
        mar = mar
      )
    }, warning = function(w) {
      logjs(w)
    }, error = function(e) {
      showNotification("There was an error",
                       "",
                       type = "error",
                       duration = 3)
      logjs(e)
    }, silent = TRUE)
  }, res = 600)
  # Display Community Counts
  output$communityCounts <- renderPrint({
    req(rv$community_result)
    # Print the counts of communities found by each algorithm
    rv$community_result$counts
  })
  # Plot the selected community algorithm
  output$communityPlot <- renderPlot({
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
        edge.label.cex = input$edge.labelCom,
        vsize = input$vsizeCom,
        layout = input$layoutCom
      )
    }, warning = function(w) {
      logjs(w)
    }, error = function(e) {
      showNotification("There was an error",
                       "",
                       type = "error",
                       duration = 3)
      logjs(e)
    }, silent = TRUE)
  }, res = 600)
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
    if (length(rv$cliques_result$inits) > 0) {
      choices <- seq_along(rv$cliques_result$inits)
      names(choices) <-
        lapply(rv$cliques_result$inits,
               \(x) names(x) |> paste(collapse = " - "))
      names(choices) <-
        paste0("Clique ", choices, ": ", names(choices))
      updateSelectInput(session,
                        "cliqueSelect",
                        choices = choices,
                        selected = 1)  # Default to first clique
    } else {
      updateSelectInput(session,
                        "cliqueSelect",
                        selected = NULL,
                        choices = NULL)
    }
  })
  # Plot Cliques
  output$cliquesPlot <- renderPlot({
    req(rv$cliques_result)
    if (is.null(input$cliqueSelect) | (input$cliqueSelect == "")) {
      return(NULL)
    } else {
      tryCatch({
        plot(
          rv$cliques_result,
          first = as.integer(input$cliqueSelect),
          n = 1,
          ask = FALSE,
          cut = input$cutClique,
          minimum = input$minimumClique,
          label.cex = input$node.labelClique,
          edge.label.cex = input$edge.labelClique,
          vsize = input$vsizeClique,
          layout = input$layoutClique,
          mar = mar
        )
      }, warning = function(w) {
        logjs(w)
      }, error = function(e) {
        showNotification("There was an error",
                         "",
                         type = "error",
                         duration = 3)
        logjs(e)
      }, silent = TRUE)
    }
  }, res = 600)
  observeEvent(input$compareSelect, {
    if (is.null(rv$data$meta_data)) {
      return()
    }
    choices <-
      unique(data.frame(rv$data$meta_data)[, input$compareSelect])
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
  output$comparisonPlot <- renderPlot({
    req(rv$data)
    tryCatch({
      group_tnad <- group_model(
        req(rv$data),
        type = req(input$type),
        group = req(input$compareSelect)
      )
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
          showNotification(
            "Paired test cannot be applied because groups have different number of observations",
            "",
            type = "warning",
            duration = 3
          )
        }
        plot(
          permtest,
          cut = input$cutGroup,
          minimum = input$minimumGroup,
          label.cex = input$node.labelGroup,
          edge.label.cex = input$edge.labelGroup,
          vsize = input$vsizeGroup,
          layout = input$layoutGroup,
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
          vsize = input$vsizeGroup,
          layout = input$layoutGroup,
          posCol = "darkblue",
          negCol  = "red",
          mar = mar
        )
      }
    }, warning = function(w) {
      logjs(w)
      print(w)
    }, error = function(e) {
      showNotification("There was an error",
                       "",
                       type = "error",
                       duration = 3)
      logjs(e)
      print(e)
    }, silent = TRUE)
  }, res = 600)
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
        rv$bootstrap_result <- prune(rv$tna_result, method = "bootstrap", boot = boot)
      }
    }, warning = function(w) {
      print(w)
      logjs(w)
    }, error = function(e) {
      showNotification("There was an error",
                       "",
                       type = "error",
                       duration = 3)
      logjs(e)
      print(e)
    }, silent = TRUE)
  })
  
  output$tnaPlotBoot <- renderPlot({
    req(rv$bootstrap_result)
    tryCatch({
      plot(
        rv$bootstrap_result,
        cut = input$cutBoot,
        minimum = input$minimumBoot,
        label.cex = input$node.labelBoot,
        edge.label.cex = input$edge.labelBoot,
        vsize = input$vsizeBoot,
        layout = input$layoutBoot,
        mar = mar
      )
    }, warning = function(w) {
      print(w)
      logjs(w)
    }, error = function(e) {
      showNotification("There was an error",
                       "",
                       type = "error",
                       duration = 3)
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
  
  output$mosaicPlot <- renderPlot({
    req(rv$tna_result)
    tryCatch({
      group_tnad <- group_model(
        req(rv$data),
        type = req(input$type),
        group = req(input$compareSelect)
      )
      plot_mosaic(group_tnad)
      
    }, warning = function(w) {
      print(w)
      logjs(w)
    }, error = function(e) {
      showNotification("There was an error",
                       "",
                       type = "error",
                       duration = 3)
      logjs(e)
      print(e)
    }, silent = TRUE)
  }, res = 100)  
  
  output$groupCentralitiesPlot <- renderPlot({
    req(rv$tna_result)
    tryCatch({
      group_tnad <- group_model(
        req(rv$data),
        type = req(input$type),
        group = req(input$compareSelect)
      )
      plot(centralities(group_tnad,
                        measures = input$centralitiesChoiceGroup,
                        normalize = input$normalizeGroup,
                        loops =  input$loopsGroup), ncol = input$nColsCentralitiesGroup)
      
    }, warning = function(w) {
      print(w)
      logjs(w)
    }, error = function(e) {
      showNotification("There was an error",
                       "",
                       type = "error",
                       duration = 3)
      logjs(e)
      print(e)
    }, silent = TRUE)
  }, res = 100)
  
  
}
# Run the application
shinyApp(ui = ui, server = server)
