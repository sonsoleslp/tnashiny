library(shiny)
library(shinydashboard)
library(bslib)
library(shinyjs)
library(shinyjqui)

mar <- c(2.5, 2.5, 2.5, 2.5)
db_header <- dashboardHeader(title = "TNA")
logo <- tags$span(
  tags$a(
    href = "https://sonsoles.me/tna",
    tags$img(
      src = "logo.png",
      height = "44",
      width = "40"
    )
  ), 
"TNA")

db_header$children[[2]]$children <- logo
ui <- dashboardPage(
  skin = "purple",
  title = "TNA",
  db_header,
  dashboardSidebar(
    sidebarMenu(
      menuItem("About TNA", tabName = "about", icon = icon("circle-info"), selected = FALSE),
      menuItem("Input Data", tabName = "input", icon = icon("table"), selected = TRUE),
      menuItem("Summary results", tabName = "results", icon = icon("list")),
      menuItem("Visualization", tabName = "tna_plot", icon = icon("circle-nodes")),
      menuItem("Sequence Analysis", tabName = "sequence", icon = icon("chart-bar")),
      menuItem("Centrality Measures", tabName = "centrality", icon = icon("chart-line")),
      menuItem("Community Detection", tabName = "communities", icon = icon("users")),
      menuItem("Edge Betweenness", tabName = "edgebet", icon = icon("people-arrows")),
      menuItem("Cliques", tabName = "cliques", icon = icon("sitemap")),
      menuItem("Validation", tabName = "bootstrap", icon = icon("check-circle")),
      menuItem("Comparison", tabName = "comparison", icon = icon("balance-scale"))
    )
  ),
  dashboardBody(
    tags$html(lang = "en"),
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
    tabItems(
      tabItem(tabName = "about", getIntro()),
      # Input Data Tab
      tabItem(tabName = "input", getInputData()),
      # Results Tab
      tabItem(tabName = "results", getSummary()),
      # Sequence Tab
      tabItem(tabName = "sequence", getSAAllowed(), getSANotAllowed()),
      # Centrality Measures Tab
      tabItem(tabName = "centrality", getCentralities()),
      # TNA Results Plot Tab
      tabItem(tabName = "tna_plot", getVisualization("")),
      # Edge Betweenness Tab
      tabItem(tabName = "edgebet", getVisualization("Ebet")),
      # Community Detection Tab
      tabItem(tabName = "communities", getCommunities()),
      # New Cliques Tab
      tabItem(tabName = "cliques", getCliques()),
      # Bootstrapping Tab
      tabItem(tabName = "bootstrap", getValidationNotAllowed(), getValidationAllowed()),
      # Comparison Tab
      tabItem(tabName = "comparison", getComparisonNotAllowed(), getComparisonAllowed())
    )
  )
)