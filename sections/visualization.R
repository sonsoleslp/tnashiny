library(colourpicker)

availableLayouts = c("circle","spring","star","tree","nicely","grid","random","dh","fr","gem","graphopt","kk","lgl")

getVisualizationSettings <- function (prefix, width = 3, twoColors = FALSE) {
    colored <- colourInput(paste0("edgeColor", prefix), "Edge color", value = "darkblue")
  
    if (twoColors) {
      colored <- div(
        colourInput(paste0("posEdgeColor", prefix), "Positive color", value = "darkblue"),
        colourInput(paste0("negEdgeColor", prefix), "Negative color", value = "#BF0000")
      )
    }
    box(title = "Settings", width = width,
        selectInput(paste0("layout", prefix), "Layout", choices = availableLayouts, selected = "circle"),
        selectInput(paste0("shape", prefix), "Shape", choices = c("circle", "square"), selected = "circle"),
        div(colored),
        selectInput(paste0("palette", prefix), "Palette", choices = getPalettes(9), selected = "Set3"),
        sliderInput(paste0("cut", prefix), "Cut Value", min = 0, max = 1, value = 0.1, step = 0.01, ticks = FALSE),
        sliderInput(paste0("minimum", prefix), "Minimum Value", min = 0, max = 1, value = 0.05, step = 0.01, ticks = FALSE),
        sliderInput(paste0("edge.label", prefix), "Edge label size", min = 0, max = 10, value = 1, step = 0.1, ticks = FALSE),
        sliderInput(paste0("vsize", prefix), "Node  size", min = 0, max = 30, value = 8, step = 0.1, ticks = FALSE),
        sliderInput(paste0("node.label", prefix), "Node label size", min = 0, max = 10, value = 1, step = 0.1, ticks = FALSE),
        checkboxInput(paste0("curveAll", prefix), "Curved edges?", value = FALSE)
    )
}

getVisualizationPlot <- function (prefix, width = 9) {
  box(title = "Visualization", width = width,
      div(jqui_resizable(
        plotOutput(paste0(prefix,"Plot"), width = "600px", height = "600px"),
        # Render the TNA plot here
        options = list(ghost = TRUE, helper = "resizable-helper")
      ), align = "center")
  )
}

getVisualization <- function (prefix) {
  return(fluidRow(
    getVisualizationSettings(prefix),
    getVisualizationPlot(prefix)
  ))
}


getLayout <- function(layout) {

  switch(layout, 
         circle = "circle", 
         spring = "spring", 
         groups = "groups", 
         star = igraph::layout_as_star, 
         tree = igraph::layout_as_tree,
         nicely = igraph::layout_nicely,
         grid = igraph::layout_on_grid,
         sphere = igraph::layout_on_sphere,
         random = igraph::layout_randomly,
         dh = igraph::layout_with_dh,
         fr = igraph::layout_with_fr,
         gem = igraph::layout_with_gem, 
         graphopt = igraph::layout_with_graphopt, 
         kk = igraph::layout_with_kk, 
         lgl = igraph::layout_with_lgl, 
         mds = igraph::layout_with_mds, 
         sugiyama = igraph::layout_with_sugiyama,
         "circle"
      )
}
