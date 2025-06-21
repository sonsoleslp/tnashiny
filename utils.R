getPalettes <- function(n) {
  palettes <- data.frame(RColorBrewer::brewer.pal.info) |> 
    dplyr::filter(maxcolors >= n) 
  palettes$category <- factor(palettes$category, levels = c("qual", "div", "seq")) 
  palettes <- dplyr::arrange(palettes, category)
  c("Default", rownames(palettes), "White")
} 


getPalette <- function(palette, n) {
  if (palette == "Default") {
    tna:::color_palette(n)
  } else if (palette == "White") {
    "white"
  } else {
    suppressWarnings(RColorBrewer::brewer.pal(n, palette))
  }
}

showGenericError <- function(e = "There was an error") {
  showNotification(e, "", type = "error", duration = 3)
}
