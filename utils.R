getPalettes <- function(n, noWhite = FALSE) {
  palettes <- data.frame(RColorBrewer::brewer.pal.info) |> 
    dplyr::filter(maxcolors >= n) 
  palettes$category <- factor(palettes$category, levels = c("qual", "div", "seq")) 
  palettes <- dplyr::arrange(palettes, category)
  if(noWhite) {
    c("Default", rownames(palettes), "White")
  } else {
    c("Default", rownames(palettes))
  }
  
} 


getPalette <- function(palette, n) {
  if (palette == "Default") {
    tna:::color_palette(n)
  } else if (palette == "White") {
    "white"
  } else {
    pal <- suppressWarnings(RColorBrewer::brewer.pal(n, palette))
    if ((n == 2) & (length(pal) == 3)) {
      c(pal[1],pal[3])
    }
  }
}

showGenericError <- function(e = "There was an error") {
  showNotification(e, "", type = "error", duration = 3)
}
