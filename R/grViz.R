#' R + viz.js
#' 
#' Make diagrams in R using \href{https://github.com/mdaines/viz.js}{viz.js}
#' with infrastructure provided by \href{http://www.htmlwidgets.org/}{htmlwidgets}.
#'
#' @param diagram \href{http://graphviz.org}{graphviz} spec
#' for a diagram as either text, filename string, or file connection
#' 
#' @return An object of class \code{htmlwidget} that will
#' intelligently print itself into HTML in a variety of contexts
#' including the R console, within R Markdown documents,
#' and within Shiny output bindings.
#' 
#' @export
#' 
grViz <- function(diagram = "", width = NULL, height = NULL) {

  # check for a connection or file
  if (inherits(diagram, "connection") || file.exists(diagram)) {
    diagram <- readLines(diagram, warn = FALSE)
    diagram <- paste0(diagram, collapse = "\n")
  } else {
    # check for vector with length > 1 and concatenate
    if (length(diagram) > 1 ){
      diagram = paste0( diagram, collapse = "\n" )
    }
  }
  
  # forward options using x
  x <- list(diagram = diagram)
  
  
  # create widget
  htmlwidgets::createWidget(
    name = "grViz",
    x = x,
    width = width,
    height = height,
    package = "DiagrammeR"
  )

}