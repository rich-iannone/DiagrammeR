#' R + viz.js
#' 
#' Make diagrams in R using \href{https://github.com/mdaines/viz.js}{viz.js}
#' with infrastructure provided by \href{http://www.htmlwidgets.org/}{htmlwidgets}.
#'
#' @param diagram \href{http://graphviz.org}{graphviz} spec
#' for a diagram as either text, filename string, or file connection
#' @param engine string for the Graphviz layout engine; can be
#' "dot" (default), "neato", "circo", or "twopi". For more information 
#' see \href{viz.js Usage}{https://github.com/mdaines/viz.js#usage}.
#' @param options parameters supplied to the htmlwidgets framework.
#' @param width an optional parameter for specifying the width of the resulting graphic
#' in pixels.
#' @param height an optional parameter for specifying the height of the resulting graphic
#' in pixels.
#' @return An object of class \code{htmlwidget} that will
#' intelligently print itself into HTML in a variety of contexts
#' including the R console, within R Markdown documents,
#' and within Shiny output bindings.
#' 
#' @export
#' 
grViz <- function(diagram = "", engine="dot", options=NULL, width = NULL, height = NULL) {

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
  
  # single quotes within a diagram spec are problematic
  #  try to replace with \"
  diagram = gsub(x=diagram,"'","\"")
  
  # forward options using x
  x <- list(
    diagram = diagram
    , config = list(
      engine = engine
      , options = options
    )
  )
  
  if(!is.null(options("viewer"))){
    warning(
      "grViz() might not work with RStudio Viewer but should work in another browser"
      , call. = F
    )
  }
  
  # create widget
  htmlwidgets::createWidget(
    name = "grViz",
    x = x,
    width = width,
    height = height,
    package = "DiagrammeR",
    # since grViz does not work in RStudio viewer
    htmlwidgets::sizingPolicy(viewer.suppress = TRUE)
  )

}