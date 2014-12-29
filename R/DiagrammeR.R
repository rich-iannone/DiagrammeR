#' R + mermaid.js
#'
#' Make diagrams in R using \href{https://github.com/knsv/mermaid/wiki}{mermaid.js}
#' with infrastructure provided by \href{http://www.htmlwidgets.org/}{htmlwidgets}.
#' 
#' @param diagram string diagram in mermaid markdown-like language.
#' If no diagram is provided \code{diagram = ""} then the function will assume that
#' a diagram will be provided by \code{\link[htmltools]{tags}} and
#' \code{DiagrammeR} is just being used for dependency injection.
#'
#' @import htmlwidgets
#'
#' @export
DiagrammeR <- function(diagram = "", width = NULL, height = NULL) {

  # forward options using x
  x = list(
    diagram = diagram
  )

  # create widget
  htmlwidgets::createWidget(
    name = 'DiagrammeR',
    x,
    width = width,
    height = height,
    package = 'DiagrammeR'
  )
}

#' Widget output function for use in Shiny
#'
#' @export
DiagrammeROutput <- function(outputId, width = '100%', height = '400px'){
  shinyWidgetOutput(outputId, 'DiagrammeR', width, height, package = 'DiagrammeR')
}

#' Widget render function for use in Shiny
#'
#' @export
renderDiagrammeR <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  shinyRenderWidget(expr, DiagrammeROutput, env, quoted = TRUE)
}
