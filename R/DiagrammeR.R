#' <Add Title>
#'
#' <Add Description>
#'
#' @import htmlwidgets
#'
#' @export
DiagrammeR <- function(message, width = NULL, height = NULL) {

  # forward options using x
  x = list(
    message = message
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
