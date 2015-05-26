#' Export grViz graph as SVG with \code{V8}
#' @description Use viz.js with \code{V8} to get the diagram rendered as SVG in R instead of the browser.
#' @param gv htmlwidget to render as SVG.
#' @return \code{string} of SVG XML text.
#' @examples
#' \dontrun{
#'  library(DiagrammeR)
#'  (svg <- exportSVG(grViz('digraph{a->b; c->a; c->b; c->d;}', engine = 'circo')))
#'
#'  # this can then be used with htmltools
#'  #   can save significantly on size of output
#'  #   using svg rather than unrendered grViz
#'  library(htmltools)
#'  html_print(HTML(svg))
#' }
#'
#' @export

exportSVG <- function(gv){
  # check to make sure that V8 is available
  if(!requireNamespace("V8")) stop("V8 is required to export.", call. = F)
  stopifnot(packageVersion("V8") >= "0.5")

  # check to make sure gv is grViz
  if(!inherits(gv,"grViz")) "gv must be a grViz htmlwidget."

  ct <- V8::new_context("window")
  invisible(ct$source(system.file("htmlwidgets/lib/viz/viz.js", package = "DiagrammeR")))

  svg <- ct$call("Viz", gv$x$diagram, "svg", gv$x$config$engine, gv$x$config$options )

  return(svg)
}
