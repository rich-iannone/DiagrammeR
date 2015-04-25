#' Render the graph or output in various formats
#'
#' Using a 'gv_graph' object, either render graph in the Viewer or output in various formats.
#'
#' @param graph a 'gv_graph' object, created using the \code{graphviz_graph} function
#' @param output a string specifying the output type; \code{graph} (the default) renders the graph using the \code{grViz} function, \code{DOT} outputs DOT code for the graph, and \code{SVG} provides SVG code for the rendered graph.
#' @param width an optional parameter for specifying the width of the resulting graphic in pixels.
#' @param height an optional parameter for specifying the height of the resulting graphic in pixels.
#' @export graphviz_render

graphviz_render <- function(graph,
                            output = "graph",
                            width = NULL,
                            height = NULL){

  stopifnot(class(graph) == "gv_graph")

  dot_code <- graph$dot_code

  if (output == "DOT"){

    return(dot_code)
  }

  if (output == "SVG"){

    svg_code <- exportSVG(grViz(diagram = dot_code,
                                width = width,
                                height = height))

    return(svg_code)
  }

  if (output == "graph"){

    grViz(diagram = dot_code, width = width, height = height)
  }

}
