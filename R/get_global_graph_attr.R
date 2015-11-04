#' Get global graph attributes
#' @description Get the presently set global attributes for a graph object of
#' class \code{dgr_graph}.
#' @param graph a graph object of class \code{dgr_graph} that is created using
#' \code{create_graph}.
#' @return a list object containing global attributes for a graph.
#' @examples
#' \dontrun{
#' library(magrittr)
#'
#' # Create a new graph and set some global attributes
#' graph <- create_graph() %>%
#'   set_global_graph_attr("graph", "overlap", "true") %>%
#'   set_global_graph_attr("node", "fontname", "Helvetica") %>%
#'   set_global_graph_attr("edge", "color", "gray")
#'
#' # Verify that the global attributes have been set
#' get_global_graph_attr(graph)
#' #> $graph_attrs
#' #> [1] "overlap = true"
#' #>
#' #> $node_attrs
#' #> [1] "fontname = Helvetica"
#' #>
#' #> $edge_attrs
#' #> [1] "color = gray"
#' }
#' @export get_global_graph_attr

get_global_graph_attr <- function(graph){

    return(list(graph_attrs = graph$graph_attrs,
                node_attrs = graph$node_attrs,
                edge_attrs = graph$edge_attrs))
}
