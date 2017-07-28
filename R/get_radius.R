#' Get graph radius
#' @description Get the radius of a graph, which is
#' the smallest eccentricity in the graph. The graph
#' eccentricity of a node is its shortest path from
#' the farthest other node in the graph.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @param direction using \code{all} (the default), the
#' search will ignore edge direction while traversing
#' through the graph. With \code{out}, measurements of
#' paths will be from a node whereas with \code{in},
#' measurements of paths will be to a node.
#' @return a single numeric value representing the
#' radius of the graph.
#' @examples
#' # Create a cycle graph
#' graph <-
#'   create_graph() %>%
#'   add_cycle(n = 5)
#'
#' # Determine the graph's radius
#' get_radius(graph)
#' #> [1] 2
#'
#' # Create a full graph and then
#' # get the radius for that
#' create_graph() %>%
#'   add_full_graph(n = 10) %>%
#'   get_radius()
#' #> [1] 1
#' @importFrom igraph radius
#' @export get_radius

get_radius <- function(graph,
                       direction = "all") {

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {
    stop("The graph object is not valid.")
  }

  # Ensure that values provided for the
  # `direction` argument are from the
  # valid options
  if (!(direction %in% c("all", "in", "out"))) {
    stop("Valid options for `direction` are `all`, `in`, or `out`.")
  }

  # If the graph is empty, then return NA
  if (nrow(graph$nodes_df) == 0) {
    return(as.numeric(NA))
  }

  # Convert the graph to an igraph object
  ig_graph <- to_igraph(graph)

  # Get the radius of the graph
  igraph::radius(ig_graph, mode = direction)
}
