#' Get a cached vector from a graph object
#'
#' @description
#'
#' Get the vector cached in a graph object of class `dgr_graph`.
#'
#' @inheritParams render_graph
#' @param name the name of the object to extract from the cache. If none
#'   supplied, the most recent object added to the cache will be returned.
#'
#' @return A vector.
#'
#' @examples
#' # Set a seed
#' suppressWarnings(RNGversion("3.5.0"))
#' set.seed(23)
#'
#' # Create a graph with 5 nodes and 5 edges
#' graph <-
#'   create_graph() %>%
#'   add_n_nodes(n = 5) %>%
#'   set_node_attrs(
#'     node_attr = value,
#'     values = rnorm(
#'       n = count_nodes(.),
#'       mean = 8,
#'       sd = 2)) %>%
#'   add_edges_w_string(
#'     edges = "1->2 1->3 2->4 2->5 3->2")
#'
#' # Cache all values from the node attribute `value`
#' # as a numeric vector
#' graph <-
#'   graph %>%
#'   set_cache(
#'     name = "value",
#'     to_cache = get_node_attrs(
#'       graph = .,
#'       node_attr = value))
#'
#' # Return the cached vector
#' graph %>% get_cache()
#'
#' @export
get_cache <- function(
    graph,
    name = NULL
) {

  # Validation: Graph object is valid
  check_graph_valid(graph)

  # If there is no cached vector available, return NA
  if (length(graph$cache) == 0) {
    return(NA)
  }

  if (is.null(name)) {
    return(graph$cache[[length(graph$cache)]])
  } else {
    return(graph$cache[[name]])
  }
}
