#' Get a cached vector from a graph object
#' @description Get the vector cached in a graph object
#' of class \code{dgr_graph}.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @return a vector.
#' @examples
#' # Set a seed
#' set.seed(22)
#'
#' # Create a graph with 5 nodes and 5 edges
#' graph <-
#'   create_graph() %>%
#'   add_n_nodes(5) %>%
#'   set_node_attrs(
#'     "value", rnorm(node_count(.), 8, 2)) %>%
#'   add_edges_w_string(
#'     "1->2 1->3 2->4 2->5 3->2")
#'
#' # Cache all values from the node attribute `value`
#' # as a numeric vector
#' graph <-
#'   graph %>%
#'   cache_node_attrs("value", "numeric")
#'
#' # Return the cached vector
#' graph %>% get_cache()
#' #> [1] 6.975722 12.970367 10.015652
#' #> [4] 8.585629 7.582081
#' @export get_cache

get_cache <- function(graph) {

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {
    stop("The graph object is not valid.")
  }

  # If there is no cached vector available, return NA
  if (is.null(graph$cache)) {
    return(NA)
  }

  return(graph$cache)
}
