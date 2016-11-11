#' Is the graph a connected graph?
#' @description Determines whether a graph is a
#' connected graph.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @return a logical value.
#' @examples
#' \dontrun{
#' # This graph, created using `create_random_graph()`
#' # is almost fully connected but there is an
#' # isolated node with no edges
#' graph_1 <-
#'   create_random_graph(
#'     30, 50, set_seed = 1)
#'
#' graph_1 %>% is_graph_connected
#' #> [1] FALSE
#'
#' # The following graph is fully connected
#' graph_2 <-
#'   create_random_graph(
#'     36, 50, set_seed = 1)
#'
#' graph_2 %>% is_graph_connected()
#' #> [1] TRUE
#'
#' # Modify `graph_2` so that there are two
#' # clusters of nodes (i.e., making the graph
#' # not connected)
#' graph_3 <-
#'   graph_2 %>%
#'   delete_edge(10, 36) %>%
#'   delete_edge(25, 27) %>%
#'   delete_edge(28, 29) %>%
#'   delete_edge(4, 29) %>%
#'   delete_edge(24, 32)
#'
#' graph_3 %>% is_graph_connected()
#' #> [1] FALSE
#' }
#' @export is_graph_connected

is_graph_connected <- function(graph) {

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {
    stop("The graph object is not valid.")
  }

  wc_components <- get_w_connected_cmpts(graph)

  if (length(unique(wc_components$wc_component)) > 1) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}
