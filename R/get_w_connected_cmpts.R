#' Get all nodes associated with connected components
#'
#' Determine which nodes in a graph belong to different weakly connected
#' components (i.e., distinct sets of nodes with traversable paths to and from
#' each node in the set).
#'
#' @inheritParams render_graph
#' @return A data frame with nodes and their membership in different weakly
#'   connected components.
#' @examples
#' # Create a graph with 2 cycles
#' graph <-
#'   create_graph() %>%
#'   add_cycle(n = 4) %>%
#'   add_cycle(n = 3)
#'
#' # Check if the graph is connected
#' graph %>%
#'   is_graph_connected()
#'
#' # Get the graph's weakly-connected
#' # components
#' graph %>% get_w_connected_cmpts()
#'
#' @export
get_w_connected_cmpts <- function(graph) {

  # Get the name of the function
  fcn_name <- get_calling_fcn()

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {

    emit_error(
      fcn_name = fcn_name,
      reasons = "The graph object is not valid")
  }

  # Create a graph where only mandatory
  # node and edge attributes are retained;
  # transform to an igraph object
  ig_graph <-
    create_graph(
      nodes_df = graph %>%
        get_node_df() %>%
        dplyr::select(id, type, label),
      edges_df = graph %>%
        get_edge_df() %>%
        dplyr::select(id, from, to, rel),
      directed = is_graph_directed(graph)) %>%
    to_igraph()

  # Get the component list from the graph
  components <-
    ig_graph %>%
    igraph::components(mode = "weak")

  # Create the output data frame
  data.frame(
    id = as.integer(names(components$membership)),
    wc_component = components$membership,
    stringsAsFactors = FALSE)
}
