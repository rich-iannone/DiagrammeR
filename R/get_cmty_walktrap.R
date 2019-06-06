#' Get community membership using the Walktrap method
#'
#' With the Walktrap community finding algorithm, obtain the group membership
#'   values for each of the nodes in the graph.
#' @inheritParams render_graph
#' @param steps the number of steps to take for each of the random walks.
#' @return a data frame with group membership assignments for each of the nodes.
#' @examples
#' # Create a random graph using the
#' # `add_gnm_graph()` function
#' graph <-
#'   create_graph(
#'     directed = FALSE) %>%
#'   add_gnm_graph(
#'     n = 10,
#'     m = 15,
#'     set_seed = 23)
#'
#' # Get the group membership
#' # values for all nodes in the
#' # graph through the Walktrap
#' # community finding algorithm
#' graph %>%
#'   get_cmty_walktrap()
#'
#' # Add the group membership
#' # values to the graph as a
#' # node attribute
#' graph <-
#'   graph %>%
#'   join_node_attrs(
#'     df = get_cmty_walktrap(.))
#'
#' # Display the graph's
#' # node data frame
#' graph %>% get_node_df()
#'
#' @export
get_cmty_walktrap <- function(graph,
                              steps = 4) {

  # Get the name of the function
  fcn_name <- get_calling_fcn()

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {

    emit_error(
      fcn_name = fcn_name,
      reasons = "The graph object is not valid")
  }

  # Convert the graph to an igraph object
  ig_graph <- to_igraph(graph)

  # Get the community object using the
  # `cluster_walktrap()` function
  cmty_walktrap_obj <-
    igraph::cluster_walktrap(ig_graph, steps = steps)

  # Create df with node memberships
  data.frame(
    id = igraph::membership(cmty_walktrap_obj) %>% names() %>% as.integer(),
    walktrap_group = as.vector(igraph::membership(cmty_walktrap_obj)),
    stringsAsFactors = FALSE)
}
