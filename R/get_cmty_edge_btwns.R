#' Get community membership by edge betweenness
#'
#' Using edge betweenness, obtain the group membership values for each of the
#'   nodes in the graph.
#' @inheritParams render_graph
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
#' # graph through calculation of
#' # the leading non-negative
#' # eigenvector of the modularity
#' # matrix of the graph
#' graph %>%
#'   get_cmty_edge_btwns()
#'
#' # Add the group membership
#' # values to the graph
#' # as a node attribute
#' graph <-
#'   graph %>%
#'   join_node_attrs(
#'      df = get_cmty_edge_btwns(.))
#'
#' # Display the graph's
#' # node data frame
#' graph %>% get_node_df()
#'
#' @export
get_cmty_edge_btwns <- function(graph) {

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
  # `cluster_edge_betweenness()` function
  cmty_edge_btwns_obj <-
    igraph::cluster_edge_betweenness(
      graph = ig_graph)

  # Create df with node memberships
  data.frame(
    id = igraph::membership(cmty_edge_btwns_obj) %>% names() %>% as.integer(),
    edge_btwns_group = as.vector(igraph::membership(cmty_edge_btwns_obj)),
    stringsAsFactors = FALSE)
}
