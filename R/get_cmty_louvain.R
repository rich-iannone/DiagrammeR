#' Get community membership by Louvain optimization
#'
#' Through the use of multi-level optimization of a modularity score, obtain the
#'   group membership values for each of the nodes in the graph.
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
#' # Get the group membership values
#' # for all nodes in the graph
#' # through the multi-level
#' # optimization of modularity
#' # algorithm
#' graph %>%
#'   get_cmty_louvain()
#'
#' # Add the group membership
#' # values to the graph as a
#' # node attribute
#' graph <-
#'   graph %>%
#'   join_node_attrs(
#'     df = get_cmty_louvain(.))
#'
#' # Display the graph's
#' # node data frame
#' graph %>% get_node_df()
#' @importFrom igraph cluster_louvain membership
#' @export
get_cmty_louvain <- function(graph) {

  # Get the name of the function
  fcn_name <- get_calling_fcn()

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {

    emit_error(
      fcn_name = fcn_name,
      reasons = "The graph object is not valid")
  }

  # If graph is directed, transform to undirected
  graph <- set_graph_undirected(graph)

  # Convert the graph to an igraph object
  ig_graph <- to_igraph(graph)

  # Get the community object using the
  # `cluster_louvain()` function
  cmty_louvain_obj <-
    igraph::cluster_louvain(ig_graph)

  # Create df with node memberships
  data.frame(
    id = igraph::membership(cmty_louvain_obj) %>% names() %>% as.integer(),
    louvain_group = as.vector(igraph::membership(cmty_louvain_obj)),
    stringsAsFactors = FALSE)
}
