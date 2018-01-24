#' Get nodes within strongly connected components
#' @description Determine which nodes in a graph belong
#' to different strongly connected components.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @return a data frame with nodes and their membership
#' in different strongly connected components.
#' @examples
#' set.seed(23)
#'
#' # Create a graph with a random
#' # connection between 2 different
#' # node cycles
#' graph <-
#'   create_graph() %>%
#'   add_cycle(
#'     n = 3,
#'     type = "cycle_1") %>%
#'   add_cycle(
#'     n = 4,
#'     type = "cycle_2") %>%
#'   add_edge(
#'     from =
#'       get_node_ids(
#'         graph = .,
#'         conditions =
#'           type == "cycle_1") %>%
#'         sample(size = 1),
#'     to =
#'       get_node_ids(
#'         graph = .,
#'         conditions =
#'           type == "cycle_2") %>%
#'         sample(size = 1))
#'
#' # Get the strongly connected
#' # components as a data frame of
#' # nodes and their groupings
#' graph %>%
#'   get_s_connected_cmpts()
#' @importFrom igraph components
#' @importFrom dplyr select
#' @export get_s_connected_cmpts

get_s_connected_cmpts <- function(graph) {

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {

    stop(
      "The graph object is not valid.",
      call. = FALSE)
  }

  # Create bindings for specific variables
  id <- from <- to <- type <- label <- rel <- NULL

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
    igraph::components(mode = "strong")

  # Create the output data frame
  data.frame(
    id = as.integer(names(components$membership)),
    sc_component = components$membership,
    stringsAsFactors = FALSE)
}
