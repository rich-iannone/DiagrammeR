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
#' get_s_connected_cmpts(graph)
#' #>   id sc_component
#' #> 1  1            1
#' #> 2  2            1
#' #> 3  3            1
#' #> 4  4            2
#' #> 5  5            2
#' #> 6  6            2
#' #> 7  7            2
#' @importFrom igraph components
#' @export get_s_connected_cmpts

get_s_connected_cmpts <- function(graph) {

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {

    stop(
      "The graph object is not valid.",
      call. = FALSE)
  }

  # Convert the graph to an igraph object
  ig_graph <- to_igraph(graph)

  # Get the component list
  components <-
    igraph::components(ig_graph, mode = "strong")

  # Create the output data frame
  data.frame(
    id = as.integer(names(components$membership)),
    sc_component = components$membership,
    stringsAsFactors = FALSE)
}
