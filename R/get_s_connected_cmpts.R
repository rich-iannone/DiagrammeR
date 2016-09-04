#' Get nodes within strongly connected components
#' @description Determine which nodes in a graph belong
#' to different strongly connected components.
#' @param graph a graph object of class
#' \code{dgr_graph} that is created using
#' \code{create_graph}.
#' @return a data frame with nodes and their membership
#' in different strongly connected components.
#' @examples
#' # Create a graph with a random connection
#' # between 2 different cycles
#' graph <-
#'   create_graph() %>%
#'   add_cycle(3, "cycle_1") %>%
#'   add_cycle(4, "cycle_2") %>%
#'   add_edge(
#'     from = get_nodes(., "type", "cycle_1") %>%
#'       sample(1),
#'     to = get_nodes(., "type", "cycle_2") %>%
#'       sample(1))
#'
#' # Get the strongly connected components as a
#' # data frame of nodes and their groupings
#' get_s_connected_cmpts(graph)
#' #>   node sc_component
#' #> 1    1            1
#' #> 2    2            1
#' #> 3    3            1
#' #> 4    4            2
#' #> 5    5            2
#' #> 6    6            2
#' #> 7    7            2
#' @importFrom igraph components
#' @export get_s_connected_cmpts

get_s_connected_cmpts <- function(graph) {

  # Convert the graph to an igraph object
  ig_graph <- to_igraph(graph)

  # Get the component list
  components <-
    igraph::components(ig_graph, mode = "strong")

  # Create the output data frame
  components_df <-
    data.frame(
      node = names(components$membership),
      sc_component = components$membership,
      stringsAsFactors = FALSE)

  return(components_df)
}
