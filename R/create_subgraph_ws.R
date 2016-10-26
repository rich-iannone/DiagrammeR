#' Create a subgraph based on a selection of nodes
#' or edges
#' @description Create a subgraph based on a
#' selection of nodes or edges extant in the graph
#' object.
#' @param graph a graph object of class
#' \code{dgr_graph} that is created using
#' \code{create_graph}.
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' # Create a simple graph
#' nodes <-
#'   create_node_df(
#'     n = 6,
#'     value = c(3.5, 2.6, 9.4,
#'               2.7, 5.2, 2.1))
#'
#' edges <-
#'   create_edge_df(
#'     from = c(1, 2, 4, 5, 2, 6),
#'     to = c(2, 4, 1, 3, 5, 5))
#'
#' graph <-
#'   create_graph(
#'     nodes_df = nodes,
#'     edges_df = edges)
#'
#' get_node_ids(graph)
#' #> [1] 1 2 3 4 5 6
#'
#' get_edges(graph, return_type = "vector")
#' #> [1] "1 -> 2" "2 -> 4" "4 -> 1"
#' #> [4] "5 -> 3" "2 -> 5" "6 -> 5"
#'
#' # Create a selection of nodes
#' graph <-
#'   select_nodes(
#'     graph = graph,
#'     node_attr = "value",
#'     search = "> 3")
#'
#' # Create a subgraph based on the selection
#' subgraph <- create_subgraph_ws(graph)
#'
#' # Check the nodes available in the subgraph
#' get_node_ids(subgraph)
#' #> [1] 1 3 5
#'
#' # Check the edges available in the subgraph
#' get_edges(subgraph, return_type = "vector")
#' #> [1] "5 -> 3"
#' @importFrom dplyr filter semi_join
#' @importFrom stringr str_split
#' @export create_subgraph_ws

create_subgraph_ws <- function(graph) {

  # Stop function if the graph does not contain a selection
  if (is.null(graph$selection)) {
    stop("The graph does not contain an active selection")
  }

  # Get the active selection
  selection <- get_selection(graph)

  # Filter the nodes in the graph
  if (inherits(selection, "numeric")) {

    ndf <-
      graph$nodes_df %>%
      dplyr::filter(id %in% selection)

    edf <-
      graph$edges_df %>%
      dplyr::filter(from %in% selection & to %in% selection)
  }

  # Filter the edges in the graph
  if (inherits(selection, "character")) {

    selection_from <-
      stringr::str_split(selection, " -> ") %>%
      sapply("[[", 1) %>%
      as.integer

    selection_to <-
      stringr::str_split(selection, " -> ") %>%
      sapply("[[", 2) %>%
      as.integer

    selection_df <-
      data.frame(from = selection_from, to = selection_to)

    edf <-
      graph$edges_df %>%
      dplyr::semi_join(selection_df, by = c("from", "to"))

    ndf <-
      graph$nodes_df %>%
      dplyr::filter(id %in% unique(c(edf$from, edf$to)))
  }

  # Create a subgraph
  graph$nodes_df <- ndf
  graph$edges_df <- edf

  return(graph)
}
