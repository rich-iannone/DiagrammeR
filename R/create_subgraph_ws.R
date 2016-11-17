#' Create a subgraph based on a selection of nodes
#' or edges
#' @description Create a subgraph based on a
#' selection of nodes or edges stored in the graph
#' object. Selections of nodes can be performed using
#' the following \code{select_...} functions:
#' \code{select_nodes()},
#' \code{select_last_node()},
#' \code{select_nodes_by_degree()},
#' \code{select_nodes_by_id()}, or
#' \code{select_nodes_in_neighborhood()}.
#' Alternatively, selections of edges can be made
#' with these functions: \code{select_edges()},
#' \code{select_last_edge()}, or
#' \code{select_edges_by_node_id()}. Selections of
#' nodes or edges can also be performed using
#' any of the traversal functions (\code{trav_...}).
#' @param graph a graph object of class
#' \code{dgr_graph} that is created using
#' \code{create_graph}.
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' # Create a node data frame (ndf)
#' ndf <-
#'   create_node_df(
#'     n = 6,
#'     value = c(3.5, 2.6, 9.4,
#'               2.7, 5.2, 2.1))
#'
#' # Create an edge data frame (edf)
#' edf <-
#'   create_edge_df(
#'     from = c(1, 2, 4, 5, 2, 6),
#'     to = c(2, 4, 1, 3, 5, 5))
#'
#' # Create a graph
#' graph <-
#'   create_graph(
#'     nodes_df = ndf,
#'     edges_df = edf)
#'
#' # Create a selection of nodes, this selects
#' # nodes `1`, `3`, and `5`
#' graph <-
#'   graph %>%
#'   select_nodes("value > 3")
#'
#' # Create a subgraph based on the selection
#' subgraph <- create_subgraph_ws(graph)
#'
#' # Verify that the nodes in the subgraph's
#' # internal node data frame match the
#' # selection in the `graph` object
#' all(
#'   get_node_ids(subgraph) ==
#'   get_selection(graph))
#' #> [1] TRUE
#'
#' # Check the edges available in the subgraph,
#' # there are usually fewer edges and the
#' # remaining edges have node IDs in the set
#' # of those used in the selection of nodes
#' get_edges(subgraph, return_type = "vector")
#' #> [1] "5 -> 3"
#' @importFrom dplyr filter semi_join
#' @importFrom stringr str_split
#' @export create_subgraph_ws

create_subgraph_ws <- function(graph) {

  # Get the time of function start
  time_function_start <- Sys.time()

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {
    stop("The graph object is not valid.")
  }

  # Stop function if the graph does not contain a selection
  if (is.null(graph$selection)) {
    stop("The graph does not contain an active selection")
  }

  # Create bindings for specific variables
  id <- from <- to <- NULL

  # Get the active selection
  selection <- get_selection(graph)

  # Filter the nodes in the graph
  if (inherits(selection, c("numeric", "integer"))) {

    ndf <-
      graph$nodes_df %>%
      dplyr::filter(id %in% selection)

    edf <-
      graph$edges_df %>%
      dplyr::filter(from %in% selection & to %in% selection)

    # Create a subgraph
    graph$nodes_df <- ndf
    graph$edges_df <- edf
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

    # Create a subgraph
    graph$nodes_df <- ndf
    graph$edges_df <- edf
  }

  # Update the `graph_log` df with an action
  graph$graph_log <-
    add_action_to_log(
      graph_log = graph$graph_log,
      version_id = nrow(graph$graph_log) + 1,
      function_used = "create_subgraph_ws",
      time_modified = time_function_start,
      duration = graph_function_duration(time_function_start),
      nodes = nrow(graph$nodes_df),
      edges = nrow(graph$edges_df))

  return(graph)
}
