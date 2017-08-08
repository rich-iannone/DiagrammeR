#' Set edge attributes with an edge selection
#' @description From a graph object of class
#' \code{dgr_graph} or an edge data frame, set edge
#' attribute properties for one or more edges.
#'
#' Selections of edges can be performed using
#' the following \code{select_...} functions:
#' \code{select_edges()},
#' \code{select_last_edge()}, or
#' \code{select_edges_by_node_id()}.
#' Selections of edges can also be performed using
#' the following traversal functions:
#' \code{trav_out_edge()}, \code{trav_in_edge()},
#' or \code{trav_both_edge()}.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @param edge_attr the name of the attribute to set.
#' @param value the value to be set for the chosen
#' attribute for the edges in the current selection.
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' # Create a simple graph
#' graph <-
#'   create_graph() %>%
#'   add_path(n = 6)
#'
#' # Select specific edges from
#' # the graph and apply the edge
#' # attribute `color = blue` to
#' # those selected edges
#' graph <-
#'   graph %>%
#'   select_nodes_by_id(nodes = 2:4) %>%
#'   trav_out_edge() %>%
#'   set_edge_attrs_ws(
#'     edge_attr = color,
#'     value = "blue")
#'
#' # Show the internal edge data
#' # frame to verify that the
#' # edge attribute has been set
#' # for specific edges
#' get_edge_df(graph)
#' #>   id from to  rel color
#' #> 1  1    1  2 <NA>  <NA>
#' #> 2  2    2  3 <NA>  blue
#' #> 3  3    3  4 <NA>  blue
#' #> 4  4    4  5 <NA>  blue
#' #> 5  5    5  6 <NA>  <NA>
#' @importFrom rlang enquo UQ
#' @export set_edge_attrs_ws

set_edge_attrs_ws <- function(graph,
                              edge_attr,
                              value) {

  # Get the time of function start
  time_function_start <- Sys.time()

  edge_attr <- rlang::enquo(edge_attr)

  edge_attr <- (rlang::UQ(edge_attr) %>% paste())[2]

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {
    stop("The graph object is not valid.")
  }

  # Validation: Graph contains edges
  if (graph_contains_edges(graph) == FALSE) {
    stop("The graph contains no edges, no edges attributes can be set.")
  }

  # Validation: Graph object has valid edge selection
  if (graph_contains_edge_selection(graph) == FALSE) {
    stop("There is no selection of edges available.")
  }

  # Get vectors of edge ID values for the
  # edge selection
  edge_ids <- graph$edge_selection$edge

  # Update the graph's internal edf
  if (edge_attr %in% colnames(graph$edges_df)) {

    graph$edges_df[
      which(graph$edges_df[, 1] %in% edge_ids),
      which(colnames(graph$edges_df) %in% edge_attr)] <- value

  } else {
    graph$edges_df <-
      graph$edges_df %>%
      dplyr::mutate(edge_attr__ = dplyr::case_when(
        id %in% edge_ids ~ value))

    colnames(graph$edges_df)[length(colnames(graph$edges_df))] <-
      edge_attr
  }

  # Update the `graph_log` df with an action
  graph$graph_log <-
    add_action_to_log(
      graph_log = graph$graph_log,
      version_id = nrow(graph$graph_log) + 1,
      function_used = "set_edge_attrs_ws",
      time_modified = time_function_start,
      duration = graph_function_duration(time_function_start),
      nodes = nrow(graph$nodes_df),
      edges = nrow(graph$edges_df))

  # Write graph backup if the option is set
  if (graph$graph_info$write_backups) {
    save_graph_as_rds(graph = graph)
  }

  graph
}
