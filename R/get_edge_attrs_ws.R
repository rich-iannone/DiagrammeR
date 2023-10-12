#' Get edge attribute values from a selection of edges
#'
#' @description
#'
#' From a graph object of class `dgr_graph`, get edge attribute values for one
#' or more edges.
#'
#' This function makes use of an active selection of edges (and the function
#' ending with `_ws` hints at this).
#'
#' Selections of edges can be performed using the following selection
#' (`select_*()`) functions: [select_edges()], [select_last_edges_created()],
#' [select_edges_by_edge_id()], or [select_edges_by_node_id()].
#'
#' Selections of edges can also be performed using the following traversal
#' (`trav_*()`) functions: [trav_out_edge()], [trav_in_edge()],
#' [trav_both_edge()], or [trav_reverse_edge()].
#'
#' @inheritParams render_graph
#' @param edge_attr the name of the attribute for which to get values.
#'
#' @return A named vector of edge attribute values for the attribute given by
#'   `edge_attr` by edge.
#'
#' @examples
#' # Create a simple graph where
#' # edges have an edge attribute
#' # named `value`
#' graph <-
#'   create_graph() %>%
#'   add_n_nodes(n = 4) %>%
#'   {
#'     edges <-
#'       create_edge_df(
#'         from = c(1, 2, 1, 4),
#'           to = c(2, 3, 4, 3),
#'          rel = "rel")
#'     add_edge_df(
#'       graph = .,
#'       edge_df = edges)
#'   } %>%
#'   set_edge_attrs(
#'     edge_attr = value,
#'     values = 1.6,
#'     from = 1,
#'       to = 2) %>%
#'   set_edge_attrs(
#'     edge_attr = value,
#'     values = 4.3,
#'     from = 1,
#'       to = 4) %>%
#'   set_edge_attrs(
#'     edge_attr = value,
#'     values = 2.9,
#'     from = 2,
#'       to = 3) %>%
#'   set_edge_attrs(
#'     edge_attr = value,
#'     values = 8.4,
#'     from = 4,
#'       to = 3)
#'
#' # Select the edges defined as
#' # `1`->`3` and `2`->`3`
#' graph <-
#'   graph %>%
#'   select_edges(
#'     from = c(1, 2),
#'     to = c(2, 3))
#'
#' # Get the edge attribute values
#' # for the `value` attribute, limited
#' # to the current edge selection
#' graph %>%
#'   get_edge_attrs_ws(
#'     edge_attr = value)
#'
#' @export
get_edge_attrs_ws <- function(
    graph,
    edge_attr
) {

  # Get the name of the function
  fcn_name <- get_calling_fcn()

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {

    emit_error(
      fcn_name = fcn_name,
      reasons = "The graph is not valid.")
  }

  # Validation: Graph object has a valid edge selection
  if (graph_contains_edge_selection(graph) == FALSE) {

    emit_error(
      fcn_name = fcn_name,
      reasons = "There is no selection of edges available.")
  }

  edge_attr <- rlang::enquo(edge_attr)

  if (rlang::enquo(edge_attr) %>%
      rlang::get_expr() %>%
      as.character() %in% c("id", "from", "to")) {

    emit_error(
      fcn_name = fcn_name,
      reasons = "This is not an edge attribute")
  }

  # Extract the edge data frame (edf)
  # from the graph
  edf <- graph$edges_df

  # Get the edge IDs from the edge selection
  edges <- sort(graph$edge_selection$edge)

  # Filter the edf by the supplied
  # edge ID values
  edf <-
    edf %>%
    dplyr::filter(id %in% edges)

  # Extract the edge attribute values
  edge_attr_vals <- edf %>% dplyr::pull(!!edge_attr)

  # Extract the edge names
  edge_names <- paste(edf$from, edf$to, sep = "->")

  # Assign edge names
  names(edge_attr_vals) <- edge_names

  edge_attr_vals
}
