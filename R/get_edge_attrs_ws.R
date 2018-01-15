#' Get edge attribute values
#' @description From a graph object of class
#' \code{dgr_graph}, get edge attribute values for one
#' or more edges.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @param edge_attr the name of the attribute for which
#' to get values.
#' @return a named vector of edge attribute values for
#' the attribute given by \code{edge_attr} by edge.
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
#' @importFrom dplyr filter pull
#' @importFrom rlang enquo UQ
#' @export get_edge_attrs_ws

get_edge_attrs_ws <- function(graph,
                              edge_attr) {

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {

    stop(
      "The graph object is not valid.",
      call. = FALSE)
  }

  # Validation: Graph object has a valid edge selection
  if (graph_contains_edge_selection(graph) == FALSE) {

    stop(
      "There is no selection of edges available.",
      call. = FALSE)
  }

  edge_attr <- rlang::enquo(edge_attr)

  # Create binding for a specific variable
  id <- NULL

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
  edge_attr_vals <-
    edf %>%
    dplyr::pull(rlang::UQ(edge_attr))

  # Extract the edge names
  edge_names <-
    paste(edf$from, edf$to, sep = "->")

  # Assign edge names
  names(edge_attr_vals) <- edge_names

  edge_attr_vals
}
