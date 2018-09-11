#' Get edge attribute values
#'
#' From a graph object of class \code{dgr_graph}, get edge attribute values for
#'   one or more edges.
#' @inheritParams render_graph
#' @param edge_attr the name of the attribute for which to get values.
#' @param from an optional vector of node IDs from which the edge is outgoing
#'   for filtering the list of edges.
#' @param to an optional vector of node IDs from which the edge is incoming for
#'   filtering the list of edges.
#' @return a named vector of edge attribute values for the attribute given by
#'   \code{edge_attr} by edge.
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
#' # Get the values for the
#' # `value` edge attribute
#' graph %>%
#'   get_edge_attrs(
#'     edge_attr = value)
#'
#' # To only return edge attribute
#' # values for specified edges, use
#' # the `from` and `to` arguments
#' graph %>%
#'   get_edge_attrs(
#'     edge_attr = value,
#'     from = c(1, 2),
#'       to = c(2, 3))
#' @importFrom dplyr mutate filter pull
#' @import rlang
#' @export
get_edge_attrs <- function(graph,
                           edge_attr,
                           from = NULL,
                           to = NULL) {

  # Get the name of the function
  fcn_name <- get_calling_fcn()

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {

    emit_error(
      fcn_name = fcn_name,
      reasons = "The graph object is not valid")
  }

  edge_attr <- rlang::enquo(edge_attr)

  # Create binding for a specific variable
  from_to <- NULL

  if (rlang::enquo(edge_attr) %>%
      rlang::get_expr() %>%
      as.character() %in% c("id", "from", "to")) {

    emit_error(
      fcn_name = fcn_name,
      reasons = "This is not an edge attribute")
  }

  if (!is.null(from) & !is.null(to)) {
    if (length(from) != length(to)) {

      emit_error(
        fcn_name = fcn_name,
        reasons = "The number of nodes in `from` and `to` must be the same")
    }
  }

  # Extract the edge data frame (ndf)
  # from the graph
  edf <- graph$edges_df

  if (is.null(from) | is.null(to)) {

    # Extract the edge attribute values
    edge_attr_vals <-
      edf %>%
      dplyr::pull(rlang::UQ(edge_attr))

    # Extract the edge names
    edge_names <-
      paste(edf$from, edf$to, sep = "->")

    # Assign edge names
    names(edge_attr_vals) <- edge_names
  }

  if (!is.null(from) & !is.null(to)) {

    # Get edges as strings for filtering
    # the `edf` object
    edges <- paste(from, to, sep = "->")

    # Filter the edf by the supplied
    # edge definitions
    edf <-
      edf %>%
      dplyr::mutate(from_to = paste(from, to, sep = "->")) %>%
      dplyr::filter(from_to %in% edges)

    # Extract the edge attribute values
    edge_attr_vals <-
      edf %>%
      dplyr::pull(rlang::UQ(edge_attr))

    # Extract the edge names
    edge_names <-
      paste(edf$from, edf$to, sep = "->")

    # Assign edge names
    names(edge_attr_vals) <- edge_names
  }

  edge_attr_vals
}
