#' Get edge attribute values
#' @description From a graph object of class
#' \code{dgr_graph} or an edge data frame, get edge
#' attribute values for one or more edges.
#' @param x either a graph object of class
#' \code{dgr_graph} that is created using
#' \code{create_graph}, or an edge data frame.
#' @param edge_attr the name of the attribute for which
#' to get values.
#' @param from an optional vector of node IDs from
#' which the edge is outgoing for filtering the list
#' of edges.
#' @param to an optional vector of node IDs from which
#' the edge is incoming for filtering the list of
#' edges.
#' @return a named vector of edge attribute values for
#' the attribute given by \code{edge_attr} by edge.
#' @examples
#' # Create a simple graph where edges have an edge
#' # attribute named `value`
#' graph <-
#'   create_graph() %>%
#'   add_n_nodes(4) %>%
#'   {
#'     edges <-
#'       create_edge_df(
#'         from = c(1, 2, 1, 4),
#'         to = c(2, 3, 4, 3),
#'         rel = "rel")
#'     add_edge_df(., edges)
#'   } %>%
#'   set_edge_attrs(
#'     "value", 1.6, 1, 2) %>%
#'   set_edge_attrs(
#'     "value", 4.3, 1, 4) %>%
#'   set_edge_attrs(
#'     "value", 2.9, 2, 3) %>%
#'   set_edge_attrs(
#'     "value", 8.4, 4, 3)
#'
#' # Get the values for the `value` edge attribute
#' graph %>% get_edge_attrs(edge_attr = "value")
#' #> 1->2 2->3 1->4 4->3
#' #>  1.6  2.9  4.3  8.4
#'
#' # To only return edge attribute values for specified
#' # edges, use the `from` and `to` arguments
#' graph %>% get_edge_attrs("value", c(1, 2), c(2, 3))
#' #> 1->2 2->3
#' #>  1.6  2.9
#' @export get_edge_attrs

get_edge_attrs <- function(x,
                           edge_attr,
                           from = NULL,
                           to = NULL) {

  if (edge_attr == "from" | edge_attr == "to") {
    stop("This is not an edge attribute.")
  }

  if (!is.null(from) & !is.null(to)) {
    if (length(from) != length(to)) {
      stop("The number of nodes 'from' and 'to' must be the same.")
    }
  }

  if (inherits(x, "dgr_graph")) {
    object_type <- "dgr_graph"
    edges_df <- x$edges_df
  }

  if (inherits(x, "data.frame")) {
    if (all(c("from", "to") %in% colnames(x))) {
      object_type <- "edge_df"
      edges_df <- x
    }
  }

  if (is.null(from) | is.null(to)) {

    # Extract the edge attribute values
    edge_attr_vals <-
      edges_df[, which(colnames(edges_df) == edge_attr)]

    # Extract the edge names
    edge_names <-
      paste(edges_df$from, edges_df$to, sep = "->")

    # Assign edge names
    names(edge_attr_vals) <- edge_names
  }

  if (!is.null(from) & !is.null(to)) {

    edges_df$from_to <-
      paste(edges_df$from, edges_df$to, sep = "^^")
    edges <- paste(from, to, sep = "^^")

    # Extract the edge attribute values
    edge_attr_vals <-
      edges_df[
        which(edges_df[
          , which(colnames(edges_df) ==
                    "from_to")] %in% edges),
        which(colnames(edges_df) == edge_attr)]

    # Extract the edge names
    edge_names <-
      paste(edges_df[
        which(edges_df[
          , which(colnames(edges_df) ==
                    "from_to")] %in% edges),
        2],
        edges_df[
          which(edges_df[
            , which(colnames(edges_df) ==
                      "from_to")] %in% edges),
          3], sep = "->")

    # Assign edge names
    names(edge_attr_vals) <- edge_names
  }

  return(edge_attr_vals)
}
