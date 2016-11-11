#' Select nodes in a graph
#' @description Select nodes from a graph object of
#' class \code{dgr_graph}.
#' @param graph a graph object of class
#' \code{dgr_graph} that is created using
#' \code{create_graph}.
#' @param node_attr an optional character vector of
#' node attribute values for filtering the node ID
#' values returned.
#' @param search an option to provide a logical
#' expression with a comparison operator (\code{>},
#' \code{<}, \code{==}, or \code{!=}) followed by a
#' number for numerical filtering, or, a regular
#' expression for filtering the nodes returned through
#' string matching.
#' @param set_op the set operation to perform upon
#' consecutive selections of graph nodes. This can
#' either be as a \code{union} (the default), as an
#' intersection of selections with \code{intersect},
#' or, as a \code{difference} on the previous
#' selection, if it exists.
#' @param nodes an optional vector of node IDs for
#' filtering list of nodes present in the graph.
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' # Create a node data frame (ndf)
#' ndf <-
#'   create_node_df(
#'     n = 4,
#'     type = c("a", "a", "z", "z"),
#'     label = TRUE,
#'     value = c(3.5, 2.6, 9.4, 2.7))
#'
#' # Create an edge data frame (edf)
#' edf <-
#'   create_edge_df(
#'     from = c(1, 2, 3),
#'     to = c(4, 3, 1),
#'     rel = c("a", "z", "a"))
#'
#' # Create a graph with the ndf and edf
#' graph <-
#'   create_graph(
#'     nodes_df = ndf,
#'     edges_df = edf)
#'
#' # Explicitly select nodes `1` and `3`
#' graph <-
#'   graph %>%
#'   select_nodes(nodes = c(1, 3))
#'
#' # Verify that the node selection has been made
#' # using the `get_selection()` function
#' get_selection(graph)
#' #> [1] 1 3
#'
#' # Select nodes based on the node `type`
#' # being `z`
#' graph <-
#'   graph %>%
#'   clear_selection() %>%
#'   select_nodes(node_attr = "type", search = "z")
#'
#' # Verify that an node selection has been made, and
#' # recall that the `3` and `4` nodes are of the
#' # `z` type
#' get_selection(graph)
#' #> [1] 3 4
#'
#' # Select edges based on the node value attribute
#' # being greater than 3.0 (first clearing the current
#' # selection of nodes)
#' graph <-
#'   graph %>%
#'   clear_selection() %>%
#'   select_nodes(node_attr = "value", search = ">3.0")
#'
#' # Verify that the correct node selection has been
#' # made; in this case, nodes `1` and `3` have values
#' # for `value` greater than 3.0
#' get_selection(graph)
#' #> [1] 1 3
#' @export select_nodes

select_nodes <- function(graph,
                         node_attr = NULL,
                         search = NULL,
                         set_op = "union",
                         nodes = NULL) {

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {
    stop("The graph object is not valid.")
  }

  # Validation: Graph contains nodes
  if (graph_contains_nodes(graph) == FALSE) {
    stop("The graph contains no nodes, so, no selections can be made.")
  }

  # Remove any selection of edges
  graph$selection$edges <- NULL

  # Remove `graph$selection` if empty
  if (length(graph$selection) == 0) {
    graph$selection <- NULL
  }

  # Extract the graph's internal ndf
  nodes_df <- graph$nodes_df

  if (!is.null(node_attr)) {
    if (length(node_attr) > 1) {
      stop("Only one node attribute can be specified.")
    }

    if (!(node_attr %in% colnames(nodes_df)[-1])) {
      stop("The specified attribute is not available.")
    }
  }

  if (is.null(node_attr)) {

    nodes_selected <- nodes_df[, 1]

    if (!is.null(nodes)) {
      if (any(!(nodes %in% nodes_selected))) {
        stop("One of more of the nodes specified are not available in the graph.")
      }
      nodes_selected <- nodes
    }
  }

  if (!is.null(node_attr)) {

    # Filter nodes_df by node ID values in `nodes`
    if (!is.null(nodes)) {
      if (any(!(nodes %in% nodes_df[, 1]))) {
        stop("One of more of the nodes specified are not available in the graph.")
      }

      nodes_df <- nodes_df[which(nodes_df[, 1] %in% nodes),]
    }

    # Determine the column number for which the value
    # for `node_attr` is available
    column_number <-
      which(colnames(nodes_df) %in% node_attr)

    # If a search term provided, filter using a logical
    # expression or a regex match
    if (!is.null(search)) {

      if (grepl("^>.*", search) | grepl("^<.*", search) |
          grepl("^==.*", search) | grepl("^!=.*", search)) {
        logical_expression <- TRUE } else {
          logical_expression <- FALSE
        }

      # Filter using a logical expression
      if (logical_expression) {

        if (grepl("^>.*", search)) {
          rows_where_true_le <-
            which(nodes_df[, column_number] >
                    as.numeric(gsub(">(.*)", "\\1", search)))
        }

        if (grepl("^<.*", search)) {
          rows_where_true_le <-
            which(nodes_df[, column_number] <
                    as.numeric(gsub("<(.*)", "\\1", search)))
        }

        if (grepl("^==.*", search)) {
          rows_where_true_le <-
            which(nodes_df[, column_number] ==
                    as.numeric(gsub("==(.*)", "\\1", search)))
        }

        if (grepl("^!=.*", search)) {
          rows_where_true_le <-
            which(nodes_df[, column_number] !=
                    as.numeric(gsub("!=(.*)", "\\1", search)))
        }

        nodes_selected <- nodes_df[rows_where_true_le, 1]
      }

      # Filter using a `search` value as a
      # regular expression
      if (logical_expression == FALSE) {

        rows_where_true_regex <-
          which(
            grepl(
              search, as.character(nodes_df[, column_number])))

        nodes_selected <- nodes_df[rows_where_true_regex, 1]
      }
    }
  }

  # Obtain vector of node IDs selection of nodes
  # already present
  if (!is.null(graph$selection)) {
    if (!is.null(graph$selection$nodes)) {
      nodes_prev_selection <- graph$selection$nodes
    }
  } else {
    nodes_prev_selection <- vector(mode = "integer")
  }

  # Incorporate the selected nodes into the
  # graph's selection
  if (set_op == "union") {
    nodes_combined <- union(nodes_prev_selection, nodes_selected)
  } else if (set_op == "intersect") {
    nodes_combined <- intersect(nodes_prev_selection, nodes_selected)
  } else if (set_op == "difference") {
    nodes_combined <- setdiff(nodes_prev_selection, nodes_selected)
  }

  graph$selection$nodes <- nodes_combined

  return(graph)
}
