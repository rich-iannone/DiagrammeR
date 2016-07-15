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
#' library(magrittr)
#'
#' # Create a node data frame (ndf)
#' nodes <-
#'   create_nodes(
#'     nodes = c("a", "b", "c", "d"),
#'     type = c("A", "A", "Z", "Z"),
#'     label = TRUE,
#'     value = c(3.5, 2.6, 9.4, 2.7))
#'
#' # Create an edge data frame (edf)
#' edges <-
#'   create_edges(
#'     from = c("a", "b", "c"),
#'     to = c("d", "c", "a"),
#'     rel = c("A", "Z", "A"))
#'
#' # Create a graph with the ndf and edf
#' graph <-
#'   create_graph(nodes_df = nodes,
#'                edges_df = edges)
#'
#' # Explicitly select nodes `a` and `c`
#' graph <-
#'   graph %>%
#'   select_nodes(
#'     nodes = c("a", "c"))
#'
#' # Verify that the node selection has been made
#' # using the `get_selection()` function
#' get_selection(graph)
#' #> $nodes
#' #> [1] "a" "c"
#'
#' # Select nodes based on the node `type`
#' # being `Z`
#' graph <-
#'   graph %>%
#'   clear_selection %>%
#'   select_nodes(
#'     node_attr = "type",
#'     search = "Z")
#'
#' # Verify that an node selection has been made, and
#' # recall that the `c` and `d` nodes are of the
#' # `Z` type
#' get_selection(graph)
#' #> $nodes
#' #> [1] "c" "d"
#'
#' # Select edges based on the node value attribute
#' # being greater than 3.0 (first clearing the current
#' # selection of nodes)
#' graph <-
#'   graph %>%
#'   clear_selection %>%
#'   select_nodes(
#'     node_attr = "value",
#'     search = ">3.0")
#'
#' # Verify that the correct node selection has been
#' # made; in this case, nodes `a` and `c` have values
#' # for `value` greater than 3.0
#' get_selection(graph)
#' #> $nodes
#' #> [1] "a" "c"
#' @export select_nodes

select_nodes <- function(graph,
                         node_attr = NULL,
                         search = NULL,
                         set_op = "union",
                         nodes = NULL) {

  if (is_graph_empty(graph)) {
    stop("The graph is empty so no selections can be made.")
  }

  # Remove any selection of edges
  graph$selection$edges <- NULL

  # Remove `graph$selection` if empty
  if (length(graph$selection) == 0){
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

    nodes_selected <- nodes_df$nodes

    if (!is.null(nodes)) {
      if (any(!(nodes %in% nodes_selected))) {
        stop("One of more of the nodes specified are not available in the graph.")
      }
      nodes_selected <- nodes
    }
  }

  if (!is.null(node_attr)) {

    # Filter nodes_df by node ID values in 'nodes'
    if (!is.null(nodes)) {
      if (any(!(nodes %in% nodes_df$nodes))) {
        stop("One of more of the nodes specified are not available in the graph.")
      }

      nodes_df <- nodes_df[which(nodes_df$nodes %in% nodes),]
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
            which(nodes_df[,column_number] >
                    as.numeric(gsub(">(.*)", "\\1", search)))
        }

        if (grepl("^<.*", search)) {
          rows_where_true_le <-
            which(nodes_df[,column_number] <
                    as.numeric(gsub("<(.*)", "\\1", search)))
        }

        if (grepl("^==.*", search)) {
          rows_where_true_le <-
            which(nodes_df[,column_number] ==
                    as.numeric(gsub("==(.*)", "\\1", search)))
        }

        if (grepl("^!=.*", search)) {
          rows_where_true_le <-
            which(nodes_df[,column_number] !=
                    as.numeric(gsub("!=(.*)", "\\1", search)))
        }

        nodes_selected <- nodes_df[rows_where_true_le, 1]
      }

      # Filter using a `search` value as a
      # regular expression
      if (logical_expression == FALSE) {

        rows_where_true_regex <-
          which(grepl(search, as.character(nodes_df[,column_number])))

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
    nodes_prev_selection <- vector(mode = "character")
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
