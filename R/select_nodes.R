#' Select nodes
#' @description Select nodes from a graph object of class \code{dgr_graph}.
#' @param graph a graph object of class \code{dgr_graph} that is created
#' using \code{create_graph}.
#' @param nodes an optional vector of node IDs for filtering list of
#' nodes present in the graph.
#' @param node_attr an optional character vector of node attribute values for
#' filtering the node ID values returned.
#' @param comparison an optional logical expression for filtering the nodes
#' returned.
#' @param regex an optional regular expression (regex) for filtering the
#' nodes returned.
#' @param set_op the set operation to perform upon consecutive selections
#' of graph nodes. This can either be as a \code{union} (the default), as an
#' \code{intersection}, or, as a \code{difference} on the previous selection,
#' if it exists.
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' \dontrun{
#' # Create a simple graph
#' nodes <-
#'   create_nodes(nodes = c("a", "b", "c", "d"),
#'                type = "letter",
#'                label = TRUE,
#'                value = c(3.5, 2.6, 9.4, 2.7))
#'
#' edges <-
#'   create_edges(from = c("a", "b", "c"),
#'                to = c("d", "c", "a"),
#'                rel = "leading_to")
#'
#' graph <-
#'   create_graph(nodes_df = nodes,
#'                edges_df = edges)
#'
#' # Select nodes "a" and "c"
#' graph <- select_nodes(graph = graph, nodes = c("a", "c"))
#'
#' # Verify that a node selection has been made
#' get_selection(graph)
#' #> $nodes
#' #> [1] "a" "c"
#' }
#' @export select_nodes

select_nodes <- function(graph,
                         nodes = NULL,
                         node_attr = NULL,
                         comparison = NULL,
                         regex = NULL,
                         set_op = "union"){

  if (!is.null(comparison) & !is.null(regex)){
    stop("A comparison and a regex pattern cannot be used together.")
  }

  if (is_graph_empty(graph)){
    stop("The graph is empty so no selections can be made.")
  }

  nodes_df <- graph$nodes_df

  if (!is.null(node_attr)){
    if (length(node_attr) > 1){
      stop("Only one node attribute can be specified.")
    }

    if (!(node_attr %in% colnames(nodes_df)[-1])){
      stop("The specified attribute is not availalbe")
    }
  }

  if (is.null(node_attr)){

    nodes_selected <- nodes_df$nodes

    if (!is.null(nodes)){

      if (any(!(nodes %in% nodes_selected))){
        stop("One of more of the nodes specified are not available in the graph.")
      }

      nodes_selected <- nodes
    }
  }

  if (!is.null(node_attr)){

    # Filter nodes_df by node ID values in 'nodes'
    if (!is.null(nodes)){

      if (any(!(nodes %in% nodes_df$nodes))){
        stop("One of more of the nodes specified are not available in the graph.")
      }

      nodes_df <- nodes_df[which(nodes_df$nodes %in% nodes),]
    }

    # Determine the column number for which the 'node_attr' is available
    column_number <-
      which(colnames(nodes_df) %in% node_attr)

    # Filter using a logical expression
    if (!is.null(comparison) & is.null(regex)){

      if (grepl("^>.*", comparison)){
        rows_where_true_le <-
          which(nodes_df[,column_number] >
                  as.numeric(gsub(">(.*)", "\\1", comparison)))
      }

      if (grepl("^<.*", comparison)){
        rows_where_true_le <-
          which(nodes_df[,column_number] <
                  as.numeric(gsub("<(.*)", "\\1", comparison)))
      }

      if (grepl("^==.*", comparison)){
        rows_where_true_le <-
          which(nodes_df[,column_number] ==
                  as.numeric(gsub("==(.*)", "\\1", comparison)))
      }

      if (grepl("^!=.*", comparison)){
        rows_where_true_le <-
          which(nodes_df[,column_number] !=
                  as.numeric(gsub("!=(.*)", "\\1", comparison)))
      }

      nodes_selected <- nodes_df[rows_where_true_le, 1]
    }

    # Filter using a regex
    if (is.null(comparison) & !is.null(regex)){

      rows_where_true_regex <-
        which(grepl(regex, as.character(nodes_df[,column_number])))

      nodes_selected <- nodes_df[rows_where_true_regex, 1]
    }
  }

  # Obtain vector of node IDs selection of nodes already present
  if (!is.null(graph$selection)){
    if (!is.null(graph$selection$nodes)){
      nodes_prev_selection <- graph$selection$nodes
    }
  } else {
    nodes_prev_selection <- vector(mode = "character")
  }

  # Incorporate selected nodes into graph's selection section
  if (set_op == "union"){
    nodes_combined <- union(nodes_prev_selection, nodes_selected)
  } else if (set_op == "intersect"){
    nodes_combined <- intersect(nodes_prev_selection, nodes_selected)
  } else if (set_op == "difference"){
    nodes_combined <- setdiff(nodes_prev_selection, nodes_selected)
  }

  graph$selection$nodes <- nodes_combined

  return(graph)
}
