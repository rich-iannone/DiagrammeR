#' Get a vector of node ID values
#' @description Obtain a vector of node ID values from a graph object or
#' a node data frame. An optional filter by node attribute can limit the set
#' of node ID values returned.
#' @param x either a graph object of class \code{dgr_graph} that is created
#' using \code{create_graph} or a node data frame.
#' @param node_attr an optional character vector of node attribute values for
#' filtering the node ID values returned.
#' @param match an option to provide a logical expression with a comparison
#' operator (\code{>}, \code{<}, \code{==}, or \code{!=}) followed by a number
#' for numerical filtering, or, a character string for filtering the edges
#' returned through string matching.
#' @return a vector of node ID values.
#' @examples
#' \dontrun{
#' # Before getting node ID values, create a simple graph
#' nodes <-
#'   create_nodes(nodes = c("a", "b", "c", "d"),
#'                type = "letter",
#'                color = c("red", "green", "grey", "blue"),
#'                value = c(3.5, 2.6, 9.4, 2.7))
#'
#' graph <-
#'   create_graph(nodes_df = nodes)
#'
#' # Get a vector of all nodes in a graph
#' get_nodes(graph)
#' #> [1] "a" "b" "c" "d"
#'
#' # Get a vector of node ID values from a node data frame
#' get_nodes(nodes)
#' #> [1] "a" "b" "c" "d"
#'
#' # Get a vector of node ID values using a numeric
#' # comparison (i.e., all nodes with 'value' attribute
#' # greater than 3)
#' get_nodes(graph,
#'           node_attr = "value",
#'           match = "> 3")
#' #> [1] "a" "c"
#'
#' # Get a vector of node ID values using a match
#' # pattern (i.e., all nodes with 'color' attribute
#' # of "green")
#' get_nodes(graph,
#'           node_attr = "color",
#'           match = "green")
#' #> [1] "b"
#' }
#' @export get_nodes

get_nodes <- function(x,
                      node_attr = NULL,
                      match = NULL){

  if (class(x) == "dgr_graph"){

    if (is_graph_empty(x)){

      node_ID <- NA

      return(node_ID)

    } else {

      nodes_df <- x$nodes_df
    }
  }

  if (class(x) == "data.frame"){

    if (colnames(x)[1] == "nodes"){

      nodes_df <- x
    }
  }

  if (!is.null(node_attr)){
    if (length(node_attr) > 1){
      stop("Only one node attribute can be specified.")
    }

    if (!(node_attr %in% colnames(nodes_df)[-1])){
      stop("The specified attribute is not available.")
    }
  }

  if (is.null(node_attr)){
    nodes <- nodes_df$nodes
  }

  if (!is.null(node_attr)){

    column_number <-
      which(colnames(nodes_df) %in% node_attr)

    # Filter using a logical expression
    if (!is.null(match)){

      if (grepl("^>.*", match) | grepl("^<.*", match) |
          grepl("^==.*", match) | grepl("^!=.*", match)){
        logical_expression <- TRUE } else {
          logical_expression <- FALSE
        }

      if (logical_expression){

        if (grepl("^>.*", match)){
          rows_where_true_le <-
            which(nodes_df[,column_number] >
                    as.numeric(gsub(">(.*)", "\\1", match)))
        }

        if (grepl("^<.*", match)){
          rows_where_true_le <-
            which(nodes_df[,column_number] <
                    as.numeric(gsub("<(.*)", "\\1", match)))
        }

        if (grepl("^==.*", match)){
          rows_where_true_le <-
            which(nodes_df[,column_number] ==
                    as.numeric(gsub("==(.*)", "\\1", match)))
        }

        if (grepl("^!=.*", match)){
          rows_where_true_le <-
            which(nodes_df[,column_number] !=
                    as.numeric(gsub("!=(.*)", "\\1", match)))
        }

        nodes <- nodes_df[rows_where_true_le, 1]
      }
    }

    # Filter using a `match` value
    if (logical_expression == FALSE){

      if (is.numeric(match)){
        match <- as.character(match)
      }

      rows_where_true_match <-
        which(match == as.character(nodes_df[,column_number]))

      nodes <- nodes_df[rows_where_true_match, 1]
    }
  }

  return(nodes)
}
