#' Get a vector of node ID values
#' @description Obtain a vector of node ID values from a graph object or
#' a node data frame, optionally using a logical expression or regular
#' expression for filtering.
#' @param x either a graph object of class \code{dgr_graph} that is created
#' using \code{create_graph} or a node data frame.
#' @param node_attr an optional character vector of node attribute values for
#' filtering the node ID values returned.
#' @param comparison an optional logical expression for filtering the nodes
#' returned.
#' @param regex an optional regular expression (regex) for filtering the
#' nodes returned.
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
#' get_nodes(graph, node_attr = "value",
#'           comparison = "> 3")
#' #> [1] "a" "c"
#'
#' # Get a vector of node ID values using a regex
#' # pattern (i.e., all nodes with 'color' attribute
#' # that begins with "gr")
#' get_nodes(graph, node_attr = "color",
#'           regex = "^gr")
#' #> [1] "b" "c"
#' }
#' @export get_nodes

get_nodes <- function(x,
                      node_attr = NULL,
                      comparison = NULL,
                      regex = NULL){

  if (!is.null(comparison) & !is.null(regex)){
    stop("A comparison and a regex pattern cannot be used together.")
  }

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

      nodes <- nodes_df[rows_where_true_le, 1]
    }

    # Filter using a regex
    if (is.null(comparison) & !is.null(regex)){

      rows_where_true_regex <-
        which(grepl(regex, as.character(nodes_df[,column_number])))

      nodes <- nodes_df[rows_where_true_regex, 1]
    }
  }

  return(nodes)
}
