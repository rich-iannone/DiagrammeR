#' Get node attributes
#' @description From a graph object of class \code{dgr_graph} or a node
#' data frame, get node attribute properties for one or more nodes.
#' @param x either a graph object of class \code{dgr_graph} that is created
#' using \code{create_graph}, or a node data frame.
#' @param nodes an optional vector of node IDs for filtering list of
#' nodes present in the graph.
#' @return a node data frame.
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
#'                rel = "leading_to",
#'                color = c("pink", "blue", "red"))
#'
#' graph <-
#'   create_graph(nodes_df = nodes,
#'                edges_df = edges)
#'
#' # Get node attributes for all nodes in the graph
#' get_node_attr(x = graph)
#'
#' # Get node attributes for nodes "a" and "c" in the graph
#' get_node_attr(x = graph, nodes = c("a", "c"))
#' }
#' @export get_node_attr

get_node_attr <- function(x,
                          node_attr = NULL,
                          mode = NULL,
                          nodes = NULL,){

  if (class(x) == "dgr_graph"){

    object_type <- "dgr_graph"

    nodes_df <- x$nodes_df
  }

  if (class(x) == "data.frame"){

    if ("nodes" %in% colnames(x)){

      object_type <- "node_df"
      nodes_df <- x
    }
  }

  if (is.null(nodes) == TRUE){

    nodes_df <- nodes_df

  } else {

    nodes_df <-
      nodes_df[which(nodes_df$nodes %in% nodes),]
  }

  return(nodes_df)
}
