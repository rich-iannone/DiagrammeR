#' Set node attributes
#' @description From a graph object of class \code{dgr_graph} or a node
#' data frame, set node attribute properties for one or more nodes.
#' @param x either a graph object of class \code{dgr_graph} that is created
#' using \code{create_graph}, or a node data frame.
#' @param node either a vector of node IDs, or, the string "*" to select
#' all nodes in the graph or node data frame.
#' @param attr the name of the attribute to set.
#' @param value the value to be set for the chosen attribute for the
#' chosen nodes.
#' @return either a graph object of class \code{dgr_graph} or a node
#' data frame, depending on what type of object was supplied to \code{x}.
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
#' # Set attribute 'color = "green"' for nodes "a" and "c" using
#' # the graph object
#' graph <-
#'   set_node_attr(x = graph, nodes = c("a", "c"),
#'                 attr = "color", value = "green")
#'
#' # Set attribute 'color = "green"' for nodes "a" and "c" using
#' # the node data frame
#' nodes <-
#'   set_node_attr(x = nodes, nodes = c("a", "c"),
#'                 attr = "color", value = "green")
#'
#' #' # Set attribute 'color = "blue"' for all nodes
#' # the node data frame
#' nodes <-
#'   set_node_attr(x = nodes, nodes = "*",
#'                 attr = "color", value = "blue")
#' }
#' @export set_node_attr

set_node_attr <- function(x,
                          nodes = NULL,
                          attr,
                          value){

  if (attr == "nodes"){
    stop("You cannot change the node ID.")
  }

  if (length(value) > 1){
    stop("Only one value should be provided.")
  }

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

  if (attr %in% colnames(nodes_df)){

    if (length(nodes) == 1 & nodes[1] == "*"){

      nodes_df[,which(colnames(nodes_df) %in% attr)] <- value

    } else {

      nodes_df[which(nodes_df$nodes %in% nodes),
               which(colnames(nodes_df) %in% attr)] <- value
    }
  }

  if (!(attr %in% colnames(nodes_df))){

    nodes_df <- cbind(nodes_df, rep("", nrow(nodes_df)))

    nodes_df[,ncol(nodes_df)] <- as.character(nodes_df[,ncol(nodes_df)])

    colnames(nodes_df)[ncol(nodes_df)] <- attr

    if (length(nodes) == 1 & nodes[1] == "*"){

      nodes_df[,ncol(nodes_df)] <- value

    } else {

      nodes_df[which(nodes_df$nodes %in% nodes),ncol(nodes_df)] <- value
    }
  }

  if (object_type == "dgr_graph"){

    # Create new graph object
    dgr_graph <-
      create_graph(nodes_df = nodes_df,
                   edges_df = graph$edges_df,
                   graph_attrs = graph$graph_attrs,
                   node_attrs = graph$node_attrs,
                   edge_attrs = graph$edge_attrs,
                   directed = ifelse(is_graph_directed(graph),
                                     TRUE, FALSE),
                   graph_name = graph$graph_name,
                   graph_time = graph$graph_time,
                   graph_tz = graph$graph_tz)

    return(dgr_graph)
  }

  if (object_type == "node_df"){
    return(nodes_df)
  }
}
