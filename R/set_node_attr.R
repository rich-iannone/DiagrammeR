#' Set node attributes
#' @description From a graph object of class \code{dgr_graph} or a node
#' data frame, set node attribute properties for one or more nodes.
#' @param x either a graph object of class \code{dgr_graph} that is created
#' using \code{create_graph}, or a node data frame.
#' @param nodes an optional vector of node IDs for filtering list of
#' nodes present in the graph.
#' @param node_attr the name of the attribute to set.
#' @param values the values to be set for the chosen attribute for the
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
#'                 node_attr = "color", values = "green")
#'
#' # Set attribute 'color = "green"' for nodes "a" and "c" using
#' # the node data frame
#' nodes <-
#'   set_node_attr(x = nodes, nodes = c("a", "c"),
#'                 node_attr = "color", values = "green")
#'
#' #' # Set attribute 'color = "blue"' for all nodes
#' # the node data frame
#' nodes <-
#'   set_node_attr(x = nodes, node_attr = "color", values = "blue")
#' }
#' @export set_node_attr

set_node_attr <- function(x,
                          nodes = NULL,
                          node_attr,
                          values){

  if (node_attr == "nodes"){
    stop("You cannot change the node ID.")
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

  if (length(values) != 1 & length(values) != nrow(nodes_df)){
    stop("The length of values provided must either be 1 or that of the number of rows in the ndf.")
  }

  if (length(values) == 1){

    if (node_attr %in% colnames(nodes_df)){

      if (is.null(nodes)){

        nodes_df[, which(colnames(nodes_df) %in% node_attr)] <- values

      } else {

        nodes_df[which(nodes_df$nodes %in% nodes),
                 which(colnames(nodes_df) %in% node_attr)] <- values
      }
    }

    if (!(node_attr %in% colnames(nodes_df))){

      nodes_df <- cbind(nodes_df, rep("", nrow(nodes_df)))

      nodes_df[, ncol(nodes_df)] <- as.character(nodes_df[,ncol(nodes_df)])

      colnames(nodes_df)[ncol(nodes_df)] <- node_attr

      if (is.null(nodes)){

        nodes_df[, ncol(nodes_df)] <- values

      } else {

        nodes_df[which(nodes_df$nodes %in% nodes),ncol(nodes_df)] <- values
      }
    }
  }

  if (length(values) == nrow(nodes_df)){

    if (length(values) == nrow(nodes_df)){

      if (node_attr %in% colnames(nodes_df)){

        nodes_df[, which(colnames(nodes_df) %in% node_attr)] <- values
      }

      if (!(node_attr %in% colnames(nodes_df))){

        nodes_df <- cbind(nodes_df, rep("", nrow(nodes_df)))

        nodes_df[, ncol(nodes_df)] <- as.character(nodes_df[,ncol(nodes_df)])

        colnames(nodes_df)[ncol(nodes_df)] <- node_attr

        nodes_df[, ncol(nodes_df)] <- values
      }
    }
  }

  if (object_type == "dgr_graph"){

    # Create new graph object
    dgr_graph <-
      create_graph(nodes_df = nodes_df,
                   edges_df = x$edges_df,
                   graph_attrs = x$graph_attrs,
                   node_attrs = x$node_attrs,
                   edge_attrs = x$edge_attrs,
                   directed = ifelse(is_graph_directed(x),
                                     TRUE, FALSE),
                   graph_name = x$graph_name,
                   graph_time = x$graph_time,
                   graph_tz = x$graph_tz)

    return(dgr_graph)
  }

  if (object_type == "node_df"){
    return(nodes_df)
  }
}
