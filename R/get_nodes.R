#' Get vector of node IDs
#' @description Provides information on the node IDs from one or several node
#' data frames, edge data frames, or graph objects.
#' @param ... a collection of node data frames, edge data frames, or a single
#' graph object.
#' @param type an optional character vector of \code{type} values to filter the
#' node ID values returned.
#' @return a vector of node ID values.
#' @examples
#' \dontrun{
#' # Before getting node ID values, create a simple graph
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
#' # Get a vector of all nodes in a graph
#' get_nodes(graph)
#' #> [1] "a" "b" "c" "d"
#'
#' # Get a vector of node ID values from a node data frame
#' get_nodes(nodes)
#' #> [1] "a" "b" "c" "d"
#'
#' # Get a vector of node ID values from an edge data frame
#' get_nodes(edges)
#' #> [1] "a" "b" "c" "d"
#' }
#' @export get_nodes

get_nodes <- function(...,
                      type = NULL){

  objects <- list(...)

  # Determine the length of the 'objects' list
  length_of_objects <- length(objects)

  # If there is more than one object supplied, check for existance
  # of a graph object
  if (length_of_objects > 1){

    # Determine the classes of the first two objects
    class_object_1 <- class(objects[[1]])
    class_object_2 <- class(objects[[2]])

    if (any("dgr_graph" %in% c(class_object_1, class_object_2))){

      stop("Only a single graph can be supplied.")
    }
  }

  for (i in 1:length(objects)){

    if (i == 1) node_ID <- vector(mode = "character")

    object <- objects[[i]]

    if (class(object) == "dgr_graph"){

      object_type <- "dgr_graph"
    }

    if (class(object) == "data.frame"){

      if ("nodes" %in% colnames(object)){

        object_type <- "node_df"
      }

      if (any(c("from", "to") %in% colnames(object))){

        object_type <- "edge_df"
      }
    }

    if (object_type == "dgr_graph"){

      if (is_graph_empty(object)){

        node_ID <- NA

        return(node_ID)
      }

      if (is.null(type)){
        node_ID <- c(node_ID, object$nodes_df$nodes)
      }

      if (!is.null(type)){
        node_ID <-
          c(node_ID,
            object$nodes_df$nodes[which(object$nodes_df$type %in% type)])
      }
    }

    if (object_type == "node_df"){

      if (is.null(type)){
        node_ID <- c(node_ID, object$nodes)
      }

      if (!is.null(type)){
        node_ID <-
          c(node_ID,
            object$nodes[which(object$type %in% type)])
      }

    }

    if (object_type == "edge_df"){

      node_ID <- c(node_ID, unique(c(object$from,
                                     object$to)))
    }
  }

  all_ID_unique <- ifelse(anyDuplicated(node_ID) == 0, TRUE, FALSE)

  if (all_ID_unique == TRUE){

    return(node_ID)
  }
}
