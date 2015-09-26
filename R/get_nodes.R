#' Get vector of node IDs
#' @description Provides information on the node IDs from one or several node
#' data frames, edge data frames, or graph objects.
#' @param ... a collection of node data frames, edge data frames, or a single
#' graph object.
#' @return a vector of node ID values.
#' @examples
#' \dontrun{
#' # Before getting node ID values, create a simple graph
#' nodes <-
#'   create_nodes(nodes = LETTERS,
#'                label = TRUE,
#'                type = c(rep("a_to_g", 7),
#'                         rep("h_to_p", 9),
#'                         rep("q_to_x", 8),
#'                         rep("y_and_z",2)))
#'
#' edges <-
#'   create_edges(from = sample(LETTERS, replace = TRUE),
#'                to = sample(LETTERS, replace = TRUE),
#'                label = "edge",
#'                rel = "letter_to_letter")
#'
#' graph <-
#'   create_graph(nodes_df = nodes,
#'                edges_df = edges,
#'                graph_attrs = "layout = neato",
#'                node_attrs = c("fontname = Helvetica",
#'                               "shape = circle"))
#'
#' # Get a vector of all nodes in a graph
#' get_nodes(graph)
#' #> [1] "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L"
#' #> [13] "M" "N" "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X"
#' #> [25] "Y" "Z"
#'
#' # Get a vector of node ID values from a node data frame
#' get_nodes(nodes)
#' #> [1] "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L"
#' #> [13] "M" "N" "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X"
#' #> [25] "Y" "Z"
#'
#' # Get a vector of node ID values from an edge data frame
#' get_nodes(edges)
#' #> [1] "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L"
#' #> [13] "M" "N" "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X"
#' #> [25] "Y" "Z"
#' }
#' @export get_nodes

get_nodes <- function(...){

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

      if (any(c("nodes", "node", "node_ID") %in% colnames(object))){

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

      object <- object$nodes_df

      if ("node" %in% colnames(object)){

        nodes_column <- which("node" %in% colnames(object))

      } else if ("nodes" %in% colnames(object)){

        nodes_column <- which("nodes" %in% colnames(object))

      } else if ("node_id" %in% colnames(object)){

        nodes_column <- which("node_id" %in% colnames(object))

      } else {

        stop("There is no column with node ID information.")

      }

      node_ID <- c(node_ID, object[,nodes_column])
    }

    if (object_type == "node_df"){

      if ("node" %in% colnames(object)){

        nodes_column <- which("node" %in% colnames(object))

      } else if ("nodes" %in% colnames(object)){

        nodes_column <- which("nodes" %in% colnames(object))

      } else if ("node_id" %in% colnames(object)){

        nodes_column <- which("node_id" %in% colnames(object))

      } else {

        stop("There is no column with node ID information.")

      }

      node_ID <- c(node_ID, object[,nodes_column])
    }

    if (object_type == "edge_df"){

      both_from_to_columns <- all(c(any(c("from") %in%
                                          colnames(object))),
                                  any(c("to") %in%
                                        colnames(object)))

      if (exists("both_from_to_columns")){

        if (both_from_to_columns == TRUE){

          from_column <- which(colnames(object) %in% "from")[1]

          to_column <- which(colnames(object) %in% "to")[1]
        }
      }

      node_ID <- c(node_ID, unique(c(object[,from_column],
                                     object[,to_column])))
    }
  }

  all_ID_unique <- ifelse(anyDuplicated(node_ID) == 0, TRUE, FALSE)

  if (all_ID_unique == TRUE){

    return(node_ID)
  }
}
