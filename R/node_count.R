#' Get count of all nodes or certain types of nodes
#' @description From a graph object of class \code{dgr_graph}, get a count of
#' nodes in the graph and optionally obtain a count of nodes by their type.
#' @param graph a graph object of class \code{dgr_graph} that is created using
#' \code{create_graph}.
#' @param type either a logical value, where \code{TRUE} provides a named vector
#' of node count by type and \code{FALSE} (the default) provides a total count,
#' or, a character vector of \code{type} values to filter the node count.
#' @return a numeric vector of single length.
#' @examples
#' \dontrun{
#' # Before getting counts of nodes, create a simple graph
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
#'                rel = "letter_to_letter")
#'
#' graph <-
#'   create_graph(nodes_df = nodes,
#'                edges_df = edges,
#'                graph_attrs = "layout = neato",
#'                node_attrs = c("fontname = Helvetica",
#'                               "shape = circle"))
#'
#' # Get counts of nodes grouped by the "type" attribute
#' node_count(graph, type = TRUE) # the default
#' #> a_to_g  h_to_p  q_to_x y_and_z
#' #>      7       9       8       2
#'
#' # Get a total count of nodes with no grouping
#' node_count(graph, type = FALSE)
#' #> [1] 26
#'
#' # Get a count of nodes of one or more specified types
#' node_count(graph, type = "a_to_g")
#' #> [1] 7
#'
#' node_count(graph, type = c("a_to_g", "q_to_x"))
#' #> [1] 15
#' }
#' @export node_count

node_count <- function(graph,
                       type = FALSE){

  # If graph is empty, return 0
  if (is_graph_empty(graph) == TRUE){

    return(0)
  }

  # If type is a string, get a count of nodes for a specific type
  if (class(type) == "character"){

    count_of_type <-
      length(which(graph$nodes_df$type %in% type))

    return(count_of_type)
  }

  # If type is FALSE, get a total count of nodes
  if (all(class(type) == "logical" & type == FALSE)){

    total_node_count <- length(graph$nodes_df$nodes)

    return(total_node_count)
  }

  # If type set to TRUE, get a named vector of counts by type
  if (all(class(type) == "logical" & type == TRUE)){

    for (i in 1:length(get_nodes(graph))){

      if (i == 1){
        all_nodes <- get_nodes(graph)
        all_types <- vector(mode = "character")
      }

      all_types <- c(all_types,
                     node_type(graph = graph,
                               all_nodes[i],
                               action = "read"))
      all_types <- unique(all_types)

      if (any(is.na(all_types))){

        all_types[which(is.na(all_types))] <- ""
      }
    }

    for (i in 1:length(all_types)){

      if (i == 1) total_node_count <- vector(mode = "numeric")

      total_node_count <-
        c(total_node_count,
          nrow(graph$nodes_df[which(graph$nodes_df$type == all_types[i]),]))

      if (i == length(all_types)){
        names(total_node_count) <- all_types

        if (any(names(total_node_count) == "")){
          names(total_node_count)[which(names(total_node_count) == "")] <- "<no type>"

          total_node_count <-
            c(total_node_count[which(names(total_node_count) == "<no type>")],
              total_node_count[-which(names(total_node_count) == "<no type>")])
        }
      }
    }

    return(total_node_count)
  }
}
