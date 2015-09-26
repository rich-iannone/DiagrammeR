#' Determine whether a specified edge is present in an existing graph object
#' @description From a graph object of class \code{dgr_graph}, determine
#' whether a directed edge (defined by a pair of node IDs extant in the graph)
#' is present.
#' @param graph a graph object of class \code{dgr_graph} that is created using
#' \code{create_graph}.
#' @param from a node ID from which the edge to be queried is outgoing.
#' @param to a node ID to which the edge to be queried is incoming.
#' @return a logical value.
#' @examples
#' \dontrun{
#' # Before finding out whether a particular edge is present,
#' # create a simple graph
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
#' # Is there any edge between nodes with IDs 'A' and 'B'?
#' edge_present(graph, from = "A", to = "B")
#' #> FALSE
#'
#' # Verify that there is an edge between nodes 'K' and 'V'
#' edge_present(graph, from = "K", to = "V")
#' #> TRUE
#' }
#' @export edge_present

edge_present <- function(graph,
                         from,
                         to){

  # Verify that each of the values for 'from' and 'to' are given
  # as a single value
  from_is_single_value <- ifelse(length(from) == 1, TRUE, FALSE)
  to_is_single_value <- ifelse(length(to) == 1, TRUE, FALSE)

  # Stop function if either node is not a single value
  if (from_is_single_value == FALSE | to_is_single_value == FALSE){

    stop("Only single nodes for 'from' and 'to' should be specified.")
  }

  # Determine whether pair of nodes provided are in the graph
  if (from_is_single_value == TRUE & to_is_single_value == TRUE){

    nodes_available_in_graph <-
      ifelse(all(c(from, to) %in% get_nodes(graph)), TRUE, FALSE)
  }

  # Stop function if both nodes not present in graph
  if (nodes_available_in_graph == FALSE){

    stop("The nodes specified are not both present in the graph.")
  }

  # Determine whether a matching edge is available in the graph
  if (nodes_available_in_graph){

    edge_is_in_graph <- ifelse(any(graph$edges_df$from == from &
                                     graph$edges_df$to == to),
                               TRUE, FALSE)

    return(edge_is_in_graph)
  }
}
