#' Add one or several unconnected nodes to the graph
#' @description Add n new nodes to a graph object of
#' class \code{dgr_graph}. Optionally, set node
#' \code{type} values for the new nodes.
#' @param graph a graph object of class
#' \code{dgr_graph} that is created using
#' \code{create_graph}.
#' @param n the number of new nodes to add to the graph.
#' @param set_node_type an optional string to apply a
#' \code{type} attribute to all newly created nodes.
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' library(magrittr)
#'
#' # Create an empty graph and add 5 nodes to it; these
#' # nodes will be given ID values from 1 to 5
#' graph <-
#'   create_graph() %>%
#'   add_n_nodes(5)
#'
#' # Get the graph's nodes
#' graph %>% get_nodes
#' #> [1] "1" "2" "3" "4" "5"
#' @export add_n_nodes

add_n_nodes <- function(graph,
                        n,
                        set_node_type = NULL) {

  if (node_count(graph) == 0){
    node <- 1
  }

  if (node_count(graph) > 0){
    if (!is.na(suppressWarnings(any(as.numeric(get_nodes(graph)))))){

      numeric_components <-
        suppressWarnings(which(!is.na(as.numeric(get_nodes(graph)))))

      node <-
        max(as.integer(as.numeric(get_nodes(graph)[numeric_components]))) + 1
    }

    if (suppressWarnings(all(is.na(as.numeric(get_nodes(graph)))))){
      node <- 1
    }
  }

  if (!is.null(set_node_type)) {
    new_nodes <-
      create_nodes(nodes = seq(node, node + n - 1, 1),
                   label = FALSE,
                   type = set_node_type)
  } else {
    new_nodes <-
      create_nodes(nodes = seq(node, node + n - 1, 1),
                   label = FALSE)
  }

  graph <-
    add_node_df(graph, new_nodes)

  return(graph)
}
