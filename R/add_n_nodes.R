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
#' \dontrun{
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
#' }
#' @export add_n_nodes

add_n_nodes <- function(graph,
                        n,
                        set_node_type = NULL) {

  # Create `n` new nodes in the graph
  for (i in 1:n){
    graph <- add_node(graph = graph,
                      label = FALSE)

    # Apply node `type` value to all new nodes,
    # if supplied
    if (!is.null(set_node_type)) {
      graph <-
        select_last_node(graph = graph)

      graph <-
        set_node_attr(
          graph = graph,
          node_attr = "type",
          value = set_node_type,
          use_selection = TRUE)
    }
  }

  return(graph)
}
