#' Add a cycle of nodes to the graph
#' @description With a graph object of class
#' \code{dgr_graph}, add a node cycle to the graph.
#' @param graph a graph object of class
#' \code{dgr_graph} that is created using
#' \code{create_graph}.
#' @param n the number of nodes comprising the cycle.
#' @param type an optional string that describes the
#' entity type for the nodes to be added.
#' @param label either a vector object of length
#' \code{n} that provides optional labels for the new
#' nodes, or, a boolean value where setting to
#' \code{TRUE} ascribes node IDs to the label and
#' \code{FALSE} yields a blank label.
#' @param rel an optional string for providing a
#' relationship label to all new edges created in the
#' node cycle.
#' @param nodes an optional vector of node IDs of
#' length \code{n} for the newly created nodes. If
#' nothing is provided, node IDs will assigned as
#' monotonically increasing integers.
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' library(magrittr)
#'
#' # Create a new graph and add a cycle of nodes to it
#' graph <-
#'   create_graph() %>%
#'   add_cycle(6)
#'
#' # Get node information from this graph
#' node_info(graph)
#' #>   node label type deg indeg outdeg loops
#' #> 1    1     1        2     1      1     0
#' #> 2    2     2        2     1      1     0
#' #> 3    3     3        2     1      1     0
#' #> 4    4     4        2     1      1     0
#' #> 5    5     5        2     1      1     0
#' #> 6    6     6        2     1      1     0
#' @export add_cycle

add_cycle <- function(graph,
                      n,
                      type = NULL,
                      label = TRUE,
                      rel = NULL,
                      nodes = NULL){

  # Stop if n is too small
  if (n <= 2)  {
    stop("The value for n must be at least 3.")
  }

  if (!is.null(nodes)) {
    if (length(nodes) != n) {
      stop("The number of node IDs supplied is not equal to n.")
    }

    if (any(get_nodes(graph) %in% nodes)) {
      stop("At least one of the node IDs is already present in the graph.")
    }
  }

  # If node IDs are not provided, create a
  # monotonically increasing ID value
  if (is.null(nodes)){

    if (node_count(graph) == 0){
      nodes <- seq(1, n)
    }

    if (node_count(graph) > 0){
      if (!is.na(suppressWarnings(
        any(as.numeric(get_nodes(graph)))))){

        numeric_components <-
          suppressWarnings(which(!is.na(as.numeric(
            get_nodes(graph)))))

        nodes <-
          seq(max(
            as.integer(
              as.numeric(
                get_nodes(graph)[
                  numeric_components]))) + 1,
            max(
              as.integer(
                as.numeric(
                  get_nodes(graph)[
                    numeric_components]))) + n)
      }

      if (suppressWarnings(all(is.na(as.numeric(
        get_nodes(graph)))))){
        nodes <- seq(1, n)
      }
    }
  }

  cycle_nodes <-
    create_nodes(
      nodes = nodes,
      type = type,
      label = label)

  graph <-
    add_node_df(graph, cycle_nodes)

  cycle_edges <-
    create_edges(
      from = nodes,
      to = c(nodes[2:length(nodes)], nodes[1]),
      rel = rel)

  graph <-
    add_edge_df(graph, cycle_edges)

  return(graph)
}
