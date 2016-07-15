#' Add a path of nodes to the graph
#' @description With a graph object of class
#' \code{dgr_graph}, add a node path to the graph.
#' @param graph a graph object of class
#' \code{dgr_graph} that is created using
#' \code{create_graph}.
#' @param n the number of nodes comprising the path.
#' @param type an optional string that describes the
#' entity type for the nodes to be added.
#' @param label either a vector object of length
#' \code{n} that provides optional labels for the new
#' nodes, or, a boolean value where setting to
#' \code{TRUE} ascribes node IDs to the label and
#' \code{FALSE} yields a blank label.
#' @param rel an optional string for providing a
#' relationship label to all new edges created in the
#' node path.
#' @param nodes an optional vector of node IDs of
#' length \code{n} for the newly created nodes. If
#' nothing is provided, node IDs will assigned as
#' monotonically increasing integers.
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' library(magrittr)
#'
#' # Create a new graph and add 3 paths of varying
#' # lengths
#' graph <-
#'   create_graph() %>%
#'   add_path(4, "four_path") %>%
#'   add_path(5, "five_path") %>%
#'   add_path(6, "six_path")
#'
#' # Get node information from this graph
#' node_info(graph)
#' #>    node label      type deg indeg outdeg loops
#' #> 1     1     1 four_path   1     0      1     0
#' #> 2     5     5 five_path   1     0      1     0
#' #> 3    10    10  six_path   1     0      1     0
#' #> 4     2     2 four_path   2     1      1     0
#' #> 5     3     3 four_path   2     1      1     0
#' #> 6     6     6 five_path   2     1      1     0
#' #> 7     7     7 five_path   2     1      1     0
#' #> 8     8     8 five_path   2     1      1     0
#' #> 9    11    11  six_path   2     1      1     0
#' #> 10   12    12  six_path   2     1      1     0
#' #> 11   13    13  six_path   2     1      1     0
#' #> 12   14    14  six_path   2     1      1     0
#' #> 13    4     4 four_path   1     1      0     0
#' #> 14    9     9 five_path   1     1      0     0
#' #> 15   15    15  six_path   1     1      0     0
#' @export add_path

add_path <- function(graph,
                     n,
                     type = NULL,
                     label = TRUE,
                     rel = NULL,
                     nodes = NULL){

  # Stop if n is too small
  if (n <= 1)  {
    stop("The value for n must be at least 2.")
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

  path_nodes <-
    create_nodes(
      nodes = nodes,
      type = type,
      label = label)

  graph <-
    add_node_df(graph, path_nodes)

  path_edges <-
    create_edges(
      from = nodes[1:length(nodes) - 1],
      to = nodes[2:length(nodes)],
      rel = rel)

  graph <-
    add_edge_df(graph, path_edges)

  return(graph)
}
