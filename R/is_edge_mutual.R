#' Is the edge mutual with another edge?
#' @description Determines whether an edge
#' definition has a mutual analogue with the
#' same node pair.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @param edge an edge ID value.
#' @return a logical value.
#' @importFrom dplyr filter pull
#' @export is_edge_mutual

is_edge_mutual <- function(graph,
                           edge) {

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {
    stop("The graph object is not valid.")
  }

  # Validation: Graph contains edges
  if (graph_contains_edges(graph) == FALSE) {
    stop("The graph contains no edges, so, no edges can be selected.")
  }

  # Obtain the graph's edf
  edf <- graph$edges_df

  # Obtain the edge definition
  from <-
    edf %>%
    dplyr::filter(id == !!edge) %>%
    dplyr::pull(from)

  to <-
    edf %>%
    dplyr::filter(id == !!edge) %>%
    dplyr::pull(to)

  # Determine if there is any row where
  # the definition of `from` and `to` is
  # reversed
  mutual_edges <-
    edf %>%
    dplyr::filter(from == !!to & to == !!from)

  if (nrow(mutual_edges) > 0) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}
