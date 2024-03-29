#' Is the edge mutual with another edge?
#'
#' @description
#'
#' Determines whether an edge definition has a mutual analogue with the same
#' node pair.
#'
#' @inheritParams render_graph
#' @param edge A numeric edge ID value.
#'
#' @return A logical value.
#'
#' @examples
#' # Create a graph that has mutual
#' # edges across some node pairs
#' graph <-
#'   create_graph() %>%
#'   add_path(n = 4) %>%
#'   add_edge(
#'     from = 4,
#'     to = 3) %>%
#'   add_edge(
#'     from = 2,
#'     to = 1)
#'
#' # Get the graph's internal
#' # edge data frame
#' graph %>% get_edge_df()
#'
#' # Determine if edge `1` has
#' # a mutual edge
#' graph %>%
#'   is_edge_mutual(edge = 1)
#'
#' # Determine if edge `2` has
#' # a mutual edge
#' graph %>%
#'   is_edge_mutual(edge = 2)
#'
#' @export
is_edge_mutual <- function(
    graph,
    edge
) {

  # Validation: Graph object is valid
  check_graph_valid(graph)

  # Validation: Graph contains edges
  check_graph_contains_edges(graph)

  # Validation: Stop function if `edge` is not a single numeric value.
  check_number_decimal(edge)

  # Obtain the graph's edf
  edf <- graph$edges_df

  # Stop function if the edge ID provided
  # is not a valid edge ID
  if (!(edge %in% edf$id)) {

    cli::cli_abort(
      "The provided edge ID is not present in the graph.")
  }

  # Obtain the edge definition
  from <-
    edf %>%
    dplyr::filter(id == !!edge) %>%
    dplyr::pull("from")

  to <-
    edf %>%
    dplyr::filter(id == !!edge) %>%
    dplyr::pull("to")

  # Determine if there is any row where
  # the definition of `from` and `to` is
  # reversed
  mutual_edges <-
    edf %>%
    dplyr::filter(from == !!to, to == !!from)

  res <- nrow(mutual_edges) > 0
  res
}
