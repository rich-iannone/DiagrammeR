#' Is the edge a multiple edge?
#'
#' Determines whether an edge definition has multiple edge IDs associated with
#' the same node pair.
#'
#' @inheritParams render_graph
#' @param edge A numeric edge ID value.
#' @return A logical value.
#' @examples
#' # Create a graph that has multiple
#' # edges across some node pairs
#' graph <-
#'   create_graph() %>%
#'   add_path(n = 4) %>%
#'   add_edge(
#'     from = 1,
#'     to = 2) %>%
#'   add_edge(
#'     from = 3,
#'     to = 4)
#'
#' # Get the graph's internal
#' # edge data frame
#' graph %>% get_edge_df()
#'
#' # Determine if edge `1` is
#' # a multiple edge
#' graph %>%
#'   is_edge_multiple(edge = 1)
#'
#' # Determine if edge `2` is
#' # a multiple edge
#' graph %>%
#'   is_edge_multiple(edge = 2)
#'
#' @export
is_edge_multiple <- function(graph,
                             edge) {

  # Get the name of the function
  fcn_name <- get_calling_fcn()

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {

    emit_error(
      fcn_name = fcn_name,
      reasons = "The graph object is not valid")
  }

  # Validation: Graph contains edges
  if (graph_contains_edges(graph) == FALSE) {

    emit_error(
      fcn_name = fcn_name,
      reasons = "The graph contains no edges")
  }

  # Stop function if more than one value
  # provided for `edge`
  if (length(edge) > 1) {

    emit_error(
      fcn_name = fcn_name,
      reasons = "Only a single edge ID should be provided for `edge`")
  }

  # Stop function if the value provided
  # in `edge` is not numeric
  if (!is.numeric(edge)) {

    emit_error(
      fcn_name = fcn_name,
      reasons = "The value provided for `edge` should be numeric")
  }

  # Obtain the graph's edf
  edf <- graph$edges_df

  # Stop function if the edge ID provided
  # is not a valid edge ID
  if (!(edge %in% edf$id)) {

    emit_error(
      fcn_name = fcn_name,
      reasons = "The provided edge ID is not present in the graph")
  }

  # Obtain the edge definition
  from <-
    edf %>%
    dplyr::filter(id == !!edge) %>%
    dplyr::pull(from)

  to <-
    edf %>%
    dplyr::filter(id == !!edge) %>%
    dplyr::pull(to)

  # Determine if there are mulitple rows
  # where the definition of `from` and `to`
  # is valid
  multiple_edges <-
    edf %>%
    dplyr::filter(from == !!from & to == !!to)

  if (nrow(multiple_edges) > 1) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}
