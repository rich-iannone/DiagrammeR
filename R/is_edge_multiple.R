#' Is the edge a multiple edge?
#' @description Determines whether an edge
#' definition has multiple edge IDs associated
#' with the same node pair.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @param edge a numeric edge ID value.
#' @return a logical value.
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
#' graph %>%
#'   get_edge_df()
#' #>   id from to  rel
#' #> 1  1    1  2 <NA>
#' #> 2  2    2  3 <NA>
#' #> 3  3    3  4 <NA>
#' #> 4  4    1  2 <NA>
#' #> 5  5    3  4 <NA>
#'
#' # Determine if edge `1` is
#' # a multiple edge
#' graph %>%
#'   is_edge_multiple(edge = 1)
#' #> [1] TRUE
#'
#' # Determine if edge `2` is
#' # a multiple edge
#' graph %>%
#'   is_edge_multiple(edge = 2)
#' #> [1] FALSE
#' @importFrom dplyr filter pull
#' @export is_edge_multiple

is_edge_multiple <- function(graph,
                             edge) {

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {
    stop("The graph object is not valid.")
  }

  # Validation: Graph contains edges
  if (graph_contains_edges(graph) == FALSE) {
    stop("The graph contains no edges, so, no edges can be selected.")
  }

  # Stop function if more than one value
  # provided for `edge`
  if (length(edge) > 1) {
    stop("Only a single should be provided for `edge`.")
  }

  # Stop function if the value provided
  # in `edge` is not numeric
  if (!is.numeric(edge)) {
    stop("The value provided for `edge` should be numeric.")
  }

  # Obtain the graph's edf
  edf <- graph$edges_df

  # Stop function if the edge ID provided
  # is not a valid edge ID
  if (!(edge %in% edf$id)) {
    stop("The provided edge ID is not present in the graph.")
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
