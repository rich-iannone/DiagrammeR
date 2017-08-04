#' Is the edge mutual with another edge?
#' @description Determines whether an edge
#' definition has a mutual analogue with the
#' same node pair.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @param edge a numeric edge ID value.
#' @return a logical value.
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
#' graph %>%
#'   get_edge_df()
#' #>   id from to  rel
#' #> 1  1    1  2 <NA>
#' #> 2  2    2  3 <NA>
#' #> 3  3    3  4 <NA>
#' #> 4  4    4  3 <NA>
#' #> 5  5    2  1 <NA>
#'
#' graph %>%
#'   is_edge_mutual(edge = 1)
#' #> [1] TRUE
#'
#' graph %>%
#'   is_edge_mutual(edge = 2)
#' #> [1] FALSE
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
