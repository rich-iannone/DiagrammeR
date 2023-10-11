#' Get a vector of edge ID values
#'
#' @description
#'
#' Obtain a vector of edge ID values from a graph object. An optional filter by
#' edge attribute can limit the set of edge ID values returned.
#'
#' @inheritParams render_graph
#' @param conditions an option to use filtering conditions for the retrieval of
#'   edges.
#'
#' @return A vector of edge ID values.
#'
#' @examples
#' # Create a node data frame (ndf)
#' ndf <-
#'   create_node_df(
#'     n = 4,
#'     type = "letter",
#'     color = c("red", "green", "grey", "blue"),
#'     value = c(3.5, 2.6, 9.4, 2.7))
#'
#' # Create an edge data frame (edf)
#' edf <-
#'   create_edge_df(
#'     from = c(1, 2, 3),
#'     to = c(4, 3, 1),
#'     rel = "leading_to",
#'     color = c("pink", "blue", "blue"),
#'     value = c(3.9, 2.5, 7.3))
#'
#' # Create a graph
#' graph <-
#'   create_graph(
#'     nodes_df = ndf,
#'     edges_df = edf)
#'
#' # Get a vector of all edges in a graph
#' graph %>% get_edge_ids()
#'
#' # Get a vector of edge ID values using a
#' # numeric comparison (i.e., all edges with
#' # `value` attribute greater than 3)
#' get_edge_ids(
#'   graph,
#'   conditions = value > 3)
#'
#' # Get a vector of edge ID values using
#' # a match pattern (i.e., all edges with
#' # `color` attribute of `pink`)
#' get_edge_ids(
#'   graph,
#'   conditions = color == "pink")
#'
#' # Use multiple conditions to return edges
#' # with the desired attribute values
#' get_edge_ids(
#'   graph,
#'   conditions =
#'     color == "blue" &
#'     value > 5)
#'
#' @export
get_edge_ids <- function(
    graph,
    conditions = NULL
) {

  # Get the name of the function
  fcn_name <- get_calling_fcn()

  # Capture provided conditions
  conditions <- rlang::enquo(conditions)

  # If the graph contains no edges, return NA
  if (nrow(graph$edges_df) == 0) {
    return(NA)
  }

  # Extract edge data frame from the graph
  edges_df <- graph$edges_df

  # If conditions are provided then
  # pass in those conditions and filter the
  # data frame of `edges_df`
  if (!is.null(
    rlang::enquo(conditions) %>%
    rlang::get_expr())) {

    edges_df <- dplyr::filter(.data = edges_df, !!conditions)
  }

  # If no edges remain then return NA
  if (nrow(edges_df) == 0) {
    return(NA)
  }

  edges_df %>% dplyr::pull(id)
}
