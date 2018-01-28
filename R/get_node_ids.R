#' Get a vector of node ID values
#' @description Obtain a vector of
#' node ID values from a graph
#' object. An optional filter by
#' node attribute can limit the set
#' of node ID values returned.
#' @param graph a graph object of
#' class \code{dgr_graph}.
#' @param conditions an option to
#' use filtering conditions for the
#' retrieval of nodes.
#' @return a vector of node ID
#' values.
#' @examples
#' # Create a node data
#' # frame (ndf)
#' ndf <-
#'   create_node_df(
#'     n = 4,
#'     type = "letter",
#'     color = c(
#'       "red", "green",
#'       "blue", "blue"),
#'     value = c(
#'       3.5, 2.6, 9.4, 2.7))
#'
#' # Create a graph using
#' # the ndf
#' graph <-
#'   create_graph(
#'     nodes_df = ndf)
#'
#' # Get a vector of all nodes in a graph
#' graph %>%
#'   get_node_ids()
#'
#' # Get a vector of node ID values using a
#' # numeric comparison (i.e., all nodes with
#' # `value` attribute greater than 3)
#' graph %>%
#'   get_node_ids(
#'     conditions = value > 3)
#'
#' # Get a vector of node ID values using
#' # a match pattern (i.e., all nodes with
#' # `color` attribute of `green`)
#' graph %>%
#'   get_node_ids(
#'     conditions = color == "green")
#'
#' # Use multiple conditions to return nodes
#' # with the desired attribute values
#' graph %>%
#'   get_node_ids(
#'     conditions =
#'       color == "blue" &
#'       value > 5)
#' @importFrom dplyr filter pull
#' @importFrom rlang enquo UQ get_expr
#' @export get_node_ids

get_node_ids <- function(graph,
                         conditions = NULL) {

  # Create binding for a specific variable
  id <- NULL

  conditions <- rlang::enquo(conditions)

  if (is_graph_empty(graph)) {
    return(NA)
  } else {
    nodes_df <- graph$nodes_df
  }

  # If conditions are provided then
  # pass in those conditions and filter the
  # data frame of `nodes_df`
  if (!is.null(
    rlang::enquo(conditions) %>%
    rlang::get_expr())) {

    nodes_df <-
      filter(
        .data = nodes_df,
        rlang::UQ(conditions))
  }

  # If no nodes remain then return NA
  if (nrow(nodes_df) == 0) {
    return(NA)
  }

  nodes_df %>%
    dplyr::pull(id)
}
