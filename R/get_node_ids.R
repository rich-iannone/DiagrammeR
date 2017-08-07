#' Get a vector of node ID values
#' @description Obtain a vector of node ID values from
#' a graph object or a node data frame. An optional
#' filter by node attribute can limit the set of node
#' ID values returned.
#' @param x either a graph object of class
#' \code{dgr_graph} or a node data frame.
#' @param conditions an option to use filtering
#' conditions for the retrieval of nodes.
#' @return a vector of node ID values.
#' @examples
#' # Create a node data frame (ndf)
#' ndf <-
#'   create_node_df(
#'     n = 4,
#'     type = "letter",
#'     color = c("red", "green",
#'               "blue", "blue"),
#'     value = c(3.5, 2.6, 9.4, 2.7))
#'
#' # Create a graph
#' graph <- create_graph(nodes_df = ndf)
#'
#' # Get a vector of all nodes in a graph
#' get_node_ids(graph)
#' #> [1] 1 2 3 4
#'
#' # Get a vector of node ID values from a
#' # node data frame
#' get_node_ids(ndf)
#' #> [1] 1 2 3 4
#'
#' # Get a vector of node ID values using a
#' # numeric comparison (i.e., all nodes with
#' # `value` attribute greater than 3)
#' get_node_ids(
#'   graph,
#'   conditions = value > 3)
#' #> [1] 1 3
#'
#' # Get a vector of node ID values using
#' # a match pattern (i.e., all nodes with
#' # `color` attribute of `green`)
#' get_node_ids(
#'   graph,
#'   conditions = color == "green")
#' #> [1] 2
#'
#' # Use multiple conditions to return nodes
#' # with the desired attribute values
#' get_node_ids(
#'   graph,
#'   conditions =
#'     color == "blue" &
#'     value > 5)
#' #> [1] 3
#' @importFrom dplyr filter pull
#' @importFrom rlang enquo UQ
#' @export get_node_ids

get_node_ids <- function(x,
                         conditions = NULL) {

  conditions <- rlang::enquo(conditions)

  if (inherits(x, "dgr_graph")) {
    if (is_graph_empty(x)) {
      return(NA)
    } else {
      nodes_df <- x$nodes_df
    }
  }

  if (inherits(x, "data.frame")) {
    if (colnames(x)[1] == "id" &
        colnames(x)[2] == "type" &
        colnames(x)[3] == "label") {
      nodes_df <- x
    }
  }

  # If conditions are provided then
  # pass in those conditions and filter the
  # data frame of `nodes_df`
  if (!((rlang::UQ(conditions) %>% paste())[2] == "NULL")) {

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
