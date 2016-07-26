#' Drop an edge attribute column
#' @description Within a graph's internal EDF, remove
#' an existing edge attribute.
#' @param graph a graph object of class
#' @param edge_attr the name of the edge attribute
#' column to drop.
#' @return a graph object of class
#' \code{dgr_graph}.
#' @examples
#' library(magrittr)
#'
#' # Create a random graph
#' graph <-
#'   create_random_graph(
#'     5, 6, set_seed = 3) %>%
#'     set_edge_attrs("value", 3) %>%
#'     mutate_edge_attrs("value", "2 * .", "penwidth")
#'
#' # Get the graph's internal edf to show which
#' # edge attributes are available
#' get_edge_df(graph)
#' #>   from to rel value penwidth
#' #> 1    4  1         3        6
#' #> 2    2  4         3        6
#' #> 3    4  3         3        6
#' #> 4    3  5         3        6
#' #> 5    5  1         3        6
#' #> 6    4  5         3        6
#'
#' # Drop the `value` edge attribute
#' graph <-
#'   graph %>%
#'   drop_edge_attrs("value")
#'
#' # Get the graph's internal edf to show that the
#' # edge attribute had been removed
#' get_edge_df(graph)
#' #>   nodes type label
#' #> 1     1          1
#' #> 2     2          2
#' #> 3     3          3
#' #> 4     4          4
#' #> 5     5          5
#' @export drop_edge_attrs

drop_edge_attrs <- function(graph,
                            edge_attr) {

  # Stop function if length of `edge_attr` is
  # greater than one
  if (length(edge_attr) > 1) {
    stop("You can only provide a single column.")
  }

  # Stop function if `edge_attr` is any of
  # `from`, `to`, or `rel`
  if (any(c("from", "to", "rel") %in%
          edge_attr)) {
    stop("You cannot drop this column.")
  }

  # Extract the graph's edf
  edges <- get_edge_df(graph)

  # Get column names from the graph's edf
  column_names_graph <- colnames(edges)

  # Stop function if `edge_attr` is not one
  # of the graph's column
  if (!any(column_names_graph %in% edge_attr)) {
    stop("The edge attribute to drop is not in the ndf.")
  }

  # Get the column number for the edge attr to drop
  col_num_drop <-
    which(colnames(edges) %in% edge_attr)

  # Remove the column
  edges <- edges[, -col_num_drop]

  # Create a new graph object
  dgr_graph <-
    create_graph(nodes_df = graph$nodes_df,
                 edges_df = edges,
                 graph_attrs = graph$graph_attrs,
                 node_attrs = graph$node_attrs,
                 edge_attrs = graph$edge_attrs,
                 directed = graph$directed,
                 graph_name = graph$graph_name,
                 graph_time = graph$graph_time,
                 graph_tz = graph$graph_tz)

  return(dgr_graph)
}
