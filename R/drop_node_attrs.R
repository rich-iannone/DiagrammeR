#' Drop a node attribute column
#' @description Within a graph's internal NDF, remove
#' an existing node attribute.
#' @param graph a graph object of class
#' @param node_attr the name of the node attribute
#' column to drop.
#' @return a graph object of class
#' \code{dgr_graph}.
#' @examples
#' # Create a random graph
#' graph <-
#'   create_random_graph(
#'     5, 10, set_seed = 3)
#'
#' # Get the graph's internal ndf to show which
#' # node attributes are available
#' get_node_df(graph)
#' #>   nodes type label value
#' #> 1     1          1     2
#' #> 2     2          2   8.5
#' #> 3     3          3     4
#' #> 4     4          4   3.5
#' #> 5     5          5   6.5
#'
#' # Drop the `value` node attribute
#' graph <-
#'   graph %>%
#'   drop_node_attrs("value")
#'
#' # Get the graph's internal ndf to show that the
#' # node attribute had been removed
#' get_node_df(graph)
#' #>   nodes type label
#' #> 1     1          1
#' #> 2     2          2
#' #> 3     3          3
#' #> 4     4          4
#' #> 5     5          5
#' @export drop_node_attrs

drop_node_attrs <- function(graph,
                            node_attr) {

  # Stop function if length of `node_attr` is
  # greater than one
  if (length(node_attr) > 1) {
    stop("You can only provide a single column.")
  }

  # Stop function if `node_attr` is any of
  # `nodes`, `node`, `type`, or `label`
  if (any(c("nodes", "node", "type", "label") %in%
          node_attr)) {
    stop("You cannot drop this column.")
  }

  # Extract the graph's ndf
  nodes <- get_node_df(graph)

  # Get column names from the graph's ndf
  column_names_graph <- colnames(nodes)

  # Stop function if `node_attr` is not one
  # of the graph's column
  if (!any(column_names_graph %in% node_attr)) {
    stop("The node attribute to drop is not in the ndf.")
  }

  # Get the column number for the node attr to drop
  col_num_drop <-
    which(colnames(nodes) %in% node_attr)

  # Remove the column
  nodes <- nodes[, -col_num_drop]

  # Create a new graph object
  dgr_graph <-
    create_graph(nodes_df = nodes,
                 edges_df = graph$edges_df,
                 graph_attrs = graph$graph_attrs,
                 node_attrs = graph$node_attrs,
                 edge_attrs = graph$edge_attrs,
                 directed = graph$directed,
                 graph_name = graph$graph_name,
                 graph_time = graph$graph_time,
                 graph_tz = graph$graph_tz)

  return(dgr_graph)
}
