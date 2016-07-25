#' Copy a node attribute column and set the name
#' @description Within a graph's internal NDF, copy the
#' contents an existing node attribute and create a
#' distinct node attribute within the NDF with a
#' different attribute name.
#' @param graph a graph object of class
#' @param node_attr_from the name of the node attribute
#' column from which values will be copied.
#' @param node_attr_to the name of the new node
#' attribute column to which the copied values will be
#' placed.
#' @return a graph object of class
#' \code{dgr_graph}.
#' @examples
#' library(magrittr)
#'
#' # Create a random graph
#' graph <-
#'   create_random_graph(
#'   5, 10, set_seed = 3) %>%
#'   set_node_attrs("shape", "circle")
#'
#' # Get the graph's internal ndf to show which
#' # node attributes are available
#' get_node_df(graph)
#' #>   nodes type label value  shape
#' #> 1     1          1     2 circle
#' #> 2     2          2   8.5 circle
#' #> 3     3          3     4 circle
#' #> 4     4          4   3.5 circle
#' #> 5     5          5   6.5 circle
#'
#' # Make a copy the `value` node attribute as
#' # the `width` node attribute
#' graph <-
#'   graph %>%
#'   copy_node_attrs("value", "size")
#'
#' # Get the graph's internal ndf to show that the
#' # node attribute had been copied
#' get_node_df(graph)
#' #>   nodes type label value  shape size
#' #> 1     1          1     2 circle    2
#' #> 2     2          2   8.5 circle  8.5
#' #> 3     3          3     4 circle    4
#' #> 4     4          4   3.5 circle  3.5
#' #> 5     5          5   6.5 circle  6.5
#' @export copy_node_attrs

copy_node_attrs <- function(graph,
                            node_attr_from,
                            node_attr_to) {

  # Stop function if `node_attr_to` is `nodes` or `node`
  if (any(c("nodes", "node") %in% node_attr_to)) {
    stop("You cannot use those names.")
  }

  # Extract the graph's ndf
  nodes <- get_node_df(graph)

  # Get column names from the graph's ndf
  column_names_graph <- colnames(nodes)

  # Stop function if `node_attr_from` is not one
  # of the graph's column
  if (!any(column_names_graph %in% node_attr_from)) {
    stop("The node attribute to copy is not in the ndf.")
  }

  # Get the column number for the node attr to copy
  col_num_copy_from <-
    which(colnames(nodes) %in% node_attr_from)

  # Copy the column through a `cbind()`
  nodes <- cbind(nodes, nodes[,col_num_copy_from])

  # Set the column name for the copied attr
  colnames(nodes)[ncol(nodes)] <- node_attr_to

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
