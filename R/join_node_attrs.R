#' Join new node attribute values using a data frame
#' @description Join new node attribute values in a
#' left join using a data frame. The data frame to join
#' should have at least one column with a name
#' identical to a column in the graph's node data frame
#' (e.g., \code{nodes} in both to join on the node ID
#' values). The use of a left join in this function
#' means that there is no possibility that nodes in the
#' graph might be removed after the join.
#' @param graph a graph object of class
#' \code{dgr_graph} that is created using
#' \code{create_graph}.
#' @return a graph object of class
#' \code{dgr_graph}.
#' @export join_node_attrs

join_node_attrs <- function(graph,
                            df) {

  # Extract the graph's ndf
  nodes <- get_node_df(graph)

  # Get column names from the graph's ndf
  column_names <- colnames(nodes)

  # Perform a left join on the `nodes` data frame
  nodes <- merge(nodes, df, all.x = TRUE)

  # Get new column names in the revised ndf
  new_col_names <-
    setdiff(colnames(nodes), column_names)

  # Get the column numbers for the new columns
  col_numbers <-
    which(colnames(nodes) %in% new_col_names)

  # Replace string <NA> values with empty strings
  for (i in 1:length(col_numbers)) {
    nodes[,col_numbers[i]][is.na(nodes[,col_numbers[i]])] <- ""
  }

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
