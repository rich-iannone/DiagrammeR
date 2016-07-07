#' Join new edge attribute values using a data frame
#' @description Join new edge attribute values in a
#' left join using a data frame. The data frame to join
#' should have at least one column with a name
#' identical to a column in the graph's edge data frame
#' (e.g., \code{rel} in both to join on the edge
#' \code{rel} values). The use of a left join in this
#' function means that there is no possibility that
#' edges in the graph might be removed after the join.
#' @param graph a graph object of class
#' @param df the data frame to use for joining.
#' \code{dgr_graph} that is created using
#' \code{create_graph}.
#' @return a graph object of class
#' \code{dgr_graph}.
#' @export join_edge_attrs

join_edge_attrs <- function(graph,
                            df) {

  # Extract the graph's edf
  edges <- get_edge_df(graph)

  # Get column names from the graph's edf
  column_names <- colnames(edges)

  # Perform a left join on the `edges` data frame
  edges <- merge(edges, df, all.x = TRUE)

  # Get new column names in the revised edf
  new_col_names <-
    setdiff(colnames(edges), column_names)

  # Get the column numbers for the new columns
  col_numbers <-
    which(colnames(edges) %in% new_col_names)

  # Replace string <NA> values with empty strings
  for (i in 1:length(col_numbers)) {
    edges[,col_numbers[i]][is.na(edges[,col_numbers[i]])] <- ""
  }

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
