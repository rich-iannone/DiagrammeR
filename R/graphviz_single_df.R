#' Create DOT code from a data frame
#' A function to generate DOT code from a single data frame
#' @param df the data frame object from which node and edge statements in DOT notation are to be generated.
#' @param node_col_names a vector object containing the data frame column names for nodes to be graphed.
#' @param edge_between a vector object containing statements that provide information on the relationships between nodes in different columns. The basic syntax takes the form of: "df_column_name_1 [->|--] df_column_name_2".
#' @param add_labels whether to automatically generate a set of node and edge labels based on the node ID and the edge operation, respectively.
#' @export graphviz_single_df

graphviz_single_df <- function(df,
                               node_col_names,
                               edge_between,
                               add_labels = FALSE){

  # Determine column indices for columns selected as nodes
  node_cols <- which(colnames(df) %in% node_col_names)

  # Get unique values for each of the columns and use as labels
  node_id <- gsub("'", "_", unique(as.character(unlist(df[,node_cols],
                                                       use.names = FALSE))))

  # Create the 'nodes_df' data frame
  if (add_labels == TRUE){
    label <- gsub("'", "&#39;", unique(as.character(unlist(df[,node_cols],
                                                           use.names = FALSE))))
    nodes_df <- data.frame(node_id = node_id, label = label)
  } else {
    nodes_df <- data.frame(node_id = node_id)
  }

  # Obtain the elements for the edge operation
  edge_between_elements <- gsub(" ", "",
                                unlist(strsplit(edge_between, "-[-|>]")))

  # Determine whether the relationship between nodes is directed or
  # undirected
  if (grepl("->", edge_between)){
    directed <- TRUE
  } else if (grepl("--", edge_between)){
    directed <- FALSE
  } else {
    directed <- FALSE
  }

  # Determine which columns contain nodes for the edge operations
  edge_cols <- which(colnames(df) %in% edge_between_elements)

  # Create the 'edges_df' data frame
  for (i in 1:nrow(df)){
    if (i == 1){
      edge_from <- vector(mode = "character", length = 0)
      edge_to <- vector(mode = "character", length = 0)
    }

    edge_from_row <- gsub("'", "_", df[i,edge_cols[1]])
    edge_from <- c(edge_from, edge_from_row)

    edge_to_row <- gsub("'", "_", df[i,edge_cols[2]])
    edge_to <- c(edge_to, edge_to_row)

    if (i == nrow(df)){
      edges_df <- data.frame(edge_from, edge_to)
    }
  }

  # Generate the combined node and edge block for insertion into the
  # Graphviz DOT statement
  combined_block <-
    graphviz_nodes_edges_df(nodes_df = nodes_df,
                            edges_df = edges_df,
                            directed = directed)

  return(combined_block)
}
