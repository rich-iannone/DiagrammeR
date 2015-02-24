#' Create DOT code from a data frame
#' A function to generate DOT code from a single data frame
#' @param df the data frame object from which node and edge statements in DOT notation are to be generated.
#' @param edge_between a vector object containing statements that provide information on the relationships between nodes in different columns. The basic syntax takes the form of: "df_column_name_1 [->|--] df_column_name_2".
#' @param add_labels whether to automatically generate a set of node and edge labels based on the node ID and the edge operation, respectively.
#' @export graphviz_single_df

graphviz_single_df <- function(df,
                               edge_between,
                               node_attr = NULL,
                               edge_attr = NULL,
                               add_labels = FALSE){

  # Extract the column names that serve as nodes
  edge_between_elements <- gsub(" ", "",
                                unlist(strsplit(edge_between, "-[-|>]")))

  # Add function 'strcount' to perform a count of pattern occurances in a string
  strcount <- function(x, pattern, split){
    unlist(lapply(
      strsplit(x, split),
      function(z) na.omit(length(grep(pattern, z)))))
  }

  # Create list of node attributes, parsed from 'node_attr' input
  for (i in 1:length(node_attr)){
    if (i == 1) node_attr_values <- vector("list", length(node_attr))

    node_attr_values[[i]] <- gsub("^(([\\w|\\+])*).*", "\\1", node_attr[i], perl = TRUE)

    for (j in 1:(strcount(node_attr[i], ",", "") + 1)){

      node_attr_values[[i]][j + 1] <-
        gsub("=", " = ", gsub(" ", "",
                              unlist(strsplit(gsub(paste0("^",
                                                          gsub("\\+", "\\\\+",
                                                               node_attr_values[[i]][1]),
                                                          ":"),
                                                   "", node_attr[i]), ","))))[j]
    }

  }

  # Determine whether column contents should be concatenated to generate
  # possibly more unique strings
  if (any(grepl("\\+", edge_between_elements, perl = TRUE)) == TRUE){

    # Determine which columns are to be concatenated to make one or
    # more synthetic IDs
    left_side_columns <-
      gsub(" ", "", unlist(strsplit(edge_between_elements[1], "\\+")))

    right_side_columns <-
      gsub(" ", "", unlist(strsplit(edge_between_elements[2], "\\+")))

    stopifnot(any(left_side_columns %in% colnames(df)))
    stopifnot(any(right_side_columns %in% colnames(df)))

    ls_cols <- which(colnames(df) %in% left_side_columns)
    rs_cols <- which(colnames(df) %in% right_side_columns)

    for (i in 1:nrow(df)){
      if (i == 1) {
        ls_synthetic <- vector(mode = "character", length = 0)
        rs_synthetic <- vector(mode = "character", length = 0)
      }

      if (length(ls_cols) > 1){
        ls_synthetic <-
          c(ls_synthetic,
            paste(df[i,ls_cols], collapse = "__"))
      } else {
        if (exists("ls_synthetic")){
          rm(ls_synthetic)
        }
      }

      if (length(rs_cols) > 1){
        rs_synthetic <-
          c(rs_synthetic,
            paste(df[i,rs_cols], collapse = "__"))
      } else {
        if (exists("rs_synthetic")){
          rm(rs_synthetic)
        }
      }

      if (i == nrow(df)){
        if (exists("ls_synthetic") & !exists("rs_synthetic")){
          node_id <- gsub("'", "_",
                          c(unique(ls_synthetic), unique(df[,rs_cols])))
        }

        if (exists("rs_synthetic") & !exists("ls_synthetic")){
          node_id <- gsub("'", "_",
                          c(unique(rs_synthetic), unique(df[,ls_cols])))
        }

        if (exists("ls_synthetic") & exists("rs_synthetic")){
          node_id <- gsub("'", "_",
                          c(unique(ls_synthetic), unique(rs_synthetic)))
        }
      }
    }

    # Create the 'nodes_df' data frame, optionally adding a 'label' column
    if (add_labels == TRUE){
      label <- gsub("'", "&#39;", node_id)

      nodes_df <- data.frame(node_id = node_id, label = label)
    } else {
      nodes_df <- data.frame(node_id = node_id)
    }

    # Create the 'edges_df' data frame
    for (i in 1:nrow(df)){
      if (i == 1){
        edge_from <- vector(mode = "character", length = 0)
        edge_to <- vector(mode = "character", length = 0)
      }

      if (exists("ls_synthetic") & !exists("rs_synthetic")){

        edge_from_row <- gsub("'", "_", ls_synthetic[i])
        edge_from <- c(edge_from, edge_from_row)

        edge_to_row <- gsub("'", "_", df[i,edge_between_elements[2]])
        edge_to <- c(edge_to, edge_to_row)

      }

      if (exists("rs_synthetic") & !exists("ls_synthetic")){

        edge_from_row <- gsub("'", "_", df[i,edge_between_elements[1]])
        edge_from <- c(edge_from, edge_from_row)

        edge_to_row <- gsub("'", "_", rs_synthetic[i])
        edge_to <- c(edge_to, edge_to_row)

      }

      if (exists("ls_synthetic") & exists("rs_synthetic")){

        edge_from_row <- gsub("'", "_", ls_synthetic[i])
        edge_from <- c(edge_from, edge_from_row)

        edge_to_row <- gsub("'", "_", rs_synthetic[i])
        edge_to <- c(edge_to, edge_to_row)

      }

      if (i == nrow(df)){
        edges_df <- data.frame(edge_from, edge_to)
      }
    }


  } else {

    # Determine column indices for the node columns
    node_cols <- which(colnames(df) %in% edge_between_elements)

    # Get unique values for each of the columns and use as labels
    node_id <- gsub("'", "_", unique(as.character(unlist(df[,node_cols],
                                                         use.names = FALSE))))

    # Create the 'nodes_df' data frame, optionally adding a 'label' column
    if (add_labels == TRUE){
      label <- gsub("'", "&#39;", unique(as.character(unlist(df[,node_cols],
                                                             use.names = FALSE))))
      nodes_df <- data.frame(node_id = node_id, label = label)
    } else {
      nodes_df <- data.frame(node_id = node_id)
    }

    # Create the 'edges_df' data frame
    for (i in 1:nrow(df)){
      if (i == 1){
        edge_from <- vector(mode = "character", length = 0)
        edge_to <- vector(mode = "character", length = 0)
      }

      edge_from_row <- gsub("'", "_", df[i,edge_between_elements[1]])
      edge_from <- c(edge_from, edge_from_row)

      edge_to_row <- gsub("'", "_", df[i,edge_between_elements[2]])
      edge_to <- c(edge_to, edge_to_row)

      if (i == nrow(df)){
        edges_df <- data.frame(edge_from, edge_to)
      }
    }
  }

  # Extract information on the relationship between nodes
  if (grepl("->", edge_between)){
    directed <- TRUE
  } else if (grepl("--", edge_between)){
    directed <- FALSE
  } else {
    directed <- FALSE
  }

  # Generate the combined node and edge block for insertion into the
  # Graphviz DOT statement
  combined_block <-
    graphviz_nodes_edges_df(nodes_df = nodes_df,
                            edges_df = edges_df,
                            directed = directed)

  return(combined_block)
}
