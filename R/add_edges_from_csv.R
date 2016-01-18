#' Add edges and attributes from a CSV file
#' @description Add edges and their attributes to an existing graph object
#' from data in a CSV file.
#' @param graph a graph object of class \code{dgr_graph} that is created
#' using \code{create_graph}.
#' @param csv_path a path to a CSV file.
#' @param from_col the name of the CSV column from which edges originate.
#' @param from_attr the mapping of \code{from_col} values to attributes of
#' the graph's nodes.
#' @param to_col to_col the name of the CSV column to which edges terminate.
#' @param to_attr the mapping of \code{to_col} values to attributes of
#' the graph's nodes.
#' @param set_rel an optional string to apply a \code{rel} attribute to
#' all edges created from the CSV records.
#' @param select_cols an optional character vector for specifying which
#' columns in the CSV file should be imported as edge attributes.
#' @param drop_cols an optional character vector for dropping columns
#' from the incoming data.
#' @param rename_attrs an optional character vector for renaming edge
#' attributes.
#' @param rel_col an option to apply a column of data in the CSV file
#' as \code{rel} attribute values.
#' @return a graph object of class \code{dgr_graph}.
#' @export add_edges_from_csv

add_edges_from_csv <- function(graph,
                               csv_path,
                               from_col,
                               from_attr,
                               to_col,
                               to_attr,
                               set_rel = NULL,
                               select_cols = NULL,
                               drop_cols = NULL,
                               rename_attrs = NULL,
                               rel_col = NULL){

  # Load in CSV file
  csv <- read.csv(csv_path, stringsAsFactors = FALSE)

  # Get numbers of rows and columns in CSV
  rows_in_csv <- nrow(csv)
  cols_in_csv <- ncol(csv)

  # Get rownames for existing edges in graph object
  edges_existing_rownames <- rownames(get_edge_df(graph))

  # Verify that value for `from_col` is in the CSV
  if (!(from_col %in% colnames(csv))){
    stop("The value specified in `from_col` is not in the CSV file.")
  }

  # Verify that value for `to_col` is in the CSV
  if (!(to_col %in% colnames(csv))){
    stop("The value specified in `to_col` is not in the CSV file.")
  }

  # Verify that value for `from_attr` is in the graph's ndf
  if (!(from_attr %in% colnames(get_node_df(graph)))){
    stop("The value specified in `from_attr` is not in the graph.")
  }

  # Verify that value for `to_attr` is in the graph's ndf
  if (!(to_attr %in% colnames(get_node_df(graph)))){
    stop("The value specified in `to_attr` is not in the graph.")
  }

  # Verify that all values in `from_col` in the CSV are available
  # in the graph
  if (!(all(csv[,which(colnames(csv) == from_col)] %in%
            get_node_df(graph)[,which(colnames(get_node_df(graph)) == from_attr)]))){
    stop(paste0("The `from` values in the CSV don't all match the requested",
                "node attribute value in the graph."))
  }

  # Verify that all values in `to_col` in the CSV are available
  # in the graph
  if (!(all(csv[,which(colnames(csv) == to_col)] %in%
            get_node_df(graph)[,which(colnames(get_node_df(graph)) == to_attr)]))){
    stop(paste0("The `to` values in the CSV don't all match the requested",
                "node attribute values in the graph."))
  }

  ## If values for 'select_cols' provided, filter the CSV columns
  ## by those named columns
  if (!is.null(select_cols)){

    # If none of the specified values in 'select_cols' are in
    # the CSV, stop the function
    if (all(select_cols %in% colnames(csv)) == FALSE){
      stop("None of the values specified for selecting columns are available.")
    }

    columns_retained <- which(colnames(csv) %in% select_cols)
    csv <- csv[,columns_retained]
  }

  # If values for 'drop_cols' provided, filter the CSV columns
  # by those named columns
  if (is.null(select_cols) & !is.null(drop_cols)){

    columns_retained <- which(!(colnames(csv) %in% drop_cols))
    csv <- csv[,columns_retained]
  }

  ## If values for 'rename_attrs' provided, rename the CSV columns
  ## by those replacement values
  if (!is.null(rename_attrs)){

    if (length(rename_attrs) != length(colnames(csv))){
      stop(paste0("The number of values specified for column name changes ",
                  "does not match the number of columns available"))
    }

    colnames(csv) <- rename_attrs
  }

  # Get relevant column numbers from the CSV table
  from_col_value <- which(colnames(csv) == from_col)
  to_col_value <- which(colnames(csv) == to_col)

  # Get relevant column numbers from the graph's ndf
  from_attr_value <- which(colnames(get_node_df(graph)) == from_attr)
  to_attr_value <- which(colnames(get_node_df(graph)) == to_attr)


  # Create edges
  for (i in 1:rows_in_csv){
    graph <- add_edge(graph = graph,
                      from = get_node_df(graph)[which(get_node_df(graph)[,from_attr_value] ==
                                                        csv[i, from_col_value]), 1],
                      to = get_node_df(graph)[which(get_node_df(graph)[,to_attr_value] ==
                                                      csv[i, to_col_value]), 1])
  }

  # Get rownames for edges created
  edges_created_rownames <-
    as.numeric(setdiff(rownames(get_edge_df(graph)),
                       edges_existing_rownames))

  # Get column numbers in CSV that are edge attributes
  if (!is.null(rel_col)){
    edge_attr_cols_csv <-
      which(colnames(csv) %in% setdiff(colnames(csv), c(from_col, to_col, rel_col)))
  } else {
    edge_attr_cols_csv <-
      which(colnames(csv) %in% setdiff(colnames(csv), c(from_col, to_col)))
  }

  # Add CSV columns as attributes
  for (i in edges_created_rownames){
    for (j in edge_attr_cols_csv){

      graph <-
        set_edge_attr(x = graph,
                      from = get_edge_df(graph)[which(rownames(get_edge_df(graph)) == i),1],
                      to = get_edge_df(graph)[which(rownames(get_edge_df(graph)) == i),2],
                      edge_attr = colnames(csv)[j],
                      values = csv[i,j])
    }

    # Optionally set the `rel` attribute from a specified
    # column from the CSV
    if (!is.null(rel_col)){
      graph <-
        set_edge_attr(x = graph,
                      from = get_edge_df(graph)[which(rownames(get_edge_df(graph)) == i),1],
                      to = get_edge_df(graph)[which(rownames(get_edge_df(graph)) == i),2],
                      edge_attr = "rel",
                      values = csv[i, which(colnames(csv) %in% rel_col)])
    }
  }

  # Optionally set the `rel` attribute with a single value repeated down
  if (!is.null(set_rel)){
    graph <- select_edges(graph = graph,
                          from = get_edge_df(graph)[edges_created_rownames, 1],
                          to = get_edge_df(graph)[edges_created_rownames, 2])
    graph <- set_edge_attr_with_selection(graph = graph,
                                          edge_attr = "rel",
                                          value = set_rel)
    graph <- clear_selection(graph = graph)
  }

  return(graph)
}
