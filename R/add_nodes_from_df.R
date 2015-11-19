#' Add nodes and attributes from a data frame
#' @description Add nodes and their attributes to an existing graph object
#' from data in a data frame.
#' @param graph a graph object of class \code{dgr_graph} that is created
#' using \code{create_graph}.
#' @param df the name of the data frame object.
#' @param set_type an optional string to apply a \code{type} attribute to
#' all nodes created from the data frame records.
#' @param select_cols an optional character vector for specifying which
#' columns in the data frame file should be imported as node attributes.
#' @param drop_cols an optional character vector for dropping columns
#' from the incoming data.
#' @param rename_attrs an optional character for renaming node attributes.
#' @param id_col an option to apply a column of data in the data frame
#' as node ID values.
#' @param type_col an option to apply a column of data in the data frame
#' as \code{type} attribute values.
#' @param label_col an option to apply a column of data in the data frame
#' as \code{label} attribute values.
#' @return a graph object of class \code{dgr_graph}.
#' @export add_nodes_from_df

add_nodes_from_df <- function(graph,
                              df,
                              set_type = NULL,
                              select_cols = NULL,
                              drop_cols = NULL,
                              rename_attrs = NULL,
                              id_col = NULL,
                              type_col = NULL,
                              label_col = NULL){

  # Get numbers of rows and columns in df
  rows_in_df <- nrow(df)
  cols_in_df <- ncol(df)

  # Get existing nodes in graph object
  nodes_existing <- get_nodes(x = graph)

  # If values for 'select_cols' provided, filter the CSV columns
  # by those named columns
  if (!is.null(select_cols)){

    # If none of the specified values in 'select_cols' are in
    # the CSV, stop the function
    if (all(select_cols %in% colnames(df)) == FALSE){
      stop("None of the values specified for selecting columns are available.")
    }

    columns_retained <- which(colnames(df) %in% select_cols)
    df <- df[,columns_retained]
  }

  # If values for 'drop_cols' provided, filter the df columns
  # by those named columns
  if (is.null(select_cols) & !is.null(drop_cols)){

    columns_retained <- which(!(colnames(df) %in% drop_cols))
    df <- df[,columns_retained]
  }

  # If values for 'rename_attrs' provided, rename the df columns
  # by those replacement values
  if (!is.null(rename_attrs)){

    if (length(rename_attrs) != length(colnames(df))){
      stop(paste0("The number of values specified for column name changes ",
                  "does not match the number of columns available"))
    }

    colnames(df) <- rename_attrs
  }

  # Create node ID values
  for (i in 1:rows_in_df){
    graph <- add_node(graph = graph, label = FALSE)
  }

  # Get vector of nodes created
  nodes_created <- setdiff(get_nodes(x = graph), nodes_existing)

  # Add df columns as attributes
  for (i in 1:rows_in_df){
    for (j in 1:cols_in_df){

      graph <-
        set_node_attr(x = graph,
                      nodes = nodes_created[i],
                      node_attr = colnames(df)[j],
                      values = df[i,j])
    }

    # Optionally set the `label` attribute from a specified
    # column from the df
    if (!is.null(label_col)){
      graph <-
        set_node_attr(x = graph,
                      nodes = nodes_created[i],
                      node_attr = "label",
                      values = df[i, which(colnames(df) %in% label_col)])
    }

    # Optionally set the `type` attribute from a specified
    # column from the df
    if (!is.null(type_col)){
      graph <-
        set_node_attr(x = graph,
                      nodes = nodes_created[i],
                      node_attr = "type",
                      values = df[i, which(colnames(df) %in% type_col)])
    }
  }

  # Optionally set the `type` attribute with a single value repeated down
  if (!is.null(set_type)){
    graph <- select_nodes(graph = graph, nodes = nodes_created)
    graph <- set_node_attr_with_selection(graph = graph,
                                          node_attr = "type",
                                          value = set_type)
    graph <- clear_selection(graph = graph)
  }

  return(graph)
}
