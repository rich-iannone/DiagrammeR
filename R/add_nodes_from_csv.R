#' Add nodes and attributes from a CSV file
#' @description Add nodes and their attributes to an existing graph object
#' from data in a CSV file.
#' @param graph a graph object of class \code{dgr_graph} that is created
#' using \code{create_graph}.
#' @param csv_path a path to a CSV file.
#' @param set_type an optional string to apply a \code{type} attribute to
#' all nodes created from the CSV records.
#' @param select_cols an optional character vector for specifying which
#' columns in the CSV file should be imported as node attributes.
#' @param drop_cols an optional character vector for dropping columns
#' from the incoming data.
#' @param rename_attrs an optional character for renaming node attributes.
#' @param id_col an option to apply a column of data in the CSV file
#' as node ID values.
#' @param type_col an option to apply a column of data in the CSV file
#' as \code{type} attribute values.
#' @param label_col an option to apply a column of data in the CSV file
#' as \code{label} attribute values.
#' @return a graph object of class \code{dgr_graph}.
#' @export add_nodes_from_csv

add_nodes_from_csv <- function(graph,
                               csv_path,
                               set_type = NULL,
                               select_cols = NULL,
                               drop_cols = NULL,
                               rename_attrs = NULL,
                               id_col = NULL,
                               type_col = NULL,
                               label_col = NULL){

  # Load in CSV file
  csv <- read.csv(csv_path, stringsAsFactors = FALSE)

  # Get numbers of rows and columns in CSV
  rows_in_csv <- nrow(csv)
  cols_in_csv <- ncol(csv)

  # Get existing nodes in graph object
  nodes_existing <- get_nodes(x = graph)

  # If values for 'select_cols' provided, filter the CSV columns
  # by those named columns
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

  # If values for 'rename_attrs' provided, rename the CSV columns
  # by those replacement values
  if (!is.null(rename_attrs)){

    if (length(rename_attrs) != length(colnames(csv))){
      stop(paste0("The number of values specified for column name changes ",
                  "does not match the number of columns available"))
    }

    colnames(csv) <- rename_attrs
  }

  # Create node ID values
  for (i in 1:rows_in_csv){
    graph <- add_node(graph = graph, label = FALSE)
  }

  # Get vector of nodes created
  nodes_created <- setdiff(get_nodes(x = graph), nodes_existing)

  # Add CSV columns as attributes
  for (i in 1:rows_in_csv){
    for (j in 1:cols_in_csv){

      graph <-
        set_node_attr(x = graph,
                      nodes = nodes_created[i],
                      node_attr = colnames(csv)[j],
                      values = csv[i,j])
    }

    # Optionally set the `label` attribute from a specified
    # column from the CSV
    if (!is.null(label_col)){
      graph <-
        set_node_attr(x = graph,
                      nodes = nodes_created[i],
                      node_attr = "label",
                      values = csv[i, which(colnames(csv) %in% label_col)])
    }

    # Optionally set the `type` attribute from a specified
    # column from the CSV
    if (!is.null(type_col)){
      graph <-
        set_node_attr(x = graph,
                      nodes = nodes_created[i],
                      node_attr = "type",
                      values = csv[i, which(colnames(csv) %in% type_col)])
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
