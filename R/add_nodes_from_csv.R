#' Add nodes and attributes from a CSV file
#' @description Add nodes and their attributes to an existing graph object
#' from data in a CSV file.
#' @param graph a graph object of class \code{dgr_graph} that is created
#' using \code{create_graph}.
#' @param csv path a path to a CSV file.
#' @param set_type an optional string to apply a `type` attribute to all
#' nodes created from the CSV records.
#' @param select_cols an optional character vector for specifying which
#' columns in the CSV file should be imported as node attributes.
#' @param rename_attrs an optional character for renaming attributes.
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

    }
  }

  # Set the `type` attribute with a single value repeated down
  if (!is.null(set_type)){
    graph <- select_nodes(graph = graph, nodes = nodes_created)
    graph <- set_node_attr_with_selection(graph = graph,
                                          node_attr = "type",
                                          value = set_type)
  }

  return(graph)
}
