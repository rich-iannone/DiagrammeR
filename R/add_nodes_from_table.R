#' Add nodes and attributes to graph from a table
#' @description Add nodes and their attributes to an
#' existing graph object from data in a CSV file or a
#' data frame.
#' @param graph a graph object of class
#' \code{dgr_graph} that is created using
#' \code{create_graph}.
#' @param table either a path to a CSV file, or, a data
#' frame object.
#' @param set_type an optional string to apply a
#' \code{type} attribute to all nodes created from the
#' table records.
#' @param select_cols an optional character vector for
#' specifying which columns in the table that should be
#' imported as node attributes.
#' @param drop_cols an optional character vector for
#' dropping columns from the incoming data.
#' @param type_col an option to apply a column of data
#' in the table as \code{type} attribute values.
#' @param label_col an option to apply a column of data
#' in the table as \code{label} attribute values.
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' \dontrun{
#' library(dplyr)
#'
#' # Specify a path to a CSV file
#' path_to_csv <-
#'   system.file("extdata", "currencies.csv",
#'               package = "DiagrammeR")
#'
#' # To add nodes from a CSV file, call the
#' # `add_nodes_from_table()` function; new node ID
#' # values will be created as a monotonically-
#' # increasing values from 1
#' graph_1 <-
#'   create_graph() %>%
#'   add_nodes_from_table(path_to_csv)
#'
#' # View part of the graph's internal node data
#' # frame (ndf) using `get_node_df()`
#' graph_1 %>% get_node_df %>% .[, 1:5] %>% head
#' #>   id type label iso_4217_code curr_number
#' #> 1  1 <NA>  <NA>           AED         784
#' #> 2  2 <NA>  <NA>           AFN         971
#' #> 3  3 <NA>  <NA>           ALL           8
#' #> 4  4 <NA>  <NA>           AMD          51
#' #> 5  5 <NA>  <NA>           ANG         532
#' #> 6  6 <NA>  <NA>           AOA         973
#'
#' # If you would like to assign any of the table's
#' # columns as `type` or `label` attributes, this can
#' # be done with the `type_col` and `label_col`
#' # arguments; to set a static `type` attribute for
#' # all of the table records, use `set_type`
#' graph_2 <-
#'   create_graph() %>%
#'   add_nodes_from_table(
#'     path_to_csv,
#'     set_type = "currency",
#'     label_col = "iso_4217_code")
#'
#' # View part of the graph's internal ndf
#' graph_2 %>% get_node_df %>% .[, 1:5] %>% head
#' #>   id     type label iso_4217_code curr_number
#' #> 1  1 currency   AED           AED         784
#' #> 2  2 currency   AFN           AFN         971
#' #> 3  3 currency   ALL           ALL           8
#' #> 4  4 currency   AMD           AMD          51
#' #> 5  5 currency   ANG           ANG         532
#' #> 6  6 currency   AOA           AOA         973
#'
#' # Suppose you would like to not include certain
#' # columns from the table in the resulting graph; you
#' # can use the `drop_cols` argument to choose which
#' # columns to not include as attributes in the graph
#' graph_3 <-
#'   create_graph() %>%
#'   add_nodes_from_table(
#'     path_to_csv,
#'     set_type = "currency",
#'     label_col = "iso_4217_code",
#'     drop_cols = "exponent")
#'
#' graph_3 %>% get_node_df %>% colnames
#' #> [1] "id"  type"  "label"  "iso_4217_code"
#' #> [5] "curr_number" "currency_name"
#' }
#' @importFrom utils read.csv
#' @importFrom dplyr bind_cols
#' @export add_nodes_from_table

add_nodes_from_table <- function(graph,
                                 table,
                                 set_type = NULL,
                                 select_cols = NULL,
                                 drop_cols = NULL,
                                 type_col = NULL,
                                 label_col = NULL) {

  # Get the number of nodes ever created for
  # this graph
  nodes_created <- graph$last_node

  if (inherits(table, "character")) {
    # Load in CSV file
    csv <- read.csv(table, stringsAsFactors = FALSE)
  } else if (inherits(table, "data.frame")) {
    # Rename `table` object as `csv`
    csv <- table
  }

  # Get number of rows in the CSV
  rows_in_csv <- nrow(csv)

  # Create an empty ndf and bind those columns
  # with the table data
  csv <-
    create_node_df(n = rows_in_csv) %>%
    dplyr::bind_cols(., csv)

  # If values for `select_cols` are provided, filter
  # the CSV columns by those named columns
  if (!is.null(select_cols)) {

    # If none of the specified values in `select_cols`
    # are in the CSV, stop the function
    if (all(select_cols %in% colnames(csv)) == FALSE) {
      stop("None of the values specified for selecting columns are available.")
    }

    columns_retained <-
      which(colnames(csv) %in% select_cols)

    csv <- csv[, columns_retained]
  }

  # If values for `drop_cols` provided, filter the CSV
  # columns by those named columns
  if (is.null(select_cols) & !is.null(drop_cols)) {

    columns_retained <-
      which(!(colnames(csv) %in% drop_cols))

    csv <- csv[, columns_retained]
  }

  # Optionally set the `label` attribute from a
  # specified column in the CSV (this copies data into
  # the `label` column)
  if (!is.null(label_col)) {
    if (any(colnames(csv) == label_col)) {
      csv$label <- csv[, which(colnames(csv) == label_col)]
    }
  }

  # Optionally set the `type` attribute from a
  # specified column in the CSV
  if (!is.null(type_col)) {
    if (any(colnames(csv) == type_col)) {
      colnames(csv)[which(colnames(csv) == type_col)] <- "type"
    }
  }

  # Optionally set the `type` attribute with a single
  # value repeated down
  if (!is.null(set_type)) {
    csv$type <- set_type
  }

  # Add as a node data frame to the graph
  graph <- add_node_df(graph, csv)

  # Update the `last_node` counter
  graph$last_node <- nodes_created + rows_in_csv

  return(graph)
}
