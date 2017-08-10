#' Add nodes and attributes to graph from a table
#' @description Add nodes and their attributes to an
#' existing graph object from data in a CSV file or a
#' data frame.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @param table either a path to a CSV file, or, a data
#' frame object.
#' @param label_col an option to apply a column of data
#' in the table as \code{label} attribute values.
#' @param type_col an option to apply a column of data
#' in the table as \code{type} attribute values.
#' @param set_type an optional string to apply a
#' \code{type} attribute to all nodes created from the
#' table records.
#' @param drop_cols an optional character vector for
#' dropping columns from the incoming data.
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' \dontrun{
#' library(dplyr)
#'
#' # Specify a path to a CSV file
#' path_to_csv <-
#'   system.file(
#'     "extdata", "currencies.csv",
#'     package = "DiagrammeR")
#'
#' # To add nodes from a CSV file, call the
#' # `add_nodes_from_table()` function; new node ID
#' # values will be created as a monotonically-
#' # increasing values from 1
#' graph_1 <-
#'   create_graph() %>%
#'   add_nodes_from_table(
#'     table = path_to_csv)
#'
#' # View part of the graph's internal node data
#' # frame (ndf) using `get_node_df()`
#' graph_1 %>%
#'   get_node_df() %>%
#'   .[, 1:5] %>%
#'   head()
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
#'     table = path_to_csv,
#'     label_col = iso_4217_code,
#'     set_type = "currency")
#'
#' # View part of the graph's internal ndf
#' graph_2 %>%
#'   get_node_df() %>%
#'   .[, 1:5] %>%
#'   head()
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
#'     table = path_to_csv,
#'     label_col = iso_4217_code,
#'     set_type = "currency",
#'     drop_cols = c("exponent", "currency_name"))
#'
#' # Show the node attribute names for the graph
#' graph_3 %>%
#'   get_node_df() %>%
#'   colnames()
#' #> [1] "id"  type"  "label"  "iso_4217_code"
#' #> [5] "curr_number"
#' }
#' @importFrom utils read.csv
#' @importFrom dplyr bind_cols mutate mutate_ select_
#' @importFrom rlang enquo UQ
#' @export add_nodes_from_table

add_nodes_from_table <- function(graph,
                                 table,
                                 label_col = NULL,
                                 type_col = NULL,
                                 set_type = NULL,
                                 drop_cols = NULL) {

  # Get the time of function start
  time_function_start <- Sys.time()

  label_col <- rlang::enquo(label_col)
  label_col <- (rlang::UQ(label_col) %>% paste())[2]

  type_col <- rlang::enquo(type_col)
  type_col <- (rlang::UQ(type_col) %>% paste())[2]

  if (label_col == "NULL") {
    label_col <- NULL
  }

  if (type_col == "NULL") {
    type_col <- NULL
  }

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {
    stop("The graph object is not valid.")
  }

  # Create bindings for specific variables
  type <- NULL

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
  empty_ndf <- create_node_df(n = rows_in_csv)
  ndf <- dplyr::bind_cols(empty_ndf, csv)

  # If values for `drop_cols` provided, filter the ndf
  # columns by those named columns
  if (!is.null(drop_cols)) {

    columns_retained <-
      which(!(colnames(ndf) %in% drop_cols))

    ndf <- ndf[, columns_retained]
  }

  # Optionally set the `label` attribute from a
  # specified column in the CSV (this copies data into
  # the `label` column)
  if (!is.null(label_col)) {
    if (any(colnames(ndf) == label_col)) {
      ndf$label <-
        as.character(ndf[, which(colnames(ndf) == label_col)])
    }
  }

  # Optionally set the `type` attribute from a
  # specified column in the CSV
  if (!is.null(type_col)) {
    ndf <-
      ndf %>%
      dplyr::mutate_(type = type_col) %>%
      dplyr::mutate(type = as.character(type)) %>%
      dplyr::select_(paste0("-", type_col))
  }

  # Optionally set the `type` attribute with a single
  # value repeated down
  if (is.null(type_col) & !is.null(set_type)) {
    ndf <-
      ndf %>%
      dplyr::mutate(type = as.character(set_type))
  }

  # Add as a node data frame to the graph
  graph <- add_node_df(graph, ndf)

  # Update the `last_node` counter
  graph$last_node <- nodes_created + rows_in_csv

  # Update the `graph_log` df with an action
  graph$graph_log <-
    add_action_to_log(
      graph_log = graph$graph_log,
      version_id = nrow(graph$graph_log) + 1,
      function_used = "add_nodes_from_table",
      time_modified = time_function_start,
      duration = graph_function_duration(time_function_start),
      nodes = nrow(graph$nodes_df),
      edges = nrow(graph$edges_df))

  # Perform graph actions, if any are available
  if (nrow(graph$graph_actions) > 0) {
    graph <-
      graph %>%
      trigger_graph_actions()
  }

  # Write graph backup if the option is set
  if (graph$graph_info$write_backups) {
    save_graph_as_rds(graph = graph)
  }

  graph
}
