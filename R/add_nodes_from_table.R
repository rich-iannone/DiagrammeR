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
#' \code{type} attribute to all nodes created from data
#' in the external table.
#' @param drop_cols an optional column selection
#' statement for dropping columns from the external
#' table before inclusion as attributes in the graph's
#' internal node data frame. Several columns can be
#' dropped by name using the syntax
#' \code{col_1 & col_2 & ...}. Columns can also be
#' dropped using a numeric column range with \code{:}
#' (e.g., \code{5:8}), or, by using the \code{:}
#' between column names to specify the range (e.g.,
#' \code{col_5_name:col_8_name}).
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' # To add nodes from the dataset called
#' # `currencies` (available as a dataset
#' # in the package), call the
#' # `add_nodes_from_table()` function
#' # after creating an empty graph; new
#' # node ID values will be created as
#' # monotonically-increasing values
#' graph_1 <-
#'   create_graph() %>%
#'   add_nodes_from_table(
#'     table = currencies)
#'
#' # View part of the graph's internal
#' # node data frame (ndf)
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
#' # If you would like to assign
#' # any of the table's columns as
#' # `type` or `label` attributes,
#' # this can be done with the `type_col`
#' # and `label_col` arguments; to set
#' # a static `type` attribute for all
#' # of the table records, use `set_type`
#' graph_2 <-
#'   create_graph() %>%
#'   add_nodes_from_table(
#'     table = currencies,
#'     label_col = iso_4217_code,
#'     set_type = currency)
#'
#' # View part of the graph's internal ndf
#' graph_2 %>%
#'   get_node_df() %>%
#'   .[, 1:5] %>%
#'   head()
#' #>   id     type label curr_number exponent
#' #> 1  1 currency   AED         784        2
#' #> 2  2 currency   AFN         971        2
#' #> 3  3 currency   ALL           8        2
#' #> 4  4 currency   AMD          51        2
#' #> 5  5 currency   ANG         532        2
#' #> 6  6 currency   AOA         973        2
#'
#' # Suppose we would like to not
#' # include certain columns from the
#' # external table in the resulting
#' # graph; we can use the `drop_cols`
#' # argument to choose which columns
#' # to not include as attributes
#' graph_3 <-
#'   create_graph() %>%
#'   add_nodes_from_table(
#'     table = currencies,
#'     label_col = iso_4217_code,
#'     set_type = currency,
#'     drop_cols = exponent & currency_name)
#'
#' # Show the node attribute names
#' # for the graph; note that the
#' # `exponent` and `currency_name`
#' # columns are not attributes in the
#' # graph's ndf
#' graph_3 %>%
#'   get_node_df() %>%
#'   colnames()
#' #> [1] "id"  type"  "label"  "curr_number"
#' @importFrom utils read.csv
#' @importFrom dplyr bind_cols mutate select
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

  set_type <- rlang::enquo(set_type)
  set_type <- (rlang::UQ(set_type) %>% paste())[2]

  drop_cols <- rlang::enquo(drop_cols)
  drop_cols <- (rlang::UQ(drop_cols) %>% paste())[2]

  if (label_col == "NULL") {
    label_col <- NULL
  }

  if (type_col == "NULL") {
    type_col <- NULL
  }

  if (set_type == "NULL") {
    set_type <- NULL
  }

  if (drop_cols == "NULL") {
    drop_cols <- NULL
  }

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {
    stop("The graph object is not valid.")
  }

  # Create bindings for specific variables
  type <- id_external <- NULL

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
  ndf <- create_node_df(n = rows_in_csv)

  # Optionally set the `label` attribute from a
  # specified column in the CSV (this copies data into
  # the `label` column)
  if (!is.null(label_col)) {

    colnames(csv)[which(colnames(csv) == label_col)] <- "label"

    ndf$label <- as.character(csv[, which(colnames(csv) == "label")])
  }

  # Optionally set the `type` attribute from a
  # specified column in the CSV (this copies data into
  # the `type` column)
  if (!is.null(type_col)) {

    colnames(csv)[which(colnames(csv) == type_col)] <- "type"

    ndf$type <- as.character(csv[, which(colnames(csv) == "type")])
  }

  # If an `id` column exists in the external
  # table, copy those values in as `id_external`
  if ("id" %in% colnames(csv)) {

    colnames(csv)[which(colnames(csv) == "id")] <- "id_external"

    ndf <-
      ndf %>%
      dplyr::mutate(id_external = csv$id_external)
  }

  # Optionally set the `type` attribute with a single
  # value repeated down
  if (is.null(type_col) & !is.null(set_type)) {
    ndf <-
      ndf %>%
      dplyr::mutate(type = as.character(set_type))
  }

  # Get the remaining columns from `csv`
  # to add to the ndf
  columns_to_add <-
    base::setdiff(colnames(csv), colnames(ndf))

  # If values for `drop_cols` provided,
  # further filter the list of column data
  # to migrate to `ndf`
  if (!is.null(drop_cols)) {

    col_selection <- get_col_selection(col_selection_stmt = drop_cols)

    if (col_selection[["selection_type"]] == "column_range") {
      col_index_1 <- which(colnames(csv) == col_selection[["column_selection"]][1])
      col_index_2 <- which(colnames(csv) == col_selection[["column_selection"]][2])

      col_indices <- col_index_1:col_index_2 %>% sort()

      columns_to_add <- base::setdiff(columns_to_add, colnames(csv)[col_indices])

    } else if (col_selection[["selection_type"]] == "column_index_range") {

      col_indices <- col_selection[["column_selection"]] %>% sort()

      columns_to_add <- base::setdiff(columns_to_add, colnames(csv)[col_indices])

    } else if (col_selection[["selection_type"]] %in% c("single_column_name", "column_names")) {

      columns_to_add <- base::setdiff(columns_to_add, col_selection[["column_selection"]])

    } else if (length(col_selection) == 0) {

      columns_to_add <- columns_to_add
    }
  }

  # Move any additional columns from the
  # external table to `ndf`
  ndf <-
    ndf %>%
    dplyr::bind_cols(
      csv %>%
        dplyr::select(columns_to_add))

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
