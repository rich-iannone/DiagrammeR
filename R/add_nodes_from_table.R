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
#' @param rename_attrs an optional character for
#' renaming node attributes.
#' @param id_col an option to apply a column of data in
#' the table as node ID values.
#' @param type_col an option to apply a column of data
#' in the table as \code{type} attribute values.
#' @param label_col an option to apply a column of data
#' in the table as \code{label} attribute values.
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' \dontrun{
#' library(magrittr)
#' library(dplyr)
#'
#' # Specify a path to a CSV file
#' path_to_csv <-
#'   system.file("examples/currencies.csv",
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
#' # View the graph's internal node data frame (ndf)
#' # with `get_node_df()` and dplyr's `as.tbl()`
#' graph_1 %>% get_node_df %>% as.tbl
#' #> Source: local data frame [171 x 7]
#' #>
#' #>    nodes  type label iso_4217_code curr_number
#' #>    (chr) (chr) (chr)         (chr)       (chr)
#' #> 1      1                       AED         784
#' #> 2      2                       AFN         971
#' #> 3      3                       ALL           8
#' #> 4      4                       AMD          51
#' #> 5      5                       ANG         532
#' #> 6      6                       AOA         973
#' #> 7      7                       ARS          32
#' #> 8      8                       AUD          36
#' #> 9      9                       AWG         533
#' #> 10    10                       AZN         944
#' #> ..   ...   ...   ...           ...         ...
#' #> Variables not shown: exponent (chr)
#' #>   currency_name (chr)
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
#' # View the first 3 lines of the graph's internal ndf
#' graph_2 %>% get_node_df %>% head(3) %>% as.tbl
#' #> Source: local data frame [3 x 6]
#' #>
#' #>   nodes     type label curr_number exponent
#' #>   (chr)    (chr) (chr)       (chr)    (chr)
#' #> 1     1 currency   AED         784        2
#' #> 2     2 currency   AFN         971        2
#' #> 3     3 currency   ALL           8        2
#' #> Variables not shown: currency_name (chr)
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
#' graph_3 %>% get_node_df %>% head(3) %>% as.tbl
#' #> Source: local data frame [3 x 5]
#' #>
#' #>   nodes     type label curr_number
#' #>   (chr)    (chr) (chr)       (chr)
#' #> 1     1 currency   AED         784
#' #> 2     2 currency   AFN         971
#' #> 3     3 currency   ALL           8
#' #> Variables not shown: currency_name (chr)
#' }
#' @export add_nodes_from_table

add_nodes_from_table <- function(graph,
                                 table,
                                 set_type = NULL,
                                 select_cols = NULL,
                                 drop_cols = NULL,
                                 rename_attrs = NULL,
                                 id_col = NULL,
                                 type_col = NULL,
                                 label_col = NULL) {

  if (inherits(table, "character")) {

    # Load in CSV file
    csv <- read.csv(table, stringsAsFactors = FALSE)
  } else if (inherits(table, "data.frame")) {
    # Rename `table` object as `csv`
    csv <- table
  }

  # Get numbers of rows in the CSV
  rows_in_csv <- nrow(csv)

  # Create an empty NDF and column bind
  # with the CSV data
  empty_ndf <-
    create_nodes(nodes = 1:rows_in_csv, label = FALSE)
  empty_ndf[1:rows_in_csv, 1] <- ""
  csv <- cbind(empty_ndf, csv)

  if (is.null(id_col)){
    if (node_count(graph) == 0) {
      starting_node <- 1
    } else {
      if (suppressWarnings(any(!(is.na(as.numeric(graph$nodes_df$nodes)))))){
        starting_node <-
          suppressWarnings(
            max(
              as.numeric(
                graph$nodes_df[
                  which(!is.na(
                    as.numeric(graph$nodes_df$nodes))),
                  1])) + 1)
      } else {
        starting_node <- 1
      }
    }
  }

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

    csv <- csv[,columns_retained]
  }

  # If values for `drop_cols` provided, filter the CSV
  # columns by those named columns
  if (is.null(select_cols) & !is.null(drop_cols)) {

    columns_retained <-
      which(!(colnames(csv) %in% drop_cols))

    csv <- csv[,columns_retained]
  }

  # Add column of ID values to `csv` object
  if (is.null(id_col)) {
    csv$nodes <-
      starting_node:(rows_in_csv + starting_node - 1)
  }

  # If values for `rename_attrs` provided, rename all
  # of the CSV columns by those replacement values
  # (number of new names should match number of columns
  # even after selecting or dropping columns)
  if (!is.null(rename_attrs)) {
    if (length(rename_attrs) != length(colnames(csv))) {
      stop(paste0("The number of values specified for column name changes ",
                  "does not match the number of columns available"))
    }
    colnames(csv) <- rename_attrs
  }

  # Optionally set the `label` attribute from a
  # specified column in the CSV (this copies data into
  # the `label` column)
  if (!is.null(label_col)) {
    if (any(colnames(csv) == label_col)) {
      csv$label <- csv[,which(colnames(csv) == label_col)]
    }
  }

  # Optionally set the `type` attribute from a
  # specified column in the CSV
  if (!is.null(type_col)) {
    if (any(colnames(csv) == type_col)) {
      colnames(csv)[which(colnames(csv) == type_col)] <- "type"
    }
  }

  # Optionally set the `id` attribute from a
  # specified column in the CSV
  if (!is.null(id_col)) {
    if (any(colnames(csv) == id_col)) {
      colnames(csv)[which(colnames(csv) == id_col)] <- "nodes"
    }
  }

  # Optionally set the `type` attribute with a single
  # value repeated down
  if (!is.null(set_type)) {
    csv$type <- set_type
  }

  # Add as a node data frame to the graph
  graph <- add_node_df(graph, csv)

  return(graph)
}
