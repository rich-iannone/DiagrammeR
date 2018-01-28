#' Add edges and attributes to graph
#' from a table
#' @description Add edges and their
#' attributes to an existing graph
#' object from data in a CSV file or a
#' data frame.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @param table either a path to a
#' CSV file, or, a data frame object.
#' @param from_col the name of the
#' table column from which edges
#' originate.
#' @param to_col the name of the table
#' column to which edges terminate.
#' @param from_to_map a single
#' character value for the mapping of
#' the \code{from} and \code{to} columns
#' in the external table (supplied as
#' \code{from_col} and \code{to_col},
#' respectively) to a column in the
#' graph's internal node data frame (ndf).
#' @param rel_col an option to apply
#' a column of data in the table as
#' \code{rel} attribute values.
#' @param set_rel an optional string
#' to apply a \code{rel} attribute to
#' all edges created from the table
#' records.
#' @param drop_cols an optional
#' column selection statement for
#' dropping columns from the external
#' table before inclusion as attributes
#' in the graph's internal edge data
#' frame. Several columns can be
#' dropped by name using the syntax
#' \code{col_1 & col_2 & ...}. Columns
#' can also be dropped using a numeric
#' column range with \code{:}
#' (e.g., \code{5:8}), or, by using
#' the \code{:} between column names
#' to specify the range (e.g.,
#' \code{col_5_name:col_8_name}).
#' @return a graph object of class
#' \code{dgr_graph}.
#' @examples
#' # Create an empty graph and then
#' # add nodes to it from the
#' # `currencies` dataset available
#' # in the package
#' graph <-
#'   create_graph() %>%
#'   add_nodes_from_table(
#'     table = currencies)
#'
#' # Now we want to add edges to the
#' # graph using an included dataset,
#' # `usd_exchange_rates`, which has
#' # exchange rates between USD and
#' # many other currencies; the key
#' # here is that the data in the
#' # `from` and `to` columns in the
#' # external table maps to graph
#' # node data available in the
#' # `iso_4217_code` column of the
#' # graph's internal node data frame
#' graph_1 <-
#'   graph %>%
#'     add_edges_from_table(
#'       table = usd_exchange_rates,
#'       from_col = from_currency,
#'       to_col = to_currency,
#'       from_to_map = iso_4217_code)
#'
#' # View part of the graph's
#' # internal edge data frame
#' graph_1 %>%
#'   get_edge_df() %>%
#'   head()
#'
#' # If you would like to assign
#' # any of the table's columns as the
#' # `rel` attribute, this can done
#' # with the `rel_col` argument; to
#' # set a static `rel` attribute for
#' # all edges created, use `set_rel`
#' graph_2 <-
#'   graph %>%
#'     add_edges_from_table(
#'       table = usd_exchange_rates,
#'       from_col = from_currency,
#'       to_col = to_currency,
#'       from_to_map = iso_4217_code,
#'       set_rel = "from_usd")
#'
#' # View part of the graph's internal
#' # edge data frame (edf)
#' graph_2 %>%
#'   get_edge_df() %>%
#'   head()
#' @importFrom utils read.csv
#' @importFrom stats setNames
#' @importFrom dplyr left_join select rename mutate bind_cols everything as_tibble
#' @importFrom rlang enquo UQ
#' @export add_edges_from_table

add_edges_from_table <- function(graph,
                                 table,
                                 from_col,
                                 to_col,
                                 from_to_map,
                                 rel_col = NULL,
                                 set_rel = NULL,
                                 drop_cols = NULL) {

  # Get the time of function start
  time_function_start <- Sys.time()

  # Get the requested `from_col`
  from_col <-
    rlang::enquo(from_col) %>% rlang::get_expr() %>% as.character()

  # Get the requested `to_col`
  to_col <-
    rlang::enquo(to_col) %>% rlang::get_expr() %>% as.character()

  # Get the requested `from_to_map`
  from_to_map <-
    rlang::enquo(from_to_map) %>% rlang::get_expr() %>% as.character()

  # Get the requested `rel_col`
  rel_col <-
    rlang::enquo(rel_col) %>% rlang::get_expr() %>% as.character()

  # Get the requested `drop_cols`
  drop_cols <-
    rlang::enquo(drop_cols) %>% rlang::get_expr() %>% as.character()

  if (length(rel_col) == 0) {
    rel_col <- NULL
  }

  if (length(drop_cols) == 0) {
    drop_cols <- NULL
  }

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {

    stop(
      "The graph object is not valid.",
      call. = FALSE)
  }

  # Validation: Graph contains nodes
  if (graph_contains_nodes(graph) == FALSE) {

    stop(
      "The graph contains no nodes, so, edges cannot be added.",
      call. = FALSE)
  }

  # Create bindings for specific variables
  rel <- id <- from <- to <- label_col <- NULL

  # Determine whether the table is a file connection
  # to a CSV file or a data frame
  if (inherits(table, "character")) {

    # Load in CSV file
    csv <- utils::read.csv(table, stringsAsFactors = FALSE)

  } else if (inherits(table, "data.frame")) {

    # Rename `table` object as `csv`
    csv <- table
  }

  # Verify that value for `from_col` is in the table
  if (!(from_col %in% colnames(csv))) {

    stop(
      "The value specified in `from_col` is not in the table.",
      call. = FALSE)
  }

  # Verify that value for `to_col` is in the table
  if (!(to_col %in% colnames(csv))) {

    stop(
      "The value specified in `to_col` is not in the table.",
      call. = FALSE)
  }

  # Verify that value for `from_to_map` is in the
  # graph's ndf
  if (!(from_to_map %in% colnames(get_node_df(graph)))) {

    stop(
      "The value specified in `from_to_map` is not in the graph.",
      call. = FALSE)
  }

  # Optionally set the `rel` attribute from a
  # specified column in the CSV
  if (!is.null(rel_col)) {

    if (any(colnames(csv) == rel_col)) {
      colnames(csv)[which(colnames(csv) == rel_col)] <- "rel"
      csv <- dplyr::mutate(csv, rel = as.character(rel))
    }
  }

  # Extract the ndf from the graph
  ndf <- graph$nodes_df

  # Exclude the `from` and `to` columns
  # from the `csv` table
  csv_data_excluding_from_to <-
    csv %>%
    dplyr::select(setdiff(colnames(csv), c(from_col, to_col)))

  # Get the `from` col
  col_from <-
    dplyr::as_tibble(csv) %>%
    dplyr::select(rlang::UQ(from_col)) %>%
    dplyr::left_join(
      ndf %>% select(id, rlang::UQ(from_to_map)),
      by = stats::setNames(from_to_map, from_col)) %>%
    dplyr::select(id) %>%
    dplyr::rename(from = id) %>%
    dplyr::mutate(from = as.integer(from))

  # Get the `to` col
  col_to <-
    dplyr::as_tibble(csv) %>%
    dplyr::select(rlang::UQ(to_col)) %>%
    dplyr::left_join(
      ndf %>% select(id, rlang::UQ(from_to_map)),
      by = stats::setNames(from_to_map, to_col)) %>%
    dplyr::select(id) %>%
    dplyr::rename(to = id) %>%
    dplyr::mutate(to = as.integer(to))

  # Combine the `from` and `to` columns together along
  # with a new `rel` column (filled with NAs) and additional
  # columns from the CSV
  edf <-
    col_from %>%
    dplyr::bind_cols(col_to) %>%
    dplyr::bind_cols(csv_data_excluding_from_to)

  # Add in a `rel` column (filled with NAs) if it's not
  # already in the table
  if (!("rel" %in% colnames(edf))) {
    edf <-
      edf %>%
      dplyr::mutate(rel = as.character(NA))
  }

  # Use the `select()` function to arrange the
  # column rows and then convert to a data frame
  edf <-
    edf %>%
    dplyr::select(from, to, rel, dplyr::everything()) %>%
    as.data.frame(stringsAsFactors = FALSE)

  # Remove any rows where there is an NA in either
  # `from` or `to`
  edf <- edf[which(!is.na(edf$from) & !is.na(edf$to)), ]
  rownames(edf) <- NULL

  # Add in an `id` column
  edf <-
    dplyr::bind_cols(
      data.frame(id = as.integer(1:nrow(edf)) + graph$last_edge),
      edf)

  # Optionally set the `rel` attribute with a single
  # value repeated down
  if (is.null(rel_col) & !is.null(set_rel)) {
    edf <-
      edf %>%
      dplyr::mutate(rel = as.character(set_rel))
  }

  # If values for `drop_cols` provided, filter the CSV
  # columns by those named columns
  if (!is.null(drop_cols)) {

    col_selection <- get_col_selection(col_selection_stmt = drop_cols)

    if (col_selection[["selection_type"]] == "column_range") {
      col_index_1 <- which(colnames(edf) == col_selection[["column_selection"]][1])
      col_index_2 <- which(colnames(edf) == col_selection[["column_selection"]][2])

      col_indices <- col_index_1:col_index_2 %>% sort()

      columns_retained <- base::setdiff(colnames(edf), colnames(edf)[col_indices])

    } else if (col_selection[["selection_type"]] == "column_index_range") {

      col_indices <- col_selection[["column_selection"]] %>% sort()

      columns_retained <- base::setdiff(colnames(edf), colnames(edf)[col_indices])

    } else if (col_selection[["selection_type"]] %in% c("single_column_name", "column_names")) {

      columns_retained <- base::setdiff(colnames(edf), col_selection[["column_selection"]])

    } else if (length(col_selection) == 0) {

      columns_retained <- colnames(edf)
    }

    edf <- edf[, c(columns_retained)]
  }

  # Get the number of edges in the graph
  edges_graph_1 <- graph %>% count_edges()

  # Add the edf to the graph object
  if (is.null(graph$edges_df)) {
    graph$edges_df <- edf
  } else {
    graph$edges_df <- dplyr::bind_rows(graph$edges_df, edf)
  }

  # Get the updated number of edges in the graph
  edges_graph_2 <- graph %>% count_edges()

  # Get the number of edges added to
  # the graph
  edges_added <- edges_graph_2 - edges_graph_1

  # Update the `last_edge` value in the graph
  graph$last_edge <- nrow(graph$edges_df)

  graph$graph_log <-
    add_action_to_log(
      graph_log = graph$graph_log,
      version_id = nrow(graph$graph_log) + 1,
      function_used = "add_edges_from_table",
      time_modified = time_function_start,
      duration = graph_function_duration(time_function_start),
      nodes = nrow(graph$nodes_df),
      edges = nrow(graph$edges_df),
      d_e = edges_added)

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
