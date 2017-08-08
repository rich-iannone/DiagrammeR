#' Add edges and attributes to graph from a table
#' @description Add edges and their attributes to an
#' existing graph object from data in a CSV file or a
#' data frame.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @param table either a path to a CSV file, or, a data
#' frame object.
#' @param from_col the name of the table column from
#' which edges originate.
#' @param to_col the name of the table column to
#' which edges terminate.
#' @param ndf_mapping a single character value for
#' the mapping of the \code{from} and \code{to} columns
#' in the external table (supplied as \code{from_col}
#' and \code{to_col}, respectively) to a column in the
#' graph's internal node data frame (ndf).
#' @param rel_col an option to apply a column of data
#' in the table as \code{rel} attribute values.
#' @param set_rel an optional string to apply a
#' \code{rel} attribute to all edges created from the
#' table records.
#' @param drop_cols an optional character vector for
#' dropping columns from the incoming data.
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' \dontrun{
#' # Create an empty graph and then add
#' # nodes to it from a CSV file; in this case
#' # we are using the `currencies` CSV file
#' # that's available in the package
#' graph <-
#'   create_graph() %>%
#'   add_nodes_from_table(
#'     system.file("extdata", "currencies.csv",
#'                 package = "DiagrammeR"))
#'
#' # Now we want to add edges to the graph
#' # using a similar CSV file that contains
#' # exchange rates between several currencies;
#' # the common attribute is the ISO-4217
#' # currency code
#' graph_1 <-
#'   graph %>%
#'   add_edges_from_table(
#'     system.file("extdata", "usd_exchange_rates.csv",
#'                 package = "DiagrammeR"),
#'     from_col = from_currency,
#'     to_col = to_currency,
#'     ndf_mapping = iso_4217_code)
#'
#' # View part of the graph's internal edge data
#' # frame (edf) using `get_edge_df()`
#' graph_1 %>%
#'   get_edge_df() %>%
#'   head()
#' #>   id from to  rel cost_unit
#' #> 1  1  148  1 <NA>  0.272300
#' #> 2  2  148  2 <NA>  0.015210
#' #> 3  3  148  3 <NA>  0.008055
#' #> 4  4  148  4 <NA>  0.002107
#' #> 5  5  148  5 <NA>  0.565000
#' #> 6  6  148  6 <NA>  0.006058
#'
#' # If you would like to assign any of the table's
#' # columns as `rel` attribute, this can done with
#' # the `rel_col` argument; to set a static `rel`
#' # attribute for all edges, use `set_rel`
#' graph_2 <-
#'   graph %>%
#'   add_edges_from_table(
#'     system.file("extdata", "usd_exchange_rates.csv",
#'                 package = "DiagrammeR"),
#'     from_col = from_currency,
#'     to_col = to_currency,
#'     ndf_mapping = iso_4217_code,
#'     set_rel = "from_usd")
#'
#' # View part of the graph's internal edge data
#' # frame (edf) using `get_edge_df()`
#' graph_2 %>%
#'   get_edge_df() %>%
#'   head()
#' #>   id from to      rel cost_unit
#' #> 1  1  148  1 from_usd  0.272300
#' #> 2  2  148  2 from_usd  0.015210
#' #> 3  3  148  3 from_usd  0.008055
#' #> 4  4  148  4 from_usd  0.002107
#' #> 5  5  148  5 from_usd  0.565000
#' #> 6  6  148  6 from_usd  0.006058
#' }
#' @importFrom utils read.csv
#' @importFrom stats setNames
#' @importFrom tibble as_tibble
#' @importFrom dplyr left_join select select_ rename mutate mutate_ bind_cols everything distinct
#' @importFrom tidyr unnest_ drop_na_
#' @importFrom rlang enquo UQ
#' @export add_edges_from_table

add_edges_from_table <- function(graph,
                                 table,
                                 from_col,
                                 to_col,
                                 ndf_mapping,
                                 rel_col = NULL,
                                 set_rel = NULL,
                                 drop_cols = NULL) {

  # Get the time of function start
  time_function_start <- Sys.time()

  from_col <- rlang::enquo(from_col)

  from_col <- (rlang::UQ(from_col) %>% paste())[2]

  to_col <- rlang::enquo(to_col)

  to_col <- (rlang::UQ(to_col) %>% paste())[2]

  ndf_mapping <- rlang::enquo(ndf_mapping)

  ndf_mapping <- (rlang::UQ(ndf_mapping) %>% paste())[2]

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {
    stop("The graph object is not valid.")
  }

  # Validation: Graph contains nodes
  if (graph_contains_nodes(graph) == FALSE) {
    stop("The graph contains no nodes, so, edges cannot be added.")
  }

  # Create bindings for specific variables
  rel <- id <- from <- to <- NULL

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
    stop("The value specified in `from_col` is not in the table.")
  }

  # Verify that value for `to_col` is in the table
  if (!(to_col %in% colnames(csv))) {
    stop("The value specified in `to_col` is not in the table.")
  }

  # Verify that value for `ndf_mapping` is in the
  # graph's ndf
  if (!(ndf_mapping %in% colnames(get_node_df(graph)))) {
    stop("The value specified in `ndf_mapping` is not in the graph.")
  }

  # If values for `drop_cols` provided, filter the CSV
  # columns by those named columns
  if (!is.null(drop_cols)) {
    columns_retained <-
      which(!(colnames(csv) %in% drop_cols))
    csv <- csv[, columns_retained]
  }

  # Optionally set the `rel` attribute from a
  # specified column in the CSV
  if (!is.null(rel_col)) {
    if (any(colnames(csv) == rel_col)) {
      colnames(csv)[which(colnames(csv) == rel_col)] <- "rel"
      csv <- mutate(csv, rel = as.character(rel))
    }
  }

  # Extract the ndf from the graph
  ndf <- graph$nodes_df

  # Get the column names from `csv` into a list,
  # and, add `id` to the list; this list is used
  # for the standard evaluation version of dplyr's
  # `select()` (`select_()`)
  csv_colnames <- list()

  if (length(setdiff(colnames(csv), c(from_col, to_col))) > 0) {
    for (i in 1:length(setdiff(colnames(csv), c(from_col, to_col)))) {
      csv_colnames[i] <- setdiff(colnames(csv), c(from_col, to_col))[i]
    }
    csv_colnames[(length(setdiff(colnames(csv), c(from_col, to_col))) + 1)] <- "id"
  } else {
    csv_colnames[1] <- "id"
  }

  # Expand the df to capture several space-delimited
  # values in the `to` column; drop NA values in the
  # `to_col` and the `from_col` columns
  csv <-
    csv %>%
    dplyr::mutate_(.dots = setNames(paste0("strsplit(", to_col, ", \" \")"), to_col)) %>%
    tidyr::unnest_(to_col) %>%
    tidyr::drop_na_(to_col) %>%
    tidyr::drop_na_(from_col)

  # Get the `from` col
  col_from <-
    tibble::as_tibble(csv) %>%
    dplyr::left_join(ndf,
                     by = stats::setNames(ndf_mapping, from_col)) %>%
    dplyr::select_(.dots = csv_colnames) %>%
    dplyr::rename(from = id) %>%
    dplyr::mutate(from = as.integer(from))

  # Get the `to` col
  col_to <-
    tibble::as_tibble(csv) %>%
    dplyr::left_join(ndf,
                     by = stats::setNames(ndf_mapping, to_col)) %>%
    dplyr::distinct() %>%
    dplyr::select_(.dots = csv_colnames) %>%
    dplyr::rename(to = id) %>%
    dplyr::mutate(to = as.integer(to)) %>%
    dplyr::select(to)

  # Combine the `from` and `to` columns together along
  # with a new `rel` column (filled with NAs) and additional
  # columns from the CSV
  edf <-
    col_from %>%
    dplyr::bind_cols(col_to)

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
      data.frame(id = as.integer(1:nrow(edf))),
      edf)

  # Optionally set the `rel` attribute with a single
  # value repeated down
  if (is.null(rel_col) & !is.null(set_rel)) {
    edf <-
      edf %>%
      dplyr::mutate(rel = as.character(set_rel))
  }

  # Add the edf to the graph object
  if (is.null(graph$edges_df)) {
    graph$edges_df <- edf
  } else {
    graph$edges_df <- dplyr::bind_rows(graph$edges_df, edf)
  }

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
