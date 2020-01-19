#' Add nodes from distinct values in data frame columns
#'
#' Add new nodes to a graph object of class `dgr_graph` using distinct values
#' from one or more columns in a data frame. The values will serve as node
#' labels and the number of nodes added depends on the number of distinct values
#' found in the specified columns.
#'
#' @inheritParams render_graph
#' @param df A data frame from which values will be taken as new nodes for the
#'   graph.
#' @param columns A character vector of column names or a numeric vector of
#'   column numbers for the data frame supplied in `df`. The distinct values in
#'   these columns will serve as labels for the nodes added to the graph.
#' @param type An optional, single-length character vector that provides a group
#'   identifier for the nodes to be added to the graph.
#' @param keep_duplicates An option to exclude incoming nodes where the labels
#'   (i.e., values found in columns of the specified `df`) match label values
#'   available in the graph's nodes. By default, this is set to `FALSE`.
#'
#' @return A graph object of class `dgr_graph`.
#'
#' @examples
#' # Create an empty graph
#' graph <- create_graph()
#'
#' # Create a data frame from
#' # which several columns have
#' # values designated as graph nodes
#' df <-
#'   data.frame(
#'     col_1 = c("f", "p", "q"),
#'     col_2 = c("q", "x", "f"),
#'     col_3 = c(1, 5, 3),
#'     col_4 = c("a", "v", "h"),
#'     stringsAsFactors = FALSE)
#'
#' # Add nodes from columns `col_1`
#' # and `col_2` from the data frame
#' # to the graph object
#' graph <-
#'   graph %>%
#'   add_nodes_from_df_cols(
#'     df = df,
#'     columns = c("col_1", "col_2"))
#'
#' # Show the graph's node data
#' # frame; duplicate labels are
#' # prevented with `keep_duplicates =
#' # FALSE`)
#' graph %>% get_node_df()
#'
#' # Add new nodes from columns 3 and 4;
#' # We can specify the columns by their
#' # numbers as well
#' graph <-
#'   graph %>%
#'   add_nodes_from_df_cols(
#'     df = df,
#'     columns = 3:4)
#'
#' # Show the graph's node data
#' # frame; note that nodes didn't
#' # get made with columns that
#' # are not character class columns
#' graph %>% get_node_df()
#'
#' @export
add_nodes_from_df_cols <- function(graph,
                                   df,
                                   columns,
                                   type = NULL,
                                   keep_duplicates = FALSE) {

  # Get the time of function start
  time_function_start <- Sys.time()

  # Get the name of the function
  fcn_name <- get_calling_fcn()

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {

    emit_error(
      fcn_name = fcn_name,
      reasons = "The graph object is not valid")
  }

  # Get the df column numbers from which nodes
  # will be generated
  if (inherits(columns, "numeric")) {

    # Verify that the none of the values provided
    # are greater than the number of df columns
    if (max(columns) > ncol(df)) {

      emit_error(
        fcn_name = fcn_name,
        reasons = "One or more of the column numbers exceeds the number of columns in `df`")
    }
  }

  # Get column numbers from the column names
  # provided and verify that at least 1 column
  # number is returned
  if (inherits(columns, "character")) {
    columns <- which(colnames(df) %in% columns)

    if (length(columns) < 1) {

      emit_error(
        fcn_name = fcn_name,
        reasons = "None of the columns specified are in the `df` object")
    }
  }

  # Get the number of nodes in the graph
  nodes_graph_1 <- graph %>% count_nodes()

  # Isolate the relevant columns in the data frame;
  # Exclude any columns that are not character class
  df <-
    dplyr::as_tibble(df) %>%
    dplyr::select(columns) %>%
    dplyr::select_if(is.character)

  # Create an empty `nodes` vector
  nodes <- vector(mode = "character")

  # Obtain a vector of values from each column
  # in the tibble object
  for (i in 1:ncol(df)) {
    nodes <-
      c(nodes,
        df[, i] %>%
          purrr::flatten_chr() %>%
          trimws() %>%
          stringr::str_split(" ") %>%
          purrr::flatten_chr() %>%
          tibble::enframe(name = NULL) %>%
          tidyr::drop_na() %>%
          dplyr::distinct() %>%
          purrr::flatten_chr())
  }

  # Get the unique set of nodes
  nodes <- unique(nodes)

  # If `keep_duplicates` is set to FALSE, exclude
  # duplicate labels from being added to the graph
  if (keep_duplicates == FALSE) {
    existing_labels <- graph$nodes_df$label
    nodes <- base::setdiff(nodes, existing_labels)
  }

  # Get the number of nodes
  n <- length(nodes)

  # If there are any unique labels, create an ndf
  # of the correct length
  if (n > 0) {

    if (is.null(type)) {
      new_nodes <-
        create_node_df(
          n = n,
          label = nodes)
    } else {
      new_nodes <-
        create_node_df(
          n = n,
          type = type,
          label = nodes)
    }

    # Renumber the node ID values based on the
    # last node in the graph
    new_nodes[, 1] <- new_nodes[, 1] + graph$last_node

    # Add `new_nodes` ndf to the graph
    graph$nodes_df <-
      dplyr::bind_rows(graph$nodes_df, new_nodes)

    # Update the `last_node` counter
    graph$last_node <- graph$last_node + n
  }

  # Get the updated number of nodes in the graph
  nodes_graph_2 <- graph %>% count_nodes()

  # Get the number of nodes added to
  # the graph
  nodes_added <- nodes_graph_2 - nodes_graph_1

  # Update the `graph_log` df with an action
  graph$graph_log <-
    add_action_to_log(
      graph_log = graph$graph_log,
      version_id = nrow(graph$graph_log) + 1,
      function_used = fcn_name,
      time_modified = time_function_start,
      duration = graph_function_duration(time_function_start),
      nodes = nrow(graph$nodes_df),
      edges = nrow(graph$edges_df),
      d_n = nodes_added)

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
