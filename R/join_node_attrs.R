#' Join new node attribute values using a data frame
#'
#' @description
#'
#' Join new node attribute values in a left join using a data frame. The use of
#' a left join in this function allows for no possibility that nodes in the
#' graph might be removed after the join.
#'
#' @inheritParams render_graph
#' @param df The data frame to use for joining.
#' @param by_graph Optional specification of the column in the graph's internal
#'   node data frame for the left join. If both `by_graph` and `by_df` are not
#'   provided, then a natural join will occur if there are columns in the
#'   graph's ndf and in `df` with identical names.
#' @param by_df Optional specification of the column in `df` for the left join.
#'   If both `by_graph` and `by_df` are not provided, then a natural join will
#'   occur if there are columns in the graph's ndf and in `df` with identical
#'   names.
#'
#' @return A graph object of class `dgr_graph`.
#'
#' @examples
#' # Set a seed
#' suppressWarnings(RNGversion("3.5.0"))
#' set.seed(23)
#'
#' # Create a simple graph
#' graph <-
#'   create_graph() %>%
#'   add_n_nodes(n = 5) %>%
#'   add_edges_w_string(
#'     edges = "1->2 1->3 2->4 2->5 3->5")
#'
#' # Create a data frame with node ID values and a
#' # set of numeric values
#' df <-
#'   data.frame(
#'     values = round(rnorm(6, 5), 2),
#'     id = 1:6)
#'
#' # Join the values in the data frame to the
#' # graph's nodes; this works as a left join using
#' # identically-named columns in the graph and the df
#' # (in this case the `id` column is common to both)
#' graph <-
#'   graph %>%
#'   join_node_attrs(
#'     df = df)
#'
#' # Get the graph's internal ndf to show that the
#' # join has been made
#' graph %>% get_node_df()
#'
#' # Get betweenness values for each node and
#' # add them as a node attribute (Note the
#' # common column name `id` in the different
#' # tables results in a natural join)
#' graph <-
#'   graph %>%
#'   join_node_attrs(
#'     df = get_betweenness(.))
#'
#' # Get the graph's internal ndf to show that
#' # this join has been made
#' graph %>% get_node_df()
#' @family node creation and removal
#' @export
join_node_attrs <- function(
    graph,
    df,
    by_graph = NULL,
    by_df = NULL
) {

  # Get the time of function start
  time_function_start <- Sys.time()

  # Validation: Graph object is valid
  check_graph_valid(graph)

  if (is.null(by_graph) && !is.null(by_df)) {

    cli::cli_abort(
      "Both column specifications must be provided.")
  }

  if (!is.null(by_graph) && is.null(by_df)) {

    cli::cli_abort(
      "Both column specifications must be provided.")
  }

  # Get the number of nodes ever created for
  # this graph
  nodes_created <- graph$last_node

  # Extract the graph's ndf
  nodes <- get_node_df(graph)

  # Get column names from the graph's ndf
  column_names_graph <- colnames(nodes)

  # Get column names from the df
  column_names_df <- colnames(df)

  if (is.null(by_graph) && is.null(by_df)) {

    # Perform a left join on the `nodes` data frame
    if ("id" %in% colnames(df)) {
      nodes <-
        merge(nodes, df,
              all.x = TRUE,
              by.x = "id",
              by.y = "id")
    } else {

      # Perform a left join on the `nodes` data frame
      nodes <- merge(nodes, df, all.x = TRUE)
    }
  }

  if (!is.null(by_graph) && !is.null(by_df)) {

    # Perform a left join on the `nodes` data frame
    nodes <-
      merge(nodes, df,
            all.x = TRUE,
            by.x = by_graph,
            by.y = by_df)
  }

  # Get new column names in the revised ndf
  new_col_names <-
    base::setdiff(colnames(nodes), column_names_graph)

  # Get the column numbers for the new columns
  col_numbers <-
    which(colnames(nodes) %in% new_col_names)

  # Ensure that the column ordering is correct
  nodes <-
    nodes %>% dplyr::relocate("id", "type", "label")

  # Modify the graph object
  graph$nodes_df <- nodes
  graph$last_node <- nodes_created

  # Get the name of the function
  fcn_name <- get_calling_fcn()

  # Update the `graph_log` df with an action
  graph$graph_log <-
    add_action_to_log(
      graph_log = graph$graph_log,
      version_id = nrow(graph$graph_log) + 1L,
      function_used = fcn_name,
      time_modified = time_function_start,
      duration = graph_function_duration(time_function_start),
      nodes = nrow(graph$nodes_df),
      edges = nrow(graph$edges_df))

  # Write graph backup if the option is set
  if (graph$graph_info$write_backups) {
    save_graph_as_rds(graph = graph)
  }

  graph
}
