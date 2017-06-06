#' Join new edge attribute values using a data frame
#' @description Join new edge attribute values in a
#' left join using a data frame. The use of a left join
#' in this function allows for no possibility that
#' edges in the graph might be removed after the join.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @param df the data frame to use for joining.
#' @param by_graph optional specification of the column
#' in the graph's internal edge data frame for the left
#' join. If both \code{by_graph} and \code{by_df} are
#' not provided, then a natural join will occur if
#' there are columns in the graph's edf and in
#' \code{df} with identical names.
#' @param by_df optional specification of the column in
#' \code{df} for the left join. If both \code{by_graph}
#' and \code{by_df} are not provided, then a natural
#' join will occur if there are columns in the graph's
#' edf and in \code{df} with identical names.
#' @return a graph object of class
#' \code{dgr_graph}.
#' @examples
#' # Create a simple graph
#' graph <-
#'   create_graph() %>%
#'   add_n_nodes(n = 5) %>%
#'   add_edges_w_string(
#'     edges = "1->2 1->3 2->4 2->5 3->5")
#'
#' # Create a data frame with node ID values
#' # representing the graph edges (with `from` and `to`
#' # columns), and, a set of numeric values
#' set.seed(25)
#'
#' df <-
#'   data.frame(
#'     from = c(1, 1, 2, 2, 3),
#'     to = c(2, 3, 4, 5, 5),
#'     values = rnorm(5, 5))
#'
#' # Join the values in the data frame to the
#' # graph's edges; this works as a left join using
#' # identically-named columns in the graph and the df
#' # (in this case `from` and `to` are common to both)
#' graph <-
#'   graph %>%
#'   join_edge_attrs(df)
#'
#' # Get the graph's internal edf to show that the
#' # join has been made
#' get_edge_df(graph)
#' #>   id from to  rel   values
#' #> 1  1    1  2 <NA> 4.788166
#' #> 2  2    1  3 <NA> 3.958409
#' #> 3  3    2  4 <NA> 3.846692
#' #> 4  4    2  5 <NA> 5.321531
#' #> 5  5    3  5 <NA> 3.499870
#' @importFrom dplyr select everything
#' @export join_edge_attrs

join_edge_attrs <- function(graph,
                            df,
                            by_graph = NULL,
                            by_df = NULL) {

  # Get the time of function start
  time_function_start <- Sys.time()

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {
    stop("The graph object is not valid.")
  }

  if (is.null(by_graph) & !is.null(by_df)) {
    stop("Both column specifications must be provided.")
  }

  if (!is.null(by_graph) & is.null(by_df)) {
    stop("Both column specifications must be provided.")
  }

  # Create bindings for specific variables
  id <- from <- to <- rel <- NULL

  # Extract the graph's edf
  edges <- get_edge_df(graph)

  # Get column names from the graph's edf
  column_names <- colnames(edges)

  if (is.null(by_graph) & is.null(by_df)) {

    # Perform a left join on the `edges` data frame
    edges <- merge(edges, df, all.x = TRUE)
  }

  if (!is.null(by_graph) & !is.null(by_df)) {

    # Perform a left join on the `edges` data frame
    edges <-
      merge(edges, df,
            all.x = TRUE,
            by.x = by_graph,
            by.y = by_df)
  }

  # Get new column names in the revised edf
  new_col_names <-
    setdiff(colnames(edges), column_names)

  # Sort the columns in `edges`
  edges <-
    edges %>%
    dplyr::select(id, from, to, rel, dplyr::everything())

  # Modify the graph object
  graph$edges_df <- edges

  # Update the `graph_log` df with an action
  graph$graph_log <-
    add_action_to_log(
      graph_log = graph$graph_log,
      version_id = nrow(graph$graph_log) + 1,
      function_used = "join_edge_attrs",
      time_modified = time_function_start,
      duration = graph_function_duration(time_function_start),
      nodes = nrow(graph$nodes_df),
      edges = nrow(graph$edges_df))

  # Write graph backup if the option is set
  if (graph$graph_info$write_backups) {
    save_graph_as_rds(graph = graph)
  }

  return(graph)
}
