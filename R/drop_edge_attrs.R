#' Drop an edge attribute column
#' @description Within a graph's internal edge data
#' frame (edf), remove an existing edge attribute.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @param edge_attr the name of the edge attribute
#' column to drop.
#' @return a graph object of class
#' \code{dgr_graph}.
#' @examples
#' # Create a random graph
#' graph <-
#'   create_random_graph(
#'     n = 5, m = 6,
#'     set_seed = 23) %>%
#'   set_edge_attrs(
#'     edge_attr = value,
#'     values = 3) %>%
#'   mutate_edge_attrs(
#'     edge_attr_from = value,
#'     expressions = "2 * ~",
#'     edge_attr_to = penwidth)
#'
#' # Get the graph's internal edf to show which
#' # edge attributes are available
#' get_edge_df(graph)
#' #>   id from to  rel value penwidth
#' #> 1  1    2  3 <NA>     3        6
#' #> 2  2    3  5 <NA>     3        6
#' #> 3  3    3  4 <NA>     3        6
#' #> 4  4    2  4 <NA>     3        6
#' #> 5  5    2  5 <NA>     3        6
#' #> 6  6    4  5 <NA>     3        6
#'
#' # Drop the `value` edge attribute
#' graph <-
#'   graph %>%
#'   drop_edge_attrs(
#'     edge_attr = value)
#'
#' # Get the graph's internal edf to show that
#' # the edge attribute `value` had been removed
#' get_edge_df(graph)
#' #>   id from to  rel penwidth
#' #> 1  1    2  3 <NA>        6
#' #> 2  2    3  5 <NA>        6
#' #> 3  3    3  4 <NA>        6
#' #> 4  4    2  4 <NA>        6
#' #> 5  5    2  5 <NA>        6
#' #> 6  6    4  5 <NA>        6
#' @importFrom rlang enquo UQ
#' @export drop_edge_attrs

drop_edge_attrs <- function(graph,
                            edge_attr) {

  # Get the time of function start
  time_function_start <- Sys.time()

  edge_attr <- rlang::enquo(edge_attr)
  edge_attr <- (rlang::UQ(edge_attr) %>% paste())[2]

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {
    stop("The graph object is not valid.")
  }

  # Stop function if length of `edge_attr` is
  # greater than one
  if (length(edge_attr) > 1) {
    stop("You can only provide a single column.")
  }

  # Stop function if `edge_attr` is any of
  # `from`, `to`, or `rel`
  if (any(c("from", "to", "rel") %in%
          edge_attr)) {
    stop("You cannot drop this column.")
  }

  # Extract the graph's edf
  edges <- get_edge_df(graph)

  # Get column names from the graph's edf
  column_names_graph <- colnames(edges)

  # Stop function if `edge_attr` is not one
  # of the graph's column
  if (!any(column_names_graph %in% edge_attr)) {
    stop("The edge attribute to drop is not in the ndf.")
  }

  # Get the column number for the edge attr to drop
  col_num_drop <-
    which(colnames(edges) %in% edge_attr)

  # Remove the column
  edges <- edges[, -col_num_drop]

  # Update the graph object
  graph$edges_df <- edges

  # Update the `graph_log` df with an action
  graph$graph_log <-
    add_action_to_log(
      graph_log = graph$graph_log,
      version_id = nrow(graph$graph_log) + 1,
      function_used = "drop_edge_attrs",
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
