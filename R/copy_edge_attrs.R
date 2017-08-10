#' Copy an edge attribute column and set the name
#' @description Within a graph's internal edge data
#' frame (edf), copy the contents an existing edge
#' attribute and create a distinct edge attribute
#' within the edf with a different attribute name.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @param edge_attr_from the name of the edge attribute
#' column from which values will be copied.
#' @param edge_attr_to the name of the new edge
#' attribute column to which the copied values will be
#' placed.
#' @return a graph object of class
#' \code{dgr_graph}.
#' @examples
#' # Create a random graph
#' graph <-
#'   create_random_graph(
#'   n = 5, m = 8,
#'   set_seed = 23) %>%
#'   set_edge_attrs(
#'     edge_attr = color,
#'     values = "green")
#'
#' # Get the graph's internal edf to show which
#' # edge attributes are available
#' get_edge_df(graph)
#' #>   id from to  rel color
#' #> 1  1    2  3 <NA> green
#' #> 2  2    3  5 <NA> green
#' #> 3  3    3  4 <NA> green
#' #> 4  4    2  4 <NA> green
#' #> 5  5    2  5 <NA> green
#' #> 6  6    4  5 <NA> green
#' #> 7  7    1  4 <NA> green
#' #> 8  8    1  3 <NA> green
#'
#' # Make a copy the `color` edge attribute as
#' # the `color_2` edge attribute
#' graph <-
#'   graph %>%
#'   copy_edge_attrs(
#'     edge_attr_from = color,
#'     edge_attr_to = color_2)
#'
#' # Get the graph's internal edf to show that the
#' # edge attribute had been copied
#' get_edge_df(graph)
#' #>   id from to  rel color color_2
#' #> 1  1    2  3 <NA> green   green
#' #> 2  2    3  5 <NA> green   green
#' #> 3  3    3  4 <NA> green   green
#' #> 4  4    2  4 <NA> green   green
#' #> 5  5    2  5 <NA> green   green
#' #> 6  6    4  5 <NA> green   green
#' #> 7  7    1  4 <NA> green   green
#' #> 8  8    1  3 <NA> green   green
#' @importFrom dplyr bind_cols
#' @importFrom rlang enquo UQ
#' @export copy_edge_attrs

copy_edge_attrs <- function(graph,
                            edge_attr_from,
                            edge_attr_to) {

  # Get the time of function start
  time_function_start <- Sys.time()

  edge_attr_from <- rlang::enquo(edge_attr_from)
  edge_attr_from <- (rlang::UQ(edge_attr_from) %>% paste())[2]

  edge_attr_to <- rlang::enquo(edge_attr_to)
  edge_attr_to <- (rlang::UQ(edge_attr_to) %>% paste())[2]

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {
    stop("The graph object is not valid.")
  }

  # Stop function if `edge_attr_from` and
  # `edge_attr_to` are identical
  if (edge_attr_from == edge_attr_to) {
    stop("You cannot make a copy with the same name.")
  }

  # Stop function if `edge_attr_to` is `from` or `to`
  if (any(c("from", "to") %in% edge_attr_to)) {
    stop("You cannot use those names.")
  }

  # Extract the graph's edf
  edges <- get_edge_df(graph)

  # Get column names from the graph's edf
  column_names_graph <- colnames(edges)

  # Stop function if `edge_attr_from` is not one
  # of the graph's column
  if (!any(column_names_graph %in% edge_attr_from)) {
    stop("The edge attribute to copy is not in the ndf.")
  }

  # Get the column number for the edge attr to copy
  col_num_copy_from <-
    which(colnames(edges) %in% edge_attr_from)

  # Copy the column using `bind_cols()`
  edges <-
    dplyr::bind_cols(
      edges,
      as.data.frame(
        edges[, col_num_copy_from],
        stringsAsFactors = FALSE))

  # Set the column name for the copied attr
  colnames(edges)[ncol(edges)] <- edge_attr_to

  # Modify the new graph object
  graph$edges_df <- edges

  # Update the `graph_log` df with an action
  graph$graph_log <-
    add_action_to_log(
      graph_log = graph$graph_log,
      version_id = nrow(graph$graph_log) + 1,
      function_used = "copy_edge_attrs",
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
