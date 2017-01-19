#' Set a data frame as an edge attribute
#' @description From a graph object of class
#' \code{dgr_graph}, bind a data frame as an edge
#' attribute property for one given graph edge. The
#' data frames are stored in list columns within
#' a \code{df_tbl} object. A \code{df_id} value is
#' generated and serves as a pointer to the table
#' row that contains the ingested data frame.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @param edge the edge ID to which the data frame
#' will be bound as an attribute.
#' @param df the data frame to be bound to the
#' edge as an attribute.
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' # Create a node data frame (ndf)
#' ndf <-
#'   create_node_df(
#'     n = 4,
#'     type = "basic",
#'     label = TRUE,
#'     value = c(3.5, 2.6, 9.4, 2.7))
#'
#' # Create an edge data frame (edf)
#' edf <-
#'   create_edge_df(
#'     from = c(1, 2, 3),
#'     to = c(4, 3, 1),
#'     rel = "leading_to")
#'
#' # Create a graph
#' graph <-
#'   create_graph(
#'     nodes_df = ndf,
#'     edges_df = edf)
#'
#' # Create a simple data frame to add as
#' # an edge attribute
#' df <-
#'   data.frame(
#'     a = c("one", "two", "three"),
#'     b = c(1, 2, 3))
#'
#' # Bind the data frame as an edge attribute
#' # to the edge with ID `1`
#' graph <-
#'   set_df_as_edge_attr(
#'     graph = graph,
#'     edge = 1,
#'     df = df)
#' @importFrom tidyr nest
#' @importFrom dplyr everything bind_rows mutate
#' @importFrom tibble tibble as_tibble
#' @export set_df_as_edge_attr

set_df_as_edge_attr <- function(graph,
                                edge,
                                df) {

  # Get the time of function start
  time_function_start <- Sys.time()

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {
    stop("The graph object is not valid.")
  }

  # Validation: Graph contains edges
  if (graph_contains_edges(graph) == FALSE) {
    stop("The graph contains no edges, so, a df cannot be added.")
  }

  # Value given for edge must only be a single value
  if (length(edge) > 1) {
    stop("Only one edge can be specified.")
  }

  # Values given for edge must correspond to an edge ID
  # in the graph
  if (!(edge %in% graph$edges_df$id)) {
    stop("The value given for `edge` does not correspond to an edge ID.")
  }

  # Create nested data frame using `df` in a list column
  df_nested <- tidyr::nest(df, dplyr::everything())

  # Create the `df_storage` tibble
  df_storage <-
    data.frame(
      df_id = as.character(NA),
      df_data = as.character(NA),
      stringsAsFactors = FALSE)[-1, ] %>%
    tibble::as_tibble()

  # Generate a random 8-character, alphanumeric
  # string to use as a data frame ID (`df_id`)
  df_id <-
    replicate(
      8, sample(c(LETTERS, letters, 0:9), 1)) %>%
    paste(collapse = "")

  # Generate row with table
  df_row <-
    tibble::tibble(df_id = df_id) %>%
    dplyr::mutate(df_data = df_nested)

  # Bind the new row with `df_id` and `df_data` to `df_storage`
  df_storage <- suppressWarnings(rbind(df_storage, df_row))

  # Bind the `df_storage` tibble to an existing `df_storage` object
  # if it exists in the graph
  if (!is.null(graph$df_storage)) {
    graph$df_storage <-
      dplyr::bind_rows(graph$df_storage, df_storage)
  } else {
    graph$df_storage <- df_storage
  }

  # Set the `df_id` edge attribute using the
  # `set_edge_attrs()` function
  graph <-
    set_edge_attrs(
      x = graph,
      edge_attr = "df_id",
      values = df_id,
      from = (graph %>% get_edges(mk_cond("id", "==", edge), return_type = "list"))[[1]],
      to = (graph %>% get_edges(mk_cond("id", "==", edge), return_type = "list"))[[2]])

  # Update the `graph_log` df with an action
  graph$graph_log <-
    graph$graph_log[-nrow(graph$graph_log),] %>%
    add_action_to_log(
      version_id = nrow(graph$graph_log) + 1,
      function_used = "set_df_as_edge_attr",
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
