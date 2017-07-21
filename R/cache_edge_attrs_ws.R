#' Cache edge attributes (based on a selection of
#' edges) in the graph
#' @description From a graph object of class
#' \code{dgr_graph}, get edge attribute properties for
#' edges available in a selection and cache those
#' values in the graph for later retrieval using
#' \code{get_cache}.
#'
#' Selections of edges can be performed using
#' the following \code{select_...} functions:
#' \code{select_edges()},
#' \code{select_last_edge()}, or
#' \code{select_edges_by_node_id()}.
#' Selections of edges can also be performed using
#' the following traversal functions:
#' \code{trav_out_edge()}, \code{trav_in_edge()},
#' or \code{trav_both_edge()}.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @param edge_attr the edge attribute from which to
#' obtain values.
#' @param name an optional name for the cached vector.
#' @param mode a option to recast the returned vector
#' of edge attribute value as \code{numeric} or
#' \code{character}.
#' @return a graph object of class \code{dgr_graph}.
#' # Set a seed
#' set.seed(23)
#'
#' # Create a graph with 6 nodes and 5 edges
#' graph <-
#'   create_graph() %>%
#'   add_path(n = 6) %>%
#'   set_edge_attrs(
#'     edge_attr = "value",
#'     values = rnorm(edge_count(.), 5, 2))
#'
#' # Select all edges where the edge attribute
#' # `value` is less than 5
#' graph <-
#'   graph %>%
#'   select_edges(
#'     conditions = "value < 5.0")
#'
#' # Show the graph's edge data frame
#' graph %>%
#'   get_edge_df()
#' #>   id from to  rel    value
#' #> 1  1    1  2 <NA> 4.595777
#' #> 2  2    2  3 <NA> 5.355001
#' #> 3  3    3  4 <NA> 7.615295
#' #> 4  4    4  5 <NA> 2.314129
#' #> 5  5    5  6 <NA> 6.722166
#'
#' # Cache available values from the edge
#' # attribute `value` from the edges that
#' # are selected; ensure that the cached
#' # vector is numeric
#' graph <-
#'   graph %>%
#'   cache_edge_attrs_ws(
#'     edge_attr = "value",
#'     name = "edge_value")
#'
#' # Get the cached vector with `get_cache()`
#' graph %>%
#'   get_cache(name = "edge_value")
#' #> [1] 4.595777 2.314129
#' @importFrom dplyr filter select_ rename_ mutate
#' @export cache_edge_attrs_ws

cache_edge_attrs_ws <- function(graph,
                                edge_attr,
                                name = NULL,
                                mode = NULL) {

  # Get the time of function start
  time_function_start <- Sys.time()

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {
    stop("The graph object is not valid.")
  }

  # Validation: Graph object has valid edge selection
  if (graph_contains_edge_selection(graph) == FALSE) {
    stop("There is no selection of edges available.")
  }

  # Create bindings for specific variables
  id <- to_cache <- NULL

  # Extract the graph's internal edf
  edges_df <- graph$edges_df

  # Stop function if value for `edge_attr` is not
  # a valid edge attribute
  if (!(edge_attr %in% colnames(edges_df)[-c(1:3)])) {
    stop("The value provided in `edge_attr` is not a valid edge attribute.")
  }

  # Get the selection of edge ID values
  edge_ids <- graph$edge_selection$edge

  # Get the values to cache in a data frame
  edges_cache <-
    edges_df %>%
    dplyr::filter(id %in% edge_ids) %>%
    dplyr::select_(edge_attr) %>%
    dplyr::rename_(.dots = setNames(edge_attr, "to_cache"))

  # If `numeric` or `character` supplied in `mode`,
  # coerce the values to cache accordingly
  if (!is.null(mode)) {
    if (mode == "numeric") {
      edges_cache <-
        edges_cache %>%
        dplyr::mutate(to_cache = as.numeric(to_cache))
    }

    if (mode == "character") {
      edges_cache <-
        edges_cache %>%
        dplyr::mutate(to_cache = as.character(to_cache))
    }
  }

  # Cache vector of edge attributes in the
  # graph's `cache` list object
  if (!is.null(name)) {
    graph$cache[[name]] <- edges_cache[, 1]
  } else {
    if (length(graph$cache) == 0) {
      graph$cache[[1]] <- edges_cache[, 1]
      names(graph$cache) <- 1
    } else {
      graph$cache[[(length(graph$cache) + 1)]] <-
        edges_cache[, 1]
    }
  }

  # Update the `graph_log` df with an action
  graph$graph_log <-
    add_action_to_log(
      graph_log = graph$graph_log,
      version_id = nrow(graph$graph_log) + 1,
      function_used = "cache_edge_attrs_ws",
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
