#' Cache node attributes (based on a selection of
#' nodes) in the graph
#' @description From a graph object of class
#' \code{dgr_graph}, get node attribute properties for
#' nodes available in a selection and cache those
#' values in the graph for later retrieval using
#' \code{get_cache}.
#'
#' Selections of nodes can be performed using
#' the following \code{select_...} functions:
#' \code{select_nodes()},
#' \code{select_last_nodes_created()},
#' \code{select_nodes_by_degree()},
#' \code{select_nodes_by_id()}, or
#' \code{select_nodes_in_neighborhood()}.
#' Selections of nodes can also be performed using
#' the following traversal functions:
#' (\code{trav_...}):
#' \code{trav_out()}, \code{trav_in()},
#' \code{trav_both()}, \code{trav_in_node()},
#' \code{trav_out_node()}.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @param node_attr the node attribute from which to
#' obtain values.
#' @param name an optional name for the cached vector.
#' @param mode a option to recast the returned vector
#' of node attribute value as \code{numeric} or
#' \code{character}.
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' # Set a seed
#' set.seed(23)
#'
#' # Create a graph with 6 nodes and 5 edges
#' graph <-
#'   create_graph() %>%
#'   add_path(n = 6) %>%
#'   set_node_attrs(
#'     node_attr = value,
#'     values = rnorm(node_count(.), 5, 2))
#'
#' # Select all nodes where the node
#' # attribute `value` is less than 5
#' graph <-
#'   graph %>%
#'   select_nodes(
#'     conditions = value < 5.0)
#'
#' # Show the graph's node data frame
#' graph %>%
#'   get_node_df()
#' #>   id type label    value
#' #> 1  1 <NA>     1 5.090874
#' #> 2  2 <NA>     2 8.151559
#' #> 3  3 <NA>     3 5.436577
#' #> 4  4 <NA>     4 2.906929
#' #> 5  5 <NA>     5 4.422623
#' #> 6  6 <NA>     6 5.963101
#'
#' # Cache available values from the node attribute
#' # `value` from the nodes that are selected;
#' # ensure that the cached vector is numeric
#' graph <-
#'   graph %>%
#'   cache_node_attrs_ws(
#'     node_attr = value,
#'     name = "node_value")
#'
#' # Get the cached vector with `get_cache()`
#' graph %>%
#'   get_cache(name = "node_value")
#' #> [1] 2.906929 4.422623
#' @importFrom dplyr filter select_ rename_ mutate
#' @importFrom rlang enquo UQ
#' @export cache_node_attrs_ws

cache_node_attrs_ws <- function(graph,
                                node_attr,
                                name = NULL,
                                mode = NULL) {

  # Get the time of function start
  time_function_start <- Sys.time()

  node_attr <- rlang::enquo(node_attr)
  node_attr <- (rlang::UQ(node_attr) %>% paste())[2]

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {
    stop("The graph object is not valid.")
  }

  # Validation: Graph object has valid node selection
  if (graph_contains_node_selection(graph) == FALSE) {
    stop("There is no selection of nodes available.")
  }

  # Extract the graph's internal ndf
  nodes_df <- graph$nodes_df

  # Create bindings for specific variables
  id <- to_cache <- NULL

  # Stop function if value for `node_attr` is not
  # a valid node attribute
  if (!(node_attr %in% colnames(nodes_df)[-1])) {
    stop("The value provided in `node_attr` is not a valid node attribute.")
  }

  # Get the selection of node ID values
  node_ids <- graph$node_selection$node

  # Get the values to cache in a data frame
  nodes_cache <-
    nodes_df %>%
    dplyr::filter(id %in% node_ids) %>%
    dplyr::select_(node_attr) %>%
    dplyr::rename_(.dots = setNames(node_attr, "to_cache"))

  # If `numeric` or `character` supplied in `mode`,
  # coerce the values to cache accordingly
  if (!is.null(mode)) {
    if (mode == "numeric") {
      nodes_cache <-
        nodes_cache %>%
        dplyr::mutate(to_cache = as.numeric(to_cache))
    }

    if (mode == "character") {
      nodes_cache <-
        nodes_cache %>%
        dplyr::mutate(to_cache = as.character(to_cache))
    }
  }

  # Cache vector of edge attributes in the
  # graph's `cache` list object
  if (!is.null(name)) {
    graph$cache[[name]] <- nodes_cache[, 1]
  } else {
    if (length(graph$cache) == 0) {
      graph$cache[[1]] <- nodes_cache[, 1]
      names(graph$cache) <- 1
    } else {
      graph$cache[[(length(graph$cache) + 1)]] <-
        nodes_cache[, 1]
    }
  }

  # Update the `graph_log` df with an action
  graph$graph_log <-
    add_action_to_log(
      graph_log = graph$graph_log,
      version_id = nrow(graph$graph_log) + 1,
      function_used = "cache_node_attrs_ws",
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
