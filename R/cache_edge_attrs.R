#' Cache edge attributes in the graph
#' @description From a graph object of class
#' \code{dgr_graph}, get edge attribute properties for
#' one or more edges and cache those values in the
#' graph for later retrieval using \code{get_cache}.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @param edge_attr the edge attribute from which to
#' obtain values.
#' @param mode a option to recast the returned vector
#' of edge attribute value as \code{numeric} or
#' \code{character}.
#' @param from an optional vector of node IDs from
#' which the edge is outgoing for filtering the list of
#' edges present in the graph.
#' @param to an optional vector of node IDs to which
#' the edge is incoming for filtering the list of
#' edges present in the graph.
#' @return a graph object of class \code{dgr_graph}.
#' # Set a seed
#' set.seed(25)
#'
#' # Create a graph with 10 nodes and 9 edges
#' graph <-
#'   create_graph() %>%
#'   add_n_nodes(10) %>%
#'   add_edges_w_string(
#'     "1->2 1->3 2->4 2->5 3->6 3->7
#'      4->8 4->9 5->10") %>%
#'   set_edge_attrs(
#'     "value", rnorm(edge_count(.), 5, 2))
#'
#' # Cache all values from the edge attribute `value`
#' # as a numeric vector
#' graph <-
#'   graph %>%
#'   cache_edge_attrs("value", "numeric")
#'
#' # Get the mean from all values available in
#' # the cache
#' graph %>% get_cache() %>% mean()
#' #> [1] 4.62536
#' @export cache_edge_attrs

cache_edge_attrs <- function(graph,
                             edge_attr,
                             mode = NULL,
                             from = NULL,
                             to = NULL) {

  # Get the time of function start
  time_function_start <- Sys.time()

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {
    stop("The graph object is not valid.")
  }

  # Extract the graph's edge data frame
  edges_df <- graph$edges_df

  if (is.null(from) & !is.null(to)) {
    edges_df <-
      edges_df[which(edges_df$to %in% to), ]
  } else if (!is.null(from) & is.null(to)) {
    edges_df <-
      edges_df[which(edges_df$from %in% from), ]
  } else if (is.null(from) & is.null(to)) {
    edges_df <- edges_df
  } else {
    edges_df <-
      edges_df[which((edges_df$from %in% from) &
                       (edges_df$to %in% to)), ]
  }

  if (any(edge_attr %in%
          colnames(edges_df)[-c(1:2)])) {

    edges_attr_vector <-
      edges_df[, which(colnames(edges_df) %in%
                        edge_attr)]

    if (!is.null(mode)) {
      if (mode == "numeric") {
        edges_attr_vector <-
          as.numeric(edges_attr_vector)

        edges_attr_vector <-
          edges_attr_vector[which(!is.na(edges_attr_vector))]
      }

      if (mode == "character") {
        edges_attr_vector <- as.character(edges_attr_vector)
      }
    }
  }

  # Cache vector of edge attributes in the graph
  graph$cache <- edges_attr_vector

  # Update the `graph_log` df with an action
  graph$graph_log <-
    add_action_to_log(
      graph_log = graph$graph_log,
      version_id = nrow(graph$graph_log) + 1,
      function_used = "cache_edge_attrs",
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
