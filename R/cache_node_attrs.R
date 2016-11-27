#' Cache node attributes in the graph
#' @description From a graph object of class
#' \code{dgr_graph}, get node attribute properties for
#' one or more nodes and cache those values in the
#' graph for later retrieval using \code{get_cache}.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @param node_attr the node attribute from which to
#' obtain values.
#' @param mode a option to recast the returned vector
#' of node attribute value as \code{numeric} or
#' \code{character}.
#' @param nodes an optional vector of node IDs for
#' filtering the list of nodes present in the graph.
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' # Set a seed
#' set.seed(25)
#'
#' # Create a graph with 10 nodes and 9 edges
#' graph <-
#'   create_graph() %>%
#'   add_n_nodes(10) %>%
#'   set_node_attrs(
#'     "value", rnorm(node_count(.), 5, 2)) %>%
#'   add_edges_w_string(
#'     "1->2 1->3 2->4 2->5 3->6
#'      3->7 4->8 4->9 5->10")
#'
#' # Cache all values from the node attribute `value`
#' # as a numeric vector
#' graph <-
#'   graph %>%
#'   cache_node_attrs("value", "numeric")
#'
#' # Get the mean from all values available in
#' # the cache
#' graph %>% get_cache() %>% mean()
#' #> [1] 4.651246
#' @export cache_node_attrs

cache_node_attrs <- function(graph,
                             node_attr,
                             mode = NULL,
                             nodes = NULL) {

  # Get the time of function start
  time_function_start <- Sys.time()

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {
    stop("The graph object is not valid.")
  }

  # Get internal node data frame as an object
  nodes_df <- graph$nodes_df

  if (is.null(nodes)) {
    nodes_df <- nodes_df
  } else {
    nodes_df <-
      nodes_df[which(nodes_df[, 1] %in% nodes),]
  }

  if (any(node_attr %in%
          colnames(nodes_df)[-1])) {

    nodes_attr_vector <-
      nodes_df[, which(colnames(nodes_df) %in% node_attr)]

    if (!is.null(mode)) {
      if (mode == "numeric") {

        nodes_attr_vector <-
          as.numeric(nodes_attr_vector)

        nodes_attr_vector <-
          nodes_attr_vector[which(!is.na(nodes_attr_vector))]
      }

      if (mode == "character") {
        nodes_attr_vector <- as.character(nodes_attr_vector)
      }
    }
  }

  # Cache vector of node attributes in the graph
  graph$cache <- nodes_attr_vector

  # Update the `graph_log` df with an action
  graph$graph_log <-
    add_action_to_log(
      graph_log = graph$graph_log,
      version_id = nrow(graph$graph_log) + 1,
      function_used = "cache_node_attrs",
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
