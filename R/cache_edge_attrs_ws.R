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
#' @param mode a option to recast the returned vector
#' of edge attribute value as \code{numeric} or
#' \code{character}.
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' # Set a seed
#' set.seed(25)
#'
#' # Create a graph with 10 nodes and 9 edges
#' graph <-
#'   create_graph() %>%
#'   add_n_nodes(10) %>%
#'   add_edges_w_string(
#'     "1->2 1->3 2->4 2->5 3->6
#'      3->7 4->8 4->9 5->10") %>%
#'   set_edge_attrs(
#'     "value", rnorm(edge_count(.), 5, 2))
#'
#' # Select all edges where the edge attribute `value`
#' # is less than 5
#' graph <-
#'   graph %>%
#'   select_edges("value < 5.0")
#'
#' # Cache available values from the edge attribute
#' # `value` from the edges that are selected; ensure
#' # that the cached vector is numeric
#' graph <-
#'   graph %>%
#'   cache_edge_attrs_ws("value", "numeric")
#'
#' # Get the cached vector and get its
#' # difference from 5
#' graph %>% get_cache() %>% {x <- .; 5 - x}
#' #> [1] 3.0002598 0.8910665 0.1157822
#' #> [4] 2.6499060 1.0958678
#' @export cache_edge_attrs_ws

cache_edge_attrs_ws <- function(graph,
                                edge_attr,
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


  edges_df <-
    get_edge_df(graph)[which(get_edge_df(graph)[,1]
                             %in% graph$selection$edges$from &
                               get_edge_df(graph)[,2]
                             %in% graph$selection$edges$to),]

  if (!is.null(edge_attr)) {
    if (any(edge_attr %in%
            colnames(edges_df)[-c(1:2)])) {

      edges_attr_vector <-
        edges_df[,which(colnames(edges_df) %in%
                          edge_attr)]

      if (!is.null(mode)) {
        if (mode == "numeric") {
          edges_attr_vector <-
            as.numeric(edges_attr_vector)

          edges_attr_vector <-
            edges_attr_vector[which(!is.na(edges_attr_vector))]
        }

        if (mode == "character") {
          edges_attr_vector <-
            as.character(edges_attr_vector)
        }
      }
    }

    # Cache vector of edge attributes  in the graph
    graph$cache <- edges_attr_vector

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

    return(graph)
  }
}
