#' Get histogram data for a graph's degree frequency
#'
#' Get histogram data for a graph's degree frequency. The bin width is set to 1
#'   and zero-value degrees are omitted from the output.
#' @inheritParams render_graph
#' @param mode using `total` (the default), degree considered for each node
#'   will be the total degree. With `in` and `out` the degree used
#'   will be the in-degree and out-degree, respectively.
#'
#' @return A data frame with degree counts.
#'
#' @examples
#' # Create a random graph using the
#' # `add_gnm_graph()` function
#' graph <-
#'   create_graph(
#'     directed = FALSE) %>%
#'   add_gnm_graph(
#'     n = 10,
#'     m = 15,
#'     set_seed = 23)
#'
#' # Get degree histogram data for
#' # the graph (reporting total degree)
#' graph %>%
#'   get_degree_histogram(
#'     mode = "total")
#'
#' @export
get_degree_histogram <- function(graph,
                                 mode = "total") {

  # Get the name of the function
  fcn_name <- get_calling_fcn()

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {

    emit_error(
      fcn_name = fcn_name,
      reasons = "The graph object is not valid")
  }

  # Validation: Graph contains nodes
  if (graph_contains_nodes(graph) == FALSE) {

    emit_error(
      fcn_name = fcn_name,
      reasons = "The graph contains no nodes")
  }

  # Convert the graph to an igraph object
  ig_graph <- to_igraph(graph)

  # Get the total degree histogram for the graph
  if (mode %in% c("all", "total", "both")) {

    deg_hist_df <-
      get_degree_distribution(graph) %>%
      dplyr::mutate(total_degree_hist = total_degree_dist * count_nodes(graph)) %>%
      dplyr::select(degree, total_degree_hist)
  }

  # Get the total in-degree distribution for the graph
  if (mode == "in") {

    deg_hist_df <-
      get_degree_distribution(graph, mode = "in") %>%
      dplyr::mutate(indegree_hist = indegree_dist * count_nodes(graph)) %>%
      dplyr::select(degree, indegree_hist)
  }

  # Get the total out-degree distribution for the graph
  if (mode == "out") {

     deg_hist_df <-
      get_degree_distribution(graph, mode = "out") %>%
      dplyr::mutate(outdegree_hist = outdegree_dist * count_nodes(graph)) %>%
      dplyr::select(degree, outdegree_hist)
  }

  deg_hist_df
}
