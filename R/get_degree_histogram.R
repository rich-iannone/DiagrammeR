#' Get histogram data for a graph's degree frequency
#' @description Get histogram data for a graph's
#' degree frequency. The bin width is set to 1 and
#' zero-value degrees are omitted from the output.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @param mode using \code{total} (the default),
#' degree considered for each node will be the total
#' degree. With \code{in} and \code{out} the degree
#' used will be the in-degree and out-degree,
#' respectively.
#' @return a data frame with degree counts.
#' @examples
#' # Create a random, directed graph
#' # with 18 nodes and 22 edges
#' graph <-
#'   create_random_graph(
#'     n = 18, m = 22,
#'     set_seed = 23)
#'
#' # Get degree histogram data for
#' # the graph (reporting total degree)
#' graph %>%
#'   get_degree_histogram(
#'     mode = "total")
#' #>   degree total_degree_hist
#' #> 1      0                 1
#' #> 2      1                 4
#' #> 3      2                 4
#' #> 4      3                 4
#' #> 5      4                 5
#' @importFrom dplyr mutate select
#' @export get_degree_histogram

get_degree_histogram <- function(graph,
                                 mode = "total") {

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {
    stop("The graph object is not valid.")
  }

  # Validation: Graph contains nodes
  if (graph_contains_nodes(graph) == FALSE) {
    stop("The graph contains no nodes, so, a degree histogram cannot be produced.")
  }

  # Create bindings for specific variables
  total_degree_dist <- total_degree_hist <-
    indegree_dist <- indegree_hist <-
    outdegree_dist <- outdegree_hist <- NULL

  # Convert the graph to an igraph object
  ig_graph <- to_igraph(graph)

  # Get the total degree histogram for the graph
  if (mode %in% c("all", "total", "both")) {

    deg_hist_df <-
      get_degree_distribution(graph) %>%
      dplyr::mutate(total_degree_hist = total_degree_dist * node_count(graph)) %>%
      dplyr::select(degree, total_degree_hist)
  }

  # Get the total in-degree distribution for the graph
  if (mode == "in") {

    deg_hist_df <-
      get_degree_distribution(graph, mode = "in") %>%
      dplyr::mutate(indegree_hist = indegree_dist * node_count(graph)) %>%
      dplyr::select(degree, indegree_hist)
  }

  # Get the total out-degree distribution for the graph
  if (mode == "out") {

     deg_hist_df <-
      get_degree_distribution(graph, mode = "out") %>%
      dplyr::mutate(outdegree_hist = outdegree_dist * node_count(graph)) %>%
      dplyr::select(degree, outdegree_hist)
  }

  deg_hist_df
}
