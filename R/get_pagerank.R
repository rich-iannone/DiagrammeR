#' Get the PageRank values for all nodes
#'
#' Get the PageRank values for all nodes in the graph.
#'
#' @inheritParams render_graph
#' @param directed If `TRUE` (the default) then directed paths will be
#'   considered for directed graphs. This is ignored for undirected graphs.
#' @param damping The damping factor. The default value is set to `0.85`.
#'
#' @return A data frame with PageRank values for each of the nodes.
#'
#' @examples
#' # Create a random graph using the
#' # `add_gnm_graph()` function
#' graph <-
#'   create_graph() %>%
#'   add_gnm_graph(
#'     n = 10,
#'     m = 15,
#'     set_seed = 23)
#'
#' # Get the PageRank scores
#' # for all nodes in the graph
#' graph %>%
#'   get_pagerank()
#'
#' # Colorize nodes according to their
#' # PageRank scores
#' graph <-
#'   graph %>%
#'   join_node_attrs(
#'     df = get_pagerank(graph = .)) %>%
#'   colorize_node_attrs(
#'     node_attr_from = pagerank,
#'     node_attr_to = fillcolor,
#'     palette = "RdYlGn")
#'
#' @export
get_pagerank <- function(graph,
                         directed = TRUE,
                         damping = 0.85) {

  # Get the name of the function
  fcn_name <- get_calling_fcn()

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {

    emit_error(
      fcn_name = fcn_name,
      reasons = "The graph object is not valid")
  }

  # Convert the graph to an igraph object
  ig_graph <- to_igraph(graph)

  # Get the PageRank values for each of the
  # graph's nodes
  pagerank_values <-
    igraph::page_rank(
      graph = ig_graph,
      directed = directed,
      damping = damping)$vector

  # Create df with the PageRank values
  data.frame(
    id = pagerank_values %>%
      names() %>%
      as.integer(),
    pagerank = pagerank_values %>% round(4),
    stringsAsFactors = FALSE)
}
