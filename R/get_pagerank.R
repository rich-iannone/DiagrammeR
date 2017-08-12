#' Get the PageRank values for nodes in the graph
#' @description Get the PageRank values for
#' all nodes in the graph.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @param directed if \code{TRUE} (the default)
#' then directed paths will be considered for
#' directed graphs. This is ignored for undirected
#' graphs.
#' @param damping the damping factor. The default
#' value is set to \code{0.85}.
#' @return a data frame with PageRank values for
#' each of the nodes.
#' @examples
#' # Create a random graph
#' graph <-
#'   create_random_graph(
#'     n = 10, m = 22,
#'     set_seed = 23)
#'
#' # Get the alpha centrality scores for nodes
#' # in the graph
#' get_pagerank(graph)
#' #>    id   pagerank
#' #> 1   1 0.04608804
#' #> 2   2 0.04608804
#' #> 3   3 0.05392301
#' #> 4   4 0.04608804
#' #> 5   5 0.07677500
#' #> 6   6 0.11684759
#' #> 7   7 0.07899491
#' #> 8   8 0.08898857
#' #> 9   9 0.16945368
#' #> 10 10 0.27675311
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
#' @importFrom igraph page_rank
#' @export get_pagerank

get_pagerank <- function(graph,
                         directed = TRUE,
                         damping = 0.85) {

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {
    stop("The graph object is not valid.")
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
    pagerank = pagerank_values,
    stringsAsFactors = FALSE)
}
