#' Get histogram data for a graph's degree frequency
#' @description Get histogram data for a graph's
#' degree frequency. The bin width is set to 1 and
#' zero-value degrees are omitted from the output.
#' @param graph a graph object of class
#' \code{dgr_graph} that is created using
#' \code{create_graph}.
#' @return a named vector of degree frequencies (with
#' bin width equal to 1) where the degree values
#' serve as names.
#' @examples
#' library(magrittr)
#'
#' # Create a random, directed graph with 18 nodes
#' # and 22 edges
#' random_graph <-
#'   create_random_graph(
#'     n = 18,
#'     m = 22,
#'     directed = TRUE,
#'     fully_connected = TRUE,
#'     set_seed = 20) %>%
#'   set_global_graph_attrs(
#'     'graph', 'layout', 'sfdp') %>%
#'   set_global_graph_attrs(
#'     'graph', 'overlap', 'false')
#'
#' # Get degree histogram data for the `random_graph`
#' random_graph %>% get_degree_histogram
#' #> 1 2 3 4 5
#' #> 4 6 3 4 1
#' @export get_degree_histogram

get_degree_histogram <- function(graph) {

  return(table(node_info(graph)[,4]))
}
