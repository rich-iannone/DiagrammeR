#' Get radiality centrality scores
#' @description Get the radiality centrality
#' for all nodes in a graph. These scores describe
#' the ease to which nodes can reach other nodes.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @param direction using \code{all} (the default), the
#' search will ignore edge direction while traversing
#' through the graph. With \code{out}, measurements of
#' paths will be from a node whereas with \code{in},
#' measurements of paths will be to a node.
#' @return a data frame with radiality centrality
#' scores for each of the nodes.
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
#' # Get the radiality scores for nodes in the graph
#' get_radiality(graph)
#' #>    id radiality
#' #> 1   1    2.3333
#' #> 2   2    3.0000
#' #> 3   3    2.6667
#' #> 4   4    2.8889
#' #> 5   5    2.5556
#' #> 6   6    2.4444
#' #> 7   7    2.6667
#' #> 8   8    2.7778
#' #> 9   9    2.1111
#' #> 10 10    2.3333
#'
#' # Add the radiality values
#' # to the graph as a node
#' # attribute
#' graph <-
#'   graph %>%
#'   join_node_attrs(
#'     df = get_radiality(.))
#'
#' # Display the graph's node data frame
#' get_node_df(graph)
#' #>    id type label radiality
#' #> 1   1 <NA>  <NA>    2.3333
#' #> 2   2 <NA>  <NA>    3.0000
#' #> 3   3 <NA>  <NA>    2.6667
#' #> 4   4 <NA>  <NA>    2.8889
#' #> 5   5 <NA>  <NA>    2.5556
#' #> 6   6 <NA>  <NA>    2.4444
#' #> 7   7 <NA>  <NA>    2.6667
#' #> 8   8 <NA>  <NA>    2.7778
#' #> 9   9 <NA>  <NA>    2.1111
#' #> 10 10 <NA>  <NA>    2.3333
#' @importFrom igraph distances diameter
#' @export get_radiality

get_radiality <- function(graph,
                          direction = "all") {

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {
    stop("The graph object is not valid.")
  }

  # Ensure that values provided for the
  # `direction` argument are from the
  # valid options
  if (!(direction %in% c("all", "in", "out"))) {
    stop("Valid options for `direction` are `all`, `in`, or `out`.")
  }

  # Get the number of nodes in the graph
  n_nodes <- count_nodes(graph)

  # Convert the graph to an igraph object
  ig_graph <- to_igraph(graph)

  # Get a matrix of shortest paths between all
  # pairs of nodes in the graph
  sp_matrix <-
    igraph::distances(
      graph = ig_graph,
      mode = direction,
      weights = NA)

  # Get the graph diameter
  diam <-
    igraph::diameter(
      graph = ig_graph,
      directed = ifelse(direction == "all", FALSE, TRUE))

  # Get the radiality values for all
  # nodes in the graph
  radiality_values <-
    apply(
      X = sp_matrix,
      MARGIN = 1,
      FUN = function(x) {
        if (all(x == Inf)) {
          return(0)
        }
        else {
          return(sum(diam + 1 - x[x != Inf])/(n_nodes - 1))
        }
      })

  # Create df with radiality scores
  data.frame(
    id = radiality_values %>%
      names() %>%
      as.integer(),
    radiality = radiality_values %>% round(4),
    stringsAsFactors = FALSE)
}
