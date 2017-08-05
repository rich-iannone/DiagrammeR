#' Get constraint scores for one or more graph nodes
#' @description Get the constraint scores (based on
#' Burt's Constraint Index) for one or more nodes in a
#' graph.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @param nodes an optional vector of node IDs to
#' consider for constraint scores. If not supplied,
#' then constraint scores for all nodes in the graph
#' will be calculated.
#' @return a data frame with constraint scores for one
#' or more graph nodes.
#' @examples
#' # Create a random graph
#' graph <-
#'   create_random_graph(
#'     n = 10, m = 22,
#'     set_seed = 23)
#'
#' # Get the constaint scores for all
#' # nodes in the graph
#' get_constraint(graph)
#' #>    id constraint
#' #> 1   1  0.2986111
#' #> 2   2  0.6400000
#' #> 3   3  1.2222222
#' #> 4   4  0.4197531
#' #> 5   5  0.5000000
#' #> 6   6  0.3333333
#' #> 7   7  1.0000000
#' #> 8   8  0.0000000
#' #> 9   9  1.0000000
#' #> 10 10  0.0000000
#'
#' # Get the constaint scores for only nodes
#' # `5` and `7`
#' get_constraint(graph, nodes = c(5, 7))
#' #>   id constraint
#' #> 1  5        0.5
#' #> 2  7        1.0
#'
#' # Add the constraint scores to the graph
#' # as a node attribute
#' graph <-
#'   graph %>%
#'   join_node_attrs(
#'     df = get_constraint(.))
#'
#' # Display the graph's node data frame
#' get_node_df(graph)
#' #>    id type label value constraint
#' #> 1   1 <NA>     1   6.0  0.2986111
#' #> 2   2 <NA>     2   2.5  0.6400000
#' #> 3   3 <NA>     3   3.5  1.2222222
#' #> 4   4 <NA>     4   7.5  0.4197531
#' #> 5   5 <NA>     5   8.5  0.5000000
#' #> 6   6 <NA>     6   4.5  0.3333333
#' #> 7   7 <NA>     7  10.0  1.0000000
#' #> 8   8 <NA>     8  10.0  0.0000000
#' #> 9   9 <NA>     9   8.5  1.0000000
#' #> 10 10 <NA>    10  10.0  0.0000000
#' @importFrom influenceR constraint
#' @export get_constraint

get_constraint <- function(graph,
                           nodes = NULL) {

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {
    stop("The graph object is not valid.")
  }

  # Convert the graph to an igraph object
  ig_graph <- to_igraph(graph)

  # Get the constraint scores for each of the
  # graph's nodes
  constraint_scores <- influenceR::constraint(ig_graph)

  # Create df with betweenness scores
  constraint_scores_df <-
    data.frame(
      id = constraint_scores %>% names() %>% as.integer(),
      constraint = constraint_scores,
      stringsAsFactors = FALSE)

  # If vector of node IDs provided in `nodes` then
  # subset the output data frame
  if (!is.null(nodes)) {

    if (!all(as.character(nodes) %in%
             get_node_ids(graph))) {
      stop("One or more nodes provided not in graph.")
    }

    constraint_scores_df <-
      constraint_scores_df[
        which(constraint_scores_df[,1] %in%
                as.character(nodes)),]

    rownames(constraint_scores_df) <- NULL
  }

  constraint_scores_df
}
