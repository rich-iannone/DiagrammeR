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
#'     10, 22, set_seed = 1)
#'
#' # Get the constaint scores for all nodes in
#' # the graph
#' get_constraint(graph)
#' #>    id constraint
#' #> 1   1  0.3536111
#' #> 2   2  0.4172222
#' #> 3   3  0.4933333
#' #> 4   4  0.4528472
#' #> 5   5  0.3711188
#' #> 6   6  0.4583333
#' #> 7   7  0.3735494
#' #> 8   8  0.3072222
#' #> 9   9  0.4479167
#' #> 10 10  0.4447222
#'
#' # Get the constaint scores for only nodes
#' # `5` and `7`
#' get_constraint(graph, c(5, 7))
#' #>   id constraint
#' #> 1  5  0.3711188
#' #> 2  7  0.3735494
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
    data.frame(id = names(constraint_scores),
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

  return(constraint_scores_df)
}
