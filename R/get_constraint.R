#' Get constraint scores for one or more graph nodes
#'
#' Get the constraint scores (based on Burt's Constraint Index) for one or more
#'   nodes in a graph.
#' @inheritParams render_graph
#' @param nodes an optional vector of node IDs to consider for constraint
#'   scores. If not supplied, then constraint scores for all nodes in the graph
#'   will be calculated.
#' @return a data frame with constraint scores for one or more graph nodes.
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
#' # Get the constaint scores for all
#' # nodes in the graph
#' graph %>% get_constraint()
#'
#' # Get the constaint scores
#' # for nodes `5` and `7`
#' graph %>%
#'   get_constraint(
#'     nodes = c(5, 7))
#'
#' # Add the constraint scores
#' # to the graph as a node
#' # attribute
#' graph <-
#'   graph %>%
#'   join_node_attrs(
#'     df = get_constraint(.))
#'
#' # Display the graph's node data frame
#' graph %>% get_node_df()
#' @importFrom influenceR constraint
#' @export
get_constraint <- function(graph,
                           nodes = NULL) {

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

      emit_error(
        fcn_name = fcn_name,
        reasons = "One or more nodes provided not in graph")
    }

    constraint_scores_df <-
      constraint_scores_df[
        which(constraint_scores_df[,1] %in%
                as.character(nodes)),]

    rownames(constraint_scores_df) <- NULL
  }

  constraint_scores_df
}
