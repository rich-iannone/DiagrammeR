#' Convert a DiagrammeR graph to an igraph one
#'
#' Convert a DiagrammeR graph to an igraph graph object.
#' @inheritParams render_graph
#' @return an igraph object.
#' @examples
#' # Create a random graph using the
#' # `add_gnm_graph()` function
#' graph <-
#'   create_graph() %>%
#'   add_gnm_graph(
#'     n = 36,
#'     m = 50,
#'     set_seed = 23)
#'
#' # Confirm that `graph` is a
#' # DiagrammeR graph by getting
#' # the object's class
#' class(graph)
#'
#' # Convert the DiagrammeR graph
#' # to an igraph object
#' ig_graph <- to_igraph(graph)
#'
#' # Get the class of the converted
#' # graph, just to be certain
#' class(ig_graph)
#'
#' # Get a summary of the igraph
#' # graph object
#' summary(ig_graph)
#' @importFrom igraph graph_from_data_frame
#' @importFrom dplyr select_
#' @export
to_igraph <- function(graph) {

  # Get the name of the function
  fcn_name <- get_calling_fcn()

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {

    emit_error(
      fcn_name = fcn_name,
      reasons = "The graph object is not valid")
  }

  # Extract the graph's node data frame
  ndf <- graph$nodes_df

  # Extract the graph's edge data frame and
  # exclude the `id` column
  edf <-
    graph$edges_df %>%
    dplyr::select_("-id")

  igraph::graph_from_data_frame(
    d = edf,
    directed = graph$directed,
    vertices = ndf)
}
