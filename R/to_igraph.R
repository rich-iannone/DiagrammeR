#' Convert a DiagrammeR graph to an igraph one
#' @description Convert a DiagrammeR graph to
#' an igraph graph object.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @return an igraph object.
#' @examples
#' # Create a DiagrammeR graph object
#' graph <-
#'   create_random_graph(
#'     n = 36, m = 50,
#'     set_seed = 23)
#'
#' # Confirm that `graph` is a DiagrammeR graph
#' # by getting the object's class
#' class(graph)
#' #> [1] "dgr_graph"
#'
#' # Convert the DiagrammeR graph to an igraph object
#' ig_graph <- to_igraph(graph)
#'
#' # Get the class of the converted graph, just
#' # to be certain
#' class(ig_graph)
#' #> [1] "igraph"
#'
#' # Get a summary of the igraph graph object
#' summary(ig_graph)
#' #> IGRAPH DN-B 36 50 --
#' #> + attr: name (v/c), type (v/c), label
#' #> | (v/c), value (v/c), rel (e/c)
#' @importFrom igraph graph_from_data_frame
#' @importFrom dplyr select_
#' @export to_igraph

to_igraph <- function(graph) {

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {
    stop("The graph object is not valid.")
  }

  # Extract the graph's node data frame
  ndf <- graph$nodes_df

  # Extract the graph's edge data frame and
  # exclude the `id` column
  edf <-
    graph$edges_df %>%
    dplyr::select_("-id")

  igraph_graph <-
    igraph::graph_from_data_frame(
      d = edf,
      directed = graph$directed,
      vertices = ndf)

  return(igraph_graph)
}
