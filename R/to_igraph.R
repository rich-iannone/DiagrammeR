#' Convert a DiagrammeR graph to an igraph one
#' @description Convert a DiagrammeR graph to
#' an igraph graph object.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @return an igraph object.
#' @examples
#' # Create a DiagrammeR graph object
#' dgr_graph <-
#'   create_random_graph(
#'     36, 50, set_seed = 1)
#'
#' # Confirm that `dgr_graph` is a DiagrammeR graph
#' # by getting the object's class
#' class(dgr_graph)
#' #> [1] "dgr_graph"
#'
#' # Convert the DiagrammeR graph to an igraph object
#' ig_graph <-
#'   to_igraph(dgr_graph)
#'
#' # Get the class of the converted graph, just
#' # to be certain
#' class(ig_graph)
#' #> [1] "igraph"
#'
#' # Get a summary of the igraph graph object
#' summary(ig_graph)
#' #> IGRAPH UN-B 36 50 --
#' #> + attr: name (v/c), type (v/c), label
#' #> | (v/c), value (v/c), rel (e/c)
#' @importFrom igraph graph_from_data_frame
#' @export to_igraph

to_igraph <- function(graph) {

igraph_graph <-
  igraph::graph_from_data_frame(
    d = graph$edges,
    directed = graph$directed,
    vertices = graph$nodes)

return(igraph_graph)
}
