#' Invert selection of nodes or edges in a graph
#' @description Modify the selection of nodes or edges
#' within a graph object such that all nodes or edges
#' previously unselected will now be selected and vice
#' versa.
#' @param graph a graph object of class
#' \code{dgr_graph} that is created using
#' \code{create_graph}.
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' # Create a node data frame (ndf)
#' ndf <-
#'   create_node_df(
#'     n = 4,
#'     type = "standard")
#'
#' # Create an edge data frame (edf)
#' edf <-
#'   create_edge_df(
#'     from = c(1, 2, 3),
#'     to = c(4, 3, 1),
#'     rel = "leading_to")
#'
#' # Create a graph
#' graph <-
#'   create_graph(
#'     nodes_df = ndf,
#'     edges_df = edf)
#'
#' # Select nodes with ID values `1` and `3`
#' graph <-
#'   select_nodes(
#'     graph = graph,
#'     nodes = c(1, 3))
#'
#' # Verify that a node selection has been made
#' get_selection(graph)
#' #> [1] 1 3
#'
#' # Invert the selection
#' graph <- invert_selection(graph)
#'
#' # Verify that the node selection has been changed
#' get_selection(graph)
#' #> [1] 2 4
#' @export invert_selection

invert_selection <- function(graph) {

  # Stop function if the graph does not contain
  # a selection
  if (is.null(graph$selection)) {
    stop("The graph does not contain an active selection")
  }

  # Invert the nodes in the selection
  if (!is.null(graph$selection$nodes)) {
    selection_nodes <- graph$selection$nodes

    graph$selection$nodes <-
      get_node_ids(graph)[which(!(get_node_ids(graph) %in%
                                 selection_nodes))]
  }

  # Invert the edges in the selection
  if (!is.null(graph$selection$edges)) {

    selection_from <- graph$selection$edges$from
    selection_to <- graph$selection$edges$to

    edges_selection <-
      sapply(1:length(selection_from),
             function(x) paste(selection_from[x],
                               "->",
                               selection_to[x]))

    graph_from <- graph$edges_df$from
    graph_to <- graph$edges_df$to

    edges_graph <-
      sapply(1:length(graph_from),
             function(x) paste(graph_from[x],
                               "->",
                               graph_to[x]))

    inverted_edges <-
      edges_graph[which(!(edges_graph %in%
                            edges_selection))]

    inverted_from <-
      gsub("\\s", "",
           gsub("(.*)(->|--)(.*)", "\\1",
                inverted_edges))

    inverted_to <-
      gsub("\\s", "",
           gsub("(.*)(->|--)(.*)", "\\3",
                inverted_edges))

    graph$selection$edges$from <- as.integer(inverted_from)
    graph$selection$edges$to <- as.integer(inverted_to)
  }

  # Return the graph
  return(graph)
}
