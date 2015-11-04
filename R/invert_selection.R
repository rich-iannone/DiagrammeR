#' Invert selection of nodes or edges in a graph
#' @description Modify the selection of nodes or edges within a graph
#' object such that all nodes or edges previously unselected will now be
#' selected and vice versa.
#' @param graph a graph object of class \code{dgr_graph} that is created
#' using \code{create_graph}.
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' \dontrun{
#' # Create a simple graph
#' nodes <-
#'   create_nodes(nodes = c("a", "b", "c", "d"),
#'                type = "letter",
#'                label = TRUE,
#'                value = c(3.5, 2.6, 9.4, 2.7))
#'
#' edges <-
#'   create_edges(from = c("a", "b", "c"),
#'                to = c("d", "c", "a"),
#'                rel = "leading_to")
#'
#' graph <-
#'   create_graph(nodes_df = nodes,
#'                edges_df = edges)
#'
#' # Select nodes "a" and "c"
#' graph <- select_nodes(graph = graph, nodes = c("a", "c"))
#'
#' # Verify that a node selection has been made
#' get_selection(graph)
#' #> $nodes
#' #> [1] "a" "c"
#'
#' # Invert the selection
#' graph <- invert_selection(graph = graph)
#'
#' # Verify that the node selection has been changed
#' get_selection(graph)
#' #> $nodes
#' #> [1] "b" "d"
#' }
#' @export invert_selection

invert_selection <- function(graph){

  # Stop function if the graph does not contain a selection
  if (is.null(graph$selection)){
    stop("The graph does not contain an active selection")
  }

  # Invert the nodes in the selection
  if (!is.null(graph$selection$nodes)){

    selection_nodes <- graph$selection$nodes

    graph$selection$nodes <-
      get_nodes(graph)[which(!(get_nodes(graph) %in% selection_nodes))]
  }

  # Invert the edges in the selection
  if (!is.null(graph$selection$edges)){

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
      edges_graph[which(!(edges_graph %in% edges_selection))]

    inverted_from <- gsub("\\s", "",
                          gsub("(.*)(->|--)(.*)", "\\1", inverted_edges))

    inverted_to <- gsub("\\s", "",
                        gsub("(.*)(->|--)(.*)", "\\3", inverted_edges))

    graph$selection$edges$from <- inverted_from
    graph$selection$edges$to <- inverted_to
  }

  # Return the graph
  return(graph)
}
