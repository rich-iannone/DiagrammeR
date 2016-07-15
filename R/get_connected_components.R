#' Get all nodes associated with connected components
#' @description Determine which nodes in a graph belong
#' to different connected components (i.e., distinct
#' sets of nodes with traversable paths to and from
#' each node in the set).
#' @param graph a graph object of class
#' \code{dgr_graph} that is created using
#' \code{create_graph}.
#' @param return_type using \code{df} (the default), a
#' data frame containing the component ID and the nodes
#' associated with each connected component is
#' provided. With \code{list}, an anologous list object
#' is provided.
#' @return a data frame or a list object, depending on
#' the value given to \code{return_type}.
#' @examples
#' \dontrun{
#' library(magrittr)
#'
#' # Create a random graph
#' graph <-
#'   create_random_graph(36, 50, set_seed = 1)
#'
#' # Check if the graph is connected
#' is_graph_connected(graph)
#' #> [1] TRUE
#'
#' # Modify the graph so that it becomes disconnected
#' # (with two clusters of nodes)
#' graph <-
#'   graph %>%
#'   delete_edge(10, 36) %>%
#'   delete_edge(25, 27) %>%
#'   delete_edge(28, 29) %>%
#'   delete_edge(4, 29) %>%
#'   delete_edge(24, 32)
#'
#' # Verify that the graph is disconnected
#' is_graph_connected(graph)
#' #> [1] FALSE
#'
#' # Get the graph's connected components
#' get_connected_components(graph)
#' #>    component node
#' #> 1          1    1
#' #> 2          1    3
#' #> 3          1    4
#' #> 4          1    5
#' #> 5          1    6
#' #> 6          1    7
#' #> 7          1    8
#' #> 8          1    9
#' #> 9          1   11
#' #> 10         1   14
#' #> ..         ..  ..
#' }
#' @importFrom stats na.omit
#' @export get_connected_components

get_connected_components <- function(graph,
                                     return_type = "df") {

  # Get all node ID values for nodes in the graph
  graph_nodes <- get_nodes(graph)

  # Determine if the node ID values in the
  # `graph_nodes` vector are numeric
  node_id_numeric <-
    ifelse(
      suppressWarnings(
        any(is.na(as.numeric(graph_nodes)))),
      FALSE, TRUE)

  # Create an empty list object
  nodes <- list()

  i <- 1

  repeat {

    # Get a random node from the vector of remaining
    # nodes
    starting_node <- sample(graph_nodes, 1)

    # Place connected nodes in `nodes` list
    nodes[[i]] <-
      stats::na.omit(
        c(get_all_connected_nodes(graph,
                                  starting_node),
          starting_node))

    # If the node ID values are numeric, then apply a
    # numeric sort and reclass as a `character` type
    if (node_id_numeric) {
      nodes[[i]] <-
        as.character(sort(as.numeric(nodes[[i]])))
    }

    # Remove nodes from `graph_nodes`
    graph_nodes <- setdiff(graph_nodes, nodes[[i]])

    # Break if there are no remaining nodes to assign
    # to a connected component
    if (length(graph_nodes) == 0) break

    i <- i + 1
  }

  # Order list component vectors by decreasing length
  nodes <-
    nodes[order(sapply(nodes, length),
                decreasing = TRUE)]

  if (return_type == "list") {
    return(nodes)
  }

  if (return_type == "df") {
    for (i in 1:length(nodes)) {
      if (i == 1) {
        df <-
          data.frame(component = i,
                     node = nodes[[i]],
                     stringsAsFactors = FALSE)
      }

      if (i > 1) {
        df <-
          rbind(df,
                data.frame(component = i,
                           node = nodes[[i]],
                           stringsAsFactors = FALSE))
      }
    }
    return(df)
  }
}
