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
#' @return a list, data frame, or a vector object,
#' depending on the value given to \code{return_type}.
#' @export get_connected_components

get_connected_components <- function(graph,
                                     return_type = "df") {

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

    # Get a random node from the graph
    starting_node <- sample(graph_nodes, 1)


    # Place connected nodes in `nodes` list
    nodes[[i]] <-
      na.omit(
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

    if (length(graph_nodes) == 0) break

    i <- i + 1
  }

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
                     nodes = nodes[[i]],
                     stringsAsFactors = FALSE)
      }

      if (i > 1) {
        df <-
          rbind(df,
                data.frame(component = i,
                           nodes = nodes[[i]],
                           stringsAsFactors = FALSE))
      }
    }
    return(df)
  }
}
