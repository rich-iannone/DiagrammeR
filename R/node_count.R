#' Get count of all nodes or certain types of nodes
#' @description From a graph object of class
#' \code{dgr_graph}, get a count of nodes in the graph
#' and optionally obtain a count of nodes by their type.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @param type either a logical value, where
#' \code{TRUE} provides a named vector of node count by
#' type and \code{FALSE} (the default) provides a total
#' count, or, a character vector of \code{type} values
#' to filter the node count.
#' @return a numeric vector of single length.
#' @examples
#' # Set a seed
#' set.seed(24)
#'
#' # Create a node data frame (ndf)
#' ndf <-
#'   create_node_df(
#'     n = 26,
#'     label = TRUE,
#'     type = c(rep("a", 7),
#'              rep("b", 9),
#'              rep("c", 8),
#'              rep("d", 2)))
#'
#' # Create an edge data frame (edf)
#' edf <-
#'   create_edge_df(
#'     from = sample(1:26, replace = TRUE),
#'     to = sample(1:26, replace = TRUE),
#'     rel = c(rep("rel_a", 7),
#'             rep("rel_b", 9),
#'             rep("rel_c", 8),
#'             rep("rel_d", 2)))
#'
#' # Create a graph using the ndf and edf
#' graph <-
#'   create_graph(
#'     nodes_df = ndf,
#'     edges_df = edf)
#'
#' # Get counts of nodes grouped by the
#' # `type` attribute
#' node_count(graph, type = TRUE) # the default
#' #> a b c d
#' #> 7 9 8 2
#'
#' # Get a total count of nodes with no grouping
#' node_count(graph, type = FALSE)
#' #> [1] 26
#'
#' # Get a count of nodes of one or more
#' # specified types
#' node_count(graph, type = "a")
#' #> [1] 7
#'
#' node_count(graph, type = c("a", "c"))
#' #> [1] 15
#' @export node_count

node_count <- function(graph,
                       type = FALSE) {

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {
    stop("The graph object is not valid.")
  }

  # If graph is empty, return 0
  if (is_graph_empty(graph)) {
    return(0)
  }

  # If value for `type` is provided as a string, get a
  # count of nodes for a that specific type
  if (inherits(type, "character")) {
    count_of_type <-
      length(which(graph$nodes_df[, 2] %in% type))
    return(count_of_type)
  }

  # If type is FALSE, get a total count of nodes
  if (all(inherits(type, "logical") & type == FALSE)) {
    return(nrow(graph$nodes_df))
  }

  # If type set to TRUE, get a named vector of counts
  # by type
  if (all(inherits(type, "logical") & type == TRUE)) {

    for (i in 1:length(get_node_ids(graph))) {
      if (i == 1) {
        all_nodes <- get_node_ids(graph)
        all_types <- vector(mode = "character")
      }

      all_types <- c(all_types,
                     node_type(graph = graph,
                               all_nodes[i],
                               action = "read"))
      all_types <- unique(all_types)

      if (any(is.na(all_types))) {
        all_types[which(is.na(all_types))] <- ""
      }
    }

    for (i in 1:length(all_types)) {

      if (i == 1) {
        total_node_count <- vector(mode = "numeric")
      }

      total_node_count <-
        c(total_node_count,
          nrow(graph$nodes_df[
            which(graph$nodes_df[, 2] ==
                    all_types[i]),]))

      if (i == length(all_types)) {
        names(total_node_count) <- all_types

        if (any(names(total_node_count) == "")) {
          names(total_node_count)[
            which(names(total_node_count) == "")] <-
            "<no type>"

          total_node_count <-
            c(total_node_count[
              which(names(total_node_count) ==
                      "<no type>")],
              total_node_count[
                -which(names(total_node_count) ==
                         "<no type>")])
        }
      }
    }
    return(total_node_count)
  }
}
