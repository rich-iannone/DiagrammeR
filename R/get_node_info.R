#' Get detailed information on nodes
#'
#' Obtain a data frame with detailed information on nodes and their
#' interrelationships within the graph.
#'
#' @param graph A graph object of class `dgr_graph`.
#'
#' @return A data frame containing information specific to each node within the
#'   graph.
#'
#' @examples
#' # Create a simple graph
#' graph <-
#'   create_graph() %>%
#'   add_gnm_graph(
#'     n = 5, m = 10,
#'     set_seed = 23)
#'
#' # Get information on the graph's nodes
#' graph %>% get_node_info()
#'
#' @export
get_node_info <- function(graph) {

  # Get the name of the function
  fcn_name <- get_calling_fcn()

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {

    emit_error(
      fcn_name = fcn_name,
      reasons = "The graph object is not valid")
  }

  # If graph is empty, return NULL
  if (is_graph_empty(graph)) {
    return(NULL)
  }

  # Get vectors of nodes in edges and
  # node `type` values
  edge_from <- graph$edges_df$from
  edge_to <- graph$edges_df$to
  type <- graph$nodes_df$type

  # Get vector of all node IDs and all labels
  all_nodes <- graph$nodes_df$id
  labels <- graph$nodes_df$label

  # For graphs with no edges, create a
  # `node_properties` data frame that doesn't
  # need to consider any edge information
  if (nrow(graph$edges_df) == 0) {

    node_properties <-
      as.data.frame(
        mat.or.vec(
          nr = length(all_nodes),
          nc = 7),
        stringsAsFactors = FALSE)

    colnames(node_properties) <-
      c("id", "type", "label", "deg",
        "indeg", "outdeg", "loops")

    node_properties[, 1] <- graph$nodes_df[, 1]
    node_properties[, 2] <- graph$nodes_df[, 2]
    node_properties[, 3] <- graph$nodes_df[, 3]

    # Ensure that the `id` column is an integer
    node_properties <-
      dplyr::mutate(node_properties, id = as.integer(id))

    # Arrange the table by `id` ascending
    node_properties <-
      dplyr::arrange(node_properties, id)

  } else if (!is.null(graph$edges_df)) {

    # Get vector of the top-level nodes
    top_nodes <-
      unique(
        edge_from[which(!(edge_from %in%
                            edge_to))])

    # Get vector of the bottom-level nodes
    bottom_nodes <-
      unique(
        edge_to[which(!(edge_to %in%
                          edge_from))])

    # Get vector of all nodes neither at the top nor
    # the bottom level
    between_nodes <-
      all_nodes[which(!(all_nodes %in%
                          c(top_nodes,
                            bottom_nodes)))]

    # Place the nodes in order
    ordered_nodes <-
      c(top_nodes, between_nodes, bottom_nodes)

    # Create data frame of node properties
    for (i in seq(ordered_nodes)) {
      if (i == 1) {
        node_properties <-
          as.data.frame(
            mat.or.vec(nr = 0,
                       nc = 7),
            stringsAsFactors = FALSE)

        colnames(node_properties) <-
          c("id", "type", "label", "deg",
            "indeg", "outdeg", "loops")
      }

      # Get degree for each node
      degree <-
        sum(c(graph$edges_df$from,
              graph$edges_df$to) %in%
              ordered_nodes[i])

      # Get indegree for each node
      if (ordered_nodes[i] %in%
          top_nodes | degree == 0) {
        indegree <- 0
      }

      if (!(ordered_nodes[i] %in%
            top_nodes) & degree != 0) {
        for (j in 1:sum(edge_to %in%
                        ordered_nodes[i])) {
          if (j == 1) {
            indegree <- vector(mode = "character")
          }
          indegree <-
            c(indegree,
              edge_from[which(edge_to %in%
                                ordered_nodes[i])[j]])
        }
        indegree <- length(indegree)
      }

      # Get outdegree for each node
      if (ordered_nodes[i] %in%
          bottom_nodes | degree == 0) {
        outdegree <- 0
      }

      if (!(ordered_nodes[i] %in% bottom_nodes) &
          degree != 0) {
        for (j in 1:sum(edge_from %in%
                        ordered_nodes[i])) {
          if (j == 1) {
            outdegree <- vector(mode = "character")
          }
          outdegree <-
            c(outdegree,
              edge_from[which(edge_from %in%
                                ordered_nodes[i])[j]])
        }
        outdegree <- length(outdegree)
      }

      # Get number of loops for each node
      loops <-
        sum(graph$edges_df$from == graph$edges_df$to &
              graph$edges_df$to == ordered_nodes[i])

      # Collect information into `node_properties`
      node_properties[i, 1] <-
        ordered_nodes[i]

      node_properties[i, 2] <-
        ifelse(exists("type"),
               type[which(all_nodes %in%
                            ordered_nodes[i])],
               rep(NA, length(ordered_nodes)))

      node_properties[i, 3] <-
        ifelse(!is.null(
          labels[which(all_nodes %in%
                         ordered_nodes[i])]),
          labels[which(all_nodes %in%
                         ordered_nodes[i])],
          NA)

      node_properties[i, 4] <- degree
      node_properties[i, 5] <- indegree
      node_properties[i, 6] <- outdegree
      node_properties[i, 7] <- loops
    }

    # Ensure that the `id` column is an integer
    node_properties <-
      dplyr::mutate(
        node_properties, id = as.integer(id))

    # Arrange the table by `id` ascending
    node_properties <-
      dplyr::arrange(
        node_properties, id)
  }

  node_properties
}
