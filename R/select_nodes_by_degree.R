#' Select nodes in the graph based on their degree
#' values
#' @description Using a graph object of class
#' \code{dgr_graph}, create a selection of nodes
#' that have certain degree values.
#' @param graph a graph object of class
#' \code{dgr_graph} that is created using
#' \code{create_graph}.
#' @param degree_type the type of degree, either:
#' \code{in} for the indegree, \code{out} for the
#' outdegree, and \code{both} or \code{degree} for the
#' total degree (i.e., indegree + outdegree).
#' @param degree_values a logical expression with a
#' comparison operator (\code{>}, \code{>=}, \code{<},
#' \code{<=}, \code{==}, or \code{!=}) followed by a
#' number for numerical filtering.
#' @param set_op the set operation to perform upon
#' consecutive selections of graph nodes. This can
#' either be as a \code{union} (the default), as an
#' intersection of selections with \code{intersect},
#' or, as a \code{difference} on the previous
#' selection, if it exists.
#' @return a graph object of class \code{dgr_graph}.
#' @export select_nodes_by_degree

select_nodes_by_degree <- function(graph,
                                   degree_type,
                                   degree_values,
                                   set_op = "union") {

  if (is_graph_empty(graph)) {
    stop("The graph is empty so no selections can be made.")
  }

  nodes_df <- graph$nodes_df
  nodes_selected <- nodes_df$nodes

  if (grepl("^>[0-9].*", degree_values)) {

    if (degree_type == "both" |
        degree_type == "degree") {
      nodes_selected <-
        node_info(graph)[
          which(node_info(graph)[
            which(node_info(graph)[, 1] %in%
                    nodes_selected), 4] >
              as.numeric(gsub(">(.*)", "\\1",
                              degree_values))), 1]
    }

    if (degree_type == "in") {
      nodes_selected <-
        node_info(graph)[
          which(node_info(graph)[
            which(node_info(graph)[, 1] %in%
                    nodes_selected), 5] >
              as.numeric(gsub(">(.*)", "\\1",
                              degree_values))), 1]
    }

    if (degree_type == "out") {
      nodes_selected <-
        node_info(graph)[
          which(node_info(graph)[
            which(node_info(graph)[, 1] %in%
                    nodes_selected), 6] >
              as.numeric(gsub(">(.*)", "\\1",
                              degree_values))), 1]
    }
  }

  if (grepl("^>=[0-9].*", degree_values)) {

    if (degree_type == "both" |
        degree_type == "degree") {
      nodes_selected <-
        node_info(graph)[
          which(node_info(graph)[
            which(node_info(graph)[, 1] %in%
                    nodes_selected), 4] >=
              as.numeric(gsub(">=(.*)", "\\1",
                              degree_values))), 1]
    }

    if (degree_type == "in") {
      nodes_selected <-
        node_info(graph)[
          which(node_info(graph)[
            which(node_info(graph)[, 1] %in%
                    nodes_selected), 5] >=
              as.numeric(gsub(">=(.*)", "\\1",
                              degree_values))), 1]
    }

    if (degree_type == "out") {
      nodes_selected <-
        node_info(graph)[
          which(node_info(graph)[
            which(node_info(graph)[, 1] %in%
                    nodes_selected), 6] >=
              as.numeric(gsub(">=(.*)", "\\1",
                              degree_values))), 1]
    }
  }

  if (grepl("^<[0-9].*", degree_values)) {

    if (degree_type == "both" |
        degree_type == "degree") {
      nodes_selected <-
        node_info(graph)[
          which(node_info(graph)[
            which(node_info(graph)[, 1] %in%
                    nodes_selected), 4] <
              as.numeric(gsub("<(.*)", "\\1",
                              degree_values))), 1]
    }

    if (degree_type == "in") {
      nodes_selected <-
        node_info(graph)[
          which(node_info(graph)[
            which(node_info(graph)[, 1] %in%
                    nodes_selected), 5] <
              as.numeric(gsub("<(.*)", "\\1",
                              degree_values))), 1]
    }

    if (degree_type == "out") {
      nodes_selected <-
        node_info(graph)[
          which(node_info(graph)[
            which(node_info(graph)[, 1] %in%
                    nodes_selected), 6] <
              as.numeric(gsub("<(.*)", "\\1",
                              degree_values))), 1]
    }
  }

  if (grepl("^<=[0-9].*", degree_values)) {

    if (degree_type == "both" |
        degree_type == "degree") {
      nodes_selected <-
        node_info(graph)[
          which(node_info(graph)[
            which(node_info(graph)[, 1] %in%
                    nodes_selected), 4] <=
              as.numeric(gsub("<=(.*)", "\\1",
                              degree_values))), 1]
    }

    if (degree_type == "in") {
      nodes_selected <-
        node_info(graph)[
          which(node_info(graph)[
            which(node_info(graph)[, 1] %in%
                    nodes_selected), 5] <=
              as.numeric(gsub("<=(.*)", "\\1",
                              degree_values))), 1]
    }

    if (degree_type == "out") {
      nodes_selected <-
        node_info(graph)[
          which(node_info(graph)[
            which(node_info(graph)[, 1] %in%
                    nodes_selected), 6] <=
              as.numeric(gsub("<=(.*)", "\\1",
                              degree_values))), 1]
    }
  }

  if (grepl("^==[0-9].*", degree_values)) {

    if (degree_type == "both" |
        degree_type == "degree") {
      nodes_selected <-
        node_info(graph)[
          which(node_info(graph)[
            which(node_info(graph)[, 1] %in%
                    nodes_selected), 4] ==
              as.numeric(gsub("==(.*)", "\\1",
                              degree_values))), 1]
    }

    if (degree_type == "in") {
      nodes_selected <-
        node_info(graph)[
          which(node_info(graph)[
            which(node_info(graph)[, 1] %in%
                    nodes_selected), 5] ==
              as.numeric(gsub("==(.*)", "\\1",
                              degree_values))), 1]
    }

    if (degree_type == "out") {
      nodes_selected <-
        node_info(graph)[
          which(node_info(graph)[
            which(node_info(graph)[, 1] %in%
                    nodes_selected), 6] ==
              as.numeric(gsub("==(.*)", "\\1",
                              degree_values))), 1]
    }
  }

  if (grepl("^!=[0-9].*", degree_values)) {

    if (degree_type == "both" |
        degree_type == "degree") {
      nodes_selected <-
        node_info(graph)[
          which(node_info(graph)[
            which(node_info(graph)[, 1] %in%
                    nodes_selected), 4] !=
              as.numeric(gsub("!=(.*)", "\\1",
                              degree_values))), 1]
    }

    if (degree_type == "in") {
      nodes_selected <-
        node_info(graph)[
          which(node_info(graph)[
            which(node_info(graph)[, 1] %in%
                    nodes_selected), 5] !=
              as.numeric(gsub("!=(.*)", "\\1",
                              degree_values))), 1]
    }

    if (degree_type == "out") {
      nodes_selected <-
        node_info(graph)[
          which(node_info(graph)[
            which(node_info(graph)[, 1] %in%
                    nodes_selected), 6] !=
              as.numeric(gsub("!=(.*)", "\\1",
                              degree_values))), 1]
    }
  }

  # If no node ID values in `nodes_selected` return
  # the graph without a changed node selection
  if (length(nodes_selected) == 0) {
    return(graph)
  }

  # Obtain vector of node IDs selection of nodes
  # already present
  if (!is.null(graph$selection)) {
    if (!is.null(graph$selection$nodes)) {
      nodes_prev_selection <- graph$selection$nodes
    }
  } else {
    nodes_prev_selection <- vector(mode = "character")
  }

  # Incorporate selected nodes into graph's
  # selection section
  if (set_op == "union") {
    nodes_combined <-
      union(nodes_prev_selection, nodes_selected)
  } else if (set_op == "intersect") {
    nodes_combined <-
      intersect(nodes_prev_selection, nodes_selected)
  } else if (set_op == "difference") {
    nodes_combined <-
      setdiff(nodes_prev_selection, nodes_selected)
  }

  graph$selection$nodes <- nodes_combined
  return(graph)
}
