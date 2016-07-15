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
#' @examples
#' library(magrittr)
#'
#' # Create a random graph with a high amount
#' # of connectedness
#' random_graph <-
#'   create_random_graph(
#'     n = 35, m = 125,
#'     fully_connected = TRUE,
#'     directed = TRUE,
#'     set_seed = 25) %>%
#'   set_global_graph_attrs(
#'     'graph', 'layout', 'neato') %>%
#'   set_global_graph_attrs(
#'     'graph', 'overlap', 'false')
#'
#' # Report which nodes have a total degree (indegree
#' # + outdegree) of exactly 9
#' random_graph %>%
#'   select_nodes_by_degree('both', '==9') %>%
#'   get_selection
#' #> $nodes
#' #> [1] "4"  "8"  "11" "18" "20" "24" "31"
#'
#' # Report which nodes have a total degree greater
#' # than or equal to 9
#' random_graph %>%
#'   select_nodes_by_degree('both', '>=9') %>%
#'   get_selection
#' #> $nodes
#' #>  [1] "4"  "5"  "7"  "8"  "9"  "11" "18" "20" "24"
#' #> [10] "31" "32"
#'
#' # Combine two calls of `select_nodes_by_degree()`
#' # to get those nodes with total degree less than 3
#' # and total degree greater than 10 (by default,
#' # those `select...()` functions `union` the sets of
#' # nodes selected)
#' random_graph %>%
#'   select_nodes_by_degree('both', '<3') %>%
#'   select_nodes_by_degree('both', '>10') %>%
#'   get_selection
#' #> $nodes
#' #> [1] "16" "5"  "7"
#'
#' # Combine two calls of `select_nodes_by_degree()`
#' # to get those nodes with total degree greater than
#' # or equal to 3 and less than or equal to 10 (the
#' # key here is to `intersect` the sets of nodes
#' # selected in the second call)
#' random_graph %>%
#'   select_nodes_by_degree('both', '>=3') %>%
#'   select_nodes_by_degree('both', '<=10', 'intersect') %>%
#'   get_selection
#' #> $nodes
#' #> [1] "1"  "2"  "3"  "4"  "6"  "8"  "9"  "11" "13"
#' #> [10] "14" "15" "17" "18" "19" "20" "21" "22" "24"
#' #> [19] "25" "26" "27" "28" "29" "30" "31" "32" "33"
#' #> [28] "34" "35" "12" "23" "10"
#'
#' # Select all nodes with an indegree greater than 5,
#' # then, apply a node attribute to those selected nodes
#' # (coloring the selected nodes red)
#' random_graph_2 <-
#'   random_graph %>%
#'   select_nodes_by_degree('in', '>5') %>%
#'   set_node_attrs_ws(
#'     node_attr = 'color', value = 'red')
#'
#' # Get the selection of nodes
#' random_graph_2 %>% get_selection
#' #> $nodes
#' #> [1] "4" "6"
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
