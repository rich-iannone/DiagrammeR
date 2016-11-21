#' Select nodes in the graph based on their degree
#' values
#' @description Using a graph object of class
#' \code{dgr_graph}, create a selection of nodes
#' that have certain degree values.
#' @param graph a graph object of class
#' \code{dgr_graph}.
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
#' # Create a random graph with a high amount
#' # of connectedness
#' graph <-
#'   create_random_graph(
#'     n = 35, m = 125,
#'     set_seed = 23)
#'
#' # Report which nodes have a total degree (indegree
#' # + outdegree) of exactly 9
#' graph %>%
#'   select_nodes_by_degree("both", "==9") %>%
#'   get_selection()
#' #> [1]  2  9 10 14 17 19 31 33
#'
#' # Report which nodes have a total degree greater
#' # than or equal to 9
#' graph %>%
#'   select_nodes_by_degree("both", ">=9") %>%
#'   get_selection()
#' #> [1]  2  6  9 10 14 17 19 22 25 29 31 33
#'
#' # Combine two calls of `select_nodes_by_degree()`
#' # to get those nodes with total degree less than 3
#' # and total degree greater than 10 (by default,
#' # those `select...()` functions `union` the sets of
#' # nodes selected)
#' graph %>%
#'   select_nodes_by_degree("both", "<3") %>%
#'   select_nodes_by_degree("both", ">10") %>%
#'   get_selection()
#' #> [1]  6 16 22
#'
#' # Combine two calls of `select_nodes_by_degree()`
#' # to get those nodes with total degree greater than
#' # or equal to 3 and less than or equal to 10 (the
#' # key here is to `intersect` the sets of nodes
#' # selected in the second call)
#' graph %>%
#'   select_nodes_by_degree("both", ">=3") %>%
#'   select_nodes_by_degree("both", "<=10", "intersect") %>%
#'   get_selection()
#' #>  [1]  1  2  3  4  5  7  8  9 10 11 12 13 14 15 17 18
#' #> [17] 19 20 21 23 24 25 26 27 28 29 30 31 32 33 34 35
#'
#' # Select all nodes with an indegree greater than 5,
#' # then, apply a node attribute to those selected nodes
#' # (coloring the selected nodes red)
#' graph_2 <-
#'   graph %>%
#'   select_nodes_by_degree("in", ">5") %>%
#'   set_node_attrs_ws("color", "red")
#'
#' # Get the selection of nodes
#' graph_2 %>% get_selection()
#' #> [1] 14 22 23 25 27 29 31 33 34
#' @export select_nodes_by_degree

select_nodes_by_degree <- function(graph,
                                   degree_type,
                                   degree_values,
                                   set_op = "union") {

  # Get the time of function start
  time_function_start <- Sys.time()

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {
    stop("The graph object is not valid.")
  }

  # Validation: Graph contains nodes
  if (graph_contains_nodes(graph) == FALSE) {
    stop("The graph contains no nodes, so, no selections can be made.")
  }

  nodes_df <- graph$nodes_df

  nodes_selected <- nodes_df[, 1]

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

  # Obtain vector with node ID selection of nodes
  # already present
  nodes_prev_selection <- graph$node_selection$node

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

  # Add the node ID values to the active selection
  # of nodes in `graph$node_selection`
  graph$node_selection <-
    replace_graph_node_selection(
      graph = graph,
      replacement = nodes_combined)

  # Replace `graph$edge_selection` with an empty df
  graph$edge_selection <- create_empty_esdf()

  # Update the `graph_log` df with an action
  graph$graph_log <-
    add_action_to_log(
      graph_log = graph$graph_log,
      version_id = nrow(graph$graph_log) + 1,
      function_used = "select_nodes_by_degree",
      time_modified = time_function_start,
      duration = graph_function_duration(time_function_start),
      nodes = nrow(graph$nodes_df),
      edges = nrow(graph$edges_df))

  return(graph)
}
