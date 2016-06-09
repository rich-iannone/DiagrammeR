#' Select neighboring nodes based on node attribute
#' similarity
#' @description Beginning with a selection of a single
#' node, select those nodes in a neighborhood
#' of nodes that have a common node attribute.
#' @param graph a graph object of class
#' \code{dgr_graph} that is created using
#' \code{create_graph}.
#' @param node_attr the name of the node attribute
#' to use to compare with adjacent nodes.
#' @param tol_abs if the values contained in the node
#' attribute \code{node_attr} are numeric, one can
#' optionally supply a numeric vector of length 2 that
#' provides a lower and upper numeric bound as criteria
#' for neighboring node similarity to the starting
#' node.
#' @param tol_pct if the values contained in the node
#' attribute \code{node_attr} are numeric, one can
#' optionally supply a numeric vector of length 2 that
#' specify lower and upper bounds as negative and
#' positive percentage changes to the value of the
#' starting node. These bounds serve as criteria for
#' neighboring node similarity to the starting node.
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' \dontrun{
#' library(magrittr)
#'
#' # Create a graph with a tree structure that's
#' # 3 levels deep (begins with node `1`, branching
#' # by 2 nodes at each level)
#' #
#' # The resulting graph contains 15 nodes, numbered
#' # `1` through `15`; one main branch has all its 7
#' # nodes colored `red`, the other main branch has
#' # 3 of its 7 nodes colored `blue`
#' #
#' # A schematic of the graph:
#' #
#' #   red->[7 red nodes]
#' #    /
#' # [1]
#' #    \
#' #  blue->[3 blue nodes, 4 black nodes]
#' #
#' graph <-
#'   create_graph() %>%
#'   add_node("A") %>%
#'   select_nodes %>%
#'   add_n_nodes_from_selection(2, "B") %>%
#'   clear_selection %>%
#'   select_nodes("type", "B") %>%
#'   add_n_nodes_from_selection(2, "C") %>%
#'   clear_selection %>%
#'   select_nodes("type", "C") %>%
#'   add_n_nodes_from_selection(2, "D") %>%
#'   clear_selection %>%
#'   select_nodes_by_id(
#'     c(2, 4, 5, 8, 9, 10, 11)) %>%
#'   set_node_attr_with_selection(
#'     node_attr = 'color',
#'     value = 'red') %>%
#'   clear_selection %>%
#'   select_nodes_by_id(
#'     c(3, 6, 7)) %>%
#'   set_node_attr_with_selection(
#'     node_attr = 'color',
#'     value = 'blue') %>%
#'   select_edges(from = 1, to = 2) %>%
#'   set_edge_attr_with_selection(
#'     edge_attr = 'color',
#'     value = 'red') %>%
#'   clear_selection %>%
#'   select_edges(from = 1, to = 3) %>%
#'   set_edge_attr_with_selection(
#'     edge_attr = 'color',
#'     value = 'blue') %>%
#'   clear_selection
#'
#' # Create a graph selection of all nodes with the
#' # attribute `color = red`; Begin at node `1` and
#' # traverse along the red edge to the first `red`
#' # node, then, find the larger neighborhood of red
#' # nodes and create a node selection from that
#' # collection
#' graph %<>%
#'   select_nodes_by_id(1) %>%
#'   trav_out_edge('color', 'red') %>%
#'   trav_in_node %>%
#'   select_nbr_nodes_by_node_attr(
#'     node_attr = 'color')
#'
#' # Get selection of nodes; it comprises the
#' # entire set of 7 red nodes that have adjacency
#' # to each other
#' graph %>% get_selection
#' #> $nodes
#' #> [1] "2"  "4"  "5"  "8"  "9"  "10" "11"
#'
#' # Create a graph selection of all nodes with the
#' # attribute `color = blue`; Begin at node `1` and
#' # traverse along the blue edge to the first `blue`
#' # node, then, find the larger neighborhood of blue
#' # nodes and create a node selection
#' graph %<>%
#'   clear_selection %>%
#'   select_nodes_by_id(1) %>%
#'   trav_out_edge('color', 'blue') %>%
#'   trav_in_node %>%
#'   select_nbr_nodes_by_node_attr(
#'     node_attr = 'color')
#'
#' # Get selection of nodes; it comprises the
#' # entire set of 3 blue nodes that have adjacency
#' # to each other
#' graph %>% get_selection
#' #> $nodes
#' #> [1] "3" "6" "7"
#' }
#' @export select_nbr_nodes_by_node_attr

select_nbr_nodes_by_node_attr <- function(graph,
                                          node_attr,
                                          tol_abs = NULL,
                                          tol_pct = NULL) {

  # Get value to match on
  match <-
    get_node_df(graph)[
      which(get_node_df(graph)[, 1] ==
              get_selection(graph)[[1]])
      , which(colnames(get_node_df(graph)) ==
                node_attr)]

  # Create an empty list object
  nodes <- list()

  # Extract all `node_attr` values to test for their
  # type
  attr_values <-
    get_node_df(graph)[
      , which(colnames(get_node_df(graph)) ==
                node_attr)]

  # Determine whether `node_attr` values are numeric
  node_attr_numeric <-
    ifelse(suppressWarnings(any(is.na(as.numeric(attr_values)))),
           FALSE, TRUE)

  if (node_attr_numeric == FALSE) {

    # Get the set of all nodes in graph that
    # satisfy one or more conditions
    graph_nodes_with_attr <-
      graph$nodes_df[
        which(
          graph$nodes_df[, which(
            colnames(graph$nodes_df) ==
              node_attr)] %in% match), 1]
  }

  if (node_attr_numeric == TRUE) {

    match <- as.numeric(match)

    if (!is.null(tol_abs)) {
      match_range <-
        c(match - tol_abs[1], match + tol_abs[2])
    }

    if (!is.null(tol_pct)) {
      match_range <-
        c(match - match * tol_pct[1]/100,
          match + match * tol_pct[2]/100)
    }

    if (is.null(tol_abs) & is.null(tol_pct)) {
      match_range <- c(match, match)
    }

    # Get the set of all nodes in graph that
    # satisfy one or more conditions
    graph_nodes_with_attr <-
      graph$nodes_df[
        intersect(
          which(
            as.numeric(graph$nodes_df[, which(
              colnames(graph$nodes_df) ==
                node_attr)]) >= match_range[1]),
          which(
            as.numeric(graph$nodes_df[, which(
              colnames(graph$nodes_df) ==
                node_attr)]) <= match_range[2]))
        , 1]
  }

  # place starting node in the neighbourhood vector
  neighborhood <- get_selection(graph)[[1]]

  # Initialize `i`
  i <- 1

  repeat {

    # From the starting node get all adjacent nodes
    # that are not in the `neighborhood` vector
    neighborhood <-
      unique(
        c(neighborhood,
          intersect(
            unique(c(
              get_edges(
                graph,
                return_type = "df")[
                  which(get_edges(
                    graph,
                    return_type = "df")[, 1] %in%
                      neighborhood), 2],
              get_edges(
                graph,
                return_type = "df")[
                  which(get_edges(
                    graph,
                    return_type = "df")[, 2] %in%
                      neighborhood), 1])),
            graph_nodes_with_attr)))

    # Place revised neighborhood nodes in `nodes` list
    nodes[[i]] <- neighborhood

    # Break if current iteration yields no change in
    # the `nodes` list
    if (i > 1){
      if (identical(nodes[[i]], nodes[[i - 1]])) break
    }

    i <- i + 1
  }

  # Get the final set of nodes that satisfy similarity
  # and adjacency conditions
  matching_nodes <- nodes[length(nodes)][[1]]

  # Replace the graph selection with the matched nodes
  graph$selection$nodes <- matching_nodes

  return(graph)
}
