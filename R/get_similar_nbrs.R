#' Get neighboring nodes based on node attribute
#' similarity
#' @description With a graph a single node serving as
#' the starting point, get those nodes in a potential
#' neighborhood of nodes (adjacent to the starting
#' node) that have a common or similar (within
#' threshold values) node attribute to the starting
#' node.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @param node a single-length vector containing a
#' node ID value.
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
#' specifies lower and upper bounds as negative and
#' positive percentage changes to the value of the
#' starting node. These bounds serve as criteria for
#' neighboring node similarity to the starting node.
#' @return a vector of node ID values.
#' @examples
#' # Getting similar neighbors can
#' # be done through numerical comparisons;
#' # start by creating a random, directed
#' # graph with 18 nodes and 22 edges
#' # using the `add_gnm_graph()` function
#' graph <-
#'   create_graph() %>%
#'   add_gnm_graph(
#'     n = 18,
#'     m = 25,
#'     set_seed = 23) %>%
#'   set_node_attrs(
#'     node_attr = value,
#'     values = rnorm(
#'       n = count_nodes(.),
#'       mean = 5,
#'       sd = 1) %>% round(0))
#'
#' # Starting with node `10`, we
#' # can test whether any nodes
#' # adjacent and beyond are
#' # numerically equivalent in `value`
#' graph %>%
#'   get_similar_nbrs(
#'     node = 10,
#'     node_attr = value)
#' #> [1] 2 14
#'
#' # We can also set a tolerance
#' # for ascribing similarly by using
#' # either the `tol_abs` or `tol_pct`
#' # arguments (the first applies absolute
#' # lower and upper bounds from the
#' # value in the starting node and the
#' # latter uses a percentage difference
#' # to do the same); try setting `tol_abs`
#' # with a fairly large range to
#' # determine if several nodes can be
#' # selected
#' graph %>%
#'   get_similar_nbrs(
#'     node = 10,
#'     node_attr = value,
#'     tol_abs = c(1, 1))
#' #> [1]  1  2  9 14
#'
#' # That resulted in a fairly large
#' # set of 4 neigboring nodes; for
#' # sake of example, setting the range
#' # to be very large will effectively
#' # return all nodes in the graph
#' # except for the starting node
#' graph %>%
#'   get_similar_nbrs(
#'     node = 10,
#'     node_attr = value,
#'     tol_abs = c(10, 10)) %>%
#'     length()
#' #> [1] 17
#' @importFrom rlang enquo UQ
#' @export get_similar_nbrs

get_similar_nbrs <- function(graph,
                             node,
                             node_attr,
                             tol_abs = NULL,
                             tol_pct = NULL) {

  node_attr <- rlang::enquo(node_attr)
  node_attr <- (rlang::UQ(node_attr) %>% paste())[2]

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {
    stop("The graph object is not valid.")
  }

  # Get value to match on
  match <-
    get_node_df(graph)[
      which(get_node_df(graph)[, 1] ==
              node),
      which(colnames(get_node_df(graph)) ==
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
    ifelse(
      suppressWarnings(
        any(is.na(as.numeric(attr_values)))),
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
                node_attr)]) <= match_range[2])), 1]
  }

  # place starting node in the neighbourhood vector
  neighborhood <- node

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
    if (i > 1) {
      if (identical(nodes[[i]], nodes[[i - 1]])) break
    }
    i <- i + 1
  }

  # Get the final set of nodes that satisfy similarity
  # and adjacency conditions
  matching_nodes <-
    setdiff(nodes[length(nodes)][[1]], node)

  # If there are no matching nodes return `NA`
  if (length(matching_nodes) == 0) {
    return(NA)
  } else {
    return(sort(matching_nodes))
  }
}
