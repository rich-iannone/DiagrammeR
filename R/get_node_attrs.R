#' Get node attribute values
#'
#' @description
#'
#' From a graph object of class `dgr_graph`, get node attribute values for one
#' or more nodes.
#'
#' @inheritParams render_graph
#' @param nodes An optional vector of node IDs for filtering list of nodes
#'   present in the graph or node data frame.
#' @param node_attr The name of the attribute for which to get values.
#'
#' @return A named vector of node attribute values for the attribute given by
#'   `node_attr` by node ID.
#'
#' @examples
#' # Create a random graph using the
#' # `add_gnm_graph()` function
#' graph <-
#'   create_graph() %>%
#'   add_gnm_graph(
#'     n = 4,
#'     m = 4,
#'     set_seed = 23) %>%
#'   set_node_attrs(
#'     node_attr = value,
#'     values = c(2.5, 8.2, 4.2, 2.4))
#'
#' # Get all of the values from
#' # the `value` node attribute
#' # as a named vector
#' graph %>%
#'   get_node_attrs(
#'     node_attr = value)
#'
#' # To only return node attribute
#' # values for specified nodes,
#' # use the `nodes` argument
#' graph %>%
#'   get_node_attrs(
#'     node_attr = value,
#'     nodes = c(1, 3))
#'
#' @export
get_node_attrs <- function(
    graph,
    node_attr,
    nodes = NULL
) {

  # Get the name of the function
  fcn_name <- get_calling_fcn()

  # Validation: Graph object is valid
  check_graph_valid(graph)

  node_attr <- rlang::enquo(node_attr)

  if (rlang::enquo(node_attr) %>%
      rlang::get_expr() %>%
      as.character() %in% c("id", "nodes")) {

    emit_error(
      fcn_name = fcn_name,
      reasons = "This is not a node attribute")
  }

  # Extract the node data frame (ndf)
  # from the graph
  ndf <- graph$nodes_df

  if (is.null(nodes)) {

    # Extract the node attribute values
    node_attr_vals <- ndf %>% dplyr::pull(!!node_attr)

    # Extract the node names
    node_names <- ndf$id

    # Assign node names
    names(node_attr_vals) <- node_names
  }

  if (!is.null(nodes)) {

    nodes <- sort(nodes)

    # Filter the ndf by the supplied
    # nodes
    ndf <-
      ndf %>%
      dplyr::filter(id %in% nodes)

    # Extract the node attribute values
    node_attr_vals <- ndf %>% dplyr::pull(!!node_attr)

    # Extract the node names
    node_names <- ndf$id

    # Assign node names
    names(node_attr_vals) <- node_names
  }

  node_attr_vals
}
