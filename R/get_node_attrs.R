#' Get node attribute values
#' @description From a graph object of class
#' \code{dgr_graph}, get node attribute values for
#' one or more nodes.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @param nodes an optional vector of node IDs for
#' filtering list of nodes present in the graph or
#' node data frame.
#' @param node_attr the name of the attribute for which
#' to get values.
#' @return a named vector of node attribute values for
#' the attribute given by \code{node_attr} by node ID.
#' @examples
#' # Create a random graph and
#' # incorporate the node attribute
#' # called `value`
#' graph <-
#'   create_random_graph(
#'     n = 4, m = 4,
#'     set_seed = 23)
#'
#' # Get all of the values from
#' # the `value` node attribute
#' # as a named vector
#' graph %>%
#'   get_node_attrs(
#'     node_attr = value)
#' #>   1   2   3   4
#' #> 6.0 2.5 3.5 7.5
#'
#' # To only return node attribute
#' # values for specified nodes,
#' # use the `nodes` argument
#' graph %>%
#'   get_node_attrs(
#'     node_attr = value,
#'     nodes = c(1, 3))
#' #>   1   3
#' #> 6.0 3.5
#' @importFrom dplyr filter pull
#' @importFrom rlang enquo UQ
#' @export get_node_attrs

get_node_attrs <- function(graph,
                           node_attr,
                           nodes = NULL) {

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {
    stop("The graph object is not valid.")
  }

  node_attr <- rlang::enquo(node_attr)

  # Create binding for a specific variable
  id <- NULL

  if ((rlang::UQ(node_attr) %>% paste())[2] %in% c("id", "nodes")) {
    stop("This is not a node attribute.")
  }

  # Extract the node data frame (ndf)
  # from the graph
  ndf <- graph$nodes_df

  if (is.null(nodes)) {

    # Extract the node attribute values
    node_attr_vals <-
      ndf %>%
      dplyr::pull(rlang::UQ(node_attr))

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
    node_attr_vals <-
      ndf %>%
      dplyr::pull(rlang::UQ(node_attr))

    # Extract the node names
    node_names <- ndf$id

    # Assign node names
    names(node_attr_vals) <- node_names
  }

  node_attr_vals
}
