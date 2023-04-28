#' Get an aggregate value from the indegree of nodes
#'
#' @description
#'
#' Get a single, aggregate value from the indegree values for all nodes in a
#' graph, or, a subset of graph nodes.
#'
#' @inheritParams render_graph
#' @param agg The aggregation function to use for summarizing indegree values
#'   from graph nodes. The following aggregation functions can be used: `sum`,
#'   `min`, `max`, `mean`, or `median`.
#' @param conditions An option to use filtering conditions for the nodes to
#'   consider.
#'
#' @return A vector with an aggregate indegree value.
#'
#' @examples
#' # Create a random graph using the
#' # `add_gnm_graph()` function
#' graph <-
#'   create_graph() %>%
#'   add_gnm_graph(
#'     n = 20,
#'     m = 35,
#'     set_seed = 23) %>%
#'   set_node_attrs(
#'     node_attr = value,
#'     values = rnorm(
#'       n = count_nodes(.),
#'       mean = 5,
#'       sd = 1) %>% round(1))
#'
#' # Get the mean indegree value
#' # from all nodes in the graph
#' graph %>%
#'   get_agg_degree_in(
#'     agg = "mean")
#'
#' # Other aggregation functions
#' # can be used (`min`, `max`,
#' # `median`, `sum`); let's get
#' # the median in this example
#' graph %>%
#'   get_agg_degree_in(
#'     agg = "median")
#'
#' # The aggregation of indegree
#' # can occur for a subset of the
#' # graph nodes and this is made
#' # possible by specifying
#' # `conditions` for the nodes
#' graph %>%
#'   get_agg_degree_in(
#'     agg = "mean",
#'     conditions = value > 5.0)
#'
#' @import rlang
#' @export
get_agg_degree_in <- function(
    graph,
    agg,
    conditions = NULL
) {

  # Get the name of the function
  fcn_name <- get_calling_fcn()

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {

    emit_error(
      fcn_name = fcn_name,
      reasons = "The graph object is not valid")
  }

  # Capture provided conditions
  conditions <- rlang::enquo(conditions)

  # If filtering conditions are provided then
  # pass in those conditions and filter the ndf
  if (!is.null(
    rlang::enquo(conditions) %>%
    rlang::get_expr())) {

    # Extract the node data frame from the graph
    ndf <- get_node_df(graph)
    ndf <- dplyr::filter(.data = ndf, !!conditions)

    # Get a vector of node ID values
    node_ids <-
      ndf %>%
      dplyr::pull(id)
  }

  # Get a data frame with indegree values for
  # all nodes in the graph
  indegree_df <- get_degree_in(graph)

  if (exists("node_ids")) {
    indegree_df <-
      indegree_df %>%
      dplyr::filter(id %in% node_ids)
  }

  # Verify that the value provided for `agg`
  # is one of the accepted aggregation types
  if (!(agg %in% c("sum", "min", "max", "mean", "median"))) {

    emit_error(
      fcn_name = fcn_name,
      reasons = c(
        "The specified aggregation method is not valid",
        "allowed choices are: `min`, `max`, `mean`, `median`, or `sum`"))
  }

  # Get the aggregate value of total degree based
  # on the aggregate function provided
  fun <- match.fun(agg)

  indegree_agg <-
    indegree_df %>%
    dplyr::group_by() %>%
    dplyr::summarize(fun(indegree, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    purrr::flatten_dbl()

  indegree_agg
}
