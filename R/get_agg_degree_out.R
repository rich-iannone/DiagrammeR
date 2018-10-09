#' Get an aggregate value from the outdegree of nodes
#'
#' Get a single, aggregate value from the outdegree values for all nodes in a
#'   graph, or, a subset of graph nodes.
#' @inheritParams render_graph
#' @param agg the aggregation function to use for summarizing outdegree values
#'   from graph nodes. The following aggregation functions can be used:
#'   \code{sum}, \code{min}, \code{max}, \code{mean}, or \code{median}.
#' @param conditions an option to use filtering conditions for the nodes to
#'   consider.
#' @return a vector with an aggregate outdegree value.
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
#' # Get the mean outdegree value from all
#' # nodes in the graph
#' graph %>%
#'   get_agg_degree_out(
#'     agg = "mean")
#'
#' # Other aggregation functions can be used
#' # (`min`, `max`, `median`, `sum`); let's
#' # get the median in this example
#' graph %>%
#'   get_agg_degree_out(
#'     agg = "median")
#'
#' # The aggregation of outdegree can occur
#' # for a subset of the graph nodes and this
#' # is made possible by specifying `conditions`
#' # for the nodes
#' graph %>%
#'   get_agg_degree_out(
#'     agg = "mean",
#'     conditions = value < 5.0)
#' @importFrom dplyr group_by summarize_ select filter ungroup pull
#' @importFrom stats as.formula
#' @importFrom purrr flatten_dbl
#' @importFrom rlang enquo get_expr UQ
#' @export
get_agg_degree_out <- function(graph,
                               agg,
                               conditions = NULL) {

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

  # Create binding for variable
  id <- NULL

  # If filtering conditions are provided then
  # pass in those conditions and filter the ndf
  if (!is.null(
    rlang::enquo(conditions) %>%
    rlang::get_expr())) {

    # Extract the node data frame from the graph
    ndf <- get_node_df(graph)

    # Apply filtering conditions to the ndf
    ndf <-
      filter(
        .data = ndf,
        UQ(conditions))

    # Get a vector of node ID values
    node_ids <-
      ndf %>%
      dplyr::pull(id)
  }

  # Get a data frame with outdegree values for
  # all nodes in the graph
  outdegree_df <- get_degree_out(graph)

  if (exists("node_ids")) {
    outdegree_df <-
      outdegree_df %>%
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
  outdegree_agg <-
    outdegree_df %>%
    dplyr::group_by() %>%
    dplyr::summarize_(stats::as.formula(
      paste0("~", agg, "(outdegree, na.rm = TRUE)"))) %>%
    dplyr::ungroup() %>%
    purrr::flatten_dbl()

  outdegree_agg
}
