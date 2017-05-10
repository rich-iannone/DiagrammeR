#' Get an aggregate value from the total degree of nodes
#' @description Get a single, aggregate value from the
#' total degree values for all nodes in a graph, or, a subset
#' of graph nodes.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @param agg the aggregation function to use for
#' summarizing total degree values from graph nodes. The
#' following aggregation functions can be used:
#' \code{sum}, \code{min}, \code{max}, \code{mean}, or
#' \code{median}.
#' @param conditions an option to use filtering
#' conditions for the nodes to consider.
#' @return a vector with an aggregate total degree value.
#' @examples
#' # Create a random graph
#' random_graph <-
#'   create_random_graph(
#'     10, 22, set_seed = 23)
#'
#' # Get the mean total degree value from all
#' # nodes in the graph
#' get_agg_degree_total(
#'   graph = random_graph,
#'   agg = "mean")
#' #> [1] 5.333333
#'
#' # Other aggregation functions can be used
#' # (`min`, `max`, `median`, `sum`); let's
#' # get the median in this example
#' get_agg_degree_total(
#'   graph = random_graph,
#'   agg = "median")
#' #> [1] 5
#'
#' # The aggregation of total degree can occur
#' # for a subset of the graph nodes and this
#' # is made possible by specifying `conditions`
#' # for the nodes
#' get_agg_degree_total(
#'   graph = random_graph,
#'   agg = "mean",
#'   conditions = "value < 5.0")
#' #> [1] 5.333333
#' @importFrom dplyr group_by summarize_ filter_ select filter
#' @importFrom stats as.formula
#' @importFrom purrr flatten_dbl flatten_int
#' @export get_agg_degree_total

get_agg_degree_total <- function(graph,
                                 agg,
                                 conditions = NULL) {

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {
    stop("The graph object is not valid.")
  }

  # If filtering conditions are provided then
  # pass in those conditions and filter the ndf
  if (!is.null(conditions)) {

    # Extract the node data frame from the graph
    ndf <- get_node_df(graph)

    # Apply filtering conditions to the ndf
    for (i in 1:length(conditions)) {
      ndf <-
        ndf %>%
        dplyr::filter_(conditions[i])
    }

    # Get a vector of node ID values
    node_ids <-
      ndf %>%
      select(id) %>%
      flatten_int()
  }

  # Get a data frame with total degree values for
  # all nodes in the graph
  total_degree_df <- get_degree_total(graph)

  if (exists("node_ids")) {
    total_degree_df <-
      total_degree_df %>%
      dplyr::filter(id %in% node_ids)
  }

  # Verify that the value provided for `agg`
  # is one of the accepted aggregation types
  if (!(agg %in% c("sum", "min", "max", "mean", "median"))) {
    stop("The aggregation method must be either `min`, `max`, `mean`, `median`, or `sum`.")
  }

  # Get the aggregate value of total degree based
  # on the aggregate function provided
  total_degree_agg <-
    total_degree_df %>%
    dplyr::group_by() %>%
    dplyr::summarize_(stats::as.formula(
      paste0("~", agg, "(total_degree, na.rm = TRUE)"))) %>%
    purrr::flatten_dbl()

  return(total_degree_agg)
}
