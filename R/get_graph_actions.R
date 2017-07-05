#' Get information on any available graph actions
#' @description Get a tibble of the available graph
#' actions, which contains information on function
#' invocations to be called on the graph at every
#' transformation step, or, when manually invoked
#' with the \code{trigger_graph_actions()} function.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @return a \code{df_tbl} object.
#' @examples
#' # Create a random graph
#' graph <-
#'   create_random_graph(
#'     n = 10, m = 22,
#'     set_seed = 23)
#'
#' # Add a graph action that sets a node
#' # attr column with a function; the
#' # main function `set_node_attr_w_fcn()`
#' # uses the `get_betweenness()` function
#' # to provide betweenness values in the
#' # `btwns` column
#' graph <-
#'   graph %>%
#'   add_graph_action(
#'     fcn = "set_node_attr_w_fcn",
#'     node_attr_fcn = "get_betweenness",
#'     column_name = "btwns",
#'     action_name = "get_btwns")
#'
#' # To ensure that the action is
#' # available in the graph, use the
#' # `get_graph_actions()` function
#' graph %>%
#'   get_graph_actions()
#' #> # A tibble: 1 x 3
#' #>   action_index action_name
#' #>          <dbl>       <chr>
#' #> 1            1   get_btwns
#' #> # ... with 1 more variables: expression <chr>
#' @importFrom tibble as_tibble
#' @export get_graph_actions

get_graph_actions <- function(graph) {

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {
    stop("The graph object is not valid.")
  }

  graph$graph_actions %>% tibble::as_tibble()
}
