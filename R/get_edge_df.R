#' Get an edge data frame from a graph
#' @description From a graph, obtain an edge data frame
#' with all current edge attributes.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @return an edge data frame.
#' @examples
#' # Create a graph using several piped functions
#' graph <-
#'   create_graph() %>%
#'   add_n_nodes(1, "a") %>%
#'   select_last_nodes_created() %>%
#'   add_n_nodes_ws(5, "from", "b") %>%
#'   select_edges_by_node_id(3:5) %>%
#'   set_edge_attrs_ws(
#'     "color", "green") %>%
#'   set_edge_attrs_ws(
#'     "rel", "a") %>%
#'   invert_selection %>%
#'   set_edge_attrs_ws(
#'     "color", "blue") %>%
#'   set_edge_attrs_ws(
#'     "rel", "b") %>%
#'   clear_selection()
#'
#' # Get the graph's internal edge data frame (edf)
#' get_edge_df(graph)
#' #>   id from to rel color
#' #> 1  1    1  2   b  blue
#' #> 2  2    1  3   a green
#' #> 3  3    1  4   a green
#' #> 4  4    1  5   a green
#' #> 5  5    1  6   b  blue
#' @export get_edge_df

get_edge_df <- function(graph) {

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {
    stop("The graph object is not valid.")
  }

  return(graph$edges_df)
}
