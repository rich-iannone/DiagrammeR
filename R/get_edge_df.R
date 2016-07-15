#' Get an edge data frame from a graph
#' @description From a graph, obtain an edge data frame
#' with all current edge attributes.
#' @param graph a graph object of class
#' \code{dgr_graph} that is created using
#' \code{create_graph}.
#' @return an edge data frame.
#' @examples
#' library(magrittr)
#'
#' # Create a graph using several piped functions
#' graph <-
#'   create_graph() %>%
#'   add_n_nodes(1, "a") %>%
#'   select_last_node %>%
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
#'   clear_selection
#'
#' # Get the graph's internal edge data frame (edf)
#' graph %>% get_edge_df
#' #>   from to rel color
#' #> 1    1  2   b  blue
#' #> 2    1  3   a green
#' #> 3    1  4   a green
#' #> 4    1  5   a green
#' #> 5    1  6   b  blue
#' @export get_edge_df

get_edge_df <- function(graph) {

  if (is.null(graph$edges_df)) {
    return(NA)
  } else {
    return(graph$edges_df)
  }
}
