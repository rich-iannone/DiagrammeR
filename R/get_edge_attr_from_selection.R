#' Get edge attributes based on a selection of edges
#' @description From a graph object of class \code{dgr_graph}, get edge
#' attribute properties for edges available in a selection.
#' @param graph a graph object of class \code{dgr_graph} that is created
#' using \code{create_graph}.
#' @examples
#' \dontrun{
#' library(magrittr)
#'
#' # Create a simple graph
#' nodes <-
#'   create_nodes(nodes = c("a", "b", "c", "d"),
#'                type = "letter",
#'                label = TRUE,
#'                value = c(3.5, 2.6, 9.4, 2.7))
#'
#' edges <-
#'   create_edges(from = c("a", "b", "c"),
#'                to = c("d", "c", "a"),
#'                rel = "leading_to",
#'                color = c("pink", "blue", "red"))
#'
#' graph <-
#'   create_graph(nodes_df = nodes,
#'                edges_df = edges)
#'
#' # Select edges "b" -> "c" and "a" -> "d" in the graph and get
#' # the edge attributes for that selection
#' graph %>% select_edges(from = c("b", "a"), to = c("c", "d")) %>%
#'   get_edge_attr_from_selection()
#' #>   from to        rel color
#' #> 1    a  d leading_to  pink
#' #> 2    b  c leading_to  blue
#' }
#' @return an edge data frame.
#' @export get_edge_attr_from_selection

get_edge_attr_from_selection <- function(graph){

  if (is.null(graph$selection$edges)){
    stop("There is no selection of edges available.")
  }

  edges_df <- get_edge_attr(graph,
                            from = graph$selection$edges$from,
                            to = graph$selection$edges$to)

  return(edges_df)
}
