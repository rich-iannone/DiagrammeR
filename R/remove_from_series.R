#' Remove graph object from a graph series object
#' @description Remove a single graph object from an array of graph objects in
#' a graph series object.
#' @param graph_series a graph series object from which the graph object will
#' be removed.
#' @param index the index of the graph object to be removed from the graph
#' series object.
#' @return a graph series object of type \code{dgr_graph_1D}.
#' @examples
#' \dontrun{
#' # Create three graphs (using \code{magrittr} pipes)
#' # and create a graph series using those graphs
#' library(magrittr)
#'
#' graph_1 <- create_graph() %>%
#'   add_node("a") %>% add_node("b") %>% add_node("c") %>%
#'   add_edge("a", "c") %>% add_edge("a", "b") %>% add_edge("b", "c")
#'
#' graph_2 <- graph_1 %>%
#'   add_node("d") %>% add_edge("d", "c")
#'
#' graph_3 <- graph_2 %>%
#'   add_node("e") %>% add_edge("e", "b")
#'
#' # Create an empty graph series
#' series <- create_series(series_type = "sequential")
#'
#' # Add graphs to the graph series
#' series <- graph_1 %>% add_to_series(series)
#' series <- graph_2 %>% add_to_series(series)
#' series <- graph_3 %>% add_to_series(series)
#'
#' # Remove the second graph from the graph series
#' series <- remove_from_series(graph_series = series, index = 2)
#' }
#' @export remove_from_series

remove_from_series <- function(graph_series,
                               index = "last"){

  if (index == "last"){
    graph_series$graphs[[length(graph_series$graphs)]] <- NULL

    return(graph_series)
  }

  if (index == "first"){
    graph_series$graphs[[1]] <- NULL

    return(graph_series)
  }

  if (class(index) == "numeric" | class(index) == "integer"){
    graph_series$graphs[[index]] <- NULL

    return(graph_series)
  }
}
