#' Count graphs in a graph series object
#' @description Counts the total number of graphs in a graph series object.
#' @param graph_series a graph series object of type \code{dgr_graph_1D}
#' @return a numeric vector representing a count of graphs in a graph series
#' object.
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
#' # Count the number of graphs in the graph series
#' graph_count(series)
#' #> [1] 3
#' }
#' @export graph_count

graph_count <- function(graph_series){

  if (class(graph_series) == "dgr_graph_1D"){

    if (is.null(graph_series$graphs)){

      return(0)
    }

    return(length(graph_series$graphs))
  }
}
