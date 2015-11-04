#' Create a graph series object
#' @description Create a graph series object for storage of multiple graphs
#' across a sequential or temporal one-dimensional array.
#' @param graph a graph object to add to the new graph series object.
#' @param series_name an optional name to ascribe to the series.
#' @param series_type either a \code{sequential} type (the default) or a
#' \code{temporal} type (which requires date-time strings and time zone codes
#' to be supplied).
#' @param series_scripts a vector of R scripts or paths to R scripts.
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
#' series <- graph_1 %>>% add_to_series(series)
#' series <- graph_2 %>>% add_to_series(series)
#' series <- graph_3 %>>% add_to_series(series)
#' }
#' @export create_series

create_series <- function(graph = NULL,
                          series_name = NULL,
                          series_type = "sequential",
                          series_scripts = NULL){

  # Initialize an empty graph series object
  graph_series <- list(graphs = NULL,
                       series_name = series_name,
                       series_type = series_type,
                       series_scripts = series_scripts)

  attr(graph_series, "class") <- "dgr_graph_1D"

  if (is.null(graph)){
    return(graph_series)
  }

  # Add a graph to the initialized graph series
  graph_series$graphs[[length(graph_series$graphs) + 1]] <- graph

  return(graph_series)
}
