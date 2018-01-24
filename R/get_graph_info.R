#' Get metrics for a graph
#' @description Get a data frame with metrics for a
#' graph.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @return a data frame containing metrics pertaining
#' to the graph
#' @examples
#' \dontrun{
#' # Import a GML graph file available
#' # in the DiagrammeR package
#' karate_club <-
#'   system.file(
#'     "extdata", "karate.gml",
#'     package = "DiagrammeR") %>%
#'   import_graph() %>%
#'   set_graph_name("karate")
#'
#' # Display a data frame with
#' # graph information
#' karate_club %>%
#'   get_graph_info()
#' }
#' @export get_graph_info

get_graph_info <- function(graph) {

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {

    stop(
      "The graph object is not valid.",
      call. = FALSE)
  }

  # Get the graph density
  density <-
    round(count_nodes(graph) / ((count_nodes(graph) * (count_nodes(graph) - 1))/2), 4)

  # Get a table of node degree values
  degree_table <-
    table(c(graph$edges_df$from,
            graph$edges_df$to))

  # Create a data frame with the graph metrics
  data.frame(
    name = as.character(graph$graph_info$graph_name),
    n = as.integer(count_nodes(graph)),
    e = as.integer(count_edges(graph)),
    dens = as.numeric(density),
    mn_deg = as.integer(min(degree_table)),
    mx_deg = as.integer(max(degree_table)),
    avg_deg = as.numeric(round(mean(degree_table))),
    time = graph$graph_info$graph_time,
    tz = as.character(graph$graph_info$graph_tz),
    stringsAsFactors = FALSE)
}
