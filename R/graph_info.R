#' Get metrics for a graph
#' @description Get a data frame with metrics for a
#' graph.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @return a data frame containing metrics pertaining
#' to the graph
#' @examples
#' \dontrun{
#' # Import a GraphML graph file available in the
#' # DiagrammeR package
#' karate_club <-
#'   system.file(
#'     "extdata", "karate.gml",
#'     package = "DiagrammeR") %>%
#'   import_graph() %>%
#'   set_graph_name("karate")
#'
#' # Display a data frame with graph information
#' graph_info(karate_club)
#' #>     name  n  e  dens mn_deg mx_deg avg_deg time   tz
#' #> 1 karate 34 78 0.139      1     17       5 <NA> <NA>
#' }
#' @export graph_info

graph_info <- function(graph) {

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {
    stop("The graph object is not valid.")
  }

  # Get the graph density
  density <-
    round(node_count(graph) / ((node_count(graph) * (node_count(graph) - 1))/2), 4)

  # Get a table of node degree values
  degree_table <-
    table(c(graph$edges_df$from,
            graph$edges_df$to))

  # Create a data frame with the graph metrics
  graph_info_df <-
    data.frame(
      name = as.character(graph$graph_info$graph_name),
      n = as.integer(node_count(graph)),
      e = as.integer(edge_count(graph)),
      dens = as.numeric(density),
      mn_deg = as.integer(min(degree_table)),
      mx_deg = as.integer(max(degree_table)),
      avg_deg = as.numeric(round(mean(degree_table))),
      time = graph$graph_info$graph_time,
      tz = as.character(graph$graph_info$graph_tz),
      stringsAsFactors = FALSE)

  return(graph_info_df)
}
