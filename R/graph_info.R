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

  # Create an empty data frame
  graph_metrics <-
    as.data.frame(mat.or.vec(nr = 0, nc = 9))

  colnames(graph_metrics) <-
    c("name", "n", "e", "dens",
      "mn_deg", "mx_deg",
      "avg_deg", "time", "tz")

  # Get the number of nodes
  n_nodes <- node_count(graph)

  # Get the number of edges
  n_edges <- edge_count(graph)

  # Get the graph density
  density <-
    round(n_edges / ((n_nodes * (n_nodes - 1))/2), 4)

  # Get a table of node degree values
  degree_table <-
    table(c(graph$edges_df$from,
            graph$edges_df$to))

  # Get the minimum degree
  min_deg <- min(degree_table)

  # Get the maximum degree
  max_deg <- max(degree_table)

  # Get the average degree (rounded)
  avg_deg <- round(mean(degree_table))

  # Add graph name to the data frame
  if (is.null(graph$graph_name)) {
    graph_metrics[1, 1] <- as.character(NA)
  } else {
    graph_metrics[1, 1] <- graph$graph_name
  }

  # Add graph time to the data frame
  if (is.null(graph$graph_time)) {
    graph_metrics[1, 8] <- as.character(NA)
  } else {
    graph_metrics[1, 8] <- graph$graph_time
  }

  # Add graph time zone (tz) to the data frame
  if (is.null(graph$graph_tz)) {
    graph_metrics[1, 9] <- as.character(NA)
  } else {
    graph_metrics[1, 9] <- graph$graph_tz
  }

  # Add count of nodes to the data frame
  graph_metrics[1, 2] <- n_nodes

  # Add count of edges to the data frame
  graph_metrics[1, 3] <- n_edges

  # Add density calculation to the data frame
  graph_metrics[1, 4] <- density

  # Add degree data to the data frame
  graph_metrics[1, 5] <- min_deg
  graph_metrics[1, 6] <- max_deg
  graph_metrics[1, 7] <- avg_deg

  return(graph_metrics)
}
