#' Add one or more edges using a text string
#' @description With a graph object of class
#' \code{dgr_graph}, add one or more edges to the graph
#' using a text string.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @param edges a single-length vector with a character
#' string specifying the edges. For a directed graph,
#' the string object should be formatted as a series of
#' node ID values as \code{[node_ID_1]->[node_ID_2]}
#' separated by a one or more space characters. For
#' undirected graphs, \code{--} should replace
#' \code{->}. Line breaks in the vector won't cause an
#' error.
#' @param rel an optional vector specifying the
#' relationship between the connected nodes.
#' @param use_labels an option to use node \code{label}
#' values in the \code{edges} string to define node
#' connections. Note that this is only possible if all
#' nodes have distinct \code{label} values set and
#' none exist as an empty string.
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' # Create a graph with 4 nodes
#' graph <-
#'   create_graph() %>%
#'   add_node(label = "one") %>%
#'   add_node(label = "two") %>%
#'   add_node(label = "three") %>%
#'   add_node(label = "four")
#'
#' # Add edges between nodes using a
#' # character string with node ID values
#' graph_node_id <-
#'   graph %>%
#'   add_edges_w_string(
#'     "1->2 1->3 2->4 2->3")
#'
#' # Show the graph's internal edge data frame
#' get_edge_df(graph_node_id)
#' #>   id from to  rel
#' #> 1  1    1  2 <NA>
#' #> 2  2    1  3 <NA>
#' #> 3  3    2  4 <NA>
#' #> 4  4    2  3 <NA>
#'
#' # Add edges between nodes using a
#' # character string with node label values
#' # and setting `use_labels = TRUE`; note
#' # that all nodes must have unique `label`
#' # values to use this option
#' graph_node_label <-
#'   graph %>%
#'   add_edges_w_string(
#'     "one->two one->three
#'      two->four two->three",
#'     use_labels = TRUE)
#'
#' # Show the graph's internal edge data frame
#' # (it's the same as before)
#' get_edge_df(graph_node_label)
#' #>   id from to  rel
#' #> 1  1    1  2 <NA>
#' #> 2  2    1  3 <NA>
#' #> 3  3    2  4 <NA>
#' #> 4  4    2  3 <NA>
#' @export add_edges_w_string

add_edges_w_string <- function(graph,
                               edges,
                               rel = NULL,
                               use_labels = FALSE) {

  # Get the time of function start
  time_function_start <- Sys.time()

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {
    stop("The graph object is not valid.")
  }

  # Validation: Graph contains nodes
  if (graph_contains_nodes(graph) == FALSE) {
    stop("The graph contains no nodes, so, edges cannot be added.")
  }

  # Remove linebreak characters from `edges`
  edges_cleaned <-
    gsub("\n", " ", edges)

  # Remove extra spaces within the string
  edges_cleaned <-
    gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "",
         edges_cleaned, perl = TRUE)

  # Split by single spaces into separate edge
  # expressions
  edges_split <-
    unlist(strsplit(edges_cleaned, " "))

  # Split the edge expressions in a directed
  # graph into `from` and `to` vectors
  if (graph$directed) {
    from <-
      sapply(strsplit(edges_split, "->"), "[[", 1)

    to <-
      sapply(strsplit(edges_split, "->"), "[[", 2)
  }

  # Split the edge expressions in an undirected
  # graph into `from` and `to` vectors
  if (graph$directed == FALSE) {
    from <-
      sapply(strsplit(edges_split, "--"), "[[", 1)

    to <-
      sapply(strsplit(edges_split, "--"), "[[", 2)
  }

  # If `use_label` is set to TRUE, treat values in
  # list as labels; need to map to node ID values
  if (use_labels) {
    from_to_node_id <-
      translate_to_node_id(
        graph = graph,
        from = from,
        to = to)

    from <- from_to_node_id$from
    to <- from_to_node_id$to
  }

  # Create an edge data frame (edf) without
  # associated `rel` values
  if (is.null(rel)) {
    new_edges <-
      create_edge_df(
        from = from,
        to = to)
  }

  # Create an edge data frame (edf) with
  # associated `rel` values
  if (!is.null(rel)) {
    new_edges <-
      create_edge_df(
        from = from,
        to = to,
        rel = rel)
  }

  # Add the new edges to the graph
  new_graph <- add_edge_df(graph, new_edges)

  new_graph$graph_log <-
    add_action_to_log(
      graph_log = graph$graph_log,
      version_id = nrow(graph$graph_log) + 1,
      function_used = "add_edges_w_string",
      time_modified = time_function_start,
      duration = graph_function_duration(time_function_start),
      nodes = nrow(new_graph$nodes_df),
      edges = nrow(new_graph$edges_df))

  # Write graph backup if the option is set
  if (new_graph$graph_info$write_backups) {
    save_graph_as_rds(graph = new_graph)
  }

  return(new_graph)
}
