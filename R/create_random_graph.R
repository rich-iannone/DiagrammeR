#' Create a randomized graph
#' @description Create a graph of up to \code{n} nodes
#' with randomized edge assignments.
#' @param n the number of nodes to use in the
#' random graph.
#' @param m the number of edges to use in the
#' random graph.
#' @param directed an option for whether the random
#' graph should be undirected (default) or directed.
#' @param graph_name an optional string for labeling
#' the graph object.
#' @param display_labels display node labels.
#' @param set_seed supplying a value sets a random seed
#' of the \code{Mersenne-Twister} implementation.
#' @param write_backups an option to write incremental
#' backups of changing graph states to disk. If
#' \code{TRUE}, a subdirectory of the working directory
#' will be used to store \code{RDS} files. The
#' default value is \code{FALSE} so one has to opt in
#' to use this functionality.
#' @examples
#' # Create a random, directed graph with 50 nodes
#' # and 75 edges
#' random_graph_directed <-
#'   create_random_graph(
#'     n = 50, m = 75)
#' @importFrom utils combn
#' @importFrom tibble as_tibble
#' @importFrom dplyr rename sample_n mutate select_
#' @export create_random_graph

create_random_graph <- function(n,
                                m,
                                directed = TRUE,
                                graph_name = NULL,
                                display_labels = TRUE,
                                set_seed = NULL,
                                write_backups = FALSE) {

  # Create bindings for specific variables
  V1 <- V2 <- NULL

  # If a seed value is supplied, set a seed
  if (!is.null(set_seed)) {
    set.seed(set_seed, kind = "Mersenne-Twister")
  }

  # Stop function if the number of edges requested
  # exceeds that of a fully connected graph
  if (m > n * ((n - 1)/ 2)) {
    stop(paste0("The number of edges exceeds the maximum possible (",
                n * ((n - 1)/ 2),
                ")"))
  }

  # Create a node data frame
  ndf <-
    create_node_df(
      n = n,
      label = ifelse(display_labels, TRUE, FALSE),
      value = sample(seq(0.5, 10, 0.5), n, replace = TRUE))

  # Get all combinations of nodes (in one direction) and
  # create a data frame of all possible edges
  possible_edges <-
    utils::combn(1:n, 2) %>%
    t() %>%
    tibble::as_tibble() %>%
    dplyr::rename(from = V1, to = V2)

  # Randomly choose `m` rows from all possible edges and
  # create an edge data frame
  if (m > 0) {
    edf <-
      dplyr::sample_n(possible_edges, m, replace = FALSE) %>%
      dplyr::mutate(id = 1:n()) %>%
      dplyr::mutate(rel = as.character(NA)) %>%
      dplyr::select_("id", "from", "to", "rel") %>%
      as.data.frame(stringsAsFactors = FALSE)

    # Create the graph
    graph <-
      create_graph(
        nodes_df = ndf,
        edges_df = edf,
        directed = ifelse(directed, TRUE, FALSE),
        graph_name = graph_name,
        write_backups = write_backups)

  } else {

    # Create the graph
    graph <-
      create_graph(
        nodes_df = ndf,
        directed = ifelse(directed, TRUE, FALSE),
        graph_name = graph_name,
        write_backups = write_backups)
  }

  # Modify the `function_used` in the `graph_log` df
  graph$graph_log$function_used[1] <- "create_random_graph"

  return(graph)
}
