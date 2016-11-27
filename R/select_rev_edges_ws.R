#' Select any reverse edges from a selection of edges
#' @description From an active selection of edges in
#' a graph object of class \code{dgr_graph}, select any
#' of the available reverse edges between the nodes
#' common to the selected edges. For instance, if an
#' active selection has the edge \code{1 -> 2} but
#' there is also an (unselected) edge \code{2 -> 1},
#' then this function can either switch to the selection
#' of \code{2 -> 1}, or, incorporate those edges in the
#' active selection of edges.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @param add_to_selection an option to either add the
#' reverse edges to the active selection of edges (the
#' default case, as \code{TRUE}) or switch the active
#' entirely to those reverse edges (\code{FALSE}).
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' # Create a node data frame (ndf)
#' ndf <-
#'   create_node_df(
#'     n = 4,
#'     type = "basic",
#'     label = TRUE)
#'
#' # Create an edge data frame (edf)
#' edf <-
#'   create_edge_df(
#'     from = c(1, 4, 2, 3, 3),
#'     to =   c(4, 1, 3, 2, 1))
#'
#' # Create a graph with the ndf and edf
#' graph <-
#'   create_graph(
#'     nodes_df = ndf,
#'     edges_df = edf)
#'
#' # Explicitly select the edges `1` -> `4`
#' # and `2` -> `3`
#' graph <-
#'   graph %>%
#'   select_edges(from = 1, to = 4) %>%
#'   select_edges(from = 2, to = 3)
#'
#' # Add to the selection the reverse edge
#' # (`4` -> `1`)
#' graph <-
#'   graph %>%
#'   select_rev_edges_ws()
#'
#' # Get the current selection of edges
#' get_selection(graph)
#' #> [1] 1 2 3 4
#' @importFrom dplyr filter_ bind_rows select rename arrange
#' @importFrom tibble as_tibble
#' @export select_rev_edges_ws

select_rev_edges_ws <- function(graph,
                                add_to_selection = TRUE) {

  # Get the time of function start
  time_function_start <- Sys.time()

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {
    stop("The graph object is not valid.")
  }

  # Validation: Graph contains edges
  if (graph_contains_edges(graph) == FALSE) {
    stop("The graph contains no edges, so, no selections can be made.")
  }

  # Validation: Graph object has valid edge selection
  if (graph_contains_edge_selection(graph) == FALSE) {
    stop("There is no selection of edges available.")
  }

  # Create bindings for specific variables
  id <- from <- to <- edge <- NULL

  # Extract the selection of edges
  edges_from <- graph$edge_selection$from
  edges_to <- graph$edge_selection$to

  # Extract the graph's internal edf
  edf <- graph$edges_df

  # Get the available reverse edges
  reverse_edges <-
    edf %>%
    {
      reverse_edges <- tibble::as_tibble()
      for (i in 1:length(edges_to)) {
        reverse_edges <-
          edf %>%
          dplyr::filter_(
            paste0(
              "from == ",
              edges_to[i],
              " & to == ",
              edges_from[i])) %>%
          dplyr::bind_rows(reverse_edges)
      }
      reverse_edges
    }

  # Add the reverse edges to the existing,
  # selected edges
  if (add_to_selection) {
    edges <-
      edf %>%
      {
        edges <- tibble::as_tibble()
        for (i in 1:length(edges_to)) {
          edges <-
            edf %>%
            dplyr::filter_(
              paste0(
                "from == ",
                edges_from[i],
                " & to == ",
                edges_to[i])) %>%
            dplyr::bind_rows(edges)
        }
        edges
      } %>%
      dplyr::bind_rows(reverse_edges)
  } else {
    edges <- reverse_edges
  }

  # Modify `edges` to create a correct esdf
  edges <-
    edges %>%
    dplyr::select(id, from, to) %>%
    dplyr::rename(edge = id) %>%
    dplyr::arrange(edge)

  # Add the edge ID values to the active selection
  # of edges in `graph$edge_selection`
  graph$edge_selection <- edges

  # Replace `graph$node_selection` with an empty df
  graph$node_selection <- create_empty_nsdf()

  # Update the `graph_log` df with an action
  graph$graph_log <-
    add_action_to_log(
      graph_log = graph$graph_log,
      version_id = nrow(graph$graph_log) + 1,
      function_used = "select_rev_edges_ws",
      time_modified = time_function_start,
      duration = graph_function_duration(time_function_start),
      nodes = nrow(graph$nodes_df),
      edges = nrow(graph$edges_df))

  # Write graph backup if the option is set
  if (graph$graph_info$write_backups) {
    save_graph_as_rds(graph = graph)
  }

  return(graph)
}
