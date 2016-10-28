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
#' \code{dgr_graph} that is created using
#' \code{create_graph}.
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
#' #> "2 -> 3" "1 -> 4" "3 -> 2" "4 -> 1"
#' @importFrom dplyr filter_ bind_rows
#' @importFrom tibble as_tibble
#' @export select_rev_edges_ws

select_rev_edges_ws <- function(graph,
                                add_to_selection = TRUE) {

  # Stop function if there are no edges in the graph
  if (edge_count(graph) == 0) {
    stop("The graph has no edges so no selections can be made.")
  }

  # Extract the selection of edges
  edges_from <- graph$selection$edges$from
  edges_to <- graph$selection$edges$to

  # Extract the graph's internal ndf
  ndf <- graph$nodes_df

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

  # Create a new selection of edges
  if (nrow(edges) > 0) {
    graph$selection$edges$from <- as.integer(edges$from)
    graph$selection$edges$to <- as.integer(edges$to)
  }

  return(graph)
}
