#' Convert an igraph graph to a DiagrammeR one
#' @description Convert an igraph graph to a DiagrammeR
#' graph object.
#' @param igraph an igraph graph object.
#' @param graph_name an optional string for labeling
#' the graph object.
#' @param write_backups an option to write incremental
#' backups of changing graph states to disk. If
#' \code{TRUE}, a subdirectory of the working directory
#' will be used to store \code{RDS} files. The
#' default value is \code{FALSE} so one has to opt in
#' to use this functionality.
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' # Create a DiagrammeR graph object
#' dgr_graph_orig <-
#'   create_random_graph(
#'     n = 36, m = 50,
#'     set_seed = 23,
#'     directed = TRUE)
#'
#' # Convert the DiagrammeR graph to an
#' # igraph object
#' ig_graph <- to_igraph(dgr_graph_orig)
#'
#' # Convert the igraph graph back to a
#' # DiagrammeR graph
#' dgr_graph_new <- from_igraph(ig_graph)
#'
#' # Get some graph information
#' graph_info(dgr_graph_new)[, 1:6]
#' #>             name  n  e   dens mn_deg mx_deg
#' #> 1 graph_eUrgZI3e 36 50 0.0571      1      6
#' @importFrom igraph V E vertex_attr_names edge_attr_names vertex_attr edge_attr is_directed ends
#' @importFrom dplyr arrange
#' @export from_igraph

from_igraph <- function(igraph,
                        graph_name = NULL,
                        write_backups = FALSE) {

  # Get the time of function start
  time_function_start <- Sys.time()

  # Create bindings for specific variables
  id <- NULL

  # Get vectors of all node and edge attributes
  node_attrs <- igraph::vertex_attr_names(igraph)
  edge_attrs <- igraph::edge_attr_names(igraph)

  # Generate a single-column ndf with node ID values
  nodes_df <-
    data.frame(
      id = as.integer(igraph::V(igraph)),
      stringsAsFactors = FALSE)

  # If the `type` attr exists, add that to the ndf
  if ("type" %in% node_attrs) {
    nodes_df <-
      cbind(
        nodes_df,
        data.frame(
          type = igraph::vertex_attr(igraph, "type"),
          stringsAsFactors = FALSE))
  } else {
    nodes_df <-
      cbind(
        nodes_df,
        data.frame(
          type = rep(as.character(NA), nrow(nodes_df)),
          stringsAsFactors = FALSE))
  }

  # If the `label` attr exists, add that to the ndf
  if ("label" %in% node_attrs) {
    nodes_df <-
      cbind(
        nodes_df,
        data.frame(
          label = igraph::vertex_attr(igraph, "label"),
          stringsAsFactors = FALSE))
  } else {
    nodes_df <-
      cbind(
        nodes_df,
        data.frame(
          label = rep(as.character(NA), nrow(nodes_df)),
          stringsAsFactors = FALSE))
  }

  # Determine if there are any extra node attrs
  extra_node_attrs <-
    setdiff(node_attrs, c("name", "type", "label"))

  # If there are extra node attrs, add to the ndf
  if (length(extra_node_attrs) > 0) {

    for (i in seq_along(extra_node_attrs)) {

      df_col <-
        data.frame(
          igraph::vertex_attr(igraph, extra_node_attrs[i]),
          stringsAsFactors = FALSE)

      colnames(df_col) <- extra_node_attrs[i]

      nodes_df <-
        cbind(nodes_df, df_col)
    }
  }

  # Generate a 2 column edf with `to` and `from` values
  edges_df <-
    data.frame(
      from = as.integer(igraph::ends(igraph, igraph::E(igraph))[, 1]),
      to = as.integer(igraph::ends(igraph, igraph::E(igraph))[, 2]),
      stringsAsFactors = FALSE)

  # If the `rel` attr exists, add that to the edf
  if ("rel" %in% edge_attrs) {
    edges_df <-
      cbind(
        edges_df,
        data.frame(
          rel = igraph::edge_attr(igraph, "rel"),
          stringsAsFactors = FALSE))
  } else {
    edges_df <-
      cbind(
        edges_df,
        data.frame(
          rel = rep(as.character(NA), nrow(edges_df)),
          stringsAsFactors = FALSE))
  }

  # Determine if there are any extra edge attrs
  extra_edge_attrs <- setdiff(edge_attrs, "rel")

  # If there are extra edge attrs, add to the edf
  if (length(extra_edge_attrs) > 0) {

    for (i in seq_along(extra_edge_attrs)) {

      df_col <-
        data.frame(
          igraph::edge_attr(igraph, extra_edge_attrs[i]),
          stringsAsFactors = FALSE)

      colnames(df_col) <- extra_edge_attrs[i]

      edges_df <- cbind(edges_df, df_col)
    }
  }

  # Ensure that the ndf is sorted ascending by node ID
  nodes_df <- dplyr::arrange(nodes_df, id)

  # Create a DiagrammeR graph object
  graph <-
    create_graph(
      nodes_df = nodes_df,
      edges_df = edges_df,
      directed = igraph::is_directed(igraph),
      graph_name = graph_name,
      write_backups = write_backups)

  graph
}
