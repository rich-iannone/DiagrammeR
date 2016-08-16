#' Convert an igraph graph to a DiagrammeR one
#' @description Convert an igraph graph to a DiagrammeR
#' graph object.
#' @param igraph an igraph graph object.
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' # Create a DiagrammeR graph object
#' dgr_graph_orig <-
#'   create_random_graph(
#'     36, 50, set_seed = 1,
#'     directed = TRUE)
#'
#' # Convert the DiagrammeR graph to an
#' # igraph object
#' ig_graph <- to_igraph(dgr_graph_orig)
#'
#' # Convert the igraph graph back to a
#' # DiagrammeR graph
#' dgr_graph_new <- from_igraph(ig_graph)
#' @importFrom igraph V E vertex_attr_names edge_attr_names vertex_attr edge_attr is_directed ends
#' @export from_igraph

from_igraph <- function(igraph) {

  # Get vectors of all node and edge attributes
  node_attrs <- igraph::vertex_attr_names(igraph)
  edge_attrs <- igraph::edge_attr_names(igraph)

  # Generate a single-column ndf with node ID values
  nodes_df <-
    data.frame(nodes = as.character(igraph::V(igraph)))

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
          type = rep("", nrow(nodes_df)),
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
          label = rep("", nrow(nodes_df)),
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
    as.data.frame(igraph::ends(igraph, igraph::E(igraph)),
                  stringsAsFactors = FALSE)

  colnames(edges_df) <- c("from", "to")

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
          rel = rep("", nrow(edges_df)),
          stringsAsFactors = FALSE))
  }

  # Determine if there are any extra edge attrs
  extra_edge_attrs <-
    setdiff(edge_attrs, "rel")

  # If there are extra edge attrs, add to the edf
  if (length(extra_edge_attrs) > 0) {

    for (i in seq_along(extra_edge_attrs)) {

      df_col <-
        data.frame(
          igraph::edge_attr(igraph, extra_edge_attrs[i]),
          stringsAsFactors = FALSE)

      colnames(df_col) <- extra_edge_attrs[i]

      edges_df <-
        cbind(edges_df, df_col)
    }
  }

  # Create a DiagrammeR graph object
  dgr_graph <-
    create_graph(
      nodes_df = nodes_df,
      edges_df = edges_df,
      directed = igraph::is_directed(igraph))

  return(dgr_graph)
}
