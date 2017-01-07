#' Display a property graph's underlying model
#' @description With a graph object of class
#' \code{dgr_graph} that is also a property
#' graph (i.e., all nodes have an assigned
#' \code{type} value and all edges have an assigned
#' \code{rel} value), display its metagraph in the
#' RStudio Viewer. This representation provides all
#' combinations of edges of different \code{rel}
#' values to all nodes with distinct \code{type}
#' values, including any edges to nodes of the same
#' \code{type} (shown as loops). The precondition
#' of the graph being a property graph can be
#' verified by using the \code{is_property_graph()}
#' function.
#' @param graph a graph object of class
#' \code{dgr_graph}. This graph must fulfill the
#' condition of being a property graph, otherwise
#' the function yields an error.
#' @examples
#' # Create a randomized property graph with 1000
#' # nodes and 1350 edges
#' property_graph <-
#'   create_random_graph(1000, 1350, set_seed = 23) %>%
#'   select_nodes_by_degree("deg >= 3") %>%
#'   set_node_attrs_ws("type", "a") %>%
#'   clear_selection() %>%
#'   select_nodes_by_degree("deg < 3") %>%
#'   set_node_attrs_ws("type", "b") %>%
#'   clear_selection() %>%
#'   select_nodes_by_degree("deg == 0") %>%
#'   set_node_attrs_ws("type", "c") %>%
#'   set_node_attr_to_display("type") %>%
#'   select_edges_by_node_id(
#'     get_node_ids(.) %>%
#'       sample(size = 0.15 * length(.) %>%floor())) %>%
#'   set_edge_attrs_ws("rel", "r_1") %>%
#'   invert_selection() %>%
#'   set_edge_attrs_ws("rel", "r_2") %>%
#'   clear_selection() %>%
#'   copy_edge_attrs("rel", "label") %>%
#'   add_global_graph_attrs("fontname", "Helvetica", "edge") %>%
#'   add_global_graph_attrs("fontcolor", "gray50", "edge") %>%
#'   add_global_graph_attrs("fontsize", 10, "edge")
#'
#' # Display this graph's metagraph, or, underlying
#' # graph model for a property graph
#' display_metagraph(property_graph)
#' @importFrom dplyr select distinct inner_join rename mutate
#' @export display_metagraph

display_metagraph <- function(graph) {

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {
    stop("The graph object is not valid.")
  }

  # Validation: Graph object is a property graph
  if (is_property_graph(graph) == FALSE) {
    stop("The graph object is not a property graph.")
  }

  # Create bindings for specific variables
  id <- from <- to <- type <- rel <- label <-
    from_type <- to_type <- NULL

  # Get a distinct list of node `type` values
  unique_node_list <-
    graph$nodes_df %>%
    dplyr::select(type) %>%
    dplyr::distinct()

  # Get a distinct list of edges between types
  unique_edge_list <-
    graph$edges_df %>%
    dplyr::inner_join(
      graph$nodes_df %>%
        dplyr::select(id, type),
      by = c("from" = "id")) %>%
    dplyr::rename(from_type = type) %>%
    dplyr::inner_join(
      graph$nodes_df %>%
        dplyr::select(id, type),
      by = c("to" = "id")) %>%
    dplyr::rename(to_type = type) %>%
    dplyr::select(rel, from_type, to_type) %>%
    dplyr::distinct()

  # Create the initial metagraph
  metagraph <-
    create_graph() %>%
    add_nodes_from_df_cols(
      unique_node_list,
      columns = "type") %>%
    add_edges_from_table(
      unique_edge_list,
      ndf_mapping = "label",
      from_col = "from_type",
      to_col = "to_type",
      rel_col = "rel")

  # Copy the `label` values to the `type` attribute
  metagraph$nodes_df <-
    metagraph$nodes_df %>%
    dplyr::mutate(type = label)

  # Apply coloring and other aesthetics to nodes and edges
  metagraph <-
    metagraph %>%
    colorize_node_attrs(
      "type", "fillcolor", alpha = 80) %>%
    copy_edge_attrs("rel", "label") %>%
    add_global_graph_attrs(
      "fontname", "Helvetica", "edge") %>%
    add_global_graph_attrs(
      "fontcolor", "gray50", "edge") %>%
    add_global_graph_attrs(
      "fontsize", 10, "edge") %>%
    colorize_edge_attrs(
      "rel", "color", palette = "OrRd", alpha = 60) %>%
    add_global_graph_attrs(
      "fontsize", 6, "edge") %>%
    add_global_graph_attrs(
      "len", 3.5, "edge") %>%
    add_global_graph_attrs(
      "layout", "dot", "graph")

  # Render the `metagraph` object
  metagraph %>% render_graph()
}
