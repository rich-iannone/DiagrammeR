#' Display a property graph's underlying model
#'
#' With a graph object of class \code{dgr_graph} that is also a property graph
#'   (i.e., all nodes have an assigned \code{type} value and all edges have an
#'   assigned \code{rel} value), display its metagraph in the RStudio Viewer.
#'   This representation provides all combinations of edges of different
#'   \code{rel} values to all nodes with distinct \code{type} values, including
#'   any edges to nodes of the same \code{type} (shown as loops). The
#'   precondition of the graph being a property graph can be verified by using
#'   the \code{\link{is_property_graph}()} function.
#' @param graph a graph object of class \code{dgr_graph}. This graph must
#'   fulfill the condition of being a property graph, otherwise the function
#'   yields an error.
#' @examples
#' \dontrun{
#' # Create a randomized property
#' # graph with 1000 nodes and 1350 edges
#' property_graph <-
#'   create_graph() %>%
#'   add_gnm_graph(
#'     n = 1000,
#'     m = 1350,
#'     set_seed = 23) %>%
#'   select_nodes_by_degree(
#'     expressions = "deg >= 3") %>%
#'   set_node_attrs_ws(
#'     node_attr = type,
#'     value = "a") %>%
#'   clear_selection() %>%
#'   select_nodes_by_degree(
#'     expressions = "deg < 3") %>%
#'   set_node_attrs_ws(
#'     node_attr = type,
#'     value = "b") %>%
#'   clear_selection() %>%
#'   select_nodes_by_degree(
#'     expressions = "deg == 0") %>%
#'   set_node_attrs_ws(
#'     node_attr = type,
#'     value = "c") %>%
#'   set_node_attr_to_display(
#'     attr = type) %>%
#'   select_edges_by_node_id(
#'     nodes =
#'       get_node_ids(.) %>%
#'       sample(
#'         size = 0.15 * length(.) %>%
#'           floor())) %>%
#'   set_edge_attrs_ws(
#'     edge_attr = rel,
#'     value = "r_1") %>%
#'   invert_selection() %>%
#'   set_edge_attrs_ws(
#'     edge_attr = rel,
#'     value = "r_2") %>%
#'   clear_selection() %>%
#'   copy_edge_attrs(
#'     edge_attr_from = rel,
#'     edge_attr_to = label) %>%
#'   add_global_graph_attrs(
#'     attr = "fontname",
#'     value = "Helvetica",
#'     attr_type = "edge") %>%
#'   add_global_graph_attrs(
#'     attr = "fontcolor",
#'     value = "gray50",
#'     attr_type = "edge") %>%
#'   add_global_graph_attrs(
#'     attr = "fontsize",
#'     value = 10,
#'     attr_type = "edge")
#'
#' # Display this graph's
#' # metagraph, or, the underlying
#' # graph model for a property graph
#' display_metagraph(property_graph)
#' }
#' @importFrom dplyr select distinct inner_join rename mutate
#' @export
display_metagraph <- function(graph) {

  # Get the name of the function
  fcn_name <- get_calling_fcn()

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {

    emit_error(
      fcn_name = fcn_name,
      reasons = "The graph object is not valid")
  }

  # Validation: Graph object is a property graph
  if (is_property_graph(graph) == FALSE) {

    emit_error(
      fcn_name = fcn_name,
      reasons = "The graph object is not a property graph")
  }

  # Create bindings for specific variables
  # id <- from <- to <- type <- rel <- label <-
  #   from_type <- to_type <- NULL

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
      from_to_map = label,
      from_col = from_type,
      to_col = to_type,
      rel_col = rel)

  # Copy the `label` values to the `type` attribute
  metagraph$nodes_df <-
    metagraph$nodes_df %>%
    dplyr::mutate(type = label)

  # Apply coloring and other aesthetics to nodes and edges
  metagraph <-
    metagraph %>%
    colorize_node_attrs(
      node_attr_from = "type",
      node_attr_to = "fillcolor") %>%
    copy_edge_attrs(
      edge_attr_from = "rel",
      edge_attr_to = "label") %>%
    add_global_graph_attrs(
      attr = "fontname",
      value = "Helvetica",
      attr_type = "edge") %>%
    add_global_graph_attrs(
      attr = "fontcolor",
      value = "gray50",
      attr_type = "edge") %>%
    add_global_graph_attrs(
      attr = "fontsize",
      value = 10,
      attr_type = "edge") %>%
    colorize_edge_attrs(
      edge_attr_from = "rel",
      edge_attr_to = "color") %>%
    add_global_graph_attrs(
      attr = "fontsize",
      value = 6,
      attr_type = "edge") %>%
    add_global_graph_attrs(
      attr = "len",
      value = 3.5,
      attr_type = "edge") %>%
    add_global_graph_attrs(
      attr = "layout",
      value = "dot",
      attr_type = "graph")

  # Render the `metagraph` object
  metagraph %>% render_graph()
}
