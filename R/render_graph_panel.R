#' Render several graphs in a single panel
#' @description Using 2 or more \code{dgr_graph}
#' objects, render theses graph in a single panel.
#' @param ... 2 or more graph objects of class
#' \code{dgr_graph}.
#' @examples
#' \dontrun{
#' # Create 3 random graphs with increasing
#' # numbers of edges
#' graph_1 <-
#'   create_random_graph(
#'     n = 25, m = 10,
#'     set_seed = 23)
#'
#' graph_2 <-
#'   create_random_graph(
#'     n = 25, m = 25,
#'     set_seed = 23)
#'
#' graph_3 <-
#'   create_random_graph(
#'     n = 25, m = 35,
#'     set_seed = 23)
#'
#' # Render all three graphs in the
#' # order specified
#' render_graph_panel(
#'   graph_1, graph_2, graph_3)
#' }
#' @export render_graph_panel

render_graph_panel <- function(...) {

  # Collect graphs into a single list object
  graphs <- list(...)

  # Get the number of columns for the panel
  ncols <- length(graphs)

  for (i in 1:length(graphs)) {
    if (i == 1) coords <- list()

    coords[[i]] <-
      graphs[[i]] %>%
      to_igraph() %>%
      layout_nicely() %>%
      as_tibble() %>%
      rename(x = V1, y = V2)
  }

  for (i in 1:length(coords)) {
    if (i == 1) {
      span_x <- vector(mode = "numeric")
      span_y <- vector(mode = "numeric")
    }

    span_x <-
      c(span_x,
        max(coords[[i]] %>% select(x) %>% flatten_dbl()) -
          min(coords[[i]] %>% select(x) %>% flatten_dbl()))

    span_y <-
      c(span_y,
        max(coords[[i]] %>% select(y) %>% flatten_dbl()) -
          min(coords[[i]] %>% select(y) %>% flatten_dbl()))
  }

  # Get the plot area size for each graph
  plot_area_size <- c(max(span_x), max(span_y))
  max_square <-
    ceiling(max(plot_area_size)) +
    (.15 * (max(plot_area_size)))

  for (i in 1:length(graphs)) {

    if (i == 1) xy_coords <- list()

    xy_coords[[i]] <-
      graphs[[i]] %>%
      to_igraph() %>%
      layout_nicely() %>%
      as_tibble() %>%
      rename(x = V1, y = V2) %>%
      mutate(x = scales::rescale(
        x,
        to = c(max_square / 2 - ((max(x) - min(x)) / 2),
               max_square / 2 + ((max(x) - min(x)) / 2)))) %>% # center x
      mutate(y = scales::rescale(
        y,
        to = c(max_square / 2 - ((max(y) - min(y)) / 2),
               max_square / 2 + ((max(y) - min(y)) / 2)))) # center y
  }

  # Create graph for grid areas
  grid <-
    create_graph() %>%
    add_global_graph_attrs(attr = "shape", value = "plaintext", attr_type = "node") %>%
    add_global_graph_attrs(attr = "width", value = 0.001, attr_type = "node") %>%
    add_global_graph_attrs(attr = "height", value = 0.001, attr_type = "node") %>%
    add_n_nodes(4, type = "plot_areas") %>%
    set_node_position(1, x = 0, y = 0) %>%
    set_node_position(2, x = 0, y = max_square * nrows) %>%
    set_node_position(3, x = max_square * ncols, y = max_square * nrows) %>%
    set_node_position(4, x = max_square * ncols, y = 0) %>%
    select_nodes() %>%
    set_node_attrs_ws(node_attr = "label", value = " ") %>%
    set_node_attrs_ws(node_attr = "shape", value = "plaintext") %>%
    add_edges_w_string(edges = "1->2 2->3 3->4 4->1") %>%
    select_edges() %>%
    set_edge_attrs_ws(edge_attr = "arrowhead", value = "none") %>%
    clear_selection()

  # Create a grid for the graph panels
  for (i in 1:ncols) {

    grid <-
      grid %>%
      add_node(
        1,
        type = paste0("plot_sep_line_v_top_", i)) %>%
      select_last_nodes_created() %>%
      set_node_attrs_ws(node_attr = "label", value = " ") %>%
      set_node_attrs_ws(node_attr = "shape", value = "plaintext") %>%
      set_node_attrs_ws(node_attr = "x", value = i * max_square)  %>%
      set_node_attrs_ws(node_attr = "y", value = max_square) %>%
      clear_selection() %>%
      add_node(
        1,
        type = paste0("plot_sep_line_v_bottom_", i)) %>%
      select_last_nodes_created() %>%
      set_node_attrs_ws(node_attr = "label", value = " ") %>%
      set_node_attrs_ws(node_attr = "shape", value = "plaintext") %>%
      set_node_attrs_ws(node_attr = "x", value = i * max_square) %>%
      set_node_attrs_ws(node_attr = "y", value = 0) %>%
      clear_selection() %>%
      add_edge(
        from = get_node_ids(., conditions = paste0("type == 'plot_sep_line_v_top_", i, "'")),
        to = get_node_ids(., conditions = paste0("type == 'plot_sep_line_v_bottom_", i, "'"))) %>%
      select_last_edges_created() %>%
      set_edge_attrs_ws(edge_attr = "arrowhead", value = "none") %>%
      clear_selection()
  }

  # Translate node positions for each graph
  for (i in 1:length(graphs)) {

    graphs[[i]]$nodes_df <-
      graphs[[i]]$nodes_df %>%
      bind_cols(xy_coords[[i]]) %>%
      mutate(x = x + (max_square * (i - 1)))
  }


  # Combine graphs into one
  for (i in 1:(length(graphs) - 1)) {

    if (i == 1) {
      combo_graph <-
        combine_graphs(
          graphs[[i]],
          graphs[[i + 1]]
        )
    }

    if (i > 1) {
      combo_graph <-
        combine_graphs(
          combo_graph,
          graphs[[i + 1]]
        )
    }
  }

  combo_graph <-
    combine_graphs(combo_graph, grid) %>%
    select_nodes(conditions = "grepl('plot_', type)") %>%
    set_node_attrs_ws(node_attr = "width", value = 0.001) %>%
    set_node_attrs_ws(node_attr = "height", value = 0.001) %>%
    invert_selection() %>%
    set_node_attrs_ws(
      node_attr = "width", value = get_global_graph_attrs(.) %>%
        filter(attr == "width") %>% .$value) %>%
    set_node_attrs_ws(
      node_attr = "height", value = get_global_graph_attrs(.) %>%
        filter(attr == "width") %>% .$value)

  render_graph(combo_graph)
}
